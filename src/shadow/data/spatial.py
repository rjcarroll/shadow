"""Build spatial weights matrix and add spatial lag predictors.

Replaces R scripts 10-makeWpol.R and 11-addSpatial.R.

For each year, constructs a row-normalised polity-similarity weight matrix W
where W[i,j] = 1 / |polity2_i − polity2_j|  (0 on diagonal; 0 if equal).
Higher weight → more similar polity score → stronger strategic influence.

10 spatial lag variables per onset row:
  spat_gov      weighted fraction of other potential interveners coded gov-biased
  spat_opp      weighted fraction coded opp-biased
  spat_US_G     1 if USA (002) is gov-biased in this conflict
  spat_USSR_G   1 if USSR/Russia (364) is gov-biased
  spat_US_O     1 if USA is opp-biased
  spat_USSR_O   1 if USSR/Russia is opp-biased
  spat_US_USRG  1 if B==USA and USSR is gov-biased  (interaction)
  spat_US_USRO  1 if B==USA and USSR is opp-biased
  spat_USR_USG  1 if B==USSR and USA is gov-biased
  spat_USR_USO  1 if B==USSR and USA is opp-biased

Output per DD file: data/interim/dd_spat_{cy}_{ud}.parquet
"""

from __future__ import annotations

import numpy as np
import pandas as pd

USA  = "002"
USSR = "364"

SPAT_COLS = [
    "spat_gov", "spat_opp",
    "spat_US_G", "spat_USSR_G", "spat_US_O", "spat_USSR_O",
    "spat_US_USRG", "spat_US_USRO", "spat_USR_USG", "spat_USR_USO",
]


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _build_W(polity: pd.Series) -> pd.DataFrame:
    """
    Build a row-normalised polity-similarity W matrix.

    Parameters
    ----------
    polity : pd.Series
        Index = ccode (str), values = polity2 score (float).
        Rows with NaN polity are excluded before computation.

    Returns
    -------
    pd.DataFrame  (n × n, index = columns = ccode)
        Row-normalised W matrix. If all rows have the same polity score
        (or n < 2), returns a zero matrix.
    """
    p = polity.dropna()
    if len(p) < 2:
        return pd.DataFrame(index=p.index, columns=p.index, dtype=float).fillna(0.0)

    vals = p.values.astype(float)
    # W[i,j] = 1 / |p_i - p_j|; 0 if equal or on diagonal.
    with np.errstate(divide="ignore", invalid="ignore"):
        diff = np.abs(vals[:, None] - vals[None, :])
        W = np.where(diff == 0.0, 0.0, 1.0 / diff)
    np.fill_diagonal(W, 0.0)

    # Row-normalise; rows that sum to 0 remain 0.
    row_sums = W.sum(axis=1, keepdims=True)
    row_sums[row_sums == 0.0] = 1.0
    W /= row_sums

    return pd.DataFrame(W, index=p.index, columns=p.index)


def _spat_for_onset(
    W_year: pd.DataFrame,
    B_ccodes: np.ndarray,
    intervention: np.ndarray,
    A_ccode: str,
) -> pd.DataFrame:
    """
    Compute 10 spatial lag columns for one onset (A, year).

    Parameters
    ----------
    W_year : pd.DataFrame
        Full W matrix for the year (index = columns = ccode).
    B_ccodes : np.ndarray of str
        Ordered ccode_B values for all potential interveners in this onset.
    intervention : np.ndarray of float/int
        intervention codes aligned to B_ccodes (0=none, 1=gov, 2=opp, NaN=non-onset).
    A_ccode : str
        Host country code. Removed from W before computation.

    Returns
    -------
    pd.DataFrame with SPAT_COLS as columns, indexed by B_ccodes.
    """
    out = pd.DataFrame(
        {c: np.zeros(len(B_ccodes)) for c in SPAT_COLS},
        index=B_ccodes,
    )

    # Remove A's row/col from W (A cannot intervene in its own conflict).
    W = W_year.drop(index=A_ccode, columns=A_ccode, errors="ignore")

    if W.empty or len(W) < 2:
        return out

    # Align B_ccodes to W's index (only B states that have polity data in W).
    b_in_W = [b for b in B_ccodes if b in W.index]
    if not b_in_W:
        return out

    # Intervention vector aligned to W's column order.
    int_series = pd.Series(intervention, index=B_ccodes, dtype=float)
    int_aligned = int_series.reindex(W.columns).fillna(0.0).values

    gov_vec = (int_aligned == 1).astype(float)
    opp_vec = (int_aligned == 2).astype(float)

    spat_gov = W.values @ gov_vec   # shape (n_W,)
    spat_opp = W.values @ opp_vec

    # Map results back to B_ccodes in W.
    spat_gov_s = pd.Series(spat_gov, index=W.index)
    spat_opp_s = pd.Series(spat_opp, index=W.index)

    out.loc[b_in_W, "spat_gov"] = spat_gov_s.reindex(b_in_W).values
    out.loc[b_in_W, "spat_opp"] = spat_opp_s.reindex(b_in_W).values

    # Superpower intervention indicators (conflict-level scalars, broadcast to all B).
    int_map = int_series.reindex(W.index).fillna(0.0)

    usa_g  = 1 if int_map.get(USA,  0) == 1 else 0
    usa_o  = 1 if int_map.get(USA,  0) == 2 else 0
    ussr_g = 1 if int_map.get(USSR, 0) == 1 else 0
    ussr_o = 1 if int_map.get(USSR, 0) == 2 else 0

    out["spat_US_G"]   = usa_g
    out["spat_USSR_G"] = ussr_g
    out["spat_US_O"]   = usa_o
    out["spat_USSR_O"] = ussr_o

    # Cross-terms: each B's own-identity when the OTHER superpower took a side.
    out["spat_US_USRG"] = ((out.index == USA)  & (ussr_g == 1)).astype(int)
    out["spat_US_USRO"] = ((out.index == USA)  & (ussr_o == 1)).astype(int)
    out["spat_USR_USG"] = ((out.index == USSR) & (usa_g  == 1)).astype(int)
    out["spat_USR_USO"] = ((out.index == USSR) & (usa_o  == 1)).astype(int)

    # Self-referential zeroing (R lines 102-105):
    # USA's own spatial effect on itself, USSR's own on itself.
    if USA in out.index:
        out.loc[USA, ["spat_US_G", "spat_US_O"]] = 0
    if USSR in out.index:
        out.loc[USSR, ["spat_USSR_G", "spat_USSR_O"]] = 0

    return out


# ---------------------------------------------------------------------------
# Probability-based helpers (for Nash fixed-point iteration)
# ---------------------------------------------------------------------------

def _spat_for_onset_proba(
    W_year: pd.DataFrame,
    B_ccodes: np.ndarray,
    p_gov: np.ndarray,
    p_opp: np.ndarray,
    A_ccode: str,
) -> pd.DataFrame:
    """
    Compute 10 spatial lag columns from predicted probability vectors.

    Nash fixed-point variant of _spat_for_onset: replaces hard intervention
    indicators with predicted probabilities, so that
        spat_gov_B = Σ_{B'≠B} W_{BB'} · P(B' gov-biased)
    and superpower scalars become probability-weighted (e.g. spat_US_G =
    P(USA gov-biased) for this conflict).

    Parameters
    ----------
    W_year : pd.DataFrame  —  row-normalised W matrix for the year.
    B_ccodes : np.ndarray of str  —  potential intervener ccodes.
    p_gov, p_opp : np.ndarray  —  predicted P(gov) and P(opp), aligned to B_ccodes.
    A_ccode : str  —  host country (excluded from W).
    """
    out = pd.DataFrame(
        {c: np.zeros(len(B_ccodes)) for c in SPAT_COLS},
        index=B_ccodes,
    )

    W = W_year.drop(index=A_ccode, columns=A_ccode, errors="ignore")
    if W.empty or len(W) < 2:
        return out

    b_in_W = [b for b in B_ccodes if b in W.index]
    if not b_in_W:
        return out

    # Align probability vectors to W's column order.
    pg_s = pd.Series(p_gov, index=B_ccodes)
    po_s = pd.Series(p_opp, index=B_ccodes)
    pg_aligned = pg_s.reindex(W.columns).fillna(0.0).values
    po_aligned = po_s.reindex(W.columns).fillna(0.0).values

    spat_gov = W.values @ pg_aligned
    spat_opp = W.values @ po_aligned

    out.loc[b_in_W, "spat_gov"] = pd.Series(spat_gov, index=W.index).reindex(b_in_W).values
    out.loc[b_in_W, "spat_opp"] = pd.Series(spat_opp, index=W.index).reindex(b_in_W).values

    # Superpower scalars: soft probability values broadcast to all B.
    pg_map = pg_s.reindex(W.index).fillna(0.0)
    po_map = po_s.reindex(W.index).fillna(0.0)

    usa_g  = float(pg_map.get(USA,  0.0))
    usa_o  = float(po_map.get(USA,  0.0))
    ussr_g = float(pg_map.get(USSR, 0.0))
    ussr_o = float(po_map.get(USSR, 0.0))

    out["spat_US_G"]   = usa_g
    out["spat_USSR_G"] = ussr_g
    out["spat_US_O"]   = usa_o
    out["spat_USSR_O"] = ussr_o

    # Interaction terms: B's identity weighted by the other superpower's probability.
    out["spat_US_USRG"] = (out.index == USA).astype(float)  * ussr_g
    out["spat_US_USRO"] = (out.index == USA).astype(float)  * ussr_o
    out["spat_USR_USG"] = (out.index == USSR).astype(float) * usa_g
    out["spat_USR_USO"] = (out.index == USSR).astype(float) * usa_o

    # Self-referential zeroing: a state's own action doesn't appear in its own lag.
    if USA in out.index:
        out.loc[USA, ["spat_US_G", "spat_US_O"]] = 0.0
    if USSR in out.index:
        out.loc[USSR, ["spat_USSR_G", "spat_USSR_O"]] = 0.0

    return out


def _build_W_cache(dd: pd.DataFrame, onset_mask: pd.Series) -> dict:
    """Build year-indexed W matrix cache from polity2_B in dd."""
    pol_lookup = (
        dd[["ccode_B", "year", "polity2_B"]]
        .dropna(subset=["polity2_B"])
        .groupby(["year", "ccode_B"])["polity2_B"]
        .first()
    )
    W_cache: dict[int, pd.DataFrame] = {}
    for yr in dd.loc[onset_mask, "year"].unique():
        if yr in pol_lookup.index.get_level_values("year"):
            pol_yr = pol_lookup.loc[yr]
        else:
            pol_yr = pd.Series(dtype=float)
        W_cache[yr] = _build_W(pol_yr)
    return W_cache


def update_spatial_lags_proba(
    dd: pd.DataFrame,
    p_gov: np.ndarray,
    p_opp: np.ndarray,
    W_cache: dict | None = None,
    onset_only: bool = True,
) -> pd.DataFrame:
    """
    Recompute SPAT_COLS using predicted probability vectors.

    Used for Nash fixed-point iteration in notebooks 05 and 06.  Replaces
    hard intervention indicators with predicted probabilities so that spatial
    lags reflect the classifier's own equilibrium predictions.

    Parameters
    ----------
    dd : pd.DataFrame
        Must have: ccode_A, ccode_B, year, polity2_B, onset_A.
        p_gov / p_opp must be aligned to dd's row order.
    p_gov, p_opp : np.ndarray
        Predicted P(gov) and P(opp) for every row in dd.
    W_cache : dict | None
        Optional pre-built {year: W_DataFrame} cache.  Computed from dd if
        not supplied (pass explicitly to avoid rebuilding on every iteration).
    onset_only : bool, default True
        If True (training mode), only update rows where onset_A == 1 and
        leave non-onset rows as NaN.
        If False (universal prediction mode), update ALL (A, year) groups so
        that the fixed-point equilibrium holds for the full model, not just
        the observed-conflict subset.  Non-onset rows represent counterfactual
        conflict scenarios; the equilibrium is a property of the model, not
        the sample.

    Returns
    -------
    dd copy with SPAT_COLS updated; behaviour on non-onset rows depends on
    onset_only.
    """
    dd = dd.copy()
    for col in SPAT_COLS:
        dd[col] = np.nan

    compute_mask = ~((dd["year"] == 1990) & (dd["ccode_B"] == "678"))
    compute_mask &= ~((dd["year"] == 1990) & (dd["ccode_A"] == "678"))

    if onset_only:
        update_mask = (dd["onset_A"] == 1) & compute_mask
    else:
        update_mask = compute_mask   # all rows (universal fixed-point)

    if update_mask.sum() == 0:
        return dd

    if W_cache is None:
        W_cache = _build_W_cache(dd, update_mask)

    p_gov_s = pd.Series(p_gov, index=dd.index)
    p_opp_s = pd.Series(p_opp, index=dd.index)

    update_df = dd.loc[update_mask]
    spat_rows = []
    for (A_ccode, year), grp in update_df.groupby(["ccode_A", "year"], sort=False):
        W_yr = W_cache.get(year, pd.DataFrame())
        if W_yr.empty:
            spat_rows.append(pd.DataFrame({c: np.nan for c in SPAT_COLS}, index=grp.index))
            continue

        B_ccodes = grp["ccode_B"].values
        pg = p_gov_s.reindex(grp.index).fillna(0.0).values
        po = p_opp_s.reindex(grp.index).fillna(0.0).values

        spat = _spat_for_onset_proba(W_yr, B_ccodes, pg, po, A_ccode)
        spat.index = grp.index
        spat_rows.append(spat)

    if spat_rows:
        spat_all = pd.concat(spat_rows)
        dd.loc[spat_all.index, SPAT_COLS] = spat_all[SPAT_COLS].values

    return dd


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def add_spatial_lags(dd: pd.DataFrame) -> pd.DataFrame:
    """
    Add 10 spatial lag columns to an intervention-coded directed-dyad file.

    Replicates 10-makeWpol.R + 11-addSpatial.R.

    Parameters
    ----------
    dd : pd.DataFrame
        Output of :func:`~shadow.data.interventions.code_interventions`.
        Must have columns: ccode_A, ccode_B, year, polity2_B, onset_A,
        intervention, ddyear.

    Returns
    -------
    dd with SPAT_COLS appended. Non-onset rows have NaN for all spat_* columns.
    """
    dd = dd.copy()
    for col in SPAT_COLS:
        dd[col] = np.nan

    # R line 48-50 of 11-addSpatial.R: drop ccode 678 (Yemen AR) in 1990.
    # Treated as a filter only within the spatial computation; the rows remain
    # in the file but their spat_* columns stay NaN.
    compute_mask = ~((dd["year"] == 1990) & (dd["ccode_B"] == "678"))
    compute_mask &= ~((dd["year"] == 1990) & (dd["ccode_A"] == "678"))

    # Only onset rows get spatial lags.
    onset_mask = (dd["onset_A"] == 1) & compute_mask
    if onset_mask.sum() == 0:
        return dd

    # ── Build W matrices per year ──────────────────────────────────────────
    # polity2_B is a property of (ccode_B, year); take the first non-NaN
    # value for each (ccode_B, year) pair.
    pol_lookup = (
        dd[["ccode_B", "year", "polity2_B"]]
        .dropna(subset=["polity2_B"])
        .groupby(["year", "ccode_B"])["polity2_B"]
        .first()
    )   # MultiIndex (year, ccode_B) → polity2

    onset_years = dd.loc[onset_mask, "year"].unique()
    W_cache: dict[int, pd.DataFrame] = {}
    for yr in onset_years:
        if yr in pol_lookup.index.get_level_values("year"):
            pol_yr = pol_lookup.loc[yr]     # Series: ccode_B → polity2
        else:
            pol_yr = pd.Series(dtype=float)
        W_cache[yr] = _build_W(pol_yr)

    # ── Compute spatial lags per onset (A, year) ──────────────────────────
    onset_df = dd.loc[onset_mask].copy()

    # Group by (ccode_A, year) — each unique onset event.
    spat_rows = []
    for (A_ccode, year), grp in onset_df.groupby(["ccode_A", "year"], sort=False):
        W_yr = W_cache.get(year, pd.DataFrame())
        if W_yr.empty:
            spat_rows.append(
                pd.DataFrame(
                    {c: np.nan for c in SPAT_COLS},
                    index=grp.index,
                )
            )
            continue

        B_ccodes    = grp["ccode_B"].values
        intervention = grp["intervention"].fillna(0).values

        spat = _spat_for_onset(W_yr, B_ccodes, intervention, A_ccode)
        # Re-index to the original DataFrame index for easy assignment.
        spat.index = grp.index
        spat_rows.append(spat)

    if spat_rows:
        spat_all = pd.concat(spat_rows)
        dd.loc[spat_all.index, SPAT_COLS] = spat_all[SPAT_COLS].values

    return dd
