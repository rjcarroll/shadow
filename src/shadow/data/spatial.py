"""Build spatial weights matrices and add spatial lag predictors.

Replaces R scripts 10-makeWpol.R and 11-addSpatial.R.

Original 10 spatial lag variables (polity-similarity W + superpower indicators):
  spat_gov, spat_opp, spat_US_G, spat_USSR_G, spat_US_O, spat_USSR_O,
  spat_US_USRG, spat_US_USRO, spat_USR_USG, spat_USR_USO

New 10 spatial lag variables (era-invariant mechanisms):
  spat_region_gov/opp   — same-region W (binary, row-normalised)
  spat_igo_gov/opp      — shared-IGO-membership W (count, row-normalised)
  spat_chain_gov/opp    — contiguity chain (both B and B' border host A)
  spat_ideal_gov/opp    — UNGA ideal-point proximity W (1/|diff|)
  spat_P5_gov/opp       — mean P5 member predictions (G&M 2022)

Output per DD file: data/interim/dd_spat_{cy}_{ud}.parquet
"""

from __future__ import annotations

import numpy as np
import pandas as pd

USA  = "002"
USSR = "364"
P5_CODES = frozenset({"002", "200", "220", "364", "710"})

REGION_COLS_B = [
    "reg_asia_B", "reg_eeurope_B", "reg_latinAmerica_B",
    "reg_mena_B", "reg_western_B",
]
REGION_NAMES = ["asia", "eeurope", "latinAmerica", "mena", "western"]

SPAT_COLS = [
    "spat_gov", "spat_opp",
    "spat_US_G", "spat_USSR_G", "spat_US_O", "spat_USSR_O",
    "spat_US_USRG", "spat_US_USRO", "spat_USR_USG", "spat_USR_USO",
]

NEW_SPAT_COLS = [
    "spat_region_gov", "spat_region_opp",
    "spat_igo_gov", "spat_igo_opp",
    "spat_chain_gov", "spat_chain_opp",
    "spat_ideal_gov", "spat_ideal_opp",
    "spat_P5_gov", "spat_P5_opp",
]

ALL_SPAT_COLS = SPAT_COLS + NEW_SPAT_COLS


# ---------------------------------------------------------------------------
# W matrix builders
# ---------------------------------------------------------------------------

def _row_normalise(W: np.ndarray) -> np.ndarray:
    """Row-normalise in place; rows summing to 0 remain 0."""
    row_sums = W.sum(axis=1, keepdims=True)
    row_sums[row_sums == 0.0] = 1.0
    W /= row_sums
    return W


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

    return pd.DataFrame(_row_normalise(W), index=p.index, columns=p.index)


def _build_W_proximity(values: pd.Series) -> pd.DataFrame:
    """Row-normalised proximity W: W[i,j] = 1/|v_i - v_j|.  Same as polity W."""
    return _build_W(values)


def _build_W_binary(labels: pd.Series) -> pd.DataFrame:
    """Row-normalised binary W: W[i,j] = 1 if labels[i] == labels[j]."""
    labs = labels.dropna()
    if len(labs) < 2:
        return pd.DataFrame(index=labs.index, columns=labs.index, dtype=float).fillna(0.0)
    vals = labs.values
    W = (vals[:, None] == vals[None, :]).astype(float)
    np.fill_diagonal(W, 0.0)
    return pd.DataFrame(_row_normalise(W), index=labs.index, columns=labs.index)


def _extract_region_labels(ccode_attrs: pd.DataFrame) -> pd.Series:
    """Extract region label per ccode from reg_*_B one-hot dummies.

    Parameters
    ----------
    ccode_attrs : pd.DataFrame
        Index = ccode_B, columns include reg_*_B.

    Returns
    -------
    pd.Series : ccode → region name (str).  Missing regions → "other".
    """
    region = pd.Series("other", index=ccode_attrs.index, dtype=object)
    for name, col in zip(REGION_NAMES, REGION_COLS_B):
        if col in ccode_attrs.columns:
            mask = ccode_attrs[col] == 1
            region.loc[mask] = name
    return region


# ---------------------------------------------------------------------------
# Shared spatial lag helper
# ---------------------------------------------------------------------------

def _w_lag(
    W_full: pd.DataFrame,
    B_ccodes: np.ndarray,
    gov_vec: np.ndarray,
    opp_vec: np.ndarray,
    A_ccode: str,
) -> tuple[np.ndarray, np.ndarray]:
    """Compute W × gov_vec and W × opp_vec, aligned to B_ccodes.

    Drops host country A from W before multiplication.
    Returns (spat_gov, spat_opp) as numpy arrays.
    """
    n = len(B_ccodes)
    W = W_full.drop(index=A_ccode, columns=A_ccode, errors="ignore")
    if W.empty or len(W) < 2:
        return np.zeros(n), np.zeros(n)

    b_in_W = [b for b in B_ccodes if b in W.index]
    if not b_in_W:
        return np.zeros(n), np.zeros(n)

    gv = pd.Series(gov_vec, index=B_ccodes).reindex(W.columns, fill_value=0.0).values
    ov = pd.Series(opp_vec, index=B_ccodes).reindex(W.columns, fill_value=0.0).values

    sg = pd.Series(W.values @ gv, index=W.index)
    so = pd.Series(W.values @ ov, index=W.index)

    return (
        sg.reindex(B_ccodes, fill_value=0.0).values,
        so.reindex(B_ccodes, fill_value=0.0).values,
    )


# ---------------------------------------------------------------------------
# Original helpers (polity W + superpower indicators) — unchanged
# ---------------------------------------------------------------------------

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


# ---------------------------------------------------------------------------
# New spatial lags (era-invariant mechanisms)
# ---------------------------------------------------------------------------

def _new_spat(
    W_yr: dict,
    B_ccodes: np.ndarray,
    gov_vec: np.ndarray,
    opp_vec: np.ndarray,
    A_ccode: str,
    contiguity: np.ndarray,
    is_P5: np.ndarray,
) -> pd.DataFrame:
    """
    Compute NEW_SPAT_COLS for one onset (A, year) group.

    Parameters
    ----------
    W_yr : dict
        {"polity": W, "region": W, "ideal": W, "igo": W} for this year.
    gov_vec, opp_vec : np.ndarray
        Either hard-coded (0/1) or predicted probabilities, aligned to B_ccodes.
    contiguity : np.ndarray
        ud_conttype values per B (1-5 = contiguous, 6 = not).
    is_P5 : np.ndarray
        is_P5_B values per B (0 or 1).
    """
    n = len(B_ccodes)
    out = pd.DataFrame(
        {c: np.zeros(n) for c in NEW_SPAT_COLS},
        index=B_ccodes,
    )

    # ── W-based lags (region, IGO, ideal) ──────────────────────────────
    for prefix, wtype in [("region", "region"), ("igo", "igo"), ("ideal", "ideal")]:
        W = W_yr.get(wtype, pd.DataFrame())
        if not W.empty and len(W) >= 2:
            sg, so = _w_lag(W, B_ccodes, gov_vec, opp_vec, A_ccode)
            out[f"spat_{prefix}_gov"] = sg
            out[f"spat_{prefix}_opp"] = so

    # ── Contiguity chain W ─────────────────────────────────────────────
    contig_mask = contiguity <= 5
    if contig_mask.sum() >= 2:
        c = contig_mask.astype(float)
        W_chain = np.outer(c, c)
        np.fill_diagonal(W_chain, 0.0)
        _row_normalise(W_chain)
        out["spat_chain_gov"] = W_chain @ gov_vec
        out["spat_chain_opp"] = W_chain @ opp_vec

    # ── P5 spatial lag (leave-one-out for P5 members) ──────────────────
    p5_mask = np.array([b in P5_CODES for b in B_ccodes])
    n_p5 = int(p5_mask.sum())
    if n_p5 > 0:
        p5_sum_g = float(gov_vec[p5_mask].sum())
        p5_sum_o = float(opp_vec[p5_mask].sum())

        # All rows get mean over all P5 members
        spat_p5_g = np.full(n, p5_sum_g / n_p5)
        spat_p5_o = np.full(n, p5_sum_o / n_p5)

        # P5 members get leave-one-out mean
        if n_p5 > 1:
            spat_p5_g[p5_mask] = (p5_sum_g - gov_vec[p5_mask]) / (n_p5 - 1)
            spat_p5_o[p5_mask] = (p5_sum_o - opp_vec[p5_mask]) / (n_p5 - 1)
        else:
            spat_p5_g[p5_mask] = 0.0
            spat_p5_o[p5_mask] = 0.0

        out["spat_P5_gov"] = spat_p5_g
        out["spat_P5_opp"] = spat_p5_o

    return out


# ---------------------------------------------------------------------------
# W cache (multi-type: polity + region + ideal + IGO)
# ---------------------------------------------------------------------------

def _build_W_cache(dd: pd.DataFrame, onset_mask: pd.Series) -> dict:
    """Build year-indexed multi-type W matrix cache from dd.

    Returns
    -------
    dict : {year: {"polity": W, "region": W, "ideal": W, "igo": W}}
    """
    # Per-ccode-year attributes (take first per (ccode_B, year))
    attr_cols = ["ccode_B", "year", "polity2_B"]
    for c in REGION_COLS_B:
        if c in dd.columns:
            attr_cols.append(c)
    if "ideal_point_B" in dd.columns:
        attr_cols.append("ideal_point_B")

    ccode_attrs = (
        dd[attr_cols]
        .dropna(subset=["ccode_B"])
        .groupby(["year", "ccode_B"])
        .first()
    )

    # Dyadic IGO lookup
    has_igo = "igo_shared" in dd.columns
    igo_lookup = None
    if has_igo:
        igo_lookup = (
            dd[["year", "ccode_A", "ccode_B", "igo_shared"]]
            .dropna(subset=["igo_shared"])
            .groupby(["year", "ccode_A", "ccode_B"])["igo_shared"]
            .first()
        )

    onset_years = dd.loc[onset_mask, "year"].unique()
    cache: dict[int, dict[str, pd.DataFrame]] = {}

    for yr in onset_years:
        yr_cache: dict[str, pd.DataFrame] = {}

        if yr in ccode_attrs.index.get_level_values("year"):
            yr_attrs = ccode_attrs.loc[yr]
        else:
            yr_attrs = pd.DataFrame()

        # Polity W
        if not yr_attrs.empty and "polity2_B" in yr_attrs.columns:
            yr_cache["polity"] = _build_W(yr_attrs["polity2_B"])
        else:
            yr_cache["polity"] = pd.DataFrame()

        # Region W
        if not yr_attrs.empty and any(c in yr_attrs.columns for c in REGION_COLS_B):
            yr_cache["region"] = _build_W_binary(_extract_region_labels(yr_attrs))
        else:
            yr_cache["region"] = pd.DataFrame()

        # Ideal point W
        if not yr_attrs.empty and "ideal_point_B" in yr_attrs.columns:
            yr_cache["ideal"] = _build_W_proximity(yr_attrs["ideal_point_B"])
        else:
            yr_cache["ideal"] = pd.DataFrame()

        # IGO W
        if (igo_lookup is not None
                and yr in igo_lookup.index.get_level_values("year")):
            yr_igo = igo_lookup.loc[yr]
            W_igo_raw = yr_igo.unstack(fill_value=0.0)
            all_codes = sorted(set(W_igo_raw.index) | set(W_igo_raw.columns))
            W_igo_raw = W_igo_raw.reindex(
                index=all_codes, columns=all_codes, fill_value=0.0
            )
            W_arr = W_igo_raw.values.astype(float)
            np.fill_diagonal(W_arr, 0.0)
            yr_cache["igo"] = pd.DataFrame(
                _row_normalise(W_arr.copy()),
                index=all_codes, columns=all_codes,
            )
        else:
            yr_cache["igo"] = pd.DataFrame()

        cache[yr] = yr_cache

    return cache


# ---------------------------------------------------------------------------
# Probability-based update (Nash fixed-point iteration)
# ---------------------------------------------------------------------------

def update_spatial_lags_proba(
    dd: pd.DataFrame,
    p_gov: np.ndarray,
    p_opp: np.ndarray,
    W_cache: dict | None = None,
    onset_only: bool = True,
) -> pd.DataFrame:
    """
    Recompute ALL_SPAT_COLS using predicted probability vectors.

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
        Optional pre-built {year: {"polity": W, ...}} cache.  Computed from
        dd if not supplied (pass explicitly to avoid rebuilding each iteration).
    onset_only : bool, default True
        If True (training mode), only update rows where onset_A == 1.
        If False (universal prediction mode), update ALL (A, year) groups.

    Returns
    -------
    dd copy with ALL_SPAT_COLS updated.
    """
    dd = dd.copy()
    for col in ALL_SPAT_COLS:
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
    new_spat_rows = []

    for (A_ccode, year), grp in update_df.groupby(["ccode_A", "year"], sort=False):
        W_yr = W_cache.get(year, {})
        # Backward compat: old cache format was {year: W_df}
        if isinstance(W_yr, pd.DataFrame):
            W_polity = W_yr
            W_yr_dict = {"polity": W_yr}
        else:
            W_polity = W_yr.get("polity", pd.DataFrame())
            W_yr_dict = W_yr

        B_ccodes = grp["ccode_B"].values
        pg = p_gov_s.reindex(grp.index).fillna(0.0).values
        po = p_opp_s.reindex(grp.index).fillna(0.0).values

        if W_polity.empty:
            spat_rows.append(pd.DataFrame({c: np.nan for c in SPAT_COLS}, index=grp.index))
        else:
            spat = _spat_for_onset_proba(W_polity, B_ccodes, pg, po, A_ccode)
            spat.index = grp.index
            spat_rows.append(spat)

        # New spatial lags
        contiguity = (grp["ud_conttype"].fillna(6).values
                      if "ud_conttype" in grp.columns
                      else np.full(len(grp), 6.0))
        is_P5_arr = (grp["is_P5_B"].fillna(0).values
                     if "is_P5_B" in grp.columns
                     else np.zeros(len(grp)))
        new_sp = _new_spat(W_yr_dict, B_ccodes, pg, po, A_ccode, contiguity, is_P5_arr)
        new_sp.index = grp.index
        new_spat_rows.append(new_sp)

    if spat_rows:
        spat_all = pd.concat(spat_rows)
        dd.loc[spat_all.index, SPAT_COLS] = spat_all[SPAT_COLS].values

    if new_spat_rows:
        new_all = pd.concat(new_spat_rows)
        dd.loc[new_all.index, NEW_SPAT_COLS] = new_all[NEW_SPAT_COLS].values

    return dd


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def add_spatial_lags(dd: pd.DataFrame) -> pd.DataFrame:
    """
    Add 20 spatial lag columns (ALL_SPAT_COLS) to an intervention-coded
    directed-dyad file.

    Replicates 10-makeWpol.R + 11-addSpatial.R, extended with era-invariant
    spatial mechanisms (region, IGO, contiguity chain, ideal point, P5).

    Parameters
    ----------
    dd : pd.DataFrame
        Output of :func:`~shadow.data.interventions.code_interventions`.
        Must have columns: ccode_A, ccode_B, year, polity2_B, onset_A,
        intervention, ddyear.

    Returns
    -------
    dd with ALL_SPAT_COLS appended. Non-onset rows have NaN for all spat_* columns.
    """
    dd = dd.copy()
    for col in ALL_SPAT_COLS:
        dd[col] = np.nan

    # R line 48-50 of 11-addSpatial.R: drop ccode 678 (Yemen AR) in 1990.
    compute_mask = ~((dd["year"] == 1990) & (dd["ccode_B"] == "678"))
    compute_mask &= ~((dd["year"] == 1990) & (dd["ccode_A"] == "678"))

    # Only onset rows get spatial lags.
    onset_mask = (dd["onset_A"] == 1) & compute_mask
    if onset_mask.sum() == 0:
        return dd

    # ── Build multi-type W cache ──────────────────────────────────────
    W_cache = _build_W_cache(dd, onset_mask)

    # ── Compute spatial lags per onset (A, year) ──────────────────────
    onset_df = dd.loc[onset_mask].copy()

    spat_rows = []
    new_spat_rows = []

    for (A_ccode, year), grp in onset_df.groupby(["ccode_A", "year"], sort=False):
        W_yr = W_cache.get(year, {})
        W_polity = W_yr.get("polity", pd.DataFrame())

        B_ccodes     = grp["ccode_B"].values
        intervention = grp["intervention"].fillna(0).values

        # Original polity + superpower lags
        if W_polity.empty:
            spat_rows.append(
                pd.DataFrame({c: np.nan for c in SPAT_COLS}, index=grp.index)
            )
        else:
            spat = _spat_for_onset(W_polity, B_ccodes, intervention, A_ccode)
            spat.index = grp.index
            spat_rows.append(spat)

        # New lags: convert intervention to gov/opp vectors
        gov_vec = (intervention == 1).astype(float)
        opp_vec = (intervention == 2).astype(float)
        contiguity = (grp["ud_conttype"].fillna(6).values
                      if "ud_conttype" in grp.columns
                      else np.full(len(grp), 6.0))
        is_P5_arr = (grp["is_P5_B"].fillna(0).values
                     if "is_P5_B" in grp.columns
                     else np.zeros(len(grp)))
        new_sp = _new_spat(W_yr, B_ccodes, gov_vec, opp_vec, A_ccode,
                           contiguity, is_P5_arr)
        new_sp.index = grp.index
        new_spat_rows.append(new_sp)

    if spat_rows:
        spat_all = pd.concat(spat_rows)
        dd.loc[spat_all.index, SPAT_COLS] = spat_all[SPAT_COLS].values

    if new_spat_rows:
        new_all = pd.concat(new_spat_rows)
        dd.loc[new_all.index, NEW_SPAT_COLS] = new_all[NEW_SPAT_COLS].values

    return dd
