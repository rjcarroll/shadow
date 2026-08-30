"""
Prong B / S1 SPIKE — random-forest predictive-significance test for E_gov / E_opp.

ADDITIVE & NON-DESTRUCTIVE BY DESIGN:
  - Reads only the committed data in data/interim/ (and the V-Dem raw zip, read-only).
  - Replicates nb07's load_analysis_data() VERBATIM so the analysis frame is identical;
    nb07 and all src/ modules are left untouched.
  - Phase 0 reproduces the existing logit and COMPARES to the committed
    results/stage2_*.parquet — it writes NOTHING to existing files.
  - Phase 1 writes only to results/spike/ (a new scratch dir).
  - To fully revert: delete this file and results/spike/. Nothing else changes.

Usage:
  .venv/bin/python scripts/spike_rf_significance.py phase0   # fast reproduction gate
  .venv/bin/python scripts/spike_rf_significance.py phase1   # the RF spike (slow; ~tens of min)
  .venv/bin/python scripts/spike_rf_significance.py all
"""
from __future__ import annotations

import sys
import time
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd
import statsmodels.api as sm
from scipy.stats import norm
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import average_precision_score, log_loss, roc_auc_score
from sklearn.model_selection import StratifiedKFold

ROOT = Path(__file__).resolve().parents[1]
INTERIM = ROOT / "data" / "interim"
RESULTS = ROOT / "results"
SPIKE = RESULTS / "spike"

# ── Config (kept modest so the spike is fast & decisive) ───────────────────
# NOTE: deliberately NO class_weight="balanced". At a ~2% onset base rate,
# balancing inflates predicted probabilities and wrecks calibration-sensitive
# scores (log-loss/PRL). We score the test with AUC-PR (average precision) — a
# RANK-based metric, immune to that miscalibration, appropriate for rare events,
# and the metric R2 asked for.
RF_KW = dict(n_estimators=200, max_features="sqrt",
             n_jobs=-1, random_state=20260608)
N_SPLITS = 5
N_PERM = 99            # permutation reps for the null (min resolvable p = 1/100)
SEED = 20260608

BASELINE_VARS = [
    "polity2_lag", "lgdp_lag", "lpop_lag",
    "lmtnest", "ncontig", "oil",
    "nwstate", "instab_lag", "prior_war",
    "ethfrac", "relfrac",
    "year",
]
SHADOW = ["E_gov_asinh", "E_opp_asinh"]
ENTRANTS = BASELINE_VARS + SHADOW


# ─────────────────────────────────────────────────────────────────────────
# Data loading — copied verbatim from nb07 cell 3 (do not "improve" it; the
# point is to reproduce the exact frame the committed results were built on).
# ─────────────────────────────────────────────────────────────────────────
def _build_preperiod() -> pd.DataFrame:
    vdem_zip = ROOT / "data" / "raw" / "vdem" / "V-Dem-CY-FullOthers-v15_csv.zip"
    if not vdem_zip.exists():
        raise FileNotFoundError(
            f"V-Dem zip not found at {vdem_zip}. Phase 0 needs it to build the "
            f"1945 pre-period (matching nb07). It lives in gitignored data/raw/."
        )
    with zipfile.ZipFile(vdem_zip) as z:
        csv_name = [n for n in z.namelist() if n.endswith(".csv")][0]
        vdem = pd.read_csv(
            z.open(csv_name),
            usecols=["COWcode", "year", "e_polity2", "e_pt_coup"],
            low_memory=False,
        )
    vdem = vdem[vdem["year"].between(1942, 1945)].copy()
    vdem = vdem.rename(columns={"COWcode": "ccode", "e_polity2": "polity2"})
    vdem["ccode"] = vdem["ccode"].astype("Int64").astype(str).str.zfill(3)
    vdem.loc[vdem["ccode"] == "365", "ccode"] = "364"
    vdem = vdem.sort_values(["ccode", "year"])
    vdem["polity2_lag3"] = vdem.groupby("ccode")["polity2"].shift(3)
    v45 = vdem[vdem["year"] == 1945].copy()
    delta3 = (v45["polity2"] - v45["polity2_lag3"]).abs()
    coup = v45["e_pt_coup"].fillna(0).astype(bool)
    v45["instab"] = ((delta3 >= 3) | coup).astype(float)
    v45.loc[v45["polity2"].isna() & ~coup, "instab"] = np.nan
    pre = v45[["ccode", "year", "polity2", "instab"]].copy()
    print(f"  pre-period 1945: {len(pre)} rows, "
          f"polity2 nn={pre['polity2'].notna().sum()}, instab nn={pre['instab'].notna().sum()}")
    return pre


def load_analysis_data() -> pd.DataFrame:
    cy_frames = [pd.read_parquet(INTERIM / f"cy_imputed_{i}.parquet") for i in range(1, 6)]
    avg_cols = ["onset", "polity2", "lgdp_lag", "lpop_lag",
                "lmtnest", "ncontig", "oil", "nwstate", "instab",
                "prior_war", "ethfrac", "relfrac"]
    cy_avg = (pd.concat(cy_frames).groupby(["ccode", "year"])[avg_cols].mean().reset_index())
    cy_avg["onset"] = (cy_avg["onset"] >= 0.5).astype(int)

    pre = _build_preperiod()
    cy_aug = pd.concat([pre, cy_avg], ignore_index=True)
    cy_aug = cy_aug.sort_values(["ccode", "year"])
    cy_aug["polity2_lag"] = cy_aug.groupby("ccode")["polity2"].shift(1)
    cy_aug["instab_lag"] = cy_aug.groupby("ccode")["instab"].shift(1)
    cy_avg = cy_aug[cy_aug["year"] >= 1946].copy()

    sh_frames = []
    for cy in range(1, 6):
        for ud in range(1, 6):
            sh_frames.append(pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet"))
    sh_all = pd.concat(sh_frames, ignore_index=True)
    shadow_cols = [c for c in sh_all.columns if c.startswith("E_") or c == "n_B"]
    shadow_avg = (sh_all.groupby(["ccode", "year"])[shadow_cols].mean().reset_index())

    merged = cy_avg.merge(shadow_avg, on=["ccode", "year"], how="left")
    return merged


def prl(y: np.ndarray, p: np.ndarray) -> float:
    """Proportional reduction in log-loss vs null (used only for Phase-0 logit)."""
    null_p = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    null_ll = -(y * np.log(null_p) + (1 - y) * np.log(1 - null_p)).mean()
    return (null_ll - log_loss(y, np.clip(p, 1e-9, 1 - 1e-9))) / null_ll


def ap(y: np.ndarray, p: np.ndarray) -> float:
    """AUC-PR (average precision): rank-based, calibration-immune, rare-event-apt.
    The score used for the RF predictive-significance test."""
    return average_precision_score(y, p)


def get_frame() -> pd.DataFrame:
    print("Loading analysis frame (replicating nb07 load_analysis_data)...")
    data = load_analysis_data()
    print(f"  frame: {len(data):,} country-years, {int(data['onset'].sum())} onsets, "
          f"{data.shape[1]} cols")
    return data


# ─────────────────────────────────────────────────────────────────────────
# Phase 0 — reproduce the committed logit (READ-ONLY; writes nothing)
# ─────────────────────────────────────────────────────────────────────────
def phase0(data: pd.DataFrame) -> None:
    print("\n" + "=" * 70)
    print("PHASE 0 — reproduce committed logit (read-only; no files written)")
    print("=" * 70)

    sub = data[["onset"] + ENTRANTS].dropna().copy()
    y = sub["onset"].values
    X = sm.add_constant(sub[ENTRANTS].values)
    logit = sm.Logit(y, X).fit(disp=False, maxiter=1000)
    names = ["const"] + ENTRANTS
    mine = pd.DataFrame({"variable": names, "coef": logit.params,
                         "se": logit.bse, "z": logit.tvalues, "p": logit.pvalues})

    print(f"\nEntrants in-sample fit: n={len(y)}, onsets={int(y.sum())}, "
          f"IS-PRL={prl(y, logit.predict(X)):.4f}, IS-AUC={roc_auc_score(y, logit.predict(X)):.4f}")
    print("\nFreshly-fit shadow coefs:")
    print(mine[mine['variable'].isin(SHADOW)].to_string(index=False))

    # Compare to committed results (read-only)
    cf = RESULTS / "stage2_coefs.parquet"
    if cf.exists():
        committed = pd.read_parquet(cf)
        comm_e = committed[(committed["model"] == "Entrants") & (committed["variable"].isin(SHADOW))]
        print("\nCommitted shadow coefs (results/stage2_coefs.parquet):")
        print(comm_e[["variable", "coef", "se", "z", "p"]].to_string(index=False))
        print("\nΔ (fresh − committed):")
        for v in SHADOW:
            a = mine.loc[mine.variable == v, "coef"].values[0]
            b = comm_e.loc[comm_e.variable == v, "coef"].values
            if len(b):
                d = a - b[0]
                flag = "OK" if abs(d) < 1e-3 else ("~close" if abs(d) < 1e-2 else "*** DRIFT")
                print(f"  {v}: Δcoef={d:+.5f}  [{flag}]")
    else:
        print(f"\n(no committed {cf.name} to compare against)")

    for fn, label in [("stage2_fit.parquet", "fit (OOS-PRL/AUC)"),
                      ("stage2_vuong.parquet", "Vuong vs Baseline")]:
        fp = RESULTS / fn
        if fp.exists():
            df = pd.read_parquet(fp)
            row = df[df.get("model", df.get("model_A", "")).astype(str).str.contains("Entrants", na=False)]
            print(f"\nCommitted {label}:")
            print((row if len(row) else df).to_string(index=False))

    print("\nGATE: confirm the Δcoef flags read OK before trusting Phase 1.")


# ─────────────────────────────────────────────────────────────────────────
# Phase 1 — RF predictive-significance spike (writes only to results/spike/)
# ─────────────────────────────────────────────────────────────────────────
def cv_rf_oof(X: np.ndarray, y: np.ndarray, seed: int = SEED) -> np.ndarray:
    skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=seed)
    oof = np.zeros(len(y))
    for tr, va in skf.split(X, y):
        rf = RandomForestClassifier(**RF_KW)
        rf.fit(X[tr], y[tr])
        oof[va] = rf.predict_proba(X[va])[:, 1]
    return oof


def cv_logit_oof(X: np.ndarray, y: np.ndarray, seed: int = SEED) -> np.ndarray:
    """Same OOF protocol with a plain logit — for an apples-to-apples RF-vs-linear
    comparison (does the flexible model extract MORE signal = is it nonlinearity?)."""
    skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=seed)
    oof = np.zeros(len(y))
    for tr, va in skf.split(X, y):
        try:
            m = sm.Logit(y[tr], sm.add_constant(X[tr])).fit(disp=False, maxiter=200)
            oof[va] = m.predict(sm.add_constant(X[va], has_constant="add"))
        except Exception:  # noqa: BLE001
            oof[va] = y[tr].mean()
    return oof


def perm_test(X: np.ndarray, y: np.ndarray, focal_idx: list[int],
              label: str, n_perm: int = N_PERM) -> dict:
    """Block-permute the focal columns (preserving their joint distribution but
    breaking their link to y), refit CV-RF, compare OOF AUC-PR. p = P(null >= obs)."""
    t0 = time.time()
    rng = np.random.default_rng(SEED)
    obs = ap(y, cv_rf_oof(X, y))
    null = np.empty(n_perm)
    for b in range(n_perm):
        Xp = X.copy()
        perm = rng.permutation(len(y))
        Xp[:, focal_idx] = X[np.ix_(perm, focal_idx)]
        null[b] = ap(y, cv_rf_oof(Xp, y, seed=SEED + b + 1))
    p = (1 + np.sum(null >= obs)) / (1 + n_perm)
    dt = time.time() - t0
    print(f"  [{label:14s}] obs-AUCPR={obs:.4f}  null mean={null.mean():.4f} "
          f"(sd={null.std():.4f})  p={p:.3f}   ({dt:.0f}s)")
    return {"test": label, "obs_ap": obs, "null_mean": float(null.mean()),
            "null_sd": float(null.std()), "p": p, "n_perm": n_perm}


def _ale_1d(model, Xa, j, n_bins=20):
    """1-D accumulated local effects for feature j (correlation-robust)."""
    xj = Xa[:, j]
    edges = np.unique(np.quantile(xj, np.linspace(0, 1, n_bins + 1)))
    nb = len(edges) - 1
    local = np.zeros(nb); counts = np.zeros(nb)
    binidx = np.clip(np.searchsorted(edges, xj, side="left") - 1, 0, nb - 1)
    for b in range(nb):
        m = binidx == b
        counts[b] = m.sum()
        if not m.any():
            continue
        lo = Xa[m].copy(); lo[:, j] = edges[b]
        hi = Xa[m].copy(); hi[:, j] = edges[b + 1]
        local[b] = (model.predict_proba(hi)[:, 1] - model.predict_proba(lo)[:, 1]).mean()
    acc = np.concatenate([[0.0], np.cumsum(local)])
    mid = 0.5 * (acc[:-1] + acc[1:])
    acc = acc - np.sum(counts * mid) / counts.sum()
    return edges, acc, xj


def make_shadow_plots(data: pd.DataFrame) -> None:
    """ALE (correlation-robust 1-D shapes) for E_gov / E_opp, plus a 2-D scatter
    of the REAL (E_gov, E_opp) points colored by predicted risk. ALE only probes
    the model within each feature's local data bins, so — unlike single-var PDP —
    it does not extrapolate off the r=0.94 manifold."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    sub = data[["onset"] + ENTRANTS].dropna().copy()
    y = sub["onset"].values.astype(int)
    X = sub[ENTRANTS].values.astype(float)
    gov_i, opp_i = ENTRANTS.index("E_gov_asinh"), ENTRANTS.index("E_opp_asinh")
    SPIKE.mkdir(parents=True, exist_ok=True)
    rf_full = RandomForestClassifier(**RF_KW).fit(X, y)

    fig, ax = plt.subplots(1, 2, figsize=(8, 3.5))
    for k, (j, name, exp) in enumerate([(gov_i, "E_gov", "↓"), (opp_i, "E_opp", "↑")]):
        edges, acc, xj = _ale_1d(rf_full, X, j)
        ax[k].plot(edges, acc, color="black")
        ax[k].axhline(0, color="grey", lw=0.6)
        ax[k].plot(xj, np.full(len(xj), acc.min()), "|", color="grey", ms=4, alpha=0.25)
        ax[k].set_title(f"{name} — ALE (expect {exp})")
        ax[k].set_xlabel(ENTRANTS[j]); ax[k].set_ylabel("ALE on P(onset)")
    fig.tight_layout()
    fig.savefig(SPIKE / "ale_shadow.png", dpi=150, bbox_inches="tight")
    print(f"saved: {SPIKE / 'ale_shadow.png'}")

    oof = cv_rf_oof(X, y)
    fig2, ax2 = plt.subplots(figsize=(5.4, 4.2))
    order = np.argsort(oof)                                       # high-risk drawn on top
    sc = ax2.scatter(X[order, gov_i], X[order, opp_i], c=oof[order],
                     s=9, cmap="viridis", alpha=0.7)
    ax2.set_xlabel("E_gov_asinh"); ax2.set_ylabel("E_opp_asinh")
    ax2.set_title("OOF predicted onset over the real (E_gov, E_opp) manifold")
    fig2.colorbar(sc, label="OOF P(onset)")
    fig2.tight_layout()
    fig2.savefig(SPIKE / "joint_manifold.png", dpi=150, bbox_inches="tight")
    print(f"saved: {SPIKE / 'joint_manifold.png'}")


def propagate(data: pd.DataFrame) -> None:
    """MEASUREMENT-ROBUST check. Redo the descriptive RF-vs-logit AUC-PR gain and
    the ALE shape PER DRAW across the 25 shadow imputations (baseline covariates
    held at their averaged values, as in nb07's T×P bootstrap), so we propagate the
    between-draw measurement variance the averaged frame hides. (1) gain distribution
    + (2) ALE band. The expensive per-draw permutation test is deferred until these
    survive."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    SPIKE.mkdir(parents=True, exist_ok=True)
    base_cols = list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))
    base_df = data[base_cols].copy()
    gov_i, opp_i = ENTRANTS.index("E_gov_asinh"), ENTRANTS.index("E_opp_asinh")
    base_idx = list(range(len(BASELINE_VARS)))

    avg = data[["onset"] + ENTRANTS].dropna()
    gov_grid = np.linspace(*np.quantile(avg["E_gov_asinh"], [0.01, 0.99]), 50)
    opp_grid = np.linspace(*np.quantile(avg["E_opp_asinh"], [0.01, 0.99]), 50)

    rf_gain, lo_gain, labels = [], [], []
    gov_curves, opp_curves = [], []
    print("\nPropagating across 25 draws (per-draw RF & logit ΔAUC-PR + ALE):")
    print(f"  {'draw':6s} {'n':>6s} {'RF Δ':>9s} {'logit Δ':>9s}")
    for cy in range(1, 6):
        for ud in range(1, 6):
            sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                                 columns=["ccode", "year"] + SHADOW)
            sub = base_df.merge(sh, on=["ccode", "year"], how="left")[["onset"] + ENTRANTS].dropna()
            y = sub["onset"].values.astype(int)
            X = sub[ENTRANTS].values.astype(float)
            rf_d = ap(y, cv_rf_oof(X, y)) - ap(y, cv_rf_oof(X[:, base_idx], y))
            lo_d = ap(y, cv_logit_oof(X, y)) - ap(y, cv_logit_oof(X[:, base_idx], y))
            rf_gain.append(rf_d); lo_gain.append(lo_d); labels.append(f"{cy}_{ud}")
            rf_full = RandomForestClassifier(**RF_KW).fit(X, y)
            eg, ag, _ = _ale_1d(rf_full, X, gov_i)
            eo, ao, _ = _ale_1d(rf_full, X, opp_i)
            gov_curves.append(np.interp(gov_grid, eg, ag))
            opp_curves.append(np.interp(opp_grid, eo, ao))
            print(f"  {labels[-1]:6s} {len(y):6d} {rf_d:+9.4f} {lo_d:+9.4f}")

    rf_gain = np.array(rf_gain); lo_gain = np.array(lo_gain)
    print("\n=== PROPAGATED (25-draw) AUC-PR gain from adding the shadow pair ===")
    print(f"  RF    Δ: mean {rf_gain.mean():+.4f}  sd {rf_gain.std():.4f}  "
          f"[min {rf_gain.min():+.4f}, max {rf_gain.max():+.4f}]")
    print(f"  logit Δ: mean {lo_gain.mean():+.4f}  sd {lo_gain.std():.4f}  "
          f"[min {lo_gain.min():+.4f}, max {lo_gain.max():+.4f}]")
    print(f"  RF Δ > logit Δ in {(rf_gain > lo_gain).sum()}/25 draws;  "
          f"RF Δ > 0 in {(rf_gain > 0).sum()}/25;  logit Δ > 0 in {(lo_gain > 0).sum()}/25")
    print(f"  (averaged-frame point estimates were: RF +0.0448, logit +0.0069)")
    pd.DataFrame({"draw": labels, "rf_gain": rf_gain, "logit_gain": lo_gain}).to_parquet(
        SPIKE / "propagate_gain.parquet", index=False)

    gov_curves = np.array(gov_curves); opp_curves = np.array(opp_curves)
    fig, ax = plt.subplots(1, 2, figsize=(8, 3.5))
    for k, (grid, curves, name, exp) in enumerate(
            [(gov_grid, gov_curves, "E_gov", "↓"), (opp_grid, opp_curves, "E_opp", "↑")]):
        for c in curves:
            ax[k].plot(grid, c, color="grey", lw=0.5, alpha=0.30)
        ax[k].plot(grid, np.median(curves, axis=0), color="black", lw=1.8, label="median")
        ax[k].axhline(0, color="grey", lw=0.6, ls=":")
        ax[k].set_title(f"{name} ALE — 25 draws (expect {exp})")
        ax[k].set_xlabel(f"{name}_asinh"); ax[k].set_ylabel("ALE on P(onset)")
    fig.tight_layout()
    fig.savefig(SPIKE / "ale_band.png", dpi=150, bbox_inches="tight")
    print(f"\nsaved: {SPIKE / 'propagate_gain.parquet'}  and  {SPIKE / 'ale_band.png'}")


def phase1(data: pd.DataFrame) -> None:
    print("\n" + "=" * 70)
    print("PHASE 1 — RF predictive-significance spike (writes only results/spike/)")
    print("=" * 70)
    SPIKE.mkdir(parents=True, exist_ok=True)

    sub = data[["onset"] + ENTRANTS].dropna().copy()
    y = sub["onset"].values.astype(int)
    X = sub[ENTRANTS].values.astype(float)
    gov_i, opp_i = ENTRANTS.index("E_gov_asinh"), ENTRANTS.index("E_opp_asinh")
    lgdp_i = ENTRANTS.index("lgdp_lag")
    print(f"n={len(y)}, onsets={int(y.sum())} ({y.mean()*100:.1f}%), features={len(ENTRANTS)}")

    # Descriptive: OOF AUC-PR, RF vs logit (does flexibility extract MORE = nonlinearity?)
    prevalence = float(y.mean())
    base_i = list(range(len(BASELINE_VARS)))
    print(f"\nDescriptive OOF AUC-PR. No-skill baseline = prevalence = {prevalence:.4f}:")
    print(f"  {'model':6s} {'controls':>10s} {'+shadow':>10s} {'Δ':>9s}")
    for lbl, oof_fn in [("RF", cv_rf_oof), ("logit", cv_logit_oof)]:
        ab = ap(y, oof_fn(X[:, base_i], y))
        af = ap(y, oof_fn(X, y))
        print(f"  {lbl:6s} {ab:10.4f} {af:10.4f} {af - ab:+9.4f}")

    # ── Correlation snapshot (explains conditional-importance behavior) ─────
    # Permutation importance can't credit a feature whose signal is recoverable
    # from correlated partners — the reason E_opp (twin of E_gov) and arguably
    # GDP/population can look null individually.
    pop_i = ENTRANTS.index("lpop_lag")
    cm = np.corrcoef(np.column_stack([y.astype(float), X]).T)
    fn = ["onset"] + ENTRANTS
    cor = lambda a, b: cm[fn.index(a), fn.index(b)]
    print("\nCorrelation snapshot:")
    print(f"  E_gov ~ E_opp       : {cor('E_gov_asinh','E_opp_asinh'):+.2f}   (the twins)")
    print(f"  lgdp_lag ~ onset    : {cor('lgdp_lag','onset'):+.2f}")
    print(f"  lpop_lag ~ onset    : {cor('lpop_lag','onset'):+.2f}")
    print(f"  lgdp_lag ~ lpop_lag : {cor('lgdp_lag','lpop_lag'):+.2f}")

    # ── Validity probes: a pure-noise null and a synthetic strong, UNCORRELATED
    #    signal (a noised copy of the label) — the clean positive control that
    #    GDP/population cannot be, because it has no correlated partner. ───────
    rng = np.random.default_rng(SEED)
    Xnoise = np.column_stack([X, rng.standard_normal(len(y))])          # pure noise
    Xsig = np.column_stack([X, y + rng.normal(0, 0.5, len(y))])         # STRONG (d'≈2), uncorrelated
    extra_i = X.shape[1]   # appended-column index in Xnoise / Xsig

    print(f"\nPermutation tests (n_perm={N_PERM}, RF={RF_KW['n_estimators']} trees, "
          f"{N_SPLITS}-fold OOF). p = P(null AUC-PR >= observed):")
    rows = [
        perm_test(X, y, [opp_i], "E_opp"),
        perm_test(X, y, [gov_i], "E_gov"),
        perm_test(X, y, [gov_i, opp_i], "E_gov+E_opp"),
        perm_test(X, y, [lgdp_i], "GDP (control)"),
        perm_test(X, y, [pop_i], "Population (ctrl)"),
        perm_test(Xnoise, y, [extra_i], "noise (NOT reject)"),     # must NOT reject
        perm_test(Xsig, y, [extra_i], "synthetic (MUST reject)"),  # must reject
    ]
    summary = pd.DataFrame(rows)
    out = SPIKE / "rfsig_summary.parquet"
    summary.to_parquet(out, index=False)
    print(f"\nsaved: {out}")
    print(summary.to_string(index=False))

    # Shape of the nonlinearity: ALE (correlation-robust) + 2-D manifold view
    make_shadow_plots(data)

    print("\n--- READOUT GUIDE ---")
    print("  E_gov & E_opp both p<0.05 + PDP signs (gov↓, opp↑)  -> two-channel holds (S1 headlines)")
    print("  E_gov sig, E_opp not                                -> gov robust; E_opp uninformative-not-null")
    print("  validity: noise p high (~>0.3) AND lgdp p low (~<0.05) before trusting the above")


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else "phase0"
    df = get_frame()
    if mode in ("phase0", "all"):
        phase0(df)
    if mode in ("phase1", "all"):
        phase1(df)
    if mode == "viz":          # fast: just regenerate the ALE + 2-D plots
        make_shadow_plots(df)
    if mode == "propagate":    # measurement-robust per-draw gain + ALE band
        propagate(df)
