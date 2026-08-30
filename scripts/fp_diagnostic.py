"""Fixed-point diagnostic for nb06's universal Nash iteration.

Re-runs nb06's universal fixed point for a single imputation draw to N_ITER
iterations with NO early stop, logging per-iteration convergence metrics AND
aggregate-shadow statistics, optionally with under-relaxation (damping). The
predict/update path is copied verbatim from nb06 cell 2 + the spatial module, so
the UNDAMPED (alpha=1.0) trace reproduces nb06's first iterations exactly.

Purpose: distinguish slow-but-converging from a true orbit (run past the iter-10
cap), measure whether the AGGREGATE shadow (E_gov) stabilizes even when the
per-component lag change does not, test a few trim thresholds tau, and test
whether damping (alpha<1) converts an orbit into convergence.

Usage:  python scripts/fp_diagnostic.py <cy> <ud> <alpha>
        alpha=1.0 -> undamped (== nb06); alpha<1.0 -> under-relaxed.
        FP_N_ITER env var overrides the iteration count (default 40).
Writes: data/interim/fpdiag_<cy>_<ud>_a<alpha*100>.parquet  (one row per iter)
"""
import os
for _v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "3")          # keep workers from oversubscribing 18 cores

import sys
from pathlib import Path
import joblib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.data.spatial import update_spatial_lags_proba, _build_W_cache, ALL_SPAT_COLS  # noqa: F401

INTERIM = ROOT / "data" / "interim"
N_ITER = int(os.environ.get("FP_N_ITER", "40"))
TAUS = [0.001, 0.005, 0.01]


# --- nb06 cell-2 functions, copied verbatim so the trace matches the pipeline ---
def add_temporal_features(df):
    df = df.copy()
    df["cold_war"] = (df["year"] <= 1990).astype(int)
    return df


def _predict_proba_3class(clf, X_val):
    raw = clf.predict_proba(X_val)
    out = np.zeros((len(X_val), 3))
    for j, cls in enumerate(clf.classes_):
        out[:, int(cls)] = raw[:, j]
    return out


def predict_proba_from_model(model_result, df):
    n = len(df)
    proba = np.zeros((n, 3))
    total_w = 0.0
    for (mode, name), clf in model_result["classifiers"].items():
        w = model_result["weights"].get((mode, name), 0.0)
        if w < 1e-8:
            continue
        pipe = model_result["pipelines"][mode]
        fc = pipe["feat_cols"]
        X_raw = df[fc].fillna(0).to_numpy(dtype=float)
        X_sc = pipe["scaler"].transform(X_raw)
        X_pc = pipe["pca"].transform(X_sc)
        proba += w * _predict_proba_3class(clf, X_pc)
        total_w += w
    return proba / total_w


def run(cy, ud, alpha):
    dd = add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{cy}_{ud}.parquet"))
    model = joblib.load(INTERIM / f"sl_model_{cy}_{ud}.pkl")
    W_cache = _build_W_cache(dd, pd.Series(True, index=dd.index))
    keys = dd[["ccode_A", "year"]].reset_index(drop=True)

    rows = []
    prev_p = None
    prev_spat = None
    sigma = None                      # damped probability state (== prediction when alpha=1)
    for it in range(N_ITER):
        p = predict_proba_from_model(model, dd)
        cur_spat = dd[["spat_gov", "spat_opp"]].fillna(0).to_numpy()
        dprob = float(np.abs(p - prev_p).mean()) if prev_p is not None else float("nan")
        dspat = float(np.abs(cur_spat - prev_spat).mean()) if prev_spat is not None else float("nan")

        agg = keys.assign(pg=p[:, 1], po=p[:, 2]).groupby(["ccode_A", "year"]).agg(
            pg=("pg", "sum"), po=("po", "sum"))
        rec = dict(cy=cy, ud=ud, alpha=alpha, it=it,
                   delta_proba=dprob, delta_spat=dspat,
                   Egov_mean=float(agg.pg.mean()), Egov_max=float(agg.pg.max()),
                   Eopp_mean=float(agg.po.mean()))
        for t in TAUS:
            pgt = np.where(p[:, 1] >= t, p[:, 1], 0.0)
            rec[f"Egov_trim_{int(round(t*1000))}"] = float(
                keys.assign(x=pgt).groupby(["ccode_A", "year"]).x.sum().mean())
        rows.append(rec)

        prev_p = p.copy()
        prev_spat = cur_spat.copy()
        # under-relaxation: feed the blended state into the lag update (alpha=1 -> nb06)
        sigma = p if (sigma is None or alpha >= 1.0) else alpha * p + (1.0 - alpha) * sigma
        dd = update_spatial_lags_proba(dd, sigma[:, 1], sigma[:, 2],
                                       W_cache=W_cache, onset_only=False)

    out = INTERIM / f"fpdiag_{cy}_{ud}_a{int(round(alpha*100))}.parquet"
    pd.DataFrame(rows).to_parquet(out, index=False)
    print(f"done {cy}_{ud} alpha={alpha} -> {out.name} ({N_ITER} iters)", flush=True)


if __name__ == "__main__":
    run(int(sys.argv[1]), int(sys.argv[2]), float(sys.argv[3]))
