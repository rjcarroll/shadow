"""Multi-start uniqueness test for the nb06 universal fixed point, one draw.

Runs the universal Nash fixed point to convergence from several different
INITIALIZATIONS and saves the converged per-CY shadow for each, to test whether
the equilibrium (hence the shadow) is start-independent. Reuses nb06's exact
predict/update path (the `standard` init == nb06), so the standard trace
reproduces nb06 / the earlier fp_diagnostic.

Usage:  python scripts/fp_multistart.py <cy> <ud> <init>
        init in {standard, centroid, zero, high, rand1, rand2}
        FP_N_ITER env overrides the cap (default 60).
Writes: data/interim/fpms_<cy>_<ud>_<init>.parquet         (converged per-CY E_gov/E_opp)
        data/interim/fpms_<cy>_<ud>_<init>_trace.parquet   (per-iteration convergence)
"""
import os
for _v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "3")

import sys
from pathlib import Path
import joblib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.data.spatial import update_spatial_lags_proba, _build_W_cache, ALL_SPAT_COLS  # noqa: F401

INTERIM = ROOT / "data" / "interim"
N_ITER = int(os.environ.get("FP_N_ITER", "60"))
FP_TOL = 5e-4


# --- nb06 cell-2 functions, copied verbatim ---
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


def seed_lags(dd, init, W_cache):
    """Overwrite the iter-0 spatial lags with those implied by a chosen initial
    probability field (so each run starts from a different point)."""
    if init == "standard":
        return dd                                  # nb06's dd_spat lags as-is
    n = len(dd)
    if init == "zero":
        pg = np.zeros(n); po = np.zeros(n)
    elif init == "centroid":
        pg = np.full(n, 1.0 / 3); po = np.full(n, 1.0 / 3)
    elif init == "high":
        pg = np.full(n, 0.30); po = np.full(n, 0.20)
    elif init.startswith("rand"):
        rng = np.random.default_rng(int(init[-1]))
        pg = rng.uniform(0.0, 0.4, n); po = rng.uniform(0.0, 0.4, n)
        s = pg + po; m = s > 0.95
        pg[m] = pg[m] * 0.95 / s[m]; po[m] = po[m] * 0.95 / s[m]
    else:
        raise ValueError(f"unknown init: {init}")
    return update_spatial_lags_proba(dd, pg, po, W_cache=W_cache, onset_only=False)


def run(cy, ud, init):
    dd = add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{cy}_{ud}.parquet"))
    model = joblib.load(INTERIM / f"sl_model_{cy}_{ud}.pkl")
    W_cache = _build_W_cache(dd, pd.Series(True, index=dd.index))
    keys = dd[["ccode_A", "year"]].reset_index(drop=True)

    dd = seed_lags(dd, init, W_cache)
    prev_spat = None
    last_p = None
    tr = []
    for it in range(N_ITER):
        p = predict_proba_from_model(model, dd)
        last_p = p
        cur_spat = dd[["spat_gov", "spat_opp"]].fillna(0).to_numpy()
        ds = float(np.abs(cur_spat - prev_spat).mean()) if prev_spat is not None else float("nan")
        ag = keys.assign(pg=p[:, 1]).groupby(["ccode_A", "year"]).pg.sum()
        tr.append(dict(init=init, it=it, delta_spat=ds,
                       Egov_mean=float(ag.mean()), Egov_max=float(ag.max())))
        prev_spat = cur_spat.copy()
        if it > 0 and ds < FP_TOL:
            break
        dd = update_spatial_lags_proba(dd, p[:, 1], p[:, 2], W_cache=W_cache, onset_only=False)

    shadow = (keys.assign(E_gov=last_p[:, 1], E_opp=last_p[:, 2])
              .groupby(["ccode_A", "year"]).agg(E_gov=("E_gov", "sum"), E_opp=("E_opp", "sum"))
              .reset_index())
    shadow["init"] = init
    shadow.to_parquet(INTERIM / f"fpms_{cy}_{ud}_{init}.parquet", index=False)
    pd.DataFrame(tr).to_parquet(INTERIM / f"fpms_{cy}_{ud}_{init}_trace.parquet", index=False)
    print(f"done {cy}_{ud} {init}: {len(tr)} iters, final delta={tr[-1]['delta_spat']:.2e}, "
          f"Egov_mean={shadow.E_gov.mean():.3f} max={shadow.E_gov.max():.1f}", flush=True)


if __name__ == "__main__":
    run(int(sys.argv[1]), int(sys.argv[2]), sys.argv[3])
