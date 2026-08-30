"""Parallel re-run of nb06's universal fixed point for ONE draw, with a raised
iteration ceiling and tighter tolerance, standard (nb06) initialization.

This reproduces nb06's per-draw cell-4 logic verbatim — universal Nash fixed
point, leak-free OOF overwrite of onset rows, full channel aggregation — so the
outputs are drop-in replacements for nb06's, just converged properly (the
capped-at-10 run left the strongly-coupled draws unconverged). The standard init
lands in the empirically-realized LOW-coordination equilibrium (verified by the
multi-start analysis).

Usage:  python scripts/fp_rerun.py <cy> <ud>
        FP_MAX_ITER (default 60), FP_TOL (default 1e-4) override the loop.
Writes (canonical nb06 names, overwriting): cy_shadow_<cy>_<ud>.parquet,
        sl_preds_<cy>_<ud>.parquet, and fpr_diag_<cy>_<ud>.parquet (convergence).
"""
import os
for _v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "2")

import sys
from pathlib import Path
import joblib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.data.spatial import update_spatial_lags_proba, _build_W_cache, ALL_SPAT_COLS  # noqa: F401

INTERIM = Path(os.environ.get("SHADOW_INTERIM", ROOT / "data" / "interim"))
TAU = 0.001
RIVAL_THRESHOLD = 0.25
MAX_FP_ITER = int(os.environ.get("FP_MAX_ITER", "60"))
FP_TOL = float(os.environ.get("FP_TOL", "1e-4"))


# --- nb06 cell-2 functions, verbatim ---
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


# --- nb06 cell-4 aggregation, verbatim ---
def _agg(group):
    pg = group["p_gov"].values
    po = group["p_opp"].values
    result = {"E_gov": pg.sum(), "E_opp": po.sum(),
              "E_gov_trim": pg[pg >= TAU].sum(), "E_opp_trim": po[po >= TAU].sum(),
              "n_B": len(group)}
    type_filters = {
        "major": group["major_power_B"].values == 1,
        "P5": group["is_P5_B"].values == 1,
        "contig": group["ud_conttype"].values <= 5,
        "coethnic": group["ud_sameFirstEth"].values == 1,
        "colonial": group["B_wasColOf_A"].values == 1,
        "rival": group["ud_peace"].values <= RIVAL_THRESHOLD,
        "ally": group["ud_defense"].values == 1,
    }
    for prefix, mask in type_filters.items():
        pg_m = pg[mask]; po_m = po[mask]
        result[f"E_{prefix}_gov"] = pg_m.sum(); result[f"E_{prefix}_opp"] = po_m.sum()
        result[f"E_{prefix}_gov_trim"] = pg_m[pg_m >= TAU].sum()
        result[f"E_{prefix}_opp_trim"] = po_m[po_m >= TAU].sum()
    hostility = 1.0 - group["ud_peace"].values
    result["E_hostile_gov"] = (pg * hostility).sum()
    result["E_hostile_opp"] = (po * hostility).sum()
    pg_h = pg * hostility; po_h = po * hostility
    result["E_hostile_gov_trim"] = pg_h[pg >= TAU].sum()
    result["E_hostile_opp_trim"] = po_h[po >= TAU].sum()
    doe_w = group["doe_pr_win_B"].values
    result["E_doe_gov"] = (pg * doe_w).sum(); result["E_doe_opp"] = (po * doe_w).sum()
    pg_d = pg * doe_w; po_d = po * doe_w
    result["E_doe_gov_trim"] = pg_d[pg >= TAU].sum()
    result["E_doe_opp_trim"] = po_d[po >= TAU].sum()
    return pd.Series(result)


def run(cy, ud, model_path=None, oof_path=None, out_dir=None, tag=None):
    out_dir = out_dir or INTERIM
    sfx = f"{tag}_{cy}_{ud}" if tag else f"{cy}_{ud}"
    dd = add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{cy}_{ud}.parquet"))
    model = joblib.load(model_path or (INTERIM / f"sl_model_{cy}_{ud}.pkl"))
    oof = pd.read_parquet(oof_path or (INTERIM / f"sl_oof_{cy}_{ud}.parquet"))
    onset_mask = dd["onset_A"] == 1
    W_cache = _build_W_cache(dd, pd.Series(True, index=dd.index))

    prev_proba = None; prev_spat = None; converged = False; diag = []
    proba_all = None
    for fp_iter in range(MAX_FP_ITER):
        proba_all = predict_proba_from_model(model, dd)
        cur_spat = dd[["spat_gov", "spat_opp"]].fillna(0).to_numpy()
        dproba = float(np.abs(proba_all - prev_proba).mean()) if prev_proba is not None else 0.0
        dspat = float(np.abs(cur_spat - prev_spat).mean()) if prev_spat is not None else 0.0
        diag.append(dict(cy=cy, ud=ud, fp_iter=fp_iter, delta_proba=dproba,
                         delta_spat_all=dspat, converged=False))
        if prev_spat is not None and dspat < FP_TOL:
            diag[-1]["converged"] = True; converged = True; break
        prev_proba = proba_all.copy(); prev_spat = cur_spat.copy()
        dd = update_spatial_lags_proba(dd, proba_all[:, 1], proba_all[:, 2],
                                       W_cache=W_cache, onset_only=False)

    dd["p_none"] = proba_all[:, 0]; dd["p_gov"] = proba_all[:, 1]; dd["p_opp"] = proba_all[:, 2]

    # leak-free: onset rows take their out-of-fold predictions
    oof_keys = set(oof["ddyear"].values)
    oof_mask = dd["ddyear"].isin(oof_keys)
    if oof_mask.any():
        oof_lookup = oof.set_index("ddyear")[["p_none", "p_gov", "p_opp"]]
        for col in ["p_none", "p_gov", "p_opp"]:
            dd.loc[oof_mask, col] = dd.loc[oof_mask, "ddyear"].map(oof_lookup[col]).values

    dd[["ccode_A", "ccode_B", "year", "p_gov", "p_opp"]].to_parquet(
        out_dir / f"sl_preds_{sfx}.parquet", index=False)

    cy_shadow = (dd.groupby(["ccode_A", "year"]).apply(_agg, include_groups=False)
                 .reset_index().rename(columns={"ccode_A": "ccode"}))
    for col in [c for c in cy_shadow.columns if c.startswith("E_")]:
        cy_shadow[f"{col}_asinh"] = np.arcsinh(cy_shadow[col])
    cy_shadow["cy_imp"] = cy; cy_shadow["ud_imp"] = ud
    cy_shadow.to_parquet(out_dir / f"cy_shadow_{sfx}.parquet", index=False)
    pd.DataFrame(diag).to_parquet(out_dir / f"fpr_diag_{sfx}.parquet", index=False)

    tag = "conv" if converged else "max-iter"
    print(f"done {cy}_{ud}: {len(diag)} iters [{tag}] |d|={diag[-1]['delta_spat_all']:.2e} "
          f"E_gov_mean={cy_shadow.E_gov.mean():.3f}", flush=True)


if __name__ == "__main__":
    run(int(sys.argv[1]), int(sys.argv[2]))
