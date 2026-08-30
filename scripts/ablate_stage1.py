"""W-specification ablation -- retrain Stage-1 for ONE draw under a chosen subset
of the 5 connectivity channels, holding everything else identical to nb05.

The ONLY departure from notebooks/05-stage1-training.ipynb is `get_feature_cols`,
which restricts the W / XW feature sets to SPAT_COLS (the original 10 lags, always
kept) + the chosen connectivity columns. The X set is always base-only. With
spec=full this is byte-for-byte nb05's feature selection, so it reproduces the
canonical sl_oof exactly (faithfulness check) given the same per-draw seed.

train_super_learner / make_classifiers / _fit_pca / _predict_proba_3class / prl
are copied VERBATIM from nb05 cells 3 and 5.

Usage:  python scripts/ablate_stage1.py <cy> <ud> <spec>
  spec in: baseline iso_region iso_igo iso_chain iso_ideal iso_P5 full
Env: RF_JOBS (RF n_jobs, default 3; affects speed only, not results),
     OMP_NUM_THREADS etc. (default 3).
Outputs -> data/interim/ablation/: sl_model_<spec>_<cy>_<ud>.pkl,
     sl_oof_<spec>_<cy>_<ud>.parquet, sl_weights_<spec>_<cy>_<ud>.parquet
"""
import os
for _v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "3")

import sys
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import nnls
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestClassifier, HistGradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.base import clone
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import log_loss, roc_auc_score
import joblib

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.data.spatial import (update_spatial_lags_proba, _build_W_cache,  # noqa: E402
                                 SPAT_COLS, NEW_SPAT_COLS, ALL_SPAT_COLS)
warnings.filterwarnings("ignore")

INTERIM = Path(os.environ.get("SHADOW_INTERIM", ROOT / "data" / "interim"))
ABL = INTERIM / "ablation"; ABL.mkdir(parents=True, exist_ok=True)

N_FOLDS = 10
MAX_FP_ITER = 5
FP_TOL = 1e-4
SEED = 90210
RF_JOBS = int(os.environ.get("RF_JOBS", "3"))   # speed only; results invariant to n_jobs

ID_COLS = ["ccode_A", "ccode_B", "ddyear", "onset_A", "regan_period", "intervention"]
FEATURE_MODES = ["X", "W", "XW"]

# The 5 connectivity channels -> their column pairs (a partition of NEW_SPAT_COLS).
CHANNEL_COLS = {
    "region": ["spat_region_gov", "spat_region_opp"],
    "igo":    ["spat_igo_gov", "spat_igo_opp"],
    "chain":  ["spat_chain_gov", "spat_chain_opp"],
    "ideal":  ["spat_ideal_gov", "spat_ideal_opp"],
    "P5":     ["spat_P5_gov", "spat_P5_opp"],
}
assert sorted(sum(CHANNEL_COLS.values(), [])) == sorted(NEW_SPAT_COLS), \
    "CHANNEL_COLS must partition NEW_SPAT_COLS"

SPECS = {
    "baseline":   [],
    "iso_region": ["region"], "iso_igo": ["igo"], "iso_chain": ["chain"],
    "iso_ideal":  ["ideal"], "iso_P5": ["P5"],
    "full":       ["region", "igo", "chain", "ideal", "P5"],
}


def spat_set_for(channels):
    cols = list(SPAT_COLS)
    for ch in channels:
        cols += CHANNEL_COLS[ch]
    return set(cols)


def get_feature_cols(df, mode, spat_set):
    """nb05's get_feature_cols, but the spatial set is the chosen subset.
    X is always base-only (excludes ALL spatial); W = chosen spatial; XW = base + chosen."""
    exclude = set(ID_COLS)
    all_feat = [c for c in df.columns
                if c not in exclude and pd.api.types.is_numeric_dtype(df[c])]
    all_spat = set(ALL_SPAT_COLS)
    if mode == "X":
        return [c for c in all_feat if c not in all_spat]
    elif mode == "W":
        return [c for c in all_feat if c in spat_set]
    else:  # XW
        excluded = all_spat - spat_set
        return [c for c in all_feat if c not in excluded]


def add_temporal_features(df):
    df = df.copy()
    df["cold_war"] = (df["year"] <= 1990).astype(int)
    return df


def prl(y_true, proba):                                       # VERBATIM nb05
    null_p = np.bincount(y_true.astype(int), minlength=3) / len(y_true)
    null_p = np.clip(null_p, 1e-9, None)
    null_loss = -np.log(null_p[y_true.astype(int)]).mean()
    model_loss = log_loss(y_true, proba, labels=[0, 1, 2])
    return (null_loss - model_loss) / null_loss


def make_classifiers(seed):                                   # VERBATIM nb05 (RF n_jobs param)
    return {
        "rf":       RandomForestClassifier(n_estimators=500, max_features="sqrt",
                                           n_jobs=RF_JOBS, random_state=seed),
        "hgb":      HistGradientBoostingClassifier(learning_rate=0.1, max_iter=300,
                        early_stopping=True, validation_fraction=0.1,
                        n_iter_no_change=15, random_state=seed),
        "hgb_lo":   HistGradientBoostingClassifier(learning_rate=0.05, max_iter=500,
                        early_stopping=True, validation_fraction=0.1,
                        n_iter_no_change=15, random_state=seed),
        "ridge":    LogisticRegression(penalty="l2", solver="lbfgs", C=1.0,
                        max_iter=2000, random_state=seed),
        "glmnet":   LogisticRegression(penalty="elasticnet", solver="saga",
                        l1_ratio=0.5, C=1.0, max_iter=2000, random_state=seed),
        "lasso":    LogisticRegression(penalty="l1", solver="saga", C=1.0,
                        max_iter=2000, random_state=seed),
        "multinom": LogisticRegression(penalty=None, solver="lbfgs",
                        max_iter=2000, random_state=seed),
        "mlp_sm":   MLPClassifier(hidden_layer_sizes=(25,), max_iter=1000,
                        early_stopping=True, random_state=seed),
        "mlp_lg":   MLPClassifier(hidden_layer_sizes=(100, 50), max_iter=1000,
                        early_stopping=True, random_state=seed),
    }


def _predict_proba_3class(clf, X_val):                        # VERBATIM nb05
    raw = clf.predict_proba(X_val)
    out = np.zeros((len(X_val), 3))
    for j, cls in enumerate(clf.classes_):
        out[:, int(cls)] = raw[:, j]
    return out


def _fit_pca(X, seed):                                        # VERBATIM nb05
    scaler = StandardScaler()
    X_sc = scaler.fit_transform(X)
    pca_full = PCA(random_state=seed).fit(X_sc)
    cumvar = np.cumsum(pca_full.explained_variance_ratio_)
    n_comp = max(5, min(int(np.searchsorted(cumvar, 0.90)) + 1, X_sc.shape[1] - 1))
    pca = PCA(n_components=n_comp, random_state=seed).fit(X_sc)
    return scaler, pca, pca.transform(X_sc)


def train_super_learner(df, y, spat_set, seed=SEED):         # nb05 verbatim, spat_set threaded into get_feature_cols
    pipelines, X_pcs = {}, {}
    for mode in FEATURE_MODES:
        feat_cols = get_feature_cols(df, mode, spat_set)
        X_raw = df[feat_cols].fillna(0).to_numpy(dtype=float)
        scaler, pca, X_pc = _fit_pca(X_raw, seed)
        pipelines[mode] = {"scaler": scaler, "pca": pca,
                           "n_pca": pca.n_components_, "feat_cols": feat_cols}
        X_pcs[mode] = X_pc

    clf_templates = make_classifiers(seed)
    keys = [(mode, name) for mode in FEATURE_MODES for name in clf_templates]
    oof = {k: np.zeros((len(y), 3)) for k in keys}

    # CV scheme: default straight stratified 10-fold; GROUPCV=1 switches to
    # StratifiedGroupKFold grouped by HOST-YEAR (ccode_A x year), so every
    # intervener row of an onset stays in one fold -- removes the within-host-year
    # leak (shared host features + spatial lags built across the same host-year).
    if os.environ.get("GROUPCV"):
        from sklearn.model_selection import StratifiedGroupKFold
        _grp = (df["ccode_A"].astype(str) + "_" + df["year"].astype(str)).to_numpy()
        cv = StratifiedGroupKFold(n_splits=N_FOLDS, shuffle=True, random_state=seed)
        _splits = list(cv.split(X_pcs["XW"], y, _grp))
    else:
        cv = StratifiedKFold(n_splits=N_FOLDS, shuffle=True, random_state=seed)
        _splits = list(cv.split(X_pcs["XW"], y))
    for fold, (tr_idx, val_idx) in enumerate(_splits):
        y_tr = y[tr_idx]
        for mode in FEATURE_MODES:
            X_tr, X_val = X_pcs[mode][tr_idx], X_pcs[mode][val_idx]
            for name, tmpl in clf_templates.items():
                clf_fold = clone(tmpl)
                clf_fold.fit(X_tr, y_tr)
                oof[(mode, name)][val_idx] = _predict_proba_3class(clf_fold, X_val)

    A = np.column_stack([oof[k].reshape(-1) for k in keys])
    b = np.eye(3)[y.astype(int)].reshape(-1)
    raw_w, _ = nnls(A, b)
    w_sum = raw_w.sum(); K = len(keys)
    weights = {k: float(raw_w[i] / w_sum if w_sum > 0 else 1.0 / K)
               for i, k in enumerate(keys)}
    oof_ensemble = sum(weights[k] * oof[k] for k in keys)

    metrics = []
    for k in keys:
        mode, name = k
        ll = log_loss(y, oof[k], labels=[0, 1, 2])
        try:
            auc = roc_auc_score((y > 0).astype(int), oof[k][:, 1:].sum(axis=1))
        except Exception:
            auc = np.nan
        metrics.append({"mode": mode, "method": name, "log_loss": ll,
                        "auc": auc, "weight": weights[k]})
    ll_ens = log_loss(y, oof_ensemble, labels=[0, 1, 2])
    auc_ens = roc_auc_score((y > 0).astype(int), oof_ensemble[:, 1:].sum(axis=1))
    metrics.append({"mode": "all", "method": "super_learner",
                    "log_loss": ll_ens, "auc": auc_ens, "weight": 1.0})
    metrics_df = pd.DataFrame(metrics)
    metrics_df["prl"] = prl(y, oof_ensemble)

    fitted_clfs = {}
    for mode in FEATURE_MODES:
        X_full = X_pcs[mode]
        for name, tmpl in clf_templates.items():
            clf = clone(tmpl)
            clf.fit(X_full, y)
            fitted_clfs[(mode, name)] = clf

    return {"oof_proba": oof_ensemble, "oof_per_model": oof, "weights": weights,
            "pipelines": pipelines, "classifiers": fitted_clfs,
            "component_metrics": metrics_df}


def run(cy, ud, spec, out_dir=None, canonical=False):
    # canonical=True writes the production names (sl_model_<cy>_<ud>) into data/interim,
    # overwriting the main pipeline; default writes spec-tagged names into ablation/.
    out_dir = out_dir or (INTERIM if canonical else ABL)
    sfx = f"{cy}_{ud}" if canonical else f"{spec}_{cy}_{ud}"
    if not os.environ.get("FORCE") and (out_dir / f"sl_model_{sfx}.pkl").exists():
        print(f"skip stage1 {sfx} (cached)", flush=True)
        return
    channels = SPECS[spec]
    spat_set = spat_set_for(channels)
    dd = add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{cy}_{ud}.parquet"))
    train = dd[(dd["onset_A"] == 1) & (dd["intervention"].notna())].copy()
    y = train["intervention"].astype(int).values
    W_cache = _build_W_cache(dd, dd["onset_A"] == 1)

    prev_spat = None
    for fp_iter in range(MAX_FP_ITER):
        result = train_super_learner(train, y, spat_set, seed=SEED + cy * 10 + ud)
        p_gov, p_opp = result["oof_proba"][:, 1], result["oof_proba"][:, 2]
        train_updated = update_spatial_lags_proba(train, p_gov, p_opp, W_cache=W_cache)
        new_spat = train_updated[["spat_gov", "spat_opp"]].fillna(0).values
        if prev_spat is not None and float(np.abs(new_spat - prev_spat).mean()) < FP_TOL:
            break
        prev_spat = new_spat
        train = train_updated

    oof_df = train[["ddyear", "ccode_A", "ccode_B", "year", "intervention"]].copy()
    oof_df["p_none"] = result["oof_proba"][:, 0]
    oof_df["p_gov"] = result["oof_proba"][:, 1]
    oof_df["p_opp"] = result["oof_proba"][:, 2]
    oof_df["cy"], oof_df["ud"], oof_df["spec"] = cy, ud, spec
    oof_df.to_parquet(out_dir / f"sl_oof_{sfx}.parquet", index=False)

    w_df = pd.DataFrame([{"cy": cy, "ud": ud, "spec": spec, "mode": k[0],
                          "method": k[1], "weight": v} for k, v in result["weights"].items()])
    w_df.to_parquet(out_dir / f"sl_weights_{sfx}.parquet", index=False)
    joblib.dump(result, out_dir / f"sl_model_{sfx}.pkl")

    slr = result["component_metrics"].query("method == 'super_learner'").iloc[0]
    wW = sum(v for k, v in result["weights"].items() if k[0] == "W")
    wXW = sum(v for k, v in result["weights"].items() if k[0] == "XW")
    print(f"done {spec} {cy}_{ud} [{fp_iter + 1} fp]: PRL={slr['prl']:.4f} "
          f"AUC={slr['auc']:.4f} wW={wW:.3f} wXW={wXW:.3f}", flush=True)


if __name__ == "__main__":
    _canon = len(sys.argv) > 4 and sys.argv[4] == "canonical"
    run(int(sys.argv[1]), int(sys.argv[2]), sys.argv[3], canonical=_canon)
