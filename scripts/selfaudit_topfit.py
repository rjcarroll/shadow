"""
Self-audit of the LOCO tab:topfit numbers: is 0.879 'too clean'?
Decompose the predict-then-average result into per-draw performance + the averaging (ensemble) bonus.
Baseline (no shadow) is the control: identical across draws -> zero bonus by construction.
ADDITIVE: reads committed data + results/spike/oos_loco_topfit.parquet; writes nothing.
"""
import sys, warnings, collections; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from joblib import Parallel, delayed
from sklearn.metrics import roc_auc_score, average_precision_score
from stage2_oos_topfit import oof_logit, entr, pair, FULLT, dd
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM

specs = {"Baseline": list(BASELINE_VARS), "Entrants": BASELINE_VARS + entr,
         "Full": BASELINE_VARS + entr + [c for t in FULLT for c in pair(t)]}
specs = {k: dd(v) for k, v in specs.items()}
data = load_analysis_data(); base_df = data[dd(["ccode", "year", "onset"] + BASELINE_VARS)].copy()


def per_draw(name, cov, sh, cy, ud):
    if sh:
        s = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year"] + sh)
        df = base_df.merge(s, on=["ccode", "year"], how="left")
    else:
        df = base_df
    sub = df[dd(["ccode", "year", "onset"] + cov)].dropna()
    y = sub.onset.values.astype(int); g = sub.ccode.values; X = sub[cov].values.astype(float)
    p = oof_logit(y, X, g)
    return name, roc_auc_score(y, p), average_precision_score(y, p)


jobs = []
for name, cov in specs.items():
    sh = [c for c in cov if c.startswith("E_")]
    draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)] if sh else [(0, 0)]
    jobs += [(name, cov, sh, cy, ud) for cy, ud in draws]
res = Parallel(n_jobs=14, backend="loky")(delayed(per_draw)(*j) for j in jobs)

by = collections.defaultdict(list)
for n, a, ap in res:
    by[n].append((a, ap))
pool = pd.read_parquet("results/spike/oos_loco_topfit.parquet").set_index("model")

print("PER-DRAW LOCO OOS (single draw, NOT averaged):")
for n in specs:
    arr = np.array(by[n]); au, ap = arr[:, 0], arr[:, 1]
    print(f"  {n:9s} AUC mean {au.mean():.3f} sd {au.std():.3f} [{au.min():.3f},{au.max():.3f}] | "
          f"AUC-PR mean {ap.mean():.3f} [{ap.min():.3f},{ap.max():.3f}]  (draws={len(arr)})")
print("\nPOOLED (predict-then-average, the reported number):")
for n in specs:
    print(f"  {n:9s} AUC {pool.loc[n,'oos_auc']:.3f} | AUC-PR {pool.loc[n,'oos_aucpr']:.3f}")
print("\nENSEMBLE BONUS (pooled - mean per-draw):")
for n in specs:
    if n == "Baseline":
        print(f"  {n:9s} (no shadow variation -> 0 by construction)"); continue
    arr = np.array(by[n])
    print(f"  {n:9s} AUC +{pool.loc[n,'oos_auc']-arr[:,0].mean():+.3f} | AUC-PR +{pool.loc[n,'oos_aucpr']-arr[:,1].mean():+.3f}")
