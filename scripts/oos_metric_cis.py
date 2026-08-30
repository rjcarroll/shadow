"""
Country-cluster bootstrap CIs for the Stage-2 LOCO gate metrics (tab:oos-gate).

Resamples countries with replacement from the stored leave-one-country-out
predictions (results/spike/oos_loco_preds.parquet) and recomputes PRL /
ROC-AUC / AUC-PR per model, plus the PAIRED AUC-PR lift Entrants/Baseline
(both models evaluated on the same resample, so the lift CI is the paired
inferential object).  Conditions on the averaged-shadow predictions -- the
measurement layer is carried separately by the T x P machinery in 3.3.

ADDITIVE: reads committed data, writes results/spike/oos_metric_cis.parquet.
"""
import warnings; warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from pathlib import Path
from sklearn.metrics import roc_auc_score, average_precision_score, log_loss

ROOT = Path(__file__).resolve().parent.parent
SPIKE = ROOT / "results" / "spike"
B, SEED = 1000, 42
MODELS = ["logit_Baseline", "logit_Entrants", "logit_Full",
          "rf_Baseline", "rf_Entrants", "rf_Full"]

df = pd.read_parquet(SPIKE / "oos_loco_preds.parquet")
y = df.onset.values.astype(int); cc = df.ccode.values
uniq = np.unique(cc); pos = {c: np.where(cc == c)[0] for c in uniq}
rng = np.random.default_rng(SEED)


def prl(yb, p):
    p = np.clip(p, 1e-9, 1 - 1e-9)
    return 1 - log_loss(yb, p) / log_loss(yb, np.full_like(p, yb.mean()))


rows = []
for b in range(B):
    idx = np.concatenate([pos[c] for c in rng.choice(uniq, size=uniq.size, replace=True)])
    yb = y[idx]
    if yb.sum() in (0, len(yb)):
        continue
    r = {}
    for m in MODELS:
        p = df[m].values[idx]
        r[f"{m}_prl"] = prl(yb, p)
        r[f"{m}_auc"] = roc_auc_score(yb, p)
        r[f"{m}_aucpr"] = average_precision_score(yb, p)
    r["lift_logit"] = r["logit_Entrants_aucpr"] / r["logit_Baseline_aucpr"]
    r["lift_rf"] = r["rf_Entrants_aucpr"] / r["rf_Baseline_aucpr"]
    r["lift_logit_full"] = r["logit_Full_aucpr"] / r["logit_Baseline_aucpr"]
    r["lift_rf_full"] = r["rf_Full_aucpr"] / r["rf_Baseline_aucpr"]
    rows.append(r)

R = pd.DataFrame(rows)
out = []
for col in R.columns:
    lo, med, hi = np.percentile(R[col], [2.5, 50, 97.5])
    out.append(dict(stat=col, median=med, ci_lo=lo, ci_hi=hi))
O = pd.DataFrame(out).set_index("stat")
O["b_eff"] = len(R)
O.reset_index().to_parquet(SPIKE / "oos_metric_cis.parquet", index=False)

print(f"effective resamples: {len(R)} / {B}")
for m in MODELS:
    print(f"  {m:16s} " + "   ".join(
        f"{k} [{O.loc[f'{m}_{k}', 'ci_lo']:+.3f}, {O.loc[f'{m}_{k}', 'ci_hi']:+.3f}]"
        for k in ["prl", "auc", "aucpr"]))
for lf in ["lift_logit", "lift_rf"]:
    print(f"  {lf}: median {O.loc[lf, 'median']:.2f}x  95% CI "
          f"[{O.loc[lf, 'ci_lo']:.2f}, {O.loc[lf, 'ci_hi']:.2f}]   frac>1: {(R[lf] > 1).mean():.3f}")
