"""
Evaluate the nested Stage-1 spot-check (run after scripts/nested_spotcheck.py
has finished its draws; see that docstring for the design).

Two questions, one per block:

A. SHADOW DRIFT -- how much does a country's shadow change when Stage 1 has
   never seen its host rows?  Reported per group (excluded training hosts /
   excluded never-onset / non-excluded control).  Drift in the CONTROL group
   bounds the pure effect of losing ~15% of training data; drift in the
   excluded groups beyond that bound is the leakage signature.

B. FORECAST TEST -- does the Stage-2 out-of-sample forecast for the excluded
   countries degrade when their shadows come from the nested model?  Per draw:
   logit + RF fit on non-excluded countries (original shadows, the estimation
   sample a real LOCO fold would use), then the excluded countries' rows are
   predicted twice -- original vs nested shadows -- and predictions averaged
   across available nested draws.  Metrics on the excluded subset only.
   If original ~ nested, the published gate numbers were not buoyed by
   Stage-1 leakage for these countries.

ADDITIVE: reads committed data + data/interim/nested/, writes
results/spike/nested_spotcheck.parquet (+ _drift.parquet).
Run:  .venv/bin/python scripts/nested_spotcheck_eval.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np, pandas as pd, statsmodels.api as sm
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE
from nested_spotcheck import EXCL, NESTED

RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=-1, random_state=20260608)
HOSTS = ["645", "840", "780", "540", "093"]
NEVER = ["200", "740", "020", "551", "510"]
SH = ["E_gov_asinh", "E_opp_asinh"]

data = load_analysis_data()
BASE = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()

draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)
         if (NESTED / f"cy_shadow_nested_{cy}_{ud}.parquet").exists()]
print(f"nested draws available: {draws}")
assert draws, "no nested draws found -- run scripts/nested_spotcheck.py first"


def logloss(y, p):
    p = np.clip(p, 1e-9, 1 - 1e-9)
    return float(-(y * np.log(p) + (1 - y) * np.log(1 - p)).mean())


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    return float((nll - (-(y * np.log(np.clip(p, 1e-9, 1 - 1e-9))
                           + (1 - y) * np.log(1 - np.clip(p, 1e-9, 1 - 1e-9))).mean())) / nll)


# ---------- A. shadow drift ----------
drift_rows = []
for cy, ud in draws:
    orig = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                           columns=["ccode", "year"] + SH)
    nest = pd.read_parquet(NESTED / f"cy_shadow_nested_{cy}_{ud}.parquet",
                           columns=["ccode", "year"] + SH)
    m = orig.merge(nest, on=["ccode", "year"], suffixes=("_o", "_n"))
    m["group"] = np.where(m.ccode.isin(HOSTS), "excl_host",
                 np.where(m.ccode.isin(NEVER), "excl_never", "control"))
    for g, sub in m.groupby("group"):
        row = dict(cy=cy, ud=ud, group=g, n=len(sub))
        for v in ["E_gov_asinh", "E_opp_asinh"]:
            d = sub[f"{v}_n"] - sub[f"{v}_o"]
            row[f"{v}_mad"] = float(d.abs().mean())
            row[f"{v}_bias"] = float(d.mean())
            row[f"{v}_corr"] = float(np.corrcoef(sub[f"{v}_o"], sub[f"{v}_n"])[0, 1])
        drift_rows.append(row)
D = pd.DataFrame(drift_rows)
D.to_parquet(SPIKE / "nested_spotcheck_drift.parquet", index=False)
print("\n=== A. shadow drift (mean across draws) ===")
print(D.groupby("group")[[c for c in D.columns if c.endswith(("_mad", "_bias", "_corr"))]]
      .mean().round(4).to_string())


# ---------- B. forecast test on the excluded countries ----------
def one(cy, ud, model_type):
    orig = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                           columns=["ccode", "year"] + SH)
    nest = pd.read_parquet(NESTED / f"cy_shadow_nested_{cy}_{ud}.parquet",
                           columns=["ccode", "year"] + SH)
    cov = BASELINE_VARS + SH
    df_o = BASE.merge(orig, on=["ccode", "year"], how="left")
    sub_o = df_o[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].dropna()
    tr = sub_o[~sub_o.ccode.isin(EXCL)]
    te_o = sub_o[sub_o.ccode.isin(EXCL)]
    df_n = BASE.merge(nest, on=["ccode", "year"], how="left")
    sub_n = df_n[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].dropna()
    te_n = sub_n[sub_n.ccode.isin(EXCL)]
    # align the two test frames on (ccode, year)
    te = te_o[["ccode", "year", "onset"]].merge(
        te_n[["ccode", "year"]], on=["ccode", "year"])
    te_o = te_o.set_index(["ccode", "year"]).loc[list(te.set_index(["ccode", "year"]).index)]
    te_n = te_n.set_index(["ccode", "year"]).loc[list(te.set_index(["ccode", "year"]).index)]

    ytr = tr.onset.values.astype(int)
    Xtr = tr[cov].values.astype(float)
    if model_type == "logit":
        m = sm.Logit(ytr, sm.add_constant(Xtr)).fit(disp=False, maxiter=1000)
        po = np.asarray(m.predict(sm.add_constant(te_o[cov].values.astype(float), has_constant="add")))
        pn = np.asarray(m.predict(sm.add_constant(te_n[cov].values.astype(float), has_constant="add")))
    else:
        m = RandomForestClassifier(**RF_KW).fit(Xtr, ytr)
        po = m.predict_proba(te_o[cov].values.astype(float))[:, 1]
        pn = m.predict_proba(te_n[cov].values.astype(float))[:, 1]
    out = te.copy()
    out["p_orig"], out["p_nest"] = po, pn
    return out


rows = []
for mt in ["logit", "rf"]:
    parts = [one(cy, ud, mt) for cy, ud in draws]
    A = (pd.concat(parts).groupby(["ccode", "year"])
         .agg(onset=("onset", "first"), p_orig=("p_orig", "mean"), p_nest=("p_nest", "mean"))
         .reset_index())
    y = A.onset.values.astype(int)
    for tag, p in [("orig", A.p_orig.values), ("nest", A.p_nest.values)]:
        rows.append(dict(model=mt, shadows=tag, n=len(A), onsets=int(y.sum()),
                         aucpr=average_precision_score(y, p), auc=roc_auc_score(y, p),
                         prl=prl(y, p), logloss=logloss(y, p)))
    # per-country log-loss deltas (positive = nested predicts WORSE)
    for cc, sub in A.groupby("ccode"):
        yy = sub.onset.values.astype(int)
        rows.append(dict(model=mt, shadows=f"delta_{cc}", n=len(sub), onsets=int(yy.sum()),
                         logloss=logloss(yy, sub.p_nest.values) - logloss(yy, sub.p_orig.values)))
R = pd.DataFrame(rows)
R.to_parquet(SPIKE / "nested_spotcheck.parquet", index=False)
print("\n=== B. forecast test, excluded countries (predict-then-average over nested draws) ===")
print(R[R.shadows.isin(["orig", "nest"])].round(4).to_string(index=False))
print("\nper-country log-loss delta (nested - original; positive = leak was helping):")
print(R[R.shadows.str.startswith("delta")].round(4).to_string(index=False))
