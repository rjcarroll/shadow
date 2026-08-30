"""
Tier-1 probe suite: does the one-feature hypothetical-war patch remove the
onset-year artifact, and what does the gate/direction look like on the
counterfactual shadows?  (Run by scripts/run_tier1.sh after the FP fan-out.)

Probes (tier1 vs canonical, tau=0 columns per the re-baseline decision):
  1. Onset-year jump diagnostic: dE_gov/dE_opp on onset vs peace years
     (canonical: -0.161/-0.084 vs +/-0.004 -- the contamination signature).
  2. Same-year gate, LOCO logit, 25-draw predict-then-average
     (canonical same-year 0.213; canonical lagged 0.059; Baseline 0.057).
  3. Lagged gate on tier1 shadows (same-year ~ lagged equality = artifact gone).
  4. Direction: tilt/common sign fractions, same-year and lagged
     (references: 92%/100% at both timings on canonical).

ADDITIVE: writes results/spike/tier1_probes.parquet.
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np, pandas as pd, statsmodels.api as sm
from joblib import Parallel, delayed
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

TIER1 = INTERIM / "tier1"
DRAWS = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]
EG, EO = "E_gov_asinh", "E_opp_asinh"

data = load_analysis_data()
BASE = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    return float((nll - (-(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean())) / nll)


def shadow(src, cy, ud, lag):
    sh = pd.read_parquet(src / f"cy_shadow_{cy}_{ud}.parquet",
                         columns=["ccode", "year", EG, EO])
    if lag:
        sh = sh.copy(); sh["year"] = sh["year"] + 1
    return sh


def loco_logit(cy, ud, src, lag):
    df = BASE.merge(shadow(src, cy, ud, lag), on=["ccode", "year"], how="left")
    cov = BASELINE_VARS + [EG, EO]
    sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].dropna()
    y = sub.onset.values.astype(int); grp = sub.ccode.values
    X = sub[cov].values.astype(float)
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, grp):
        m = make_pipeline(StandardScaler(),
                          LogisticRegression(penalty=None, max_iter=5000)).fit(X[tr], y[tr])
        pred[te] = m.predict_proba(X[te])[:, 1]
    return sub[["ccode", "year", "onset"]].assign(pred=pred)


def gate(src, lag, label, rows):
    parts = Parallel(n_jobs=12, backend="loky")(
        delayed(loco_logit)(cy, ud, src, lag) for cy, ud in DRAWS)
    A = (pd.concat(parts).groupby(["ccode", "year"])
         .agg(onset=("onset", "first"), pred=("pred", "mean")).reset_index())
    y = A.onset.values.astype(int)
    r = dict(probe=label, aucpr=average_precision_score(y, A.pred),
             auc=roc_auc_score(y, A.pred), prl=prl(y, A.pred.values), n=len(A))
    rows.append(r)
    print(f"{label:28s} aucpr={r['aucpr']:.4f} prl={r['prl']:.4f} auc={r['auc']:.4f}", flush=True)


def direction(src, lag, label, rows):
    tilt_pos, common_neg = [], []
    for cy, ud in DRAWS:
        df = BASE.merge(shadow(src, cy, ud, lag), on=["ccode", "year"], how="left")
        sub = df[list(dict.fromkeys(["onset"] + BASELINE_VARS + [EG, EO]))].dropna().copy()
        sub["common"] = sub[EG] + sub[EO]; sub["tilt"] = sub[EO] - sub[EG]
        y = sub.onset.values.astype(int)
        X = sub[BASELINE_VARS + ["common", "tilt"]].astype(float)
        try:
            m = sm.Logit(y, sm.add_constant(X)).fit(disp=False, maxiter=1000)
            tilt_pos.append(m.params["tilt"] > 0); common_neg.append(m.params["common"] < 0)
        except Exception:
            pass
    r = dict(probe=label, tilt_frac=float(np.mean(tilt_pos)),
             common_frac=float(np.mean(common_neg)), n=len(tilt_pos))
    rows.append(r)
    print(f"{label:28s} tilt>0 in {r['tilt_frac']:.0%}, common<0 in {r['common_frac']:.0%} "
          f"of {r['n']} draws", flush=True)


if __name__ == "__main__":
    # 1. onset-year jump diagnostic on the draw-averaged tier1 shadow
    shs = [shadow(TIER1, cy, ud, lag=False) for cy, ud in DRAWS]
    sh = pd.concat(shs).groupby(["ccode", "year"]).mean().reset_index().sort_values(["ccode", "year"])
    for v in [EG, EO]:
        sh[f"d_{v}"] = sh[v] - sh.groupby("ccode")[v].shift(1)
    d = sh.merge(data[["ccode", "year", "onset"]], on=["ccode", "year"])
    j = d.dropna(subset=[f"d_{EG}"]).groupby("onset")[[f"d_{EG}", f"d_{EO}"]].mean()
    print("=== tier1 onset-year jump (canonical: gov -0.161 / opp -0.084 on onset=1) ===")
    print(j.round(3).to_string(), flush=True)

    rows = []
    print("\n=== gates (references: canonical same-year 0.213, canonical lagged 0.059, Baseline 0.057) ===")
    gate(TIER1, False, "tier1 same-year", rows)
    gate(TIER1, True, "tier1 lagged", rows)
    print("\n=== direction (references: 92%/100% at both timings on canonical) ===")
    direction(TIER1, False, "tier1 same-year", rows)
    direction(TIER1, True, "tier1 lagged", rows)
    pd.DataFrame(rows).to_parquet(SPIKE / "tier1_probes.parquet", index=False)
    j.to_parquet(SPIKE / "tier1_jump.parquet")
