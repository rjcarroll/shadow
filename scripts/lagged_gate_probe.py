"""
Fast logit probes for the contamination question raised by the placebo (Rob #3).

Finding that motivates this: ongoing_wars_A(t) counts the war that BEGINS at t
(nb01 builds ongoing rows over range(start, end+1)), so it is definitionally >= 1
on onset years -- corr(onset, ongoing_wars_A) = 0.545 -- and a plain logit on the
host-side Stage-1 block hits LOCO AUC-PR 0.62 on the back of it.  Stage 1 consumed
this feature and the shadow enters Stage 2 SAME-YEAR, so the published gate could
be partly reading the outcome through the shadow.  (The Stage-2 baseline lags its
time-varying covariates against exactly this; the shadow never got the same
treatment.)

Probes (all LOCO logit, 25 draws, predict-then-average -- the fast learner only):
  HostClean      Baseline + host block MINUS ongoing_wars_A (the one mechanical
                 leak; everything else is F&L-magnitude).
  EntrantsLag    Baseline + shadow at t-1 (expectations formed before the onset
                 year -- cannot see the onset-year war).  The clean gate.
  EntrantsLag0   Same but tau=0 columns (E_gov_asinh), anticipating the tau=0
                 re-baseline.

Reference points: Entrants same-year 0.213; Baseline 0.057; Host-with-leak 0.62.

ADDITIVE: writes results/spike/lagged_gate_probe.parquet.
Run:  .venv/bin/python scripts/lagged_gate_probe.py    (~20 min)
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from joblib import Parallel, delayed
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE
from placebo_host import host_block

DRAWS = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]
data = load_analysis_data()
BASE = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    return float((nll - (-(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean())) / nll)


def loco_logit(sub, cov):
    y = sub["onset"].values.astype(int)
    grp = sub["ccode"].values
    X = sub[cov].values.astype(float)
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, grp):
        m = make_pipeline(StandardScaler(),
                          LogisticRegression(penalty=None, max_iter=5000)).fit(X[tr], y[tr])
        pred[te] = m.predict_proba(X[te])[:, 1]
    return sub[["ccode", "year", "onset"]].assign(pred=pred)


def one_draw(cy, ud, spec):
    if spec == "HostClean":
        hb, host = host_block(cy, ud)
        host = [c for c in host if c != "ongoing_wars_A"]
        df = BASE.merge(hb[["ccode", "year"] + host], on=["ccode", "year"], how="left")
        cov = list(dict.fromkeys(BASELINE_VARS + host))
        sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].copy()
        sub[host] = sub[host].fillna(0)
        sub = sub.dropna()
        # drop exact duplicates of baseline covariates (fixed structural twins)
        Xf = sub[cov].astype(float); keep, seen = [], []
        for c in cov:
            v = Xf[c].values
            if any(np.array_equal(v, s) for s in seen):
                continue
            keep.append(c); seen.append(v)
        return loco_logit(sub, keep)
    if spec in ("EntrantsLag", "EntrantsLag0"):
        cols = (["E_gov_trim_asinh", "E_opp_trim_asinh"] if spec == "EntrantsLag"
                else ["E_gov_asinh", "E_opp_asinh"])
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                             columns=["ccode", "year"] + cols)
        sh = sh.rename(columns={c: f"{c}_lag" for c in cols})
        sh["year"] = sh["year"] + 1              # E(t-1) attached to onset year t
        cov = BASELINE_VARS + [f"{c}_lag" for c in cols]
        df = BASE.merge(sh, on=["ccode", "year"], how="left")
        sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].dropna()
        return loco_logit(sub, cov)
    raise ValueError(spec)


if __name__ == "__main__":
    rows = []
    for spec in ["EntrantsLag", "EntrantsLag0", "HostClean"]:
        parts = Parallel(n_jobs=12, backend="loky")(
            delayed(one_draw)(cy, ud, spec) for cy, ud in DRAWS)
        A = (pd.concat(parts).groupby(["ccode", "year"])
             .agg(onset=("onset", "first"), pred=("pred", "mean")).reset_index())
        y = A.onset.values.astype(int)
        rows.append(dict(spec=spec, n=len(A), onsets=int(y.sum()),
                         aucpr=average_precision_score(y, A.pred),
                         auc=roc_auc_score(y, A.pred), prl=prl(y, A.pred.values)))
        print(f"done {spec:14s} aucpr={rows[-1]['aucpr']:.4f} prl={rows[-1]['prl']:.4f} "
              f"auc={rows[-1]['auc']:.4f}  (n={rows[-1]['n']}, onsets={rows[-1]['onsets']})", flush=True)
        pd.DataFrame(rows).to_parquet(SPIKE / "lagged_gate_probe.parquet", index=False)
    print("\nreference: Entrants same-year 0.213 / Baseline 0.057 / Host-with-leak 0.620")
