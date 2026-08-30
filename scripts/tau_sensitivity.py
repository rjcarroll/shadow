"""
Sensitivity of the Stage-2 LOCO gate to the aggregation cutpoint tau (Rob concern #2b).

constructing.tex admits tau was "selected by out-of-fold performance in the Stage-2
onset models" -- so the gate metric is partially tuned on itself as written.  This
script re-aggregates the stored per-dyad predictions (sl_preds_{cy}_{ud}.parquet,
nb06's canonical output incl. the OOF overwrite on onset rows) under a tau grid and
re-runs the full gate -- exhaustive leave-one-country-out, 25-draw predict-then-average,
logit + RF, Entrants spec -- at each tau.  Baseline is tau-invariant (no shadow).
If the lift is flat in tau, the tuning is innocuous and the sensitivity is reportable;
if not, tau selection must move inside the CV loop.

Self-check: tau = 0.001 must reproduce the committed cy_shadow E_*_trim aggregates and
the published gate metrics (oos_loco_metrics.parquet: logit 0.213 / rf 0.229 AUC-PR).

ADDITIVE: reads committed data, writes results/spike/tau_grid.parquet.
Run:  .venv/bin/python scripts/tau_sensitivity.py   (background; ~40 min)
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from joblib import Parallel, delayed
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

TAUS = [0.0, 0.001, 0.005, 0.01, 0.05, 0.1]
RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=2, random_state=20260608)
DRAWS = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]

data = load_analysis_data()
BASE = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    mll = -(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean()
    return (nll - mll) / nll


def shadow_at_tau(cy, ud, tau):
    sp = pd.read_parquet(INTERIM / f"sl_preds_{cy}_{ud}.parquet")
    g = sp.groupby(["ccode_A", "year"])
    agg = pd.DataFrame({
        "E_gov_tau": g.apply(lambda x: x.p_gov[x.p_gov >= tau].sum(), include_groups=False),
        "E_opp_tau": g.apply(lambda x: x.p_opp[x.p_opp >= tau].sum(), include_groups=False),
    }).reset_index().rename(columns={"ccode_A": "ccode"})
    agg["E_gov_tau"] = np.arcsinh(agg["E_gov_tau"])
    agg["E_opp_tau"] = np.arcsinh(agg["E_opp_tau"])
    return agg


def one_draw(cy, ud, tau, model_type):
    sh = shadow_at_tau(cy, ud, tau)
    df = BASE.merge(sh, on=["ccode", "year"], how="left")
    cov = BASELINE_VARS + ["E_gov_tau", "E_opp_tau"]
    sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].dropna()
    y = sub["onset"].values.astype(int)
    grp = sub["ccode"].values
    X = sub[cov].values.astype(float)
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, grp):
        if model_type == "logit":
            try:
                m = sm.Logit(y[tr], sm.add_constant(X[tr])).fit(disp=False, maxiter=1000)
            except np.linalg.LinAlgError:
                m = sm.Logit(y[tr], sm.add_constant(X[tr])).fit(disp=False, method="bfgs", maxiter=2000)
            pred[te] = m.predict(sm.add_constant(X[te], has_constant="add"))
        else:
            pred[te] = RandomForestClassifier(**RF_KW).fit(X[tr], y[tr]).predict_proba(X[te])[:, 1]
    return sub[["ccode", "year", "onset"]].assign(pred=pred)


def gate_at_tau(tau, model_type, n_jobs):
    parts = Parallel(n_jobs=n_jobs, backend="loky")(
        delayed(one_draw)(cy, ud, tau, model_type) for cy, ud in DRAWS)
    A = (pd.concat(parts).groupby(["ccode", "year"])
         .agg(onset=("onset", "first"), pred=("pred", "mean")).reset_index())
    yy = A["onset"].values
    return dict(tau=tau, model=model_type, n=len(A), onsets=int(yy.sum()),
                auc=roc_auc_score(yy, A["pred"]), aucpr=average_precision_score(yy, A["pred"]),
                prl=prl(yy, A["pred"].values))


if __name__ == "__main__":
    # self-check: tau=0.001 aggregation reproduces the committed cy_shadow trim columns
    chk = shadow_at_tau(1, 1, 0.001)
    ref = pd.read_parquet(INTERIM / "cy_shadow_1_1.parquet",
                          columns=["ccode", "year", "E_gov_trim_asinh", "E_opp_trim_asinh"])
    m = chk.merge(ref, on=["ccode", "year"])
    dg = float((m.E_gov_tau - m.E_gov_trim_asinh).abs().max())
    do = float((m.E_opp_tau - m.E_opp_trim_asinh).abs().max())
    print(f"self-check vs cy_shadow_1_1 (tau=0.001): max|dE_gov|={dg:.2e} max|dE_opp|={do:.2e}", flush=True)
    assert dg < 1e-9 and do < 1e-9, "tau=0.001 aggregation does not reproduce the committed shadow"

    rows = []
    for tau in TAUS:
        for mt, nj in [("logit", 12), ("rf", 6)]:
            rows.append(gate_at_tau(tau, mt, nj))
            r = rows[-1]
            print(f"tau={tau:<6g} {mt:5s} aucpr={r['aucpr']:.4f} prl={r['prl']:.4f} auc={r['auc']:.4f}", flush=True)
            pd.DataFrame(rows).to_parquet(SPIKE / "tau_grid.parquet", index=False)
    print("\n" + pd.DataFrame(rows).round(4).to_string(index=False))
