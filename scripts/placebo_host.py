"""
Rich-baseline placebo for the Stage-2 gate (Rob concern #3).

The Baseline-vs-Entrants horse race pits 11 structural covariates (logit) against a
shadow built by flexible machinery from ~109 features that include host-side,
onset-relevant signal (prior war, recent interventions, instability, exclusion).
The telling detail: prior_war enters the Baseline-only logit at +0.53 and collapses
to -0.02 dead once the shadow is added -- absorption consistent with EITHER genuine
mediation (prior war raises intervention expectations) OR laundering (the shadow is
a flexibly-transformed host profile).  This script distinguishes them on predictive
content:

  Host        = Baseline covariates + the full numeric host-side (_A) Stage-1 block,
                through the same LOCO logit + RF.  "Same flexible machinery, same
                host information, no intervention labels."
  Host+Shadow = Host + the two aggregate shadow variables.  If the shadow still adds
                out of sample ON TOP of the rich host block, the intervention content
                is earning the gain directly.

Verdicts: Entrants (0.213/0.229 AUC-PR) vs Host decides whether the gate's lift is
laundered host signal; Host+Shadow minus Host is the shadow's value-added holding
the host profile fixed.  Note recent_int_A is a lagged intervention OBSERVABLE (a
legitimate covariate any baseline could carry), not a Stage-1 label; it stays in.

Host features come from dd_spat_{cy}_{ud} (one row per host-year; the exact columns
Stage 1 consumed), 25 draws, predict-then-average, exhaustive LOCO -- identical
protocol to stage2_oos_final.py.

ADDITIVE: reads committed data, writes results/spike/placebo_host.parquet.
Run:  .venv/bin/python scripts/placebo_host.py   (background; ~1 h)
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from joblib import Parallel, delayed
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=2, random_state=20260608)
DRAWS = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]

# numeric host-side block: every _A feature Stage 1 saw, minus identifiers, the
# outcome-adjacent onset_A universe marker, string group labels, and dyadic B_wasColOf_A
HOST_EXCLUDE = {"ccode_A", "cyear_A", "cname_A", "onset_A",
                "first_eth_grp_A", "second_eth_grp_A", "first_lin_grp_A",
                "second_lin_grp_A", "first_rel_grp_A", "second_rel_grp_A",
                "B_wasColOf_A"}

data = load_analysis_data()
BASE = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    mll = -(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean()
    return (nll - mll) / nll


def host_block(cy, ud):
    import pyarrow.parquet as pq
    cols = pq.ParquetFile(INTERIM / f"dd_spat_{cy}_{ud}.parquet").schema.names
    host = [c for c in cols if c.endswith("_A") and c not in HOST_EXCLUDE]
    dd = pd.read_parquet(INTERIM / f"dd_spat_{cy}_{ud}.parquet",
                         columns=["ccode_A", "year"] + host)
    hb = dd.groupby(["ccode_A", "year"]).first().reset_index().rename(columns={"ccode_A": "ccode"})
    return hb, host


def one_draw(cy, ud, spec, model_type):
    hb, host = host_block(cy, ud)
    df = BASE.merge(hb, on=["ccode", "year"], how="left")
    cov = list(dict.fromkeys(BASELINE_VARS + host))
    if spec == "HostPlusShadow":
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                             columns=["ccode", "year", "E_gov_trim_asinh", "E_opp_trim_asinh"])
        df = df.merge(sh, on=["ccode", "year"], how="left")
        cov += ["E_gov_trim_asinh", "E_opp_trim_asinh"]
    sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].copy()
    sub[host] = sub[host].fillna(0)          # Stage 1's own missing-value convention
    sub = sub.dropna()
    # the host block repeats several fixed structural covariates verbatim (ncontig_A,
    # oil_A, ...); drop exact duplicates so the unpenalized logit is estimable
    Xf = sub[cov].astype(float)
    keep, seen = [], []
    for c in cov:
        v = Xf[c].values
        if any(np.array_equal(v, s) for s in seen):
            continue
        keep.append(c); seen.append(v)
    cov = keep
    y = sub["onset"].values.astype(int)
    grp = sub["ccode"].values
    X = sub[cov].values.astype(float)
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, grp):
        if model_type == "logit":
            m = make_pipeline(StandardScaler(),
                              LogisticRegression(penalty=None, max_iter=5000)).fit(X[tr], y[tr])
            pred[te] = m.predict_proba(X[te])[:, 1]
        else:
            pred[te] = RandomForestClassifier(**RF_KW).fit(X[tr], y[tr]).predict_proba(X[te])[:, 1]
    return sub[["ccode", "year", "onset"]].assign(pred=pred)


def gate(spec, model_type, n_jobs):
    parts = Parallel(n_jobs=n_jobs, backend="loky")(
        delayed(one_draw)(cy, ud, spec, model_type) for cy, ud in DRAWS)
    A = (pd.concat(parts).groupby(["ccode", "year"])
         .agg(onset=("onset", "first"), pred=("pred", "mean")).reset_index())
    yy = A["onset"].values
    return A, dict(spec=spec, model=model_type, n=len(A), onsets=int(yy.sum()),
                   auc=roc_auc_score(yy, A["pred"]), aucpr=average_precision_score(yy, A["pred"]),
                   prl=prl(yy, A["pred"].values))


if __name__ == "__main__":
    hb, host = host_block(1, 1)
    print(f"host block: {len(host)} features, {len(hb)} host-years", flush=True)
    rows, preds = [], None
    for spec in ["Host", "HostPlusShadow"]:
        for mt, nj in [("logit", 12), ("rf", 6)]:
            A, r = gate(spec, mt, nj)
            rows.append(r)
            key = f"{mt}_{spec}"
            A = A.rename(columns={"pred": key})
            preds = A if preds is None else preds.merge(A.drop(columns="onset"),
                                                        on=["ccode", "year"], how="outer")
            print(f"done {key:22s} aucpr={r['aucpr']:.4f} prl={r['prl']:.4f} auc={r['auc']:.4f}", flush=True)
            pd.DataFrame(rows).to_parquet(SPIKE / "placebo_host.parquet", index=False)
            preds.to_parquet(SPIKE / "placebo_host_preds.parquet", index=False)
    ref = pd.read_parquet(SPIKE / "oos_loco_metrics.parquet").set_index("model")
    print("\nreference (committed): logit_Entrants aucpr={:.3f}  rf_Entrants aucpr={:.3f}".format(
        ref.loc["logit_Entrants", "aucpr"], ref.loc["rf_Entrants", "aucpr"]))
    print(pd.DataFrame(rows).round(4).to_string(index=False))
