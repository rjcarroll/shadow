"""
FINAL Stage-2 OOS pipeline: exhaustive leave-one-country-out, 25-draw PREDICT-then-average,
logit + RF, for Baseline / Entrants / Full.

ADDITIVE & NON-DESTRUCTIVE: reads committed data/interim/, writes only results/spike/.
This is the publication-grade version of the corrected OOS (see notes/cv-leave-one-country-out-fix.md).
Per Rob: out-of-sample, across ALL 25 imputations, averaged AFTER prediction, both model classes.

Saves INCREMENTALLY after each (model, spec) so a long run survives interruption:
  results/spike/oos_loco_preds.parquet    (ccode, year, onset, <model_spec> columns of averaged OOF preds)
  results/spike/oos_loco_metrics.parquet  (auc, aucpr, prl per model_spec)

Long run (~exhaustive LOCO × 25 draws × 6 model-specs). Run in background.
Usage:  .venv/bin/python scripts/stage2_oos_final.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=-1, random_state=20260608)


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    mll = -(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean()
    return (nll - mll) / nll


def oof_logit(y, X, groups):
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, groups):
        try:
            m = sm.Logit(y[tr], sm.add_constant(X[tr])).fit(disp=False, maxiter=1000)
        except np.linalg.LinAlgError:
            m = sm.Logit(y[tr], sm.add_constant(X[tr])).fit(disp=False, method="bfgs", maxiter=2000)
        pred[te] = m.predict(sm.add_constant(X[te], has_constant="add"))
    return pred


def oof_rf(y, X, groups):
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, groups):
        pred[te] = RandomForestClassifier(**RF_KW).fit(X[tr], y[tr]).predict_proba(X[te])[:, 1]
    return pred


def main():
    base, entr = BASELINE_VARS, ["E_gov_asinh", "E_opp_asinh"]
    E = lambda t: [f"E_{t}_gov_asinh", f"E_{t}_opp_asinh"]
    specs = {"Baseline": base, "Entrants": base + entr,
             "Full": base + entr + E("major") + E("contig") + E("coethnic")
                     + E("colonial") + E("hostile") + E("doe")}
    data = load_analysis_data()
    base_df = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()
    prevalence = float(data["onset"].mean())
    SPIKE.mkdir(parents=True, exist_ok=True)

    master = None
    metrics = []
    for mt, oof in [("logit", oof_logit), ("rf", oof_rf)]:
        for spec_name, cov in specs.items():
            key = f"{mt}_{spec_name}"
            try:
                cov = list(dict.fromkeys(cov))
                sh_cols = [c for c in cov if c.startswith("E_")]
                draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)] if sh_cols else [(1, 1)]
                parts = []
                for cy, ud in draws:
                    if sh_cols:
                        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                                             columns=["ccode", "year"] + sh_cols)
                        df = base_df.merge(sh, on=["ccode", "year"], how="left")
                    else:
                        df = base_df
                    sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + cov))].dropna()
                    y = sub["onset"].values.astype(int)
                    g = sub["ccode"].values
                    X = sub[cov].values.astype(float)
                    parts.append(sub[["ccode", "year", "onset"]].assign(pred=oof(y, X, g)))
                A = (pd.concat(parts).groupby(["ccode", "year"])
                     .agg(onset=("onset", "first"), **{key: ("pred", "mean")}).reset_index())
                yy = A["onset"].values
                metrics.append(dict(model=key, n=len(A), onsets=int(yy.sum()),
                                    auc=roc_auc_score(yy, A[key]), aucpr=average_precision_score(yy, A[key]),
                                    prl=prl(yy, A[key].values)))
                master = A if master is None else master.merge(A.drop(columns="onset"),
                                                               on=["ccode", "year"], how="outer")
                master.to_parquet(SPIKE / "oos_loco_preds.parquet", index=False)
                pd.DataFrame(metrics).to_parquet(SPIKE / "oos_loco_metrics.parquet", index=False)
                m = metrics[-1]
                print(f"done {key:16s} auc={m['auc']:.4f} aucpr={m['aucpr']:.4f} prl={m['prl']:.4f}", flush=True)
            except Exception as e:  # noqa: BLE001
                print(f"FAILED {key}: {e}", flush=True)
    print(f"\nprevalence (AUC-PR no-skill) = {prevalence:.4f}")
    print(pd.DataFrame(metrics).round(4).to_string(index=False))


if __name__ == "__main__":
    main()
