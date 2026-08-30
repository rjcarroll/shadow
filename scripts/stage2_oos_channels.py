"""
Channel decomposition of the Full spec: turn the WHOLE gov channel on/off vs the
WHOLE opp channel on/off (all 6 disaggregate types + the aggregate), out-of-sample.

Motivation (Rob): the aggregate E_opp is weak/insignificant in the linear logit. Ask whether
the opposition channel, FULLY DISAGGREGATED by intervention type, carries standalone OOS signal
that the aggregate masks. r(E_gov,E_opp)=0.94, so in the both-channels Full the two sponge each
other and neither coef is identified; killing one channel removes the competition and makes each
channel's *predictive* content estimable. Claim is PREDICTIVE, not causal (on-guardrail for R1/ed).

Ladder (per model class): Baseline -> {gov,opp}_aggregate -> {gov,opp}_full. Baseline/Entrants/Full
already live in results/spike/oos_loco_metrics.parquet (the main run); this adds the 4 channel rungs.

ADDITIVE & NON-DESTRUCTIVE: reads committed data/interim/, writes ONLY new files in results/spike/
  oos_loco_channel_preds.parquet
  oos_loco_channel_metrics.parquet
Mirrors stage2_oos_final.py exactly (LOCO, 25-draw PREDICT-then-average, logit+RF) so the new rungs
are bit-comparable to the existing 6 specs. Difference: outer-loop joblib parallelism over the 25
draws (RF n_jobs=1 per fit to avoid nested oversubscription) -> saturates the box, bit-identical.

Usage:  .venv/bin/python scripts/stage2_oos_channels.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from joblib import Parallel, delayed
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

# RF n_jobs=1: we parallelize the OUTER draw loop, so each fit stays single-core (no nesting).
RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=1, random_state=20260608)
N_JOBS = 14  # leave headroom on 18 cores for Rob's editor + the main run's tail

# The 6 disaggregate types in the pipeline's Full (NOT all 10 on disk: P5/rival/ally are excluded,
# to match the existing Full so Full_gov + Full_opp reconstructs it exactly).
TYPES = ["major", "contig", "coethnic", "colonial", "hostile", "doe"]
GOV7 = ["E_gov_asinh"] + [f"E_{t}_gov_asinh" for t in TYPES]
OPP7 = ["E_opp_asinh"] + [f"E_{t}_opp_asinh" for t in TYPES]


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


def _draw_oof(model_type, cov, sh_cols, cy, ud, base_df):
    """One imputation draw: build frame, LOCO out-of-fold preds. Runs in a worker process."""
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
    pred = oof_logit(y, X, g) if model_type == "logit" else oof_rf(y, X, g)
    return sub[["ccode", "year", "onset"]].assign(pred=pred)


def main():
    base = BASELINE_VARS
    specs = {
        "Entrants_gov": base + ["E_gov_asinh"],
        "Entrants_opp": base + ["E_opp_asinh"],
        "Full_gov": base + GOV7,
        "Full_opp": base + OPP7,
    }
    data = load_analysis_data()
    base_df = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()
    prevalence = float(data["onset"].mean())
    SPIKE.mkdir(parents=True, exist_ok=True)

    master, metrics = None, []
    for mt in ["logit", "rf"]:
        for spec_name, cov in specs.items():
            key = f"{mt}_{spec_name}"
            try:
                cov = list(dict.fromkeys(cov))
                sh_cols = [c for c in cov if c.startswith("E_")]
                draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)] if sh_cols else [(1, 1)]
                parts = Parallel(n_jobs=N_JOBS, backend="loky")(
                    delayed(_draw_oof)(mt, cov, sh_cols, cy, ud, base_df) for cy, ud in draws)
                A = (pd.concat(parts).groupby(["ccode", "year"])
                     .agg(onset=("onset", "first"), **{key: ("pred", "mean")}).reset_index())
                yy = A["onset"].values
                metrics.append(dict(model=key, n=len(A), onsets=int(yy.sum()),
                                    auc=roc_auc_score(yy, A[key]),
                                    aucpr=average_precision_score(yy, A[key]),
                                    prl=prl(yy, A[key].values)))
                master = A if master is None else master.merge(A.drop(columns="onset"),
                                                               on=["ccode", "year"], how="outer")
                master.to_parquet(SPIKE / "oos_loco_channel_preds.parquet", index=False)
                pd.DataFrame(metrics).to_parquet(SPIKE / "oos_loco_channel_metrics.parquet", index=False)
                m = metrics[-1]
                print(f"done {key:16s} auc={m['auc']:.4f} aucpr={m['aucpr']:.4f} prl={m['prl']:.4f}", flush=True)
            except Exception as e:  # noqa: BLE001
                print(f"FAILED {key}: {e}", flush=True)
    print(f"\nprevalence (AUC-PR no-skill) = {prevalence:.4f}")
    print(pd.DataFrame(metrics).round(4).to_string(index=False))


if __name__ == "__main__":
    main()
