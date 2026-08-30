"""
RF twin of stage2_oos_topfit.py: same 10 specs, same LOCO x 25 predict-then-average, RANDOM FOREST.
IS and OOS by the same estimator (predict-then-average) for apples-to-apples. NOTE: RF in-sample (IS)
is near-memorization and uninformative; the OOS columns are the meaningful comparison vs the logit table.
RF n_jobs=1 (outer loop parallelized). ADDITIVE -> results/spike/oos_loco_topfit_rf.parquet.
"""
import sys, warnings, collections; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from joblib import Parallel, delayed
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

N_JOBS = 14
RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=1, random_state=20260608)
dd = lambda xs: list(dict.fromkeys(xs))
entr = ["E_gov_asinh", "E_opp_asinh"]
TPAIR = {"Powers": "major", "Neighbors": "contig", "Coethnics": "coethnic", "Rulers": "colonial",
         "Rivals (bin)": "rival", "Rivals (cts)": "hostile", "DOE": "doe"}
pair = lambda t: [f"E_{t}_gov_asinh", f"E_{t}_opp_asinh"]
FULLT = ["major", "contig", "coethnic", "colonial", "hostile", "doe"]


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9); pp = np.clip(p, 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    mll = -(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean()
    return (nll - mll) / nll


def oof_rf(y, X, g):
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, g):
        pred[te] = RandomForestClassifier(**RF_KW).fit(X[tr], y[tr]).predict_proba(X[te])[:, 1]
    return pred


def main():
    data = load_analysis_data()
    base_df = data[dd(["ccode", "year", "onset"] + BASELINE_VARS)].copy()
    specs = {"Baseline": list(BASELINE_VARS), "Entrants": BASELINE_VARS + entr}
    for name, t in TPAIR.items():
        specs[name] = BASELINE_VARS + entr + pair(t)
    specs["Full"] = BASELINE_VARS + entr + [c for t in FULLT for c in pair(t)]
    specs = {k: dd(v) for k, v in specs.items()}

    def task(name, cov, sh, cy, ud):
        if sh:
            s = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year"] + sh)
            df = base_df.merge(s, on=["ccode", "year"], how="left")
        else:
            df = base_df
        sub = df[dd(["ccode", "year", "onset"] + cov)].dropna()
        y = sub["onset"].values.astype(int); g = sub["ccode"].values; X = sub[cov].values.astype(float)
        oos = oof_rf(y, X, g)
        is_ = RandomForestClassifier(**RF_KW).fit(X, y).predict_proba(X)[:, 1]
        return name, sub[["ccode", "year", "onset"]].assign(oos=oos, is_=is_), \
            (roc_auc_score(y, oos), average_precision_score(y, oos))

    jobs = []
    for name, cov in specs.items():
        sh = [c for c in cov if c.startswith("E_")]
        draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)] if sh else [(0, 0)]
        jobs += [(name, cov, sh, cy, ud) for cy, ud in draws]
    out = Parallel(n_jobs=N_JOBS, backend="loky")(delayed(task)(*j) for j in jobs)

    frames, perdraw = collections.defaultdict(list), collections.defaultdict(list)
    for name, fr, pdm in out:
        frames[name].append(fr); perdraw[name].append(pdm)
    rows = []
    for name in specs:
        A = (pd.concat(frames[name]).groupby(["ccode", "year"])
             .agg(onset=("onset", "first"), oos=("oos", "mean"), is_=("is_", "mean")).reset_index())
        y = A["onset"].values; pdm = np.array(perdraw[name])
        rows.append(dict(model=name, n=len(A), onsets=int(y.sum()),
                         is_prl=prl(y, A["is_"].values), is_auc=roc_auc_score(y, A["is_"]),
                         is_aucpr=average_precision_score(y, A["is_"]),
                         oos_prl=prl(y, A["oos"].values), oos_auc=roc_auc_score(y, A["oos"]),
                         oos_aucpr=average_precision_score(y, A["oos"]),
                         oos_auc_pd=pdm[:, 0].mean(), oos_aucpr_pd=pdm[:, 1].mean()))
    R = pd.DataFrame(rows)
    R.to_parquet(SPIKE / "oos_loco_topfit_rf.parquet", index=False)
    b = R.set_index("model").loc["Baseline"]; aug = R[R.model != "Baseline"]
    print(f"prevalence (AUC-PR no-skill) = {data['onset'].mean():.4f}  [RANDOM FOREST]")
    print(R.round(4).to_string(index=False))
    print(f"of {len(aug)} shadow-augmented specs, beat Baseline OOS:  AUC {int((aug.oos_auc>b.oos_auc).sum())}/{len(aug)}   AUC-PR {int((aug.oos_aucpr>b.oos_aucpr).sum())}/{len(aug)}")


if __name__ == "__main__":
    main()
