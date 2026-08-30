"""
Predictive subsumption under LOCO (logit, 25-draw predict-then-average), for BOTH benchmark proxies:
  - Cunningham (2016) Lake US security hierarchy  [us_SH1995, country-year; data/raw/cunningham/cunningham.dta]
  - Gibilisco & Montero (2024) structural P5 intervention prob [total_P5, cross-sectional, broadcast by
    country; read from replication/dataverse_files.zip]
Replaces the LR-significance subsumption with a Ward-Greenhill-Bakke predictive test: does adding the
proxy improve OOS prediction beyond the shadow, and does the shadow improve it beyond the proxy?
ADDITIVE -> results/spike/subsumption_loco.parquet.
"""
import sys, warnings, collections, zipfile; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from joblib import Parallel, delayed
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

N_JOBS = 14
dd = lambda xs: list(dict.fromkeys(xs))
entr = ["E_gov_asinh", "E_opp_asinh"]
FULLT = ["major", "contig", "coethnic", "colonial", "hostile", "doe"]
full_sh = entr + [f"E_{t}_{s}_asinh" for t in FULLT for s in ("gov", "opp")]
pad = lambda s: s.astype(int).astype(str).str.zfill(3)


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9); pp = np.clip(p, 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    mll = -(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean()
    return (nll - mll) / nll


def fit_logit(y, X):
    try:
        return sm.Logit(y, sm.add_constant(X)).fit(disp=False, maxiter=1000)
    except np.linalg.LinAlgError:
        return sm.Logit(y, sm.add_constant(X)).fit(disp=False, method="bfgs", maxiter=2000)


def oof_logit(y, X, g):
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, g):
        pred[te] = fit_logit(y[tr], X[tr]).predict(sm.add_constant(X[te], has_constant="add"))
    return pred


def run_proxy(base_df, proxy, label):
    common = base_df.dropna(subset=BASELINE_VARS + [proxy]).copy()
    print(f"\n### {label}: common sample {len(common)} cy, {common.ccode.nunique()} countries, {int(common.onset.sum())} onsets ###")
    specs = {"Baseline": list(BASELINE_VARS), f"Baseline+{label}": BASELINE_VARS + [proxy],
             "Entrants": BASELINE_VARS + entr, f"Entrants+{label}": BASELINE_VARS + entr + [proxy],
             "Full": BASELINE_VARS + full_sh, f"Full+{label}": BASELINE_VARS + full_sh + [proxy]}
    specs = {k: dd(v) for k, v in specs.items()}

    def task(name, cov, sh, cy, ud):
        df = common
        if sh:
            s = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year"] + sh)
            df = common.merge(s, on=["ccode", "year"], how="left")
        sub = df[dd(["ccode", "year", "onset"] + cov)].dropna()
        y = sub.onset.values.astype(int); g = sub.ccode.values; X = sub[cov].values.astype(float)
        return name, sub[["ccode", "year", "onset"]].assign(pred=oof_logit(y, X, g))

    jobs = []
    for name, cov in specs.items():
        sh = [c for c in cov if c.startswith("E_")]
        draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)] if sh else [(0, 0)]
        jobs += [(name, cov, sh, cy, ud) for cy, ud in draws]
    out = Parallel(n_jobs=N_JOBS, backend="loky")(delayed(task)(*j) for j in jobs)
    frames = collections.defaultdict(list)
    for name, fr in out:
        frames[name].append(fr)
    rows = []
    for name in specs:
        A = pd.concat(frames[name]).groupby(["ccode", "year"]).agg(onset=("onset", "first"), pred=("pred", "mean")).reset_index()
        y = A.onset.values
        rows.append(dict(proxy=label, model=name, n=len(A), onsets=int(y.sum()),
                         oos_prl=prl(y, A.pred.values), oos_auc=roc_auc_score(y, A.pred), oos_aucpr=average_precision_score(y, A.pred)))
    R = pd.DataFrame(rows); print(R.round(4).to_string(index=False)); g = R.set_index("model")
    print(f"  proxy adds beyond shadow?  Entrants {g.loc['Entrants','oos_aucpr']:.3f}->{g.loc['Entrants+'+label,'oos_aucpr']:.3f} | Full {g.loc['Full','oos_aucpr']:.3f}->{g.loc['Full+'+label,'oos_aucpr']:.3f}")
    print(f"  shadow adds beyond proxy?  Base+{label} {g.loc['Baseline+'+label,'oos_aucpr']:.3f} -> Entrants+{label} {g.loc['Entrants+'+label,'oos_aucpr']:.3f} / Full+{label} {g.loc['Full+'+label,'oos_aucpr']:.3f}")
    return R


def main():
    data = load_analysis_data()
    base_df = data[dd(["ccode", "year", "onset"] + BASELINE_VARS)].copy()  # ccode is str '002'
    # Cunningham Lake hierarchy (country-year)
    lake = pd.read_stata("data/raw/cunningham/cunningham.dta", columns=["ccode", "year", "us_SH1995"])
    lake["ccode"] = pad(lake["ccode"]); lake["year"] = lake["year"].astype(int)
    base_df = base_df.merge(lake, on=["ccode", "year"], how="left")
    # G&M structural P5 (cross-sectional -> broadcast by country)
    with zipfile.ZipFile("replication/dataverse_files.zip") as z:
        with z.open("Gibilisco_Montero_Intervention/matlab/baseline_model/conditionalInterventionProbs_replication.csv") as f:
            gm = pd.read_csv(f)
    gm["gm_P5"] = gm[["US", "UK", "FRN", "RUS", "CHN"]].sum(axis=1)
    gm["ccode"] = pad(gm["ccode"])
    base_df = base_df.merge(gm[["ccode", "gm_P5"]], on="ccode", how="left")

    R1 = run_proxy(base_df, "us_SH1995", "Lake")
    R2 = run_proxy(base_df, "gm_P5", "GM")
    pd.concat([R1, R2]).to_parquet(SPIKE / "subsumption_loco.parquet", index=False)
    print(f"\nprevalence (full sample) = {data.onset.mean():.4f}")


if __name__ == "__main__":
    main()
