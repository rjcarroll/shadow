"""
Stage-2 OOS recompute under the CORRECTED CV scheme (leave-one-country-out).

ADDITIVE & NON-DESTRUCTIVE: reads committed data in data/interim/ (via
spike_rf_significance.load_analysis_data), writes only to results/spike/. Nothing in
the committed pipeline/results is touched.

Context (see notes/cv-leave-one-country-out-fix.md): nb07's `cv_group` mis-implements
"leave-one-war-out" — `onset_id` is a per-country onset ORDINAL, not a war id, so the CV
buckets onsets by within-country index (73/191 in one fold) AND leaks a country's own years
into its prediction. Fix = leave-one-country-out (group by ccode).

STEP 1 (this file): logit OOS for all 10 specs, buggy-CV vs leave-one-country-out, on the
averaged frame. Confirms the fix and sizes the change to tab:topfit.
TODO next: RF under the SAME leave-one-country-out scheme (step 2); then the 25-draw
predict-then-average upgrade (per Rob: out-of-sample, across all 25 imputations, averaged
AFTER prediction, for both logit and RF). Report AUC-PR against the prevalence baseline (R2).

Usage:  .venv/bin/python scripts/stage2_oos_recompute.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import roc_auc_score, average_precision_score
from spike_rf_significance import load_analysis_data, BASELINE_VARS, RESULTS, SPIKE


def fit(y, X):
    try:
        return sm.Logit(y, X).fit(disp=False, maxiter=1000)
    except np.linalg.LinAlgError:
        return sm.Logit(y, X).fit(disp=False, method="bfgs", maxiter=2000)


def prl(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    mll = -(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean()
    return (nll - mll) / nll


def oof(y, X, g):
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, g):
        pred[te] = fit(y[tr], X[tr]).predict(X[te])
    return pred


def make_specs():
    base, entr = BASELINE_VARS, ["E_gov_trim_asinh", "E_opp_trim_asinh"]
    E = lambda t: [f"E_{t}_gov_trim_asinh", f"E_{t}_opp_trim_asinh"]
    return {
        "Baseline": base, "Entrants": base + entr,
        "Powers": base + entr + E("major"), "Neighbors": base + entr + E("contig"),
        "Coethnics": base + entr + E("coethnic"), "Rulers": base + entr + E("colonial"),
        "Rivals (bin)": base + entr + E("rival"), "Rivals (cts)": base + entr + E("hostile"),
        "DOE": base + entr + E("doe"),
        "Full": base + entr + E("major") + E("contig") + E("coethnic") + E("colonial")
                + E("hostile") + E("doe"),
    }


def main():
    d = load_analysis_data().sort_values(["ccode", "year"]).copy()
    # buggy cv_group (verbatim nb07 cell-3 logic) and the fix
    d["onset_id"] = d.groupby("ccode")["onset"].transform(lambda s: (s.cumsum() * s).replace(0, np.nan))
    d["cv_buggy"] = d["onset_id"].fillna(d.groupby("ccode").ngroup() + d["onset_id"].max() + 1)
    d["cv_loco"] = d["ccode"]
    prevalence = float(d["onset"].mean())

    rows = []
    for name, cov in make_specs().items():
        cov = list(dict.fromkeys(cov))
        if [c for c in cov if c not in d.columns]:
            print(f"SKIP {name} (missing cols)"); continue
        sub = d[["onset", "cv_buggy", "cv_loco"] + cov].dropna()
        y = sub["onset"].values
        X = sm.add_constant(sub[cov].values)
        pis = fit(y, X).predict(X)
        pb, pl = oof(y, X, sub["cv_buggy"].values), oof(y, X, sub["cv_loco"].values)
        rows.append(dict(model=name, n=len(y), onsets=int(y.sum()),
                         is_prl=prl(y, pis), is_auc=roc_auc_score(y, pis),
                         oos_prl_buggy=prl(y, pb), oos_auc_buggy=roc_auc_score(y, pb),
                         oos_prl_loco=prl(y, pl), oos_auc_loco=roc_auc_score(y, pl),
                         oos_aucpr_loco=average_precision_score(y, pl)))
        print(f"done {name}")
    R = pd.DataFrame(rows)
    SPIKE.mkdir(parents=True, exist_ok=True)
    R.to_parquet(SPIKE / "logit_step1_loco.parquet", index=False)
    pd.set_option("display.width", 200, "display.max_columns", 20)
    print(f"\nAUC-PR no-skill baseline (prevalence) = {prevalence:.4f}")
    print(R.round(4).to_string(index=False))


if __name__ == "__main__":
    main()
