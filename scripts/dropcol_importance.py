"""
Drop-column (leave-one-COVARIATE-out) importance for the Entrants onset model
(Rob, 2026-07-12; §3.2 framing).  Lei-G'Sell-Rinaldo-Tibshirani-Wasserman (2018)
"LOCO" importance; equivalently the reduced-model architecture of McAlexander &
Mentch (2020), scored by out-of-sample performance change instead of their
prediction-difference test.

For the full Entrants model (baseline covariates + E_gov + E_opp), drop each
covariate one at a time (and each shadow, and BOTH shadows = Baseline), refit,
and measure how much out-of-sample AUC-PR / PRL degrade.  Puts the shadow on the
same footing as the canonical predictors.

DESIGN (consistency is the point):
  * CV = leave-one-COUNTRY-out (deterministic; folds identical across every
    dropped-covariate config -> point estimates are paired by construction).
    NOTE: "LOCO" is overloaded in this repo -- here the drop is a COVARIATE; the
    CV scheme is leave-one-country-out, same as the main gate.
  * Measurement uncertainty: 25 shadow draws, predict-then-average (each config's
    OOF prediction is averaged over the 25 draws), matching the gate.
  * Sampling uncertainty: PAIRED country-cluster bootstrap on the averaged OOF
    predictions -- one resample of countries drawn once, every config scored on
    the same resampled rows, differences (Entrants - dropped) taken WITHIN each
    resample.  This is tight and correct; comparing marginal CIs would not be.

Usage:  .venv/bin/python scripts/dropcol_importance.py logit      # ~1h (primary)
        .venv/bin/python scripts/dropcol_importance.py rf         # ~5h (robustness)
        .venv/bin/python scripts/dropcol_importance.py logit --avg # fast pilot (averaged shadow, 1 pass)
Writes results/spike/dropcol_<learner>.parquet.
"""
import os, sys, warnings
os.environ.setdefault("OMP_NUM_THREADS", "1")  # RF parallelism is over draws, not trees; logit single-thread
sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from joblib import Parallel, delayed
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.metrics import average_precision_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

EG, EO = "E_gov_asinh", "E_opp_asinh"
ENT = BASELINE_VARS + [EG, EO]
B, SEED = 1000, 20260712
RF_KW = dict(n_estimators=200, max_features="sqrt", n_jobs=1, random_state=20260608)
LABELS = {"prior_war": "Prior war", "lgdp_lag": "Log income", "lpop_lag": "Log pop",
          "lmtnest": "Mountains", "oil": "Oil", "nwstate": "New state", "instab_lag": "Instability",
          "instab": "Instability", "ethfrac": "Eth. frac.", "relfrac": "Rel. frac.",
          "polity2_lag": "Democracy", "ncontig": "Noncontiguous", "year": "Year trend",
          EG: "E_gov (shadow)", EO: "E_opp (shadow)"}

data = load_analysis_data()
BASE = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()


def prl_vec(y, p):
    q = np.clip(y.mean(), 1e-9, 1 - 1e-9)
    nll = -(y * np.log(q) + (1 - y) * np.log(1 - q)).mean()
    pp = np.clip(p, 1e-9, 1 - 1e-9)
    return (nll - (-(y * np.log(pp) + (1 - y) * np.log(1 - pp)).mean())) / nll


def _oof_one_draw(cols, learner, sub):
    y = sub["onset"].values.astype(int); g = sub["ccode"].values; X = sub[cols].values.astype(float)
    pred = np.zeros(len(y))
    for tr, te in LeaveOneGroupOut().split(X, y, g):
        if learner == "logit":
            m = make_pipeline(StandardScaler(), LogisticRegression(penalty=None, max_iter=5000)).fit(X[tr], y[tr])
        else:
            m = RandomForestClassifier(**RF_KW).fit(X[tr], y[tr])
        pred[te] = m.predict_proba(X[te])[:, 1]
    return pred


def averaged_oof(cols, learner, draws):
    """Predict-then-average OOF prediction over the shadow draws, aligned to the master index."""
    frames = []
    for cy, ud in draws:
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
        df = BASE.merge(sh, on=["ccode", "year"], how="left")
        sub = df[list(dict.fromkeys(["ccode", "year", "onset"] + ENT))].dropna().reset_index(drop=True)
        frames.append(sub.assign(pred=_oof_one_draw(cols, learner, sub))[["ccode", "year", "onset", "pred"]])
    A = pd.concat(frames).groupby(["ccode", "year"]).agg(onset=("onset", "first"), pred=("pred", "mean")).reset_index()
    return A


def main(learner, avg_only):
    draws = [(1, 1)] if avg_only else [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]
    if avg_only:  # fast pilot: average the shadow first, single LOCO pass per config
        shs = [pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
               for cy in range(1, 6) for ud in range(1, 6)]
        sh = pd.concat(shs).groupby(["ccode", "year"]).mean().reset_index()
        draws = [("avg", sh)]

    configs = {"Entrants": ENT}
    for v in BASELINE_VARS:
        configs[f"drop:{LABELS.get(v, v)}"] = [c for c in ENT if c != v]
    configs["drop:E_gov (shadow)"] = [c for c in ENT if c != EG]
    configs["drop:E_opp (shadow)"] = [c for c in ENT if c != EO]
    configs["drop:BOTH shadows"] = BASELINE_VARS

    # OOF prediction per config, aligned on (ccode, year)
    preds = {}
    for name, cols in configs.items():
        if avg_only:
            _, shdf = draws[0]
            sub = BASE.merge(shdf, on=["ccode", "year"], how="left")[
                list(dict.fromkeys(["ccode", "year", "onset"] + ENT))].dropna().reset_index(drop=True)
            preds[name] = sub.assign(pred=_oof_one_draw(cols, learner, sub))[["ccode", "year", "onset", "pred"]]
        else:
            preds[name] = averaged_oof(cols, learner, draws)
        print(f"  OOF done: {name}", flush=True)

    # master alignment: inner-join all configs on (ccode, year)
    key = ["ccode", "year"]
    M = preds["Entrants"][key + ["onset"]].copy()
    for name in configs:
        M = M.merge(preds[name][key + ["pred"]].rename(columns={"pred": name}), on=key)
    y = M["onset"].values.astype(int); cc = M["ccode"].values
    uniq = np.unique(cc); pos = {c: np.where(cc == c)[0] for c in uniq}
    ent_ap = average_precision_score(y, M["Entrants"]); ent_pr = prl_vec(y, M["Entrants"].values)

    # PAIRED country-cluster bootstrap: differences (Entrants - config) within each resample
    rng = np.random.default_rng(SEED)
    boot = {n: {"dap": [], "dpr": []} for n in configs if n != "Entrants"}
    if not avg_only:
        for _ in range(B):
            idx = np.concatenate([pos[c] for c in rng.choice(uniq, size=uniq.size, replace=True)])
            yb = y[idx]
            if yb.sum() == 0:
                continue
            ap_e = average_precision_score(yb, M["Entrants"].values[idx]); pr_e = prl_vec(yb, M["Entrants"].values[idx])
            for n in boot:
                boot[n]["dap"].append(ap_e - average_precision_score(yb, M[n].values[idx]))
                boot[n]["dpr"].append(pr_e - prl_vec(yb, M[n].values[idx]))

    rows = []
    for n in configs:
        ap = average_precision_score(y, M[n]); pr = prl_vec(y, M[n].values)
        r = dict(config=n, aucpr=ap, prl=pr, d_aucpr=ent_ap - ap, d_prl=ent_pr - pr)
        if n != "Entrants" and not avg_only and boot[n]["dpr"]:
            dp = np.array(boot[n]["dpr"]); da = np.array(boot[n]["dap"])
            r.update(dprl_lo=np.percentile(dp, 2.5), dprl_hi=np.percentile(dp, 97.5),
                     daucpr_lo=np.percentile(da, 2.5), daucpr_hi=np.percentile(da, 97.5),
                     dprl_pos=(dp > 0).mean())
        rows.append(r)
    R = pd.DataFrame(rows)
    tag = f"{learner}_avg" if avg_only else learner
    R.to_parquet(SPIKE / f"dropcol_{tag}.parquet", index=False)
    print(f"\n=== drop-column importance ({learner}{' AVG-pilot' if avg_only else ', 25-draw'}) ===")
    print(f"Entrants: AUC-PR {ent_ap:.3f}  PRL {ent_pr:.3f}   (prevalence floor {y.mean():.3f})")
    show = R[R.config != "Entrants"].sort_values("d_prl", ascending=False)
    for _, r in show.iterrows():
        ci = f"[{r.get('dprl_lo', np.nan):+.3f},{r.get('dprl_hi', np.nan):+.3f}]" if "dprl_lo" in r and pd.notna(r.get("dprl_lo")) else ""
        print(f"  {r.config:24s} dPRL {r.d_prl:+.3f} {ci:18s} dAUC-PR {r.d_aucpr:+.3f}")


if __name__ == "__main__":
    learner = sys.argv[1] if len(sys.argv) > 1 else "logit"
    avg_only = "--avg" in sys.argv
    main(learner, avg_only)
