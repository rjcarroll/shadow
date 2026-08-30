r"""
export_numbers.py -- single source of truth for the reported statistics.

Reads committed pipeline outputs and writes, by construction:
  paper/generated/numbers.tex     \newcommand macros for in-prose numbers (S2 + S1)
  paper/tables/interveners.tex    booktabs tab:interveners (onset-coded, total >= 5)
  paper/tables/stage1-perf.tex    booktabs tab:stage1-perf (selected classifier perf)
  paper/tables/sl-components.tex   booktabs tab:sl-components (27 candidates)

The manuscript \input's these, so the page cannot diverge from the code.

Section-2 (data/coding): interveners/intervening-state counts are grouped by
COUNTRY via the pipeline's ccode->cname map (USSR/Russia 364/365, Ethiopia
530/529, Vietnam 816/818 each counted once).

Stage-1: per-component metrics are computed from the per-candidate OOF
predictions in `sl_oofpm_*` (extracted from the saved model files by
scripts/distill_sl_models.py; the `prl` column in sl_cv_metrics is the
*ensemble* value broadcast to every row, so it is NOT used); ensemble
metrics + AUC-PR + the PRL decomposition come from sl_oof; PCA/feature
counts + NNLS weights from `sl_model_meta.parquet`; burnout convergence
from `sl_spat_conv_*.parquet`.  Per-component log-loss uses scikit-learn's
default handling (no pre-clip), so hard-zero predictions from tree/MLP learners
are penalized uniformly; this makes a few overfit candidates' standalone PRL
strongly negative but does not affect the (smoothed) ensemble or the gap.

Run:  .venv/bin/python scripts/export_numbers.py
(Reads the sl_oofpm_*/sl_model_meta files written by
scripts/distill_sl_models.py, plus sl_oof_*, cy_shadow_*, dd_int_1_1,
results/spike, and the raw Regan/Cunningham/G&M inputs.)
"""
import sys, glob, re, json
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.metrics import log_loss, roc_auc_score, average_precision_score

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.data.interventions import build_intervention_table, load_post1999_interventions

INTERIM = ROOT / "data" / "interim"
GEN = ROOT / "paper" / "generated"; GEN.mkdir(exist_ok=True)
TAB = ROOT / "paper" / "tables";    TAB.mkdir(exist_ok=True)
SPIKE = ROOT / "results" / "spike"

P5_CCODES = {2, 200, 220, 710, 364, 365}          # USA, UK, France, China, USSR/Russia
DISPLAY = {"United States of America": "United States", "Russia": "USSR / Russia"}

# ======================================================================
# SECTION 2 -- data & coding counts
# ======================================================================
cy = pd.read_parquet(INTERIM / "country_year.parquet", columns=["ccode", "cname"])
cy["ccode"] = cy["ccode"].astype(int)
NAME = cy.drop_duplicates("ccode").set_index("ccode")["cname"].to_dict()
P5_NAMES = {NAME[c] for c in P5_CCODES if c in NAME}

regan = build_intervention_table(ROOT / "data/raw/regan/replication.10.26.01.dta")
post = load_post1999_interventions(ROOT / "data/raw/regan/post1999_interventions.csv")
comb = (pd.concat([regan, post]).sort_values("ddyear")
        .drop_duplicates("ddyear").reset_index(drop=True))

dd = pd.read_parquet(INTERIM / "dd_int_1_1.parquet",
                     columns=["ccode_A", "ccode_B", "year", "onset_A", "intervention"])
onset = dd[dd.onset_A == 1].copy()
oi = onset[onset.intervention != 0].copy()
oi["intervener"] = oi.ccode_B.astype(int).map(NAME)
oi["host"] = oi.ccode_A.astype(int).map(NAME)
coded = len(oi); p5_events = int(oi.ccode_B.astype(int).isin(P5_CCODES).sum())

N = {
    "CombinedInterventions": len(comb),
    "PostExtInterventions":  len(post),
    "PostExtHosts":          int(post.ccode_A.astype(int).map(NAME).nunique()),
    "OnsetCYsTotal":         onset.drop_duplicates(["ccode_A", "year"]).shape[0],
    "OnsetCYsColdWar":       onset[(onset.year >= 1946) & (onset.year <= 1999)]
                                 .drop_duplicates(["ccode_A", "year"]).shape[0],
    "OnsetCoded":            coded,
    "OnsetCodedGov":         int((onset.intervention == 1).sum()),
    "OnsetCodedOpp":         int((onset.intervention == 2).sum()),
    "OnsetHosts":            int(oi.host.nunique()),
    "OnsetInterveners":      int(oi.intervener.nunique()),
    "PFiveEvents":           p5_events,
    "PFivePct":              round(100 * p5_events / coded),
    "NonPFiveStates":        int(oi.loc[~oi.intervener.isin(P5_NAMES), "intervener"].nunique()),
}

# tab:interveners
g = (oi.assign(isP5=oi.intervener.isin(P5_NAMES)).groupby("intervener")
       .agg(gov=("intervention", lambda s: int((s == 1).sum())),
            opp=("intervention", lambda s: int((s == 2).sum())),
            tot=("intervention", "size"), isP5=("isP5", "max")).reset_index())
g["name"] = g.intervener.map(lambda n: DISPLAY.get(n, n))
g = g[g.tot >= 5].sort_values(["tot", "name"], ascending=[False, True]).reset_index(drop=True)


def _irow(r):
    dag = "\\textsuperscript{\\dag}" if r.isP5 else ""
    return f"    {r.name}{dag} & {r.gov} & {r.opp} & {r.tot} \\\\"


interveners_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}lrrr@{}}", "    \\toprule",
    "    State & Gov. & Opp. & Total \\\\", "    \\midrule",
    *[_irow(r) for r in g.itertuples()],
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Most frequent military interveners, 1946--2014 (states with fewer than",
    "    five interventions omitted).  \\textsuperscript{\\dag}~UN Security Council P5",
    "    member.  Gov.\\ and Opp.\\ from the Regan \\texttt{target} coding and the",
    "    post-1999 extension.}",
    "  \\label{tab:interveners}", "\\end{table}"]
(TAB / "interveners.tex").write_text("\n".join(interveners_tex) + "\n")

# ======================================================================
# STAGE 1 -- measurement model (from sl_oofpm_* / sl_model_meta + sl_oof +
# notebook). sl_oofpm_*/sl_model_meta are extracted from the saved model
# files by scripts/distill_sl_models.py; the fitted estimators are unused.
# ======================================================================
LBL = {"rf": "Random forest", "hgb": "HGB (lr=0.10)", "hgb_lo": "HGB (lr=0.05)",
       "ridge": "Ridge", "glmnet": "Elastic net", "lasso": "Lasso",
       "multinom": "Multinomial", "mlp_sm": "MLP (25)", "mlp_lg": "MLP (100, 50)"}
ORDER = ["mlp_lg", "rf", "hgb_lo", "hgb", "mlp_sm", "multinom", "ridge", "glmnet", "lasso"]
LOGITS = {"ridge", "glmnet", "lasso", "multinom"}

ens, comp, pca = [], [], {"X": [], "W": [], "XW": []}
feat = {}
META = pd.read_parquet(INTERIM / "sl_model_meta.parquet")
for f in sorted(glob.glob(str(INTERIM / "sl_oofpm_*.parquet"))):
    draw = re.search(r"sl_oofpm_(\d_\d)\.parquet", f).group(1)
    oofpm = pd.read_parquet(f)
    oof = pd.read_parquet(INTERIM / f"sl_oof_{draw}.parquet")
    y = oof.intervention.astype(int).values
    p = np.clip(np.bincount(y, minlength=3) / len(y), 1e-12, None); null = -(p * np.log(p)).sum()
    ep = oof[["p_none", "p_gov", "p_opp"]].values
    red = (-np.log(p[y])) - (-np.log(np.clip(ep[np.arange(len(y)), y], 1e-15, 1.0)))
    ens.append(dict(prl=1 - log_loss(y, ep, labels=[0, 1, 2]) / null,
                    auc=roc_auc_score(y, ep, multi_class="ovr", average="macro"),
                    ll=log_loss(y, ep, labels=[0, 1, 2]),
                    apr_gov=average_precision_score(y == 1, ep[:, 1]),
                    apr_opp=average_precision_score(y == 2, ep[:, 2]),
                    apr_any=average_precision_score(y > 0, ep[:, 1] + ep[:, 2]),
                    decomp=red[y > 0].sum() / red.sum(), null=null))
    dmeta = META[META.draw == draw]
    weights = {(t.mode, t.learner): t.weight for t in dmeta.itertuples()}
    wsum = sum(weights.values()) or 1.0
    for mode, name in dict.fromkeys(tuple(c.split("__")[:2]) for c in oofpm.columns):
        probs = oofpm[[f"{mode}__{name}__p{k}" for k in range(3)]].to_numpy()
        comp.append(dict(draw=draw, mode=mode, learner=name,
                         prl=1 - log_loss(y, probs, labels=[0, 1, 2]) / null,
                         auc=roc_auc_score(y, probs, multi_class="ovr", average="macro"),
                         ll=log_loss(y, probs, labels=[0, 1, 2]),
                         apr_any=average_precision_score(y > 0, probs[:, 1] + probs[:, 2]),
                         wt=weights.get((mode, name), 0.0) / wsum))
    counts = {t.mode: (int(t.pca_ncomp), int(t.n_feat)) for t in dmeta.itertuples()}
    for m in ("X", "W", "XW"):
        pca[m].append(counts[m][0]); feat[m] = counts[m][1]

E = pd.DataFrame(ens); C = pd.DataFrame(comp)
cm = C.groupby(["learner", "mode"]).agg(
        prl=("prl", "mean"), prl_sd=("prl", "std"), auc=("auc", "mean"), auc_sd=("auc", "std"),
        ll=("ll", "mean"), ll_sd=("ll", "std"), apr=("apr_any", "mean"), apr_sd=("apr_any", "std"),
        wt=("wt", "mean")).reset_index()


def _cell(learner, mode, col):
    row = cm[(cm.learner == learner) & (cm["mode"] == mode)]
    return float(row[col].iloc[0]) if len(row) else float("nan")


def _ms(learner, mode, col):           # (mean, sd) for a component cell
    return (_cell(learner, mode, col), _cell(learner, mode, col + "_sd"))


# best single logit on XW, per draw, then mean -> the ensemble gap
bl = C[(C.learner.isin(LOGITS)) & (C["mode"] == "XW")].groupby("draw").prl.max()
best_logit = bl.mean()
# per-learner & per-feature-set weight (mean of per-draw shares)
wl = C.groupby(["draw", "learner"]).wt.sum().groupby("learner").mean()
_wmg = C.groupby(["draw", "mode"]).wt.sum().groupby("mode")
wm, wm_sd = _wmg.mean(), _wmg.std()
# burnout convergence from sl_spat_conv_*.parquet (frozen-model burnout diagnostic,
# regenerated for the current canonical pipeline by scripts/burnout_rerun.py). The
# `deltas` column is the per-iteration mean-|Δ spatial lag| trajectory (one JSON list
# per draw); iters = converged index (matches the appendix "converges in N iterations").
iters, deltas, starts = [], [], []
for f in sorted(glob.glob(str(INTERIM / "sl_spat_conv_*.parquet"))):
    dl = json.loads(pd.read_parquet(f, columns=["deltas"])["deltas"].iloc[0])
    starts.append(dl[0])                    # residual at the 5-iteration approximation (all draws)
    if dl[-1] < 5e-4:                       # converged draw
        iters.append(len(dl) - 1); deltas.append(dl[-1])

N1 = {
    "SLprl":        round(E.prl.mean() * 100, 1),
    "SLrocauc":     f"{E.auc.mean():.3f}",
    "SLlogloss":    round(E.ll.mean(), 3),
    "SLnullauc":    "0.500",
    "SLnullll":     round(E.null.mean(), 3),
    "SLaucprGov":   round(E.apr_gov.mean(), 2),
    "SLaucprOpp":   round(E.apr_opp.mean(), 2),
    "SLaucprAny":   round(E.apr_any.mean(), 2),
    "WtRF":         round(wl.get("rf", 0) * 100, 1),
    "WtMLP":        round((wl.get("mlp_lg", 0) + wl.get("mlp_sm", 0)) * 100, 1),
    "WtMultinom":   round(wl.get("multinom", 0) * 100, 1),
    "WtHGB":        round((wl.get("hgb", 0) + wl.get("hgb_lo", 0)) * 100, 1),
    "WtX":          round(wm.get("X", 0) * 100, 1),
    "WtW":          round(wm.get("W", 0) * 100, 1),
    "WtXW":         round(wm.get("XW", 0) * 100, 1),
    "WtXsd":        round(wm_sd.get("X", 0) * 100, 1),
    "WtWsd":        round(wm_sd.get("W", 0) * 100, 1),
    "WtXWsd":       round(wm_sd.get("XW", 0) * 100, 1),
    "BestLogitPRL": round(best_logit * 100, 1),
    "EnsembleGap":  round((E.prl.mean() - best_logit) * 100, 1),
    "PRLfromEvents": round(E.decomp.mean() * 100),
    "PRLfromCalib":  round((1 - E.decomp.mean()) * 100),
    "PCAxwLo":      min(pca["XW"]), "PCAxwHi": max(pca["XW"]),
    "FeatXW":       feat["XW"], "FeatX": feat["X"], "FeatW": feat["W"],
    "BurnoutLo":    min(iters), "BurnoutHi": max(iters), "BurnoutMean": round(np.mean(iters), 1),
    "BurnoutDeltaLo": round(min(deltas) * 1e4, 1), "BurnoutDeltaHi": round(max(deltas) * 1e4, 1),
    "BurnoutStart":  f"{np.mean(starts):.3f}",
    "BurnoutStartX": str(int(round(np.mean(starts) / 1e-4 / 10) * 10)),
}

# ======================================================================
# SECTION 3 -- shadow example values (cy_shadow, mean over 25 draws)
# ======================================================================
_cyx = pd.concat([pd.read_parquet(f, columns=["ccode", "year", "E_gov", "E_opp"])
                  for f in sorted(glob.glob(str(INTERIM / "cy_shadow_*.parquet")))])
_cym = _cyx.groupby(["ccode", "year"])[["E_gov", "E_opp"]].mean()


def _shadow(ccode, year, side):          # mean E across 25 draws, 2 dp (keep trailing zero)
    return f"{float(_cym.loc[(ccode, year), 'E_gov' if side == 'G' else 'E_opp']):.2f}"


# Collinearity of the two channels (the Stage-2 regressors are the asinh sums), as the
# per-draw correlation averaged over the 25 measurement draws -- reported as $r \approx$ in prose.
_corr_egeo = np.mean([
    (lambda d: d["E_gov_asinh"].corr(d["E_opp_asinh"]))(
        pd.read_parquet(f, columns=["E_gov_asinh", "E_opp_asinh"]).dropna())
    for f in sorted(glob.glob(str(INTERIM / "cy_shadow_*.parquet")))])


N2 = {                                   # Cold-War internationalized-civil-war showcases
    "ShAngolaG":   _shadow("540", 1976, "G"),
    "ShAngolaO":   _shadow("540", 1976, "O"),
    "ShEthiopiaG": _shadow("530", 1978, "G"),
    "ShAfghanG":   _shadow("700", 1979, "G"),
    "ShAfghanO":   _shadow("700", 1979, "O"),
}

# ── convergent-validity correlations + top-dyad maxima (from source) ──
# Lake (Cunningham us_SH1995) and G&M total-P5 intervention prob., per-country E^G.
_egc = _cyx.groupby("ccode")["E_gov"].mean()
_egc.index = _egc.index.astype(int)
_lk = (pd.read_stata(ROOT / "data/raw/cunningham/cunningham.dta", columns=["ccode", "year", "us_SH1995"])
         .assign(ccode=lambda d: d.ccode.astype(int)).groupby("ccode")["us_SH1995"].mean())
_L = pd.concat([_egc.rename("E_gov"), _lk], axis=1).dropna()
# Gibilisco & Montero structural P5 probabilities: single CSV extracted
# from their Dataverse archive (see data/README.md for provenance).
_gm = pd.read_csv(ROOT / "data/raw/gm/conditionalInterventionProbs_replication.csv")
_gm5 = (_gm.assign(total_P5=_gm[["US", "UK", "FRN", "RUS", "CHN"]].sum(axis=1),
                   ccode=_gm.ccode.astype(int)).groupby("ccode")["total_P5"].mean())
_G = pd.concat([_egc.rename("E_gov"), _gm5], axis=1).dropna()
_oofall = pd.concat([pd.read_parquet(f, columns=["ccode_A", "ccode_B", "year", "intervention", "p_gov", "p_opp"])
                     for f in sorted(glob.glob(str(INTERIM / "sl_oof_*.parquet")))])
_oofm = _oofall.groupby(["ccode_A", "ccode_B", "year"])[["p_gov", "p_opp"]].mean()
N3 = {
    "CorrLake": f"{_L.E_gov.corr(_L.us_SH1995):.2f}", "NLake": len(_L),
    "CorrGM":   f"{_G.E_gov.corr(_G.total_P5):.2f}",   "NGM":   len(_G),
    "CorrEgEo": f"{_corr_egeo:.2f}",   # E^G/E^O channel collinearity (per-draw mean)
    "pGovMax":  f"{_oofm.p_gov.max():.2f}",            "pOppMax": f"{_oofm.p_opp.max():.2f}",
    "DyadIntPct": f"{100 * (_oofall.intervention != 0).mean():.1f}",
}
# n_B (potential interveners/CY) + universal-FP convergence, from source
_nb = pd.read_parquet(sorted(glob.glob(str(INTERIM / "cy_shadow_*.parquet")))[0], columns=["n_B"])["n_B"]
_fpd = pd.read_parquet(INTERIM / "sl_fp_diag.parquet")
_fpit = _fpd.groupby(["cy", "ud"]).fp_iter.max() + 1
_fpconv = _fpd.sort_values("fp_iter").groupby(["cy", "ud"]).converged.last()
N3["NumInterveners"] = str(int(round(_nb.median() / 5) * 5))
N3["MaxInterveners"] = str(int(_nb.max()))
N3["FPconv"] = str(int(_fpconv.sum()))
N3["FPiterMean"] = str(int(round(_fpit.mean())))
N3["AucprBase"] = f"{(_oofall.intervention != 0).mean():.3f}"  # any-intervention AUC-PR no-skill baseline

# ── tab:shadow-dyads: top-5 predicted interveners for three canonical onsets ──
_DISPLAY_DYAD = {2: "United States", 200: "United Kingdom", 364: "USSR", 365: "USSR",
                 490: "Zaire", 678: "North Yemen", 680: "South Yemen"}
def _dname(c):
    return _DISPLAY_DYAD.get(int(c), NAME.get(int(c), str(c)))
def _pp(x):
    return f"{x:.2f}"[1:] if 0 <= x < 1 else f"{x:.2f}"
_dyad = (_oofall.assign(ccode_A=_oofall.ccode_A.astype(int), ccode_B=_oofall.ccode_B.astype(int),
                        year=_oofall.year.astype(int))
         .groupby(["ccode_A", "ccode_B", "year"]).agg(
             iv=("intervention", "first"), pg=("p_gov", "mean"), po=("p_opp", "mean")).reset_index())
_drows, _hit, _tot = [], 0, 0
for _name, _host, _yr in [("Angola", 540, 1976), ("Ethiopia", 530, 1978), ("Afghanistan", 700, 1978)]:
    _s = _dyad[(_dyad.ccode_A == _host) & (_dyad.year == _yr)]
    _nint = int((_s.iv != 0).sum()); _tot += _nint
    _tg = _s.nlargest(5, "pg").reset_index(drop=True)
    _to = _s.nlargest(5, "po").reset_index(drop=True)
    _hit += int(_s[(_s.iv == 1) & (_s.ccode_B.isin(_tg.ccode_B))].shape[0])
    _hit += int(_s[(_s.iv == 2) & (_s.ccode_B.isin(_to.ccode_B))].shape[0])
    _drows.append(f"    \\multicolumn{{6}}{{c}}{{\\textbf{{{_name} ({_yr}) --- {_nint} actual interventions}}}} \\\\")
    _drows.append("    \\cmidrule(lr){1-6}")
    for _i in range(5):
        _g, _o = _tg.iloc[_i], _to.iloc[_i]
        _gp5 = "$^\\ast$" if int(_g.ccode_B) in P5_CCODES else ""
        _op5 = "$^\\ast$" if int(_o.ccode_B) in P5_CCODES else ""
        _gck = "$\\checkmark$" if _g.iv == 1 else ""
        _ock = "$\\checkmark$" if _o.iv == 2 else ""
        _drows.append(f"    {_dname(_g.ccode_B)}{_gp5} & {_pp(_g.pg)} & {_gck} & "
                      f"{_dname(_o.ccode_B)}{_op5} & {_pp(_o.po)} & {_ock} \\\\")
    if _host != 700:
        _drows.append("    \\addlinespace")
N3["DyadHits"] = f"{_hit} of {_tot}"
_dyad_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\small", "  \\setlength{\\tabcolsep}{4pt}",
    "  \\begin{tabular}{@{}lr c @{\\quad} lr c@{}}", "    \\toprule",
    "    \\textbf{Gov-biased} & $\\hat{p}$ & & \\textbf{Opp-biased} & $\\hat{p}$ & \\\\",
    "    \\midrule", *_drows, "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Top five predicted interveners for three canonical onsets; out-of-fold",
    "    probabilities averaged across the 25 imputation draws.  $^\\ast$P5 member;",
    "    $\\checkmark$ the state actually intervened in that direction (Regan coding).}",
    "  \\label{tab:shadow-dyads}", "\\end{table}"]
(TAB / "shadow-dyads.tex").write_text("\n".join(_dyad_tex) + "\n")

# ======================================================================
# SECTION 3.2 -- prediction gate + subsumption (OOS leave-one-country-out)
#   gate:  results/spike/oos_loco_metrics.parquet   (logit/rf x Baseline/Entrants/Full)
#   subs:  results/spike/subsumption_loco.parquet   (nested proxy comparison)
# Full rows are DEFERRED to Section 3.4 (disaggregation not yet introduced at 3.2).
# ======================================================================
_gate = pd.read_parquet(SPIKE / "oos_loco_metrics.parquet").set_index("model")
_sub = pd.read_parquet(SPIKE / "subsumption_loco.parquet")


def _gv(model, col):
    return float(_gate.loc[model, col])


def _sv(proxy, model, col):
    return float(_sub[(_sub.proxy == proxy) & (_sub.model == model)][col].iloc[0])


def _comma(n):
    return f"{int(n):,}".replace(",", "{,}")


_oos_n, _oos_onsets = int(_gate.loc["logit_Baseline", "n"]), int(_gate.loc["logit_Baseline", "onsets"])
N4 = {
    "OosN":           _comma(_oos_n),
    "OosOnsets":      str(_oos_onsets),
    "OosPrevFloor":   f"{_oos_onsets / _oos_n:.3f}",
    "OosBaseAucpr":   f"{_gv('logit_Baseline', 'aucpr'):.3f}",
    "OosEntAucpr":    f"{_gv('logit_Entrants', 'aucpr'):.3f}",
    "OosBaseAucprRF": f"{_gv('rf_Baseline', 'aucpr'):.3f}",
    "OosEntAucprRF":  f"{_gv('rf_Entrants', 'aucpr'):.3f}",
    "OosGateLift":    f"{_gv('logit_Entrants', 'aucpr') / _gv('logit_Baseline', 'aucpr'):.1f}",
    "OosGateLiftRF":  f"{_gv('rf_Entrants', 'aucpr') / _gv('rf_Baseline', 'aucpr'):.1f}",
    "OosNLake":       _comma(_sv("Lake", "Baseline", "n")),
    "OosNGM":         _comma(_sv("GM", "Baseline", "n")),
    "SubEntLake":     f"{_sv('Lake', 'Entrants', 'oos_aucpr'):.3f}",
    "SubEntPlusLake": f"{_sv('Lake', 'Entrants+Lake', 'oos_aucpr'):.3f}",
    "SubEntGM":       f"{_sv('GM', 'Entrants', 'oos_aucpr'):.3f}",
    "SubEntPlusGM":   f"{_sv('GM', 'Entrants+GM', 'oos_aucpr'):.3f}",
    "SubBasePlusLake": f"{_sv('Lake', 'Baseline+Lake', 'oos_aucpr'):.3f}",
    "SubBasePlusGM":  f"{_sv('GM', 'Baseline+GM', 'oos_aucpr'):.3f}",
}


# country-cluster bootstrap CIs on the gate metrics (scripts/oos_metric_cis.py)
_mci = pd.read_parquet(SPIKE / "oos_metric_cis.parquet").set_index("stat")


def _ci(stat, fmt=".3f"):
    return f"[{_mci.loc[stat, 'ci_lo']:{fmt}},\\ {_mci.loc[stat, 'ci_hi']:{fmt}}]"


N4.update({
    "OosGateLiftCiLo":   f"{_mci.loc['lift_logit', 'ci_lo']:.1f}",
    "OosGateLiftCiHi":   f"{_mci.loc['lift_logit', 'ci_hi']:.1f}",
    "OosGateLiftRFCiLo": f"{_mci.loc['lift_rf', 'ci_lo']:.1f}",
    "OosGateLiftRFCiHi": f"{_mci.loc['lift_rf', 'ci_hi']:.1f}",
    # Full (heterogeneous-utilities) model -- the strong prediction result (Section 3.4)
    "OosFullAucpr":      f"{_gv('logit_Full', 'aucpr'):.3f}",
    "OosFullAucprRF":    f"{_gv('rf_Full', 'aucpr'):.3f}",
    "OosFullPrl":        f"{_gv('logit_Full', 'prl'):.3f}",
    "OosFullLift":       f"{_gv('logit_Full', 'aucpr') / _gv('logit_Baseline', 'aucpr'):.1f}",
    "OosFullLiftCiLo":   f"{_mci.loc['lift_logit_full', 'ci_lo']:.1f}",
    "OosFullLiftCiHi":   f"{_mci.loc['lift_logit_full', 'ci_hi']:.1f}",
    # Full subsumes the proxies (strong version; aggregate version above)
    "SubFullLake":       f"{_sv('Lake', 'Full', 'oos_aucpr'):.3f}",
    "SubFullPlusLake":   f"{_sv('Lake', 'Full+Lake', 'oos_aucpr'):.3f}",
    "SubFullGM":         f"{_sv('GM', 'Full', 'oos_aucpr'):.3f}",
    "SubFullPlusGM":     f"{_sv('GM', 'Full+GM', 'oos_aucpr'):.3f}",
})


# tab:oos-gate -- Baseline vs Entrants, logit + RF panels ("long" layout: a CI row
# under each estimate row, paired AUC-PR lift row closing each panel). Full deferred to 3.4.
def _grow(spec, learner):
    r = _gate.loc[f"{learner}_{spec}"]
    return f"    \\quad {spec} & ${r.prl:.3f}$ & ${r.auc:.3f}$ & ${r.aucpr:.3f}$ \\\\"


def _cirow(spec, learner):
    cis = " & ".join("{\\footnotesize $" + _ci(f"{learner}_{spec}_{m}") + "$}" for m in ("prl", "auc", "aucpr"))
    return f"    & {cis} \\\\[2pt]"


def _liftrow(learner, key, spec="Entrants"):
    lift = _gate.loc[f"{learner}_{spec}", "aucpr"] / _gate.loc[f"{learner}_Baseline", "aucpr"]
    label = "AUC-PR lift, paired" if spec == "Entrants" else f"AUC-PR lift ({spec}), paired"
    return (f"    \\quad {label} & & & {lift:.1f}$\\times$ "
            "{\\footnotesize $" + _ci(key, ".1f") + "$} \\\\")


_gate_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}l rrr@{}}", "    \\toprule",
    "    & PRL & ROC-AUC & AUC-PR \\\\", "    \\midrule",
    "    \\textit{Logit} \\\\",
    _grow("Baseline", "logit"), _cirow("Baseline", "logit"),
    _grow("Entrants", "logit"), _cirow("Entrants", "logit"),
    _grow("Full", "logit"), _cirow("Full", "logit"),
    _liftrow("logit", "lift_logit"),
    _liftrow("logit", "lift_logit_full", spec="Full"),
    "    \\addlinespace",
    "    \\textit{Random forest} \\\\",
    _grow("Baseline", "rf"), _cirow("Baseline", "rf"),
    _grow("Entrants", "rf"), _cirow("Entrants", "rf"),
    _grow("Full", "rf"), _cirow("Full", "rf"),
    _liftrow("rf", "lift_rf"),
    _liftrow("rf", "lift_rf_full", spec="Full"),
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Out-of-sample onset prediction under leave-one-country-out",
    f"    cross-validation ($N = {N4['OosN']}$ country-years, {_oos_onsets} onsets;",
    f"    AUC-PR no-skill baseline $= {N4['OosPrevFloor']}$).",
    "    \\textit{Entrants} adds the two aggregate shadow variables to the",
    "    \\textit{Baseline}; \\textit{Full} resolves the shadow by intervener type",
    "    (Section~\\ref{sec:heterogeneity}).  Brackets hold 95\\% intervals from a",
    "    country-cluster bootstrap of the held-out predictions ($B = 1{,}000$); the lift",
    "    rows report the AUC-PR ratio over \\textit{Baseline}, paired within each resample.}",
    "  \\label{tab:oos-gate}", "\\end{table}"]
(TAB / "oos-gate.tex").write_text("\n".join(_gate_tex) + "\n")

# tab:dropcol -- leave-one-COVARIATE-out (drop-column) importance for the Entrants model.
# Lei et al. (2018) LOCO importance / the reduced-model logic of McAlexander & Mentch (2020),
# scored by OOS degradation.  Paired country-cluster bootstrap (scripts/dropcol_importance.py).
_dc = pd.read_parquet(SPIKE / "dropcol_logit.parquet").set_index("config")
_dcrf = (pd.read_parquet(SPIKE / "dropcol_rf.parquet").set_index("config")
         if (SPIKE / "dropcol_rf.parquet").exists() else None)
# order: both shadows, then each covariate by logit dPRL desc; shadows flagged
_dc_order = ["drop:BOTH shadows", "drop:E_gov (shadow)", "drop:E_opp (shadow)"]
_dc_canon = sorted([c for c in _dc.index if c not in _dc_order + ["Entrants"]],
                   key=lambda c: -float(_dc.loc[c, "d_prl"]))
_dc_rows_order = _dc_order + _dc_canon


_DC_DISP = {"BOTH shadows": "Both shadows ($E^G,\\,E^O$)",
            "E_gov (shadow)": "$E^G$ (gov.\\ shadow)",
            "E_opp (shadow)": "$E^O$ (opp.\\ shadow)"}


def _dcrow(cfg):
    r = _dc.loc[cfg]
    name = cfg.replace("drop:", "")
    disp = _DC_DISP.get(name, name)
    ci = f"[{r['dprl_lo']:+.3f},\\ {r['dprl_hi']:+.3f}]"
    rfcol = ""
    if _dcrf is not None and cfg in _dcrf.index:
        rfcol = f" & ${_dcrf.loc[cfg, 'd_aucpr']:+.3f}$"
    return f"    {disp} & ${r['d_prl']:+.3f}$ & {{\\footnotesize ${ci}$}} & ${r['d_aucpr']:+.3f}${rfcol} \\\\"


_ent_dc_ap, _ent_dc_pr = float(_dc.loc["Entrants", "aucpr"]), float(_dc.loc["Entrants", "prl"])
_rfhdr = " & \\multicolumn{1}{c}{RF $\\Delta$AUC-PR}" if _dcrf is not None else ""
_rfcolspec = "r" if _dcrf is not None else ""
_dropcol_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\small",
    f"  \\begin{{tabular}}{{@{{}}l r c r {_rfcolspec}@{{}}}}", "    \\toprule",
    f"    Dropped from \\textit{{Entrants}} & $\\Delta$PRL & 95\\% interval & $\\Delta$AUC-PR{_rfhdr} \\\\",
    "    \\midrule", *[_dcrow(c) for c in _dc_rows_order], "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Out-of-sample drop-column (leave-one-covariate-out) importance for the",
    "    \\textit{Entrants} onset model: the degradation in out-of-sample fit when each",
    "    covariate is removed and the model refit \\citep{lei2018}, under",
    "    leave-one-country-out cross-validation.",
    "    Larger $\\Delta$ = more important.  Intervals are",
    "    95\\% paired country-cluster bootstrap (differences taken within each resample,",
    "    $B = 1{,}000$)." + (" Final column: the same exercise refit with a random forest." if _dcrf is not None else ""),
    "    }",
    "  \\label{tab:dropcol}", "\\end{table}"]
(TAB / "dropcol.tex").write_text("\n".join(_dropcol_tex) + "\n")

N4.update({
    "DropBothPrl":      f"{float(_dc.loc['drop:BOTH shadows', 'd_prl']):+.3f}",
    "DropBothCiLo":     f"{float(_dc.loc['drop:BOTH shadows', 'dprl_lo']):+.3f}",
    "DropBothCiHi":     f"{float(_dc.loc['drop:BOTH shadows', 'dprl_hi']):+.3f}",
    "DropEgovPrl":      f"{float(_dc.loc['drop:E_gov (shadow)', 'd_prl']):+.3f}",
    "DropPriorWarPrl":  f"{float(_dc.loc['drop:Prior war', 'd_prl']):+.3f}",
    "DropEntAucpr":     f"{_ent_dc_ap:.3f}",
})


# tab:subsumption -- proxies add nothing to the shadow (Full rows omitted)
def _srow(proxy, model, label):
    r = _sub[(_sub.proxy == proxy) & (_sub.model == model)].iloc[0]
    return f"    {label} & {r.oos_prl:.3f} & {r.oos_auc:.3f} & {r.oos_aucpr:.3f} \\\\"


_sub_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}l rrr@{}}", "    \\toprule",
    "    Specification & PRL & ROC-AUC & AUC-PR \\\\", "    \\midrule",
    f"    \\multicolumn{{4}}{{@{{}}l}}{{\\textit{{Cunningham subsample ($N = {N4['OosNLake']}$)}}}} \\\\",
    _srow("Lake", "Baseline", "Baseline"),
    _srow("Lake", "Baseline+Lake", "\\quad $+$ Cunningham"),
    _srow("Lake", "Entrants", "Entrants"),
    _srow("Lake", "Entrants+Lake", "\\quad $+$ Cunningham"),
    _srow("Lake", "Full", "Full"),
    _srow("Lake", "Full+Lake", "\\quad $+$ Cunningham"),
    "    \\addlinespace",
    f"    \\multicolumn{{4}}{{@{{}}l}}{{\\textit{{G\\&M subsample ($N = {N4['OosNGM']}$)}}}} \\\\",
    _srow("GM", "Baseline", "Baseline"),
    _srow("GM", "Baseline+GM", "\\quad $+$ G\\&M"),
    _srow("GM", "Entrants", "Entrants"),
    _srow("GM", "Entrants+GM", "\\quad $+$ G\\&M"),
    _srow("GM", "Full", "Full"),
    _srow("GM", "Full+GM", "\\quad $+$ G\\&M"),
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Out-of-sample metrics (leave-one-country-out), computed on each proxy's",
    "    coverage subsample.  Each panel adds the Cunningham security-hierarchy measure",
    "    or the Gibilisco--Montero (G\\&M) structural P5 estimates to the \\textit{Baseline},",
    "    \\textit{Entrants}, and type-resolved \\textit{Full} models.}",
    "  \\label{tab:subsumption}", "\\end{table}"]
(TAB / "subsumption.tex").write_text("\n".join(_sub_tex) + "\n")

# ======================================================================
# SECTION 3.3 -- direction: net tilt + common intensity, sign-consistency + variance decomposition
#   signs: results/spike/direction_signs.parquet   (25 per-draw coefs + onset first-diff)
#   txp:   results/spike/direction_txp.parquet      (T×P variance decomposition, measurement share)
#   joint: results/spike/direction_joint.parquet    (D2 + T×P Wald joint tests)
# Reporting rule (Rob): NO SEs / p-values / stars -- sign-consistency + magnitude + measurement share.
# ======================================================================
_ds = pd.read_parquet(SPIKE / "direction_signs.parquet")
_dt = pd.read_parquet(SPIKE / "direction_txp.parquet").set_index("coef")
_dj = pd.read_parquet(SPIKE / "direction_joint.parquet").set_index("test")
_dcf = pd.read_parquet(SPIKE / "direction_coefs_full.parquet").set_index("name")

# coefficients all from the T×P bootstrap MEANS (means commute with the reparameterization -> exact
# consistency between the caption gammas, the two identified axes, and the appendix table)
_gG, _gO = float(_dcf.loc["gammaG", "mean"]), float(_dcf.loc["gammaO", "mean"])
_tilt_coef, _common_coef = (_gO - _gG) / 2, (_gG + _gO) / 2
# sign-consistency (fraction of the 25 measurement draws) from the per-draw fits
_tilt_frac, _common_frac = (_ds.tilt_coef > 0).mean(), (_ds.common_coef < 0).mean()
_gov_frac, _opp_frac = (_ds.gov_coef < 0).mean(), (_ds.opp_coef > 0).mean()
# magnitude: predicted onset at a representative (mean-covariate) country, axis 10th -> 90th (percent)
_t_lo, _t_hi = _ds.p_tilt_lo.mean() * 100, _ds.p_tilt_hi.mean() * 100
_c_lo, _c_hi = _ds.p_common_lo.mean() * 100, _ds.p_common_hi.mean() * 100
_meas = {k: float(_dt.loc[k, "meas_share"]) for k in ["tilt", "common", "gov", "opp"]}
_meas_vals = list(_meas.values())
# pooled T×P percentile intervals (same source as tab:direction-full's raw-basis intervals)
_tci = {k: (float(_dt.loc[k, "ci_lo"]), float(_dt.loc[k, "ci_hi"])) for k in ["tilt", "common"]}

N5 = {
    "DirTiltCoef":    f"{_tilt_coef:+.2f}",
    "DirTiltCiLo":    f"{_tci['tilt'][0]:+.2f}",
    "DirTiltCiHi":    f"{_tci['tilt'][1]:+.2f}",
    "DirCommonCiLo":  f"{_tci['common'][0]:+.2f}",
    "DirCommonCiHi":  f"{_tci['common'][1]:+.2f}",
    "DirTiltMeas":    f"{100 * _meas['tilt']:.0f}",
    "DirTiltFrac":    f"{100 * _tilt_frac:.0f}",
    "DirTiltPlo":     f"{_t_lo:.2f}",
    "DirTiltPhi":     f"{_t_hi:.2f}",
    "DirCommonCoef":  f"{_common_coef:+.2f}",
    "DirCommonFrac":  f"{100 * _common_frac:.0f}",
    "DirCommonPlo":   f"{_c_lo:.2f}",
    "DirCommonPhi":   f"{_c_hi:.2f}",
    "DirGovCoef":     f"{_gG:+.2f}",
    "DirGovFrac":     f"{100 * _gov_frac:.0f}",
    "DirOppCoef":     f"{_gO:+.2f}",
    "DirOppFrac":     f"{100 * _opp_frac:.0f}",
    "DirMeasLo":      f"{100 * min(_meas_vals):.0f}",
    "DirMeasHi":      f"{100 * max(_meas_vals):.0f}",
    "DirInterFrac":   f"{100 * (_ds.inter_coef > 0).mean():.0f}",
    "DirJointP":      f"{float(_dj.loc['TxP_Wald', 'p']):.3f}",
    "DirJointDtwoP":  f"{float(_dj.loc['D2', 'p']):.3f}",
}

# tab:direction -- two identified axes; magnitude = representative-country predicted onset. Raw gammas
# -> caption; full model -> appendix.
_dir_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\small", "  \\begin{tabular}{@{}l r c r@{}}", "    \\toprule",
    "    & Coef. & $P$(onset), 10th\\,$\\to$\\,90th & Sign-consistent \\\\",
    "    & & {\\footnotesize representative country} & {\\footnotesize of 25 draws} \\\\",
    "    \\midrule",
    f"    Net tilt ($E^O - E^G$) & ${_tilt_coef:+.2f}$ & "
    f"{_t_lo:.2f}\\% $\\to$ {_t_hi:.2f}\\% & {100 * _tilt_frac:.0f}\\% \\\\",
    f"    \\multicolumn{{2}}{{r}}{{\\footnotesize $[{_tci['tilt'][0]:+.2f},\\ {_tci['tilt'][1]:+.2f}]$}} & & \\\\[2pt]",
    f"    Common intensity ($E^O + E^G$) & ${_common_coef:+.2f}$ & "
    f"{_c_lo:.2f}\\% $\\to$ {_c_hi:.2f}\\% & {100 * _common_frac:.0f}\\% \\\\",
    f"    \\multicolumn{{2}}{{r}}{{\\footnotesize $[{_tci['common'][0]:+.2f},\\ {_tci['common'][1]:+.2f}]$}} & & \\\\",
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Direction of the shadow's effect on onset.  The collinear raw channels",
    f"    ($\\gamma^G = {N5['DirGovCoef']}$, $\\gamma^O = {N5['DirOppCoef']}$; $r \\approx 0.94$)",
    "    reparameterize into a net tilt ($E^O - E^G$) and a common intensity",
    "    ($E^O + E^G$).  Intervals pool the $T \\times P$ bootstrap---measurement",
    "    spread and sampling together.  $\\Delta P$(onset) is the change at a",
    "    mean-covariate country as the axis moves from its tenth to its ninetieth",
    "    percentile.  Full model, Table~\\ref{tab:direction-full}.}",
    "  \\label{tab:direction}", "\\end{table}"]
(TAB / "direction.tex").write_text("\n".join(_dir_tex) + "\n")

# tab:direction-full -- full Entrants model (raw basis), T×P mean + 95% interval, for the appendix
_COEF_LBL = {"const": "Constant", "polity2_lag": "Democracy", "lgdp_lag": "Log GDP per capita",
             "lpop_lag": "Log population", "lmtnest": "Mountainous terrain", "ncontig": "Noncontiguous",
             "oil": "Oil exporter", "nwstate": "New state", "instab_lag": "Political instability",
             "prior_war": "Prior war", "ethfrac": "Ethnic frac.", "relfrac": "Religious frac.",
             "year": "Year trend", "gammaG": "$\\gamma^G$ (gov.-biased shadow)",
             "gammaO": "$\\gamma^O$ (opp.-biased shadow)"}


def _cfrow(nm):
    r = _dcf.loc[nm]
    return f"    {_COEF_LBL.get(nm, nm)} & ${r['mean']:+.2f}$ & $[{r['lo']:+.2f},\\ {r['hi']:+.2f}]$ \\\\"


_ctrl = [n for n in _dcf.index if n not in ("gammaG", "gammaO")]
_full_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}lrr@{}}", "    \\toprule",
    "    & Coef. & 95\\% $T\\times P$ interval \\\\", "    \\midrule",
    _cfrow("gammaG"), _cfrow("gammaO"), "    \\midrule",
    *[_cfrow(nm) for nm in _ctrl],
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{The full \\textit{Entrants} onset model.  Coefficients are $T \\times P$",
    "    means; intervals are the 2.5th--97.5th percentiles.  Net tilt",
    "    $= (\\gamma^O - \\gamma^G)/2$, common intensity $= (\\gamma^O + \\gamma^G)/2$.}",
    "  \\label{tab:direction-full}", "\\end{table}"]
(TAB / "direction-full.tex").write_text("\n".join(_full_tex) + "\n")

_tf = pd.read_parquet(SPIKE / "oos_loco_topfit.parquet").set_index("model")
_ch = pd.read_parquet(SPIKE / "oos_loco_channel_metrics.parquet").set_index("model")
_dbt = pd.read_parquet(SPIKE / "direction_by_type.parquet")

# Gate: interpret a type's direction only if it beats the aggregate Entrants out of sample.
# Clear passers = Neighbors, Powers, Rivals (Rulers hurts, Coethnics marginal, DOE degenerate/dropped).
_ent = float(_tf.loc["Entrants", "oos_aucpr"])
_GATED = ["Neighbors", "Powers", "Rivals"]           # direction-by-type labels
_OOS_KEY = {"Neighbors": "Neighbors", "Powers": "Powers", "Rivals": "Rivals (cts)"}


def _tfa(m):
    return float(_tf.loc[m, "oos_aucpr"])


N6 = {
    "ChanGovAgg":  f"{float(_ch.loc['logit_Entrants_gov', 'aucpr']):.2f}",
    "ChanGovFull": f"{float(_ch.loc['logit_Full_gov', 'aucpr']):.2f}",
    "ChanOppAgg":  f"{float(_ch.loc['logit_Entrants_opp', 'aucpr']):.2f}",
    "ChanOppFull": f"{float(_ch.loc['logit_Full_opp', 'aucpr']):.2f}",
    "HetEntOos":   f"{_ent:.2f}",
    "HetFullOos":  f"{_tfa('Full'):.2f}",
    "HetNeighborsOos": f"{_tfa('Neighbors'):.2f}",
    "HetPowersOos":    f"{_tfa('Powers'):.2f}",
    "HetRivalsOos":    f"{_tfa('Rivals (cts)'):.2f}",
    "HetRulersOos":    f"{_tfa('Rulers'):.2f}",
}
# opposition-side consistency for the gated types (majority direction), for the prose
for _lab in _GATED:
    _o = _dbt[_dbt.label == _lab]["opp"]
    N6[f"Het{_lab}OppPct"] = f"{100 * (( _o > 0).mean() if _o.mean() > 0 else (_o < 0).mean()):.0f}"


# tab:channels -- channel decomposition (gov/opp x aggregate/disaggregated), OOS AUC-PR
_both_agg, _both_full = _ent, _tfa("Full")
_chan_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}lcc@{}}", "    \\toprule",
    "    Shadow channel & Aggregate & By type \\\\", "    \\midrule",
    f"    Government & {N6['ChanGovAgg']} & {N6['ChanGovFull']} \\\\",
    f"    Opposition & {N6['ChanOppAgg']} & {N6['ChanOppFull']} \\\\",
    f"    Both & {_both_agg:.2f} & {_both_full:.2f} \\\\",
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Out-of-sample onset prediction (AUC-PR, leave-one-country-out).  Each",
    "    cell is a separate onset model: the structural baseline plus the indicated",
    "    channel, entered as a single aggregate expectation or split into the six",
    "    intervener types.}",
    "  \\label{tab:channels}", "\\end{table}"]
(TAB / "channels.tex").write_text("\n".join(_chan_tex) + "\n")


# tab:het-direction -- direction of the gate-passing types (net-tilt-robust, sign-consistency)
def _sidecell(vals, bold_if_deter=False):
    if vals.mean() < 0:
        cell = f"deters ({100 * (vals < 0).mean():.0f}\\%)"
        return f"\\textbf{{{cell}}}" if bold_if_deter else cell
    return f"emboldens ({100 * (vals > 0).mean():.0f}\\%)"


def _hetrow(label):
    r = _dbt[_dbt.label == label]
    return (f"    {label} & {_tfa(_OOS_KEY[label]):.2f} & {_sidecell(r['gov'])} & "
            f"{_sidecell(r['opp'], bold_if_deter=True)} \\\\")


_hd_tex = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}lccc@{}}", "    \\toprule",
    "    Intervener type & OOS AUC-PR & Government side & Opposition side \\\\", "    \\midrule",
    *[_hetrow(l) for l in _GATED],
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Direction of the shadow's effect, by intervener type.  Each entry gives",
    "    the majority direction and its consistency across the 25 measurement draws.}",
    "  \\label{tab:het-direction}", "\\end{table}"]
(TAB / "het-direction.tex").write_text("\n".join(_hd_tex) + "\n")


# ---- tab:stage1-perf (Super-learner + top nonlinear (RF) + best logit), XW ----
def _f(m, s, dec, pct=False):          # "mean (sd)", SDs rounding to 0 -> "<eps"
    mul, u = (100, "\\%") if pct else (1, "")
    sd = f"{s*mul:.{dec}f}"
    if float(sd) == 0:
        sd = "$<$" + f"{10**-dec:.{dec}f}"
    return f"{m*mul:.{dec}f}{u} ({sd}{u})"


def _perf(label, prl, auc, apr, ll):   # each arg = (mean, sd)
    return f"    {label} & {_f(*prl, 1, pct=True)} & {_f(*auc, 3)} & {_f(*apr, 2)} & {_f(*ll, 3)} \\\\"


s1 = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\begin{tabular}{@{}lrrrr@{}}", "    \\toprule",
    "    Model & PRL & ROC-AUC & AUC-PR & Log-loss \\\\", "    \\midrule",
    _perf("Super-learner", (E.prl.mean(), E.prl.std()), (E.auc.mean(), E.auc.std()),
          (E.apr_any.mean(), E.apr_any.std()), (E.ll.mean(), E.ll.std())),
    _perf("Random forest", _ms("rf", "XW", "prl"), _ms("rf", "XW", "auc"), _ms("rf", "XW", "apr"), _ms("rf", "XW", "ll")),
    _perf("Lasso logit",   _ms("lasso", "XW", "prl"), _ms("lasso", "XW", "auc"), _ms("lasso", "XW", "apr"), _ms("lasso", "XW", "ll")),
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Stage~1 classifier performance (out-of-fold; mean and SD across the 25",
    "    draws) for the super-learner, its top-weighted nonlinear component, and the",
    f"    best logit.  AUC-PR is for any intervention (no-skill baseline $\\approx${N3['AucprBase']});",
    "    ROC-AUC is macro-averaged.  Full components in Table~\\ref{tab:sl-components}.}",
    "  \\label{tab:stage1-perf}", "\\end{table}"]
(TAB / "stage1-perf.tex").write_text("\n".join(s1) + "\n")

# ---- tab:sl-components (9 learners x PRL{X,W,XW} + weight{X,W,XW}) ----
def _comprow(learner):
    pr = [_cell(learner, m, "prl") * 100 for m in ("X", "W", "XW")]
    wt = [_cell(learner, m, "wt") * 100 for m in ("X", "W", "XW")]
    cells = " & ".join(f"${v:.1f}$" for v in pr) + " & " + " & ".join(f"${v:.1f}$" for v in wt)
    return f"    {LBL[learner]:14s} & {cells} \\\\"


order_by_wt = sorted(ORDER, key=lambda l: -wl.get(l, 0))
sc = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT.",
    "\\begin{table}[t]", "  \\centering", "  \\small", "  \\begin{tabular}{@{}l rrr rrr@{}}", "    \\toprule",
    "    & \\multicolumn{3}{c}{PRL (\\%)} & \\multicolumn{3}{c}{NNLS weight (\\%)} \\\\",
    "    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "    Learner & $X$ & $W$ & $XW$ & $X$ & $W$ & $XW$ \\\\", "    \\midrule",
    *[_comprow(l) for l in order_by_wt],
    "    \\midrule",
    f"    \\textbf{{Super-learner}} & \\multicolumn{{3}}{{c}}{{\\textbf{{{E.prl.mean()*100:.1f}}}}} & \\multicolumn{{3}}{{c}}{{\\textbf{{100}}}} \\\\",
    "    \\bottomrule", "  \\end{tabular}",
    "  \\caption{Out-of-fold PRL and NNLS weight for the 27 candidate models (9 learners",
    "    $\\times$ 3 feature sets) and the super-learner, averaged across the 25 draws.",
    "    $X$ = dyad features; $W$ = spatial lags; $XW$ = both.  Sorted by total weight.}",
    "  \\label{tab:sl-components}", "\\end{table}"]
(TAB / "sl-components.tex").write_text("\n".join(sc) + "\n")

# ======================================================================
# N7: Appendix I predictive-significance tests (mh_chisq_* + rfsig_perdraw)
# Protocol constants (k=75, 99 perms, minsplit, five-seeds-per-size, df=20)
# stay literal in the .tex; every RESULT number is generated here.
# ======================================================================
import math as _math
_ceil2 = lambda p: f"{_math.ceil(p * 100) / 100:.2f}"       # ceiling so "p <= X" stays true
N7 = {}
_calib = pd.read_parquet(SPIKE / "mh_chisq_calib.parquet")
_pn = _calib[_calib.test.str.startswith("noise")]["p"]
N7["MhNoiseReps"] = len(_pn)
N7["MhNoiseMean"] = f"{_pn.mean():.2f}"
N7["MhNoiseHits"] = f"{int((_pn <= 0.05).sum())} of {len(_pn)}"
N7["MhSynthStat"] = f"{_calib[_calib.test.str.startswith('synthetic')].iloc[0].stat:.0f}"
_js = pd.read_parquet(SPIKE / "mh_chisq_joint_seeds.parquet")
N7["MhJointTight"] = f"{int((_js.p <= 0.005).sum())} of {len(_js)}"
N7["MhJointMaxP"] = _ceil2(_js.p.max())
_comb = max(min(1.0, 2 * _js[_js.ntest == nt].p.median()) for nt in _js.ntest.unique())
N7["MhJointComb"] = f"{max(_comb, 0.001):.3f}"
_foc = pd.read_parquet(SPIKE / "mh_chisq_focal.parquet").set_index("test")
N7["MhGdpP"] = f"{_foc.loc['GDP (control)'].p:.2f}"
N7["MhPopP"] = f"{_foc.loc['Population (ctrl)'].p:.2f}"
_pdw = pd.read_parquet(SPIKE / "rfsig_perdraw.parquet")
_pj = _pdw[_pdw.test == "E_gov+E_opp"]["p"]
N7["PdDraws"] = len(_pj)
N7["PdJointDraws"] = ("every draw" if (_pj <= 0.05).all()
                      else f"{int((_pj <= 0.05).sum())} of the {len(_pj)} draws")
N7["PdJointMed"] = f"{_pj.median():.2f}"
N7["PdJointMax"] = _ceil2(_pj.max())
N7["PdEgovHits"] = int((_pdw[_pdw.test == "E_gov"]["p"] <= 0.05).sum())
N7["PdEoppHits"] = int((_pdw[_pdw.test == "E_opp"]["p"] <= 0.05).sum())

# ======================================================================
# write all macros (S2 + S1)
# ======================================================================
macros = ["% AUTO-GENERATED by scripts/export_numbers.py -- DO NOT EDIT BY HAND.",
          "% Source: committed pipeline outputs. See notes/verification-ledger.md.", "",
          "% --- Section 2: data & coding ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N.items()]
macros += ["", "% --- Stage 1: measurement model ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N1.items()]
macros += ["", "% --- Section 3: shadow example values (cy_shadow) ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N2.items()]
macros += ["", "% --- Section 3: validation correlations + top-dyad maxima ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N3.items()]
macros += ["", "% --- Section 3.2: prediction gate + subsumption (OOS) ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N4.items()]
macros += ["", "% --- Section 3.3: direction (net tilt + common intensity) ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N5.items()]
macros += ["", "% --- Section 3.4: heterogeneity (channels + direction by type) ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N6.items()]
macros += ["", "% --- Appendix I: predictive-significance tests ---"]
macros += [f"\\newcommand{{\\{k}}}{{{v}}}" for k, v in N7.items()]
(GEN / "numbers.tex").write_text("\n".join(macros) + "\n")

# ---- report ----
print("numbers.tex (Stage-1):")
for k, v in N1.items():
    print(f"  {k:<14s} {v}")
print(f"\nburnout: {len(iters)} draws, iters {min(iters)}-{max(iters)} mean {np.mean(iters):.1f}, "
      f"delta [{min(deltas):.2e},{max(deltas):.2e}]")
print(f"\nfeature-set weights: X {N1['WtX']}({N1['WtXsd']}) W {N1['WtW']}({N1['WtWsd']}) XW {N1['WtXW']}({N1['WtXWsd']})")
print(f"N3: Lake r={N3['CorrLake']}(n={N3['NLake']}) GM r={N3['CorrGM']}(n={N3['NGM']}) "
      f"pGovMax={N3['pGovMax']} pOppMax={N3['pOppMax']}")
