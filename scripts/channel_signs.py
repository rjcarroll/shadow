"""
Confirm the channel-narrative claims (Part 2 of the results story):
 (a) a single vanilla logit on the AVERAGED shadow gives 'stars' on gov but not opp;
 (c) once we propagate measurement uncertainty (25 imputation draws), how sign-consistent
     are gov (expected negative) and opp (expected positive)?
ADDITIVE: reads committed data, writes nothing.
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM

EG, EO = "E_gov_asinh", "E_opp_asinh"
data = load_analysis_data()
base = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()

# (a) averaged shadow, single fit (average-then-fit = manuscript main spec, no propagation)
parts = [pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
         for cy in range(1, 6) for ud in range(1, 6)]
avg = pd.concat(parts).groupby(["ccode", "year"]).mean().reset_index()
dfa = base.merge(avg, on=["ccode", "year"]).dropna(subset=BASELINE_VARS + [EG, EO])
ma = sm.Logit(dfa.onset.values.astype(int),
              sm.add_constant(dfa[BASELINE_VARS + [EG, EO]].astype(float))).fit(disp=False, maxiter=1000)
print("=== (a) VANILLA single logit on AVERAGED shadow (no measurement propagation) ===")
for v in (EG, EO):
    print(f"    {v:18s} coef={ma.params[v]:+.3f}  p={ma.pvalues[v]:.4f}  {'***' if ma.pvalues[v] < .05 else '(n.s.)'}")

# (c) 25-draw per-imputation sign consistency
rows = []
for cy in range(1, 6):
    for ud in range(1, 6):
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
        d = base.merge(sh, on=["ccode", "year"]).dropna(subset=BASELINE_VARS + [EG, EO])
        m = sm.Logit(d.onset.values.astype(int),
                     sm.add_constant(d[BASELINE_VARS + [EG, EO]].astype(float))).fit(disp=False, maxiter=1000)
        rows.append(dict(gc=m.params[EG], gp=m.pvalues[EG], oc=m.params[EO], op=m.pvalues[EO]))
R = pd.DataFrame(rows)
print("\n=== (c) 25-draw sign consistency (per-imputation Entrants logit) ===")
print(f"    gov sign right (negative):       {int((R.gc < 0).sum())}/25")
print(f"    gov sign right & p<.05:          {int(((R.gc < 0) & (R.gp < .05)).sum())}/25")
print(f"    opp sign right (positive):       {int((R.oc > 0).sum())}/25")
print(f"    opp sign right & p<.05:          {int(((R.oc > 0) & (R.op < .05)).sum())}/25")
