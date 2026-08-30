"""
Clean-vs-dirty shadow comparison, stratified by onset status (Rob, Sat 7/11):
"the predicted probabilities WITH onset should be similar dirty and clean; the
question is whether the WITHOUT-onset ones are, or if it's a noise thing."

For every lag1 draw that exists, compares canonical (dirty) vs lag1 (clean):

A. Country-year shadows (E_gov_asinh / E_opp_asinh), separately for onset==1
   and onset==0 rows: Pearson + Spearman corr (rank preservation), mean shift
   (clean - dirty), MAD.  Discriminates: (a) high corr + level shift = biased
   levels, preserved structure (direction results carry over cleanly);
   (b) low corr = reshuffling (dirty peace shadows substantially artifact);
   (c) near-identity.

B. Peace rows stratified by prior_war (the F&L conflict-history axis): where
   does the clean-vs-dirty displacement concentrate -- war-history countries
   (near the training manifold) or never-war countries (deep extrapolation)?

C. Dyad level (sl_preds p_gov/p_opp) on the SAME draw, onset vs peace rows --
   catches compositional shifts the country-year sum could mask.

ADDITIVE: writes results/spike/lag1_compare.parquet.  Safe to run while the
fan-out is in flight (only touches completed draws).
Run:  .venv/bin/python scripts/lag1_vs_dirty_compare.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from scipy.stats import spearmanr
from spike_rf_significance import load_analysis_data, INTERIM, SPIKE

LAG1 = INTERIM / "lag1"
SH = ["E_gov_asinh", "E_opp_asinh"]

draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)
         if (LAG1 / f"cy_shadow_{cy}_{ud}.parquet").exists()]
print(f"lag1 draws available: {len(draws)}: {draws}")
assert draws, "no lag1 shadows yet"

data = load_analysis_data()[["ccode", "year", "onset", "prior_war"]]

rows = []
for cy, ud in draws:
    dirty = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                            columns=["ccode", "year"] + SH)
    clean = pd.read_parquet(LAG1 / f"cy_shadow_{cy}_{ud}.parquet",
                            columns=["ccode", "year"] + SH)
    m = (dirty.merge(clean, on=["ccode", "year"], suffixes=("_d", "_c"))
         .merge(data, on=["ccode", "year"]))
    strata = [("onset", m[m.onset == 1]), ("peace", m[m.onset == 0]),
              ("peace_priorwar", m[(m.onset == 0) & (m.prior_war == 1)]),
              ("peace_nowar", m[(m.onset == 0) & (m.prior_war == 0)])]
    for label, sub in strata:
        r = dict(cy=cy, ud=ud, stratum=label, n=len(sub))
        for v in SH:
            d, c = sub[f"{v}_d"], sub[f"{v}_c"]
            r[f"{v}_pearson"] = float(np.corrcoef(d, c)[0, 1])
            r[f"{v}_spearman"] = float(spearmanr(d, c).statistic)
            r[f"{v}_shift"] = float((c - d).mean())
            r[f"{v}_mad"] = float((c - d).abs().mean())
        rows.append(r)

R = pd.DataFrame(rows)
R.to_parquet(SPIKE / "lag1_compare.parquet", index=False)
agg = (R.groupby("stratum")[[c for c in R.columns if c.startswith("E_")]]
       .mean().reindex(["onset", "peace", "peace_priorwar", "peace_nowar"]))
print("\n=== A/B. country-year shadows, clean vs dirty (mean over draws) ===")
print(agg.round(3).to_string())

# C. dyad level on the first available draw
cy, ud = draws[0]
dp = pd.read_parquet(INTERIM / f"sl_preds_{cy}_{ud}.parquet")
cp = pd.read_parquet(LAG1 / f"sl_preds_{cy}_{ud}.parquet")
mm = dp.merge(cp, on=["ccode_A", "ccode_B", "year"], suffixes=("_d", "_c"))
on = data[data.onset == 1][["ccode", "year"]].rename(columns={"ccode": "ccode_A"})
mm = mm.merge(on.assign(is_onset=1), on=["ccode_A", "year"], how="left")
mm["is_onset"] = mm["is_onset"].fillna(0)
print(f"\n=== C. dyad-level p_gov/p_opp, draw {cy}_{ud} ===")
for label, sub in [("onset", mm[mm.is_onset == 1]), ("peace", mm[mm.is_onset == 0])]:
    for v in ["p_gov", "p_opp"]:
        d, c = sub[f"{v}_d"], sub[f"{v}_c"]
        print(f"  {label:6s} {v}: pearson {np.corrcoef(d, c)[0,1]:.3f}  "
              f"spearman {spearmanr(d, c).statistic:.3f}  "
              f"shift {float((c-d).mean()):+.4f}  mad {float((c-d).abs().mean()):.4f}")
