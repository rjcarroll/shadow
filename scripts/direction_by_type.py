"""
Direction BY intervener type.  Tests whether each type's government side deters (coef < 0) and its
opposition side emboldens (coef > 0), and whether opposition emboldening is confined to the "right"
types.  For each of the 6 types, fit  onset ~ baseline + common_t + tilt_t  where
common_t = E_t_gov + E_t_opp and tilt_t = E_t_opp - E_t_gov (the collinearity-robust net-tilt
parameterization of net_tilt_direction.py, one type at a time), per imputation draw; report
sign-consistency across the 25 draws.  PREDICTIVE/directional, not causal.  ADDITIVE.

Run:  .venv/bin/python scripts/direction_by_type.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

TYPES = {"major": "Powers", "contig": "Neighbors", "coethnic": "Coethnics",
         "colonial": "Rulers", "hostile": "Rivals", "doe": "DOE"}

data = load_analysis_data()
base = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()

rows = []
for cy in range(1, 6):
    for ud in range(1, 6):
        cols = [f"E_{t}_{s}_asinh" for t in TYPES for s in ("gov", "opp")]
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year"] + cols)
        df = base.merge(sh, on=["ccode", "year"], how="left")
        for t, label in TYPES.items():
            g, o = f"E_{t}_gov_asinh", f"E_{t}_opp_asinh"
            sub = df[list(dict.fromkeys(["onset"] + BASELINE_VARS + [g, o]))].dropna().copy()
            sub["common"] = sub[g] + sub[o]
            sub["tilt"] = sub[o] - sub[g]
            y = sub.onset.values.astype(int)
            X = sub[BASELINE_VARS + ["common", "tilt"]].astype(float)
            try:
                m = sm.Logit(y, sm.add_constant(X)).fit(disp=False, maxiter=1000)
                tc, cc = m.params["tilt"], m.params["common"]
                rows.append(dict(type=t, label=label, cy=cy, ud=ud,
                                 tilt=tc, common=cc, gov=cc - tc, opp=cc + tc))
            except Exception as e:  # noqa: BLE001
                print(f"  FAIL {label} draw {cy}_{ud}: {e}")

R = pd.DataFrame(rows)
SPIKE.mkdir(parents=True, exist_ok=True)
R.to_parquet(SPIKE / "direction_by_type.parquet", index=False)

print("\n=== direction by intervener type (net-tilt parameterization, one type at a time) ===")
print(f"{'Type':11s} | {'net tilt':>9s} {'>0':>4s} | {'gov':>8s} {'<0':>4s} | {'opp':>8s} {'>0':>4s} | n")
print("-" * 70)
for t, label in TYPES.items():
    r = R[R.type == t]
    print(f"{label:11s} | {r.tilt.mean():+9.3f} {(r.tilt > 0).mean():4.2f} | "
          f"{r.gov.mean():+8.3f} {(r.gov < 0).mean():4.2f} | "
          f"{r.opp.mean():+8.3f} {(r.opp > 0).mean():4.2f} | {len(r)}")
