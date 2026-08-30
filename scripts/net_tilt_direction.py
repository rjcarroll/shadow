"""
Directional (sign) check for the opp/emboldening reading — collinearity-robust via the NET TILT.

r(E_gov,E_opp)=0.94, so the identified direction is the contrast: reparameterize the pair into
  common = E_gov + E_opp   (overall intervention intensity)
  tilt   = E_opp - E_gov   (NET pro-opposition tilt)
Emboldening+deterrence as one axis predicts tilt_coef > 0 (more pro-opp -> more onset).
Also reports the raw joint gov/opp signs for transparency (collinear -> unstable, caveat).
Across all 25 imputations -> sign-consistency. ADDITIVE: reads committed data, writes nothing.
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

data = load_analysis_data()
base = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()

rows = []
for cy in range(1, 6):
    for ud in range(1, 6):
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                             columns=["ccode", "year", "E_gov_asinh", "E_opp_asinh"])
        df = base.merge(sh, on=["ccode", "year"], how="left")
        sub = df[list(dict.fromkeys(["onset"] + BASELINE_VARS
                                    + ["E_gov_asinh", "E_opp_asinh"]))].dropna().copy()
        sub["common"] = sub.E_gov_asinh + sub.E_opp_asinh
        sub["tilt"] = sub.E_opp_asinh - sub.E_gov_asinh
        y = sub.onset.values.astype(int)

        Xa = sub[BASELINE_VARS + ["common", "tilt"]].astype(float)
        ma = sm.Logit(y, sm.add_constant(Xa)).fit(disp=False, maxiter=1000)
        Xb = sub[BASELINE_VARS + ["E_gov_asinh", "E_opp_asinh"]].astype(float)
        mb = sm.Logit(y, sm.add_constant(Xb)).fit(disp=False, maxiter=1000)
        # interaction probe: the additive threshold pi(tilt) - k(common) implies no tilt x common
        # interaction on the index scale; a scaling variant (burden multiplying the contest) implies
        # a negative one. Sign-consistency across draws only -- no SEs (per reporting rule).
        sub["ct"] = sub["common"] * sub["tilt"]
        Xc = sub[BASELINE_VARS + ["common", "tilt", "ct"]].astype(float)
        mc = sm.Logit(y, sm.add_constant(Xc)).fit(disp=False, maxiter=1000)

        # predicted onset at a representative (mean-covariate) country, each axis moved 10th -> 90th
        # percentile.  Extrapolation-free: one representative profile, not an average over the
        # out-of-support counterfactuals an AME would visit (which push predictions to ~76%).
        mr = sub[BASELINE_VARS + ["common", "tilt"]].mean()

        def _pat(nm, q):
            r = mr.copy(); r[nm] = np.percentile(sub[nm], q)
            return float(ma.predict(sm.add_constant(
                pd.DataFrame([r])[BASELINE_VARS + ["common", "tilt"]].astype(float), has_constant="add"))[0])

        rows.append(dict(tilt_coef=ma.params["tilt"], tilt_p=ma.pvalues.get("tilt", np.nan),
                         common_coef=ma.params["common"],
                         gov_coef=mb.params["E_gov_asinh"], opp_coef=mb.params["E_opp_asinh"],
                         inter_coef=mc.params["ct"],
                         p_tilt_lo=_pat("tilt", 10), p_tilt_hi=_pat("tilt", 90),
                         p_common_lo=_pat("common", 10), p_common_hi=_pat("common", 90)))

R = pd.DataFrame(rows)
SPIKE.mkdir(parents=True, exist_ok=True)
R.to_parquet(SPIKE / "direction_signs.parquet", index=False)
print("=== NET TILT (opp - gov), identified directional axis ===")
print(f"  tilt coef:   mean={R.tilt_coef.mean():+.4f}  frac>0={(R.tilt_coef>0).mean():.2f}  median p={R.tilt_p.median():.3f}")
print(f"  rep-country P(onset), tilt 10th->90th:   {R.p_tilt_lo.mean()*100:.2f}% -> {R.p_tilt_hi.mean()*100:.2f}%")
print(f"  common (intensity) coef: mean={R.common_coef.mean():+.4f}  frac<0={(R.common_coef<0).mean():.2f}")
print(f"  rep-country P(onset), common 10th->90th: {R.p_common_lo.mean()*100:.2f}% -> {R.p_common_hi.mean()*100:.2f}%")
print("=== RAW JOINT gov/opp (collinear -> unstable, transparency only) ===")
print(f"  gov coef: mean={R.gov_coef.mean():+.4f}  frac<0 (deterrence)={(R.gov_coef<0).mean():.2f}")
print(f"  opp coef: mean={R.opp_coef.mean():+.4f}  frac>0 (emboldening)={(R.opp_coef>0).mean():.2f}")
print("=== INTERACTION PROBE (tilt x common; additive threshold predicts no stable sign) ===")
print(f"  interaction coef: mean={R.inter_coef.mean():+.4f}  frac>0={(R.inter_coef>0).mean():.2f}")
