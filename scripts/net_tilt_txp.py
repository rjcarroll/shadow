"""
Net-tilt under the manuscript's OWN inference standard: the T x P bootstrap
(T=25 measurement draws x P=200 pairs-cluster resamples by country, pooled).

Fits logit(onset ~ controls + common + tilt) where common=E_gov+E_opp, tilt=E_opp-E_gov.
One fit per resample yields tilt & common; gov/opp are derived (gov=common-tilt, opp=common+tilt),
so this ALSO reproduces the paper's published individual gov/opp CIs as a self-validation check.

ADDITIVE: reads committed data, writes nothing. Parallel over the 25 draws.
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from joblib import Parallel, delayed
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

EG, EO = "E_gov_asinh", "E_opp_asinh"
P = 200
data = load_analysis_data()
base = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()


def one_draw(cy, ud, seed):
    sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
    d = base.merge(sh, on=["ccode", "year"]).dropna(subset=BASELINE_VARS + [EG, EO]).copy()
    d["common"] = d[EG] + d[EO]; d["tilt"] = d[EO] - d[EG]
    cols = BASELINE_VARS + ["common", "tilt"]
    y = d.onset.values.astype(int); X = d[cols].values.astype(float); cc = d.ccode.values
    uniq = np.unique(cc); pos = {c: np.where(cc == c)[0] for c in uniq}; rng = np.random.default_rng(seed)
    out = []
    for _ in range(P):
        idx = np.concatenate([pos[c] for c in rng.choice(uniq, size=uniq.size, replace=True)])
        try:
            m = sm.Logit(y[idx], sm.add_constant(X[idx])).fit(disp=False, maxiter=500)
            out.append((m.params[-1], m.params[-2]))  # (tilt, common): order = const, controls..., common, tilt
        except Exception:
            pass
    return out


draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]
res = Parallel(n_jobs=14, backend="loky")(delayed(one_draw)(cy, ud, 1000 + i) for i, (cy, ud) in enumerate(draws))
V = np.array([v for sub in res for v in sub])
bt, bc = V[:, 0], V[:, 1]
bg, bo = bc - bt, bc + bt  # derived individual coefficients


def line(name, x, tail):
    lo, med, hi = np.percentile(x, [2.5, 50, 97.5])
    print(f"  {name:16s} median={med:+.3f}  95% CI [{lo:+.3f}, {hi:+.3f}]   {tail}")


print(f"pooled coefficient vectors: {len(V)}  (target ~{25*P})")
print("=== T×P bootstrap (measurement × pairs-cluster), net-tilt parameterization ===")
line("tilt (opp-gov)", bt, f"frac>0={ (bt>0).mean():.3f}   <-- the identified axis")
line("common(opp+gov)", bc, f"frac<0={ (bc<0).mean():.3f}")
print("--- derived individual coefs (self-validation vs paper: gov~[-4.63,+0.69], opp~[-1.94,+2.59]) ---")
line("gov (derived)", bg, f"frac<0={ (bg<0).mean():.3f}")
line("opp (derived)", bo, f"frac>0={ (bo>0).mean():.3f}")

# --- variance decomposition: measurement (between-draw) vs sampling (within-bootstrap) ---
# res is grouped by draw; between-draw spread of the per-draw means = generated-regressor error,
# within-draw bootstrap spread = sampling/clustering. Uses only the bootstrap -- no MLE SEs.
_pd = [np.array(sub) for sub in res if len(sub) > 1]     # per draw: (P_d, 2) = [tilt, common]
_m = len(_pd)


def _decomp(vals):                                       # vals: list of 1d arrays, one per draw
    theta = np.array([v.mean() for v in vals])
    Ud = np.array([v.var(ddof=1) for v in vals])
    Ubar, B = Ud.mean(), theta.var(ddof=1)
    T = Ubar + (1 + 1 / _m) * B
    return Ubar, B, T, (1 + 1 / _m) * B / T


_get = {"tilt": lambda a: a[:, 0], "common": lambda a: a[:, 1],
        "gov": lambda a: a[:, 1] - a[:, 0], "opp": lambda a: a[:, 1] + a[:, 0]}
_allv = {"tilt": bt, "common": bc, "gov": bg, "opp": bo}
_sgn = {"tilt": (bt > 0).mean(), "common": (bc < 0).mean(), "gov": (bg < 0).mean(), "opp": (bo > 0).mean()}
_rows = []
for _n in ["tilt", "common", "gov", "opp"]:
    _lo, _med, _hi = np.percentile(_allv[_n], [2.5, 50, 97.5])
    _U, _B, _T, _lam = _decomp([_get[_n](a) for a in _pd])
    _rows.append(dict(coef=_n, median=_med, ci_lo=_lo, ci_hi=_hi, frac_right=_sgn[_n],
                      within_var=_U, between_var=_B, total_var=_T, meas_share=_lam))
SPIKE.mkdir(parents=True, exist_ok=True)
pd.DataFrame(_rows).to_parquet(SPIKE / "direction_txp.parquet", index=False)
print("\n=== variance decomposition: share of coefficient variance from the generated regressor ===")
for _r in _rows:
    print(f"  {_r['coef']:8s} meas_share={_r['meas_share']:.2f}  (between={_r['between_var']:.4f} within={_r['within_var']:.4f})")
