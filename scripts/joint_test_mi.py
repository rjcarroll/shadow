"""
Properly-pooled JOINT test that the shadow pair (E_gov, E_opp) enters onset, under measurement
uncertainty. Replaces the naive single-fit LR (32.7) for Beat 3. Two routes:
  (1) D2 (Li-Meng-Raghunathan-Rubin 1991 / van Buuren) — pools the 25 per-draw LR statistics
      (Baseline vs Entrants). Captures BETWEEN-draw (measurement) variance.
  (2) T x P bootstrap joint Wald — uses the 5,000 (gamma^G, gamma^O) vectors (BOTH measurement +
      pairs-cluster variance) and tests (0,0) against their covariance. Most consistent with the paper.
ADDITIVE: reads committed data, writes nothing.
"""
import sys, warnings; sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np, pandas as pd, statsmodels.api as sm
from scipy.stats import f as fdist, chi2
from joblib import Parallel, delayed
from spike_rf_significance import load_analysis_data, BASELINE_VARS, INTERIM, SPIKE

EG, EO = "E_gov_asinh", "E_opp_asinh"
data = load_analysis_data()
base = data[list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))].copy()

# ---------- (1) per-draw LRs -> D2 ----------
LRs = []
for cy in range(1, 6):
    for ud in range(1, 6):
        sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
        d = base.merge(sh, on=["ccode", "year"]).dropna(subset=BASELINE_VARS + [EG, EO])
        y = d.onset.values.astype(int)
        mB = sm.Logit(y, sm.add_constant(d[BASELINE_VARS].astype(float))).fit(disp=False, maxiter=1000)
        mE = sm.Logit(y, sm.add_constant(d[BASELINE_VARS + [EG, EO]].astype(float))).fit(disp=False, maxiter=1000)
        LRs.append(2 * (mE.llf - mB.llf))
LRs = np.array(LRs); m, k = 25, 2
dbar = LRs.mean(); r = (1 + 1/m) * np.sqrt(LRs).var(ddof=1)
D2 = (dbar/k - (m+1)/(m-1)*r) / (1 + r)
nu = k**(-3/m) * (m-1) * (1 + 1/r)**2
p_d2 = fdist.sf(D2, k, nu)
print("=== (1) D2 pooled LR test (Li-Meng-Raghunathan-Rubin) ===")
print(f"    per-draw LR: mean {dbar:.1f}, range [{LRs.min():.1f},{LRs.max():.1f}], sig(p<.05) in {int((chi2.sf(LRs,2)<.05).sum())}/25")
print(f"    D2 = {D2:.3f}  ~ F({k}, {nu:.1f})   p = {p_d2:.4f}")

# ---------- (2) T x P bootstrap joint Wald ----------
P = 200
def one_draw(cy, ud, seed):
    sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet", columns=["ccode", "year", EG, EO])
    d = base.merge(sh, on=["ccode", "year"]).dropna(subset=BASELINE_VARS + [EG, EO]).copy()
    cols = BASELINE_VARS + [EG, EO]
    y = d.onset.values.astype(int); X = d[cols].values.astype(float); cc = d.ccode.values
    uniq = np.unique(cc); pos = {c: np.where(cc == c)[0] for c in uniq}; rng = np.random.default_rng(seed)
    out = []
    for _ in range(P):
        idx = np.concatenate([pos[c] for c in rng.choice(uniq, size=uniq.size, replace=True)])
        try:
            mm = sm.Logit(y[idx], sm.add_constant(X[idx])).fit(disp=False, maxiter=500)
            out.append(np.asarray(mm.params))  # full raw-basis vector: const, controls..., EG (gamma^G), EO (gamma^O)
        except Exception:
            pass
    return out
draws = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]
res = Parallel(n_jobs=14, backend="loky")(delayed(one_draw)(cy, ud, 1000 + i) for i, (cy, ud) in enumerate(draws))
V = np.array([v for sub in res for v in sub])           # (~5000, n_params) full raw-basis vectors
Vg = V[:, -2:]                                          # (gamma^G, gamma^O) columns
mvec = Vg.mean(axis=0); S = np.cov(Vg, rowvar=False)
W = float(mvec @ np.linalg.inv(S) @ mvec); p_w = chi2.sf(W, 2)
print("\n=== (2) T x P bootstrap joint Wald (measurement + cluster) ===")
print(f"    mean (gG,gO) = ({mvec[0]:+.3f}, {mvec[1]:+.3f});  corr = {S[0,1]/np.sqrt(S[0,0]*S[1,1]):+.2f}")
print(f"    W = {W:.3f} ~ chi2(2)   p = {p_w:.4f}")
SPIKE.mkdir(parents=True, exist_ok=True)
pd.DataFrame([dict(test="D2", stat=float(D2), p=float(p_d2)),
             dict(test="TxP_Wald", stat=float(W), p=float(p_w),
                  gG=float(mvec[0]), gO=float(mvec[1]))]).to_parquet(
    SPIKE / "direction_joint.parquet", index=False)
# full-model T×P coefficient table (raw basis) for the appendix
_names = ["const"] + list(BASELINE_VARS) + ["gammaG", "gammaO"]
assert V.shape[1] == len(_names), f"param count {V.shape[1]} != names {len(_names)}"
pd.DataFrame([dict(name=nm, mean=float(np.mean(V[:, i])), median=float(np.median(V[:, i])),
                   lo=float(np.percentile(V[:, i], 2.5)), hi=float(np.percentile(V[:, i], 97.5)),
                   frac_pos=float((V[:, i] > 0).mean()))
             for i, nm in enumerate(_names)]).to_parquet(SPIKE / "direction_coefs_full.parquet", index=False)
print("\n(naive single-fit LR for reference: 32.7, p<0.001)")
