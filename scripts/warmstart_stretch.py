"""Focused warm-start test: take 'worst' (inflated) iso_region countries and a
matched number of random ones, and for each a CONNECTED 5-year stretch, solve the
per-(A,year) fixed point two ways --
  (a) independent  : each year from its realized init (= the current pipeline)
  (b) warm-start    : year t initialized from year t-1's converged probabilities
and report the E^G trajectory under each. If temporal continuity can tame region's
contagion, warm-start should sit below independent across an inflated stretch
(esp. one that rises from a clean year into the inflated period).

Draw 1_1, iso_region model.
"""
import os, sys, random
from pathlib import Path
ROOT = Path(__file__).resolve().parent.parent
for v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
          "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(v, "6")
sys.path.insert(0, str(ROOT / "src")); sys.path.insert(0, str(ROOT / "scripts"))
import numpy as np, pandas as pd, joblib
import fp_rerun
from shadow.data.spatial import update_spatial_lags_proba, _build_W_cache

INTERIM = ROOT / "data" / "interim"; ABL = INTERIM / "ablation"
CY, UD = 1, 1
random.seed(90210)
model = joblib.load(ABL / f"sl_model_iso_region_{CY}_{UD}.pkl")
dd = fp_rerun.add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{CY}_{UD}.parquet"))
W_cache = _build_W_cache(dd, pd.Series(True, index=dd.index))
sh = pd.read_parquet(ABL / f"cy_shadow_iso_region_{CY}_{UD}.parquet",
                     columns=["ccode", "year", "E_gov", "n_B"])
sh["floor"] = sh.E_gov / sh.n_B


def solve_year(A, t, carry=None):
    """Solve (A,t)'s fixed point. carry={ccode_B:(p_gov,p_opp)} -> warm-start init."""
    sub = dd[(dd.ccode_A == A) & (dd.year == t)].copy()
    if carry is not None:
        pg0 = np.array([carry.get(b, (0., 0.))[0] for b in sub.ccode_B])
        po0 = np.array([carry.get(b, (0., 0.))[1] for b in sub.ccode_B])
        sub = update_spatial_lags_proba(sub, pg0, po0, W_cache=W_cache, onset_only=False)
    prev = None
    for it in range(80):
        proba = fp_rerun.predict_proba_from_model(model, sub)
        cur = sub[["spat_gov", "spat_opp"]].fillna(0).values
        if prev is not None and np.abs(cur - prev).mean() < 1e-4:
            break
        prev = cur
        sub = update_spatial_lags_proba(sub, proba[:, 1], proba[:, 2], W_cache=W_cache, onset_only=False)
    conv = {b: (float(proba[i, 1]), float(proba[i, 2])) for i, b in enumerate(sub.ccode_B)}
    return float(proba[:, 1].sum()), conv


def run_stretch(A, years):
    indep = [solve_year(A, t)[0] for t in years]
    warm, carry = [], None
    for t in years:
        eg, carry = solve_year(A, t, carry=carry)
        warm.append(eg)
    return indep, warm


def best_rise_window(A):
    """5 consecutive years (present in data) maximizing E^G[last]-E^G[first]."""
    s = sh[sh.ccode == A].set_index("year")["E_gov"].sort_index()
    best = None
    for y0 in s.index:
        win = list(range(y0, y0 + 5))
        if all(y in s.index for y in win):
            rise = s[win[-1]] - s[win[0]]
            if best is None or rise > best[0]:
                best = (rise, win)
    return best[1] if best else None


# worst countries: highest peak floor, with a 5-consecutive-year window
cand = (sh[sh.floor > 0.10].groupby("ccode").floor.max().sort_values(ascending=False).index)
worst = []
for A in cand:
    w = best_rise_window(A)
    if w:
        worst.append((A, w))
    if len(worst) == 4:
        break

# random controls: countries with a 5-consecutive-year window, not in worst
allc = [c for c in sh.ccode.unique() if c not in dict(worst)]
random.shuffle(allc)
rand = []
for A in allc:
    w = best_rise_window(A)
    if w:
        rand.append((A, w))
    if len(rand) == 4:
        break

NAMES = {"371": "Armenia", "375": "Finland", "385": "Norway", "694": "Qatar",
         "352": "Cyprus", "344": "Croatia", "349": "Slovenia", "346": "Bosnia"}
for label, group in [("WORST (inflated iso_region)", worst), ("RANDOM controls", rand)]:
    print(f"\n===== {label} =====")
    for A, years in group:
        indep, warm = run_stretch(A, years)
        nm = NAMES.get(A, A)
        print(f"  {nm} ({A}) {years[0]}-{years[-1]}:")
        print(f"    independent: {[round(x,1) for x in indep]}")
        print(f"    warm-start : {[round(x,1) for x in warm]}")
