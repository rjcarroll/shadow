"""Go/no-go probe for the temporal warm-start: do the inflated iso_region CYs
have a reachable LOW fixed point (bistable), or are they stuck-high regardless
of init? If stuck-high, a warm-start cannot tame region's contagion.

Solves each inflated CY's per-(A,year) fixed point from zero-init and high-init.
"""
import os, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parent.parent
for v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
          "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(v, "6")
sys.path.insert(0, str(ROOT / "src")); sys.path.insert(0, str(ROOT / "scripts"))
import numpy as np, pandas as pd, joblib
import fp_rerun
from shadow.data.spatial import update_spatial_lags_proba, _build_W_cache, ALL_SPAT_COLS

INTERIM = ROOT / "data" / "interim"; ABL = INTERIM / "ablation"
CY, UD = 1, 1
model = joblib.load(ABL / f"sl_model_iso_region_{CY}_{UD}.pkl")
dd = fp_rerun.add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{CY}_{UD}.parquet"))
W_cache = _build_W_cache(dd, pd.Series(True, index=dd.index))
sh = pd.read_parquet(ABL / f"cy_shadow_iso_region_{CY}_{UD}.parquet",
                     columns=["ccode", "year", "E_gov", "n_B"])
sh["floor"] = sh.E_gov / sh.n_B
infl = sh[sh.floor > 0.10].sort_values("E_gov", ascending=False).head(6)


def solve(A, t, init_val):
    sub = dd[(dd.ccode_A == A) & (dd.year == t)].copy()
    for c in ALL_SPAT_COLS:
        sub[c] = float(init_val)
    prev = None
    for it in range(80):
        proba = fp_rerun.predict_proba_from_model(model, sub)
        cur = sub[["spat_gov", "spat_opp"]].fillna(0).values
        if prev is not None and np.abs(cur - prev).mean() < 1e-4:
            break
        prev = cur
        sub = update_spatial_lags_proba(sub, proba[:, 1], proba[:, 2],
                                        W_cache=W_cache, onset_only=False)
    return float(proba[:, 1].sum()), it + 1


print("probe: inflated iso_region CYs, zero-init vs high-init (draw 1_1)")
for A, t, eg in zip(infl.ccode, infl.year, infl.E_gov):
    z, zi = solve(A, t, 0.0)
    h, hi = solve(A, t, 0.3)
    verdict = "BISTABLE -- low FP reachable" if z < 0.6 * h else "stuck-high"
    print(f"  {A} {int(t)} (abl E_gov={eg:.1f}): zero-init={z:.1f} ({zi}it)  "
          f"high-init={h:.1f} ({hi}it)  ->  {verdict}")
