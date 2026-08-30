"""Stability check for baseline + chain (= the iso_chain spec).

For the CYs that blew up under region/full, solve their fixed point under the
iso_chain model from BOTH zero-init and high-init. If both converge to the same
low value, the CY has a unique low fixed point -> no multistability -> stable
(the mirror of the region probe, which found unique HIGH fixed points).
Plus a few random CYs as controls.
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
model = joblib.load(ABL / f"sl_model_iso_chain_{CY}_{UD}.pkl")
dd = fp_rerun.add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{CY}_{UD}.parquet"))
W_cache = _build_W_cache(dd, pd.Series(True, index=dd.index))


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
        sub = update_spatial_lags_proba(sub, proba[:, 1], proba[:, 2], W_cache=W_cache, onset_only=False)
    return float(proba[:, 1].sum()), it + 1


# CYs that blew up under region/full (from the probe + stretch tests)
contagion = [("040", 1978, "Cuba"), ("620", 1980, "Libya"), ("816", 1966, "N.Vietnam"),
             ("371", 1996, "Armenia"), ("385", 2013, "Norway"), ("375", 2013, "Finland")]
import random
random.seed(7)
pool = dd[["ccode_A", "year"]].drop_duplicates().values.tolist()
rand = [(a, int(y), "random") for a, y in random.sample(pool, 5)]

print("former contagion CYs, solved under iso_chain (baseline+chain):")
for A, t, nm in contagion:
    z, zi = solve(A, t, 0.0); h, hi = solve(A, t, 0.3)
    print(f"  {nm:10s} {A} {t}: zero={z:.2f}  high={h:.2f}  -> "
          f"{'UNIQUE & low (stable)' if abs(z-h) < 0.5 and z < 5 else 'check'}")
print("\nrandom CYs (controls):")
for A, t, nm in rand:
    z, zi = solve(A, t, 0.0); h, hi = solve(A, t, 0.3)
    print(f"  {A} {t}: zero={z:.2f}  high={h:.2f}  -> {'unique' if abs(z-h) < 0.5 else 'differ'}")

print("\naggregate (from ablation, both draws):")
for d in ["1_1", "4_2"]:
    s = pd.read_parquet(ABL / f"cy_shadow_iso_chain_{d}.parquet", columns=["E_gov", "n_B"])
    s["fl"] = s.E_gov / s.n_B
    f = pd.read_parquet(ABL / f"cy_shadow_full_{d}.parquet", columns=["E_gov", "n_B"])
    f["fl"] = f.E_gov / f.n_B
    print(f"  {d}: iso_chain max E^G={s.E_gov.max():.1f} %infl={(s.fl>0.1).mean()*100:.1f}  |  "
          f"full max E^G={f.E_gov.max():.1f} %infl={(f.fl>0.1).mean()*100:.1f}")
