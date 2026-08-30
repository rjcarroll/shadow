r"""Frozen-model burnout diagnostic (nb05 cell 9), re-run for the current canonical
models (baseline + chain). For each draw: freeze the fitted super-learner ensemble
and iterate the spatial lags on the ONSET/training rows to convergence, recording the
mean-|Delta spatial lag| trajectory. Writes sl_spat_conv_<cy>_<ud>.parquet
(ddyear, spat_gov_conv, spat_opp_conv, cy, ud, deltas) -- the from-source input for
the appendix \Burnout* macros (via export_numbers.py) and fig-fp-convergence.

This is the TRAINING-VALIDITY diagnostic (does the 5-iteration warm-start land on
essentially-converged lags), NOT the universal shadow fixed point (that is fp_rerun /
sl_fp_diag). Cheap: prediction sweeps on the onset sample, no retraining. Faithful
port of nb05 cell 9; reuses fp_rerun's ensemble predictor (identical to the notebook's
_predict_with_bundle) so there is one prediction code path.

Usage:  python scripts/burnout_rerun.py [cy ud]   (no args => all 25 draws)
"""
import os
for _v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "2")

import sys
from pathlib import Path
import joblib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
sys.path.insert(0, str(ROOT / "scripts"))
from shadow.data.spatial import _build_W_cache, update_spatial_lags_proba
from fp_rerun import add_temporal_features, predict_proba_from_model  # nb05-verbatim helpers

INTERIM = ROOT / "data" / "interim"
MAX_BURNOUT = 20
BURNOUT_TOL = 5e-4          # achievable floor given RF prediction noise (nb05 cell 9)


def run(cy, ud):
    bundle = joblib.load(INTERIM / f"sl_model_{cy}_{ud}.pkl")
    dd = add_temporal_features(pd.read_parquet(INTERIM / f"dd_spat_{cy}_{ud}.parquet"))
    train = dd[(dd["onset_A"] == 1) & (dd["intervention"].notna())].copy()
    W_cache = _build_W_cache(dd, dd["onset_A"] == 1)

    current = train.copy()
    prev_spat = current[["spat_gov", "spat_opp"]].fillna(0).values
    deltas = []
    for bo in range(MAX_BURNOUT):
        proba = predict_proba_from_model(bundle, current)
        updated = update_spatial_lags_proba(current, proba[:, 1], proba[:, 2], W_cache=W_cache)
        new_spat = updated[["spat_gov", "spat_opp"]].fillna(0).values
        delta = float(np.abs(new_spat - prev_spat).mean())
        deltas.append(delta)
        if delta < BURNOUT_TOL:
            print(f"{cy}_{ud} burnout converged iter={bo}  Δ={delta:.6f}", flush=True)
            break
        prev_spat = new_spat
        current = updated
    else:
        print(f"{cy}_{ud} burnout MAX iter  Δ={delta:.6f}", flush=True)

    conv = current[["ddyear", "spat_gov", "spat_opp"]].rename(
        columns={"spat_gov": "spat_gov_conv", "spat_opp": "spat_opp_conv"})
    conv["cy"], conv["ud"] = cy, ud
    conv["deltas"] = str(deltas)
    conv.to_parquet(INTERIM / f"sl_spat_conv_{cy}_{ud}.parquet", index=False)


if __name__ == "__main__":
    if len(sys.argv) > 2:
        run(int(sys.argv[1]), int(sys.argv[2]))
    else:
        for _cy in range(1, 6):
            for _ud in range(1, 6):
                run(_cy, _ud)
