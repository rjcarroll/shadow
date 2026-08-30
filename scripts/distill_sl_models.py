"""Distill sl_model_*.pkl (fitted ensembles, ~70 MB each) into the small
data files the exhibit pipeline actually needs.

scripts/export_numbers.py uses exactly four things from each pickle:
  - weights:        NNLS ensemble weights, keyed (mode, learner)
  - oof_per_model:  per-candidate out-of-fold probability arrays (n x 3),
                    row-aligned with sl_oof_{draw}.parquet
  - pipelines[m]["pca"].n_components_   (m in X / W / XW)
  - len(pipelines[m]["feat_cols"])

This script writes, per draw:
  data/interim/sl_oofpm_{draw}.parquet   per-model OOF probabilities, wide:
                                         one column per (mode, learner, class)
                                         named "{mode}__{learner}__p{k}", float64
and once, across draws:
  data/interim/sl_model_meta.parquet     long table: draw, mode, learner,
                                         weight, pca_ncomp, n_feat
                                         (pca_ncomp/n_feat repeated per learner
                                         within a feature set)

The replication package ships these instead of the pickles; the fitted
estimators themselves are never used by any exhibit.

Run:  .venv/bin/python scripts/distill_sl_models.py   (loads 25 pkls once)
"""
import glob
import re
import sys
from pathlib import Path

import joblib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
INTERIM = ROOT / "data" / "interim"

meta_rows = []
pkls = sorted(glob.glob(str(INTERIM / "sl_model_*.pkl")))
if not pkls:
    sys.exit("no sl_model_*.pkl found")

for f in pkls:
    draw = re.search(r"sl_model_(\d_\d)\.pkl", f).group(1)
    r = joblib.load(f)

    cols, ns = {}, set()
    for (mode, name), probs in r["oof_per_model"].items():
        probs = np.asarray(probs, dtype=np.float64)
        ns.add(len(probs))
        for k in range(probs.shape[1]):
            cols[f"{mode}__{name}__p{k}"] = probs[:, k]
    assert len(ns) == 1, f"{draw}: ragged oof_per_model lengths {ns}"
    oofpm = pd.DataFrame(cols)

    oof_n = len(pd.read_parquet(INTERIM / f"sl_oof_{draw}.parquet",
                                columns=["intervention"]))
    assert len(oofpm) == oof_n, (
        f"{draw}: oof_per_model rows {len(oofpm)} != sl_oof rows {oof_n}")

    out = INTERIM / f"sl_oofpm_{draw}.parquet"
    oofpm.to_parquet(out)

    pca = {m: int(r["pipelines"][m]["pca"].n_components_) for m in ("X", "W", "XW")}
    nft = {m: len(r["pipelines"][m]["feat_cols"]) for m in ("X", "W", "XW")}
    for (mode, name), w in r["weights"].items():
        meta_rows.append(dict(draw=draw, mode=mode, learner=name,
                              weight=float(w), pca_ncomp=pca[mode],
                              n_feat=nft[mode]))
    print(f"{draw}: {out.name} ({out.stat().st_size/1e6:.1f} MB), "
          f"{len(r['weights'])} weights")
    del r

meta = pd.DataFrame(meta_rows)
meta.to_parquet(INTERIM / "sl_model_meta.parquet")
print(f"sl_model_meta.parquet: {len(meta)} rows "
      f"({(INTERIM / 'sl_model_meta.parquet').stat().st_size/1e3:.0f} KB)")
