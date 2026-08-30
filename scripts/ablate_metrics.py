"""Summarize the W-ablation: per (spec, draw), predictive value + contagion.

Scans data/interim/ablation/ for whatever has completed and writes a tidy table
(one row per spec x draw) for the dial-back decision:
  predictive  -- PRL, ROC-AUC, AUC-PR (from sl_oof); W / XW ensemble weight
  contagion   -- mean E^G, % CYs floor>0.10, mean floor, Egypt-1967 E^G

Usage:  python scripts/ablate_metrics.py
"""
import re
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.metrics import log_loss, roc_auc_score, average_precision_score

ROOT = Path(__file__).resolve().parent.parent
ABL = ROOT / "data" / "interim" / "ablation"

SPEC_ORDER = ["baseline", "iso_region", "iso_igo", "iso_chain", "iso_ideal", "iso_P5", "full"]


def _prl(y, p):
    nullp = np.clip(np.bincount(y, minlength=3) / len(y), 1e-9, None)
    null_loss = -np.log(nullp[y]).mean()
    return 1 - log_loss(y, p, labels=[0, 1, 2]) / null_loss


rows = []
for f in sorted(ABL.glob("sl_oof_*.parquet")):
    m = re.match(r"sl_oof_(.+)_(\d)_(\d)\.parquet", f.name)
    spec, cy, ud = m.group(1), m.group(2), m.group(3)
    oof = pd.read_parquet(f)
    y = oof.intervention.astype(int).values
    p = oof[["p_none", "p_gov", "p_opp"]].values
    pos = (y > 0).astype(int)
    rec = dict(spec=spec, draw=f"{cy}_{ud}",
               PRL=round(_prl(y, p), 4),
               AUC=round(roc_auc_score(pos, p[:, 1:].sum(1)), 4),
               AUCPR=round(average_precision_score(pos, p[:, 1:].sum(1)), 4))
    wf = ABL / f"sl_weights_{spec}_{cy}_{ud}.parquet"
    if wf.exists():
        w = pd.read_parquet(wf)
        rec["wW"] = round(w.loc[w["mode"] == "W", "weight"].sum(), 3)
        rec["wXW"] = round(w.loc[w["mode"] == "XW", "weight"].sum(), 3)
    sf = ABL / f"cy_shadow_{spec}_{cy}_{ud}.parquet"
    if sf.exists():
        s = pd.read_parquet(sf, columns=["ccode", "year", "E_gov", "n_B"])
        s["floor"] = s.E_gov / s.n_B
        rec["meanEg"] = round(s.E_gov.mean(), 2)
        rec["pct_infl"] = round((s.floor > 0.10).mean() * 100, 1)
        rec["mean_floor"] = round(s.floor.mean(), 3)
        eg = s.loc[(s.ccode == "651") & (s.year == 1967), "E_gov"]
        rec["Egypt67"] = round(float(eg.iloc[0]), 1) if len(eg) else None
    rows.append(rec)

if not rows:
    print("no ablation outputs found yet in", ABL)
else:
    df = pd.DataFrame(rows)
    df["_o"] = df.spec.map({s: i for i, s in enumerate(SPEC_ORDER)}).fillna(99)
    df = df.sort_values(["draw", "_o"]).drop(columns="_o")
    print(df.to_string(index=False))
    out = ABL / "ablation_summary.csv"
    df.to_csv(out, index=False)
    print(f"\nsaved {out}")
