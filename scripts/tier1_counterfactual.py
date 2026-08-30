"""
Tier 1 of the timing-contamination fix (plan: i-went-home-so-wiggly-quill.md):
one-feature counterfactual re-prediction with the EXISTING trained models.

ongoing_wars_A(t) counts the war that begins at t, so Stage-1's universal
prediction grid evaluates peaceful country-years off-manifold (the model never
saw ongoing_wars_A = 0 in training).  The HYPOTHETICAL-WAR PATCH makes the
input semantically uniform w.r.t. the game's war node -- "the count were this
war underway":

    dd.loc[dd.onset_A == 0, "ongoing_wars_A"] += 1

Onset rows already include the contested war and are untouched; the _B side
(the intervener's own wars) is untouched.  Re-running the universal Nash FP
per draw with the patched inputs and the CANONICAL models isolates how much of
the gate / onset-year dip was this one feature, holding everything trained
fixed.  (Tier 2 -- full t-1 information-set rebuild + retrain -- follows.)

setup:  builds data/interim/tier1/ = patched dd_spat copies + symlinked
        sl_model_* / sl_oof_* (fp_rerun then reads AND writes tier1 via
        SHADOW_INTERIM, so canonical files are never touched).
run:    one draw's universal FP (invoked by scripts/run_tier1.sh).

Usage:
  .venv/bin/python scripts/tier1_counterfactual.py setup            # ~5 min
  .venv/bin/python scripts/tier1_counterfactual.py run <cy> <ud>    # pilot
"""
import os
os.environ.setdefault("SHADOW_INTERIM", str(__import__("pathlib").Path(
    __file__).resolve().parent.parent / "data" / "interim" / "tier1"))
for _v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_v, "3")

import sys
from pathlib import Path
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
CANON = ROOT / "data" / "interim"
TIER1 = CANON / "tier1"
DRAWS = [(cy, ud) for cy in range(1, 6) for ud in range(1, 6)]


def setup():
    TIER1.mkdir(exist_ok=True)
    for cy, ud in DRAWS:
        out = TIER1 / f"dd_spat_{cy}_{ud}.parquet"
        if not out.exists():
            dd = pd.read_parquet(CANON / f"dd_spat_{cy}_{ud}.parquet")
            n_peace = int((dd["onset_A"] == 0).sum())
            dd.loc[dd["onset_A"] == 0, "ongoing_wars_A"] += 1
            dd.to_parquet(out, index=False)
            print(f"patched dd_spat_{cy}_{ud}: {n_peace:,}/{len(dd):,} peace rows +=1", flush=True)
        for stem in (f"sl_model_{cy}_{ud}.pkl", f"sl_oof_{cy}_{ud}.parquet"):
            link = TIER1 / stem
            if not link.exists():
                link.symlink_to(CANON / stem)
    # sanity: patched file differs from canonical exactly on peace rows
    a = pd.read_parquet(CANON / "dd_spat_1_1.parquet", columns=["onset_A", "ongoing_wars_A"])
    b = pd.read_parquet(TIER1 / "dd_spat_1_1.parquet", columns=["onset_A", "ongoing_wars_A"])
    d = (b.ongoing_wars_A - a.ongoing_wars_A)
    assert (d[a.onset_A == 0] == 1).all() and (d[a.onset_A == 1] == 0).all()
    print(f"setup OK: {len(DRAWS)} patched draws in {TIER1}", flush=True)


def run(cy, ud):
    assert os.environ["SHADOW_INTERIM"] == str(TIER1)
    if not os.environ.get("FORCE") and (TIER1 / f"cy_shadow_{cy}_{ud}.parquet").exists():
        print(f"skip tier1 {cy}_{ud} (cached)", flush=True)
        return
    import fp_rerun                      # imports AFTER env is set
    assert str(fp_rerun.INTERIM) == str(TIER1), fp_rerun.INTERIM
    fp_rerun.run(cy, ud)                 # reads + writes tier1 (canonical names)


if __name__ == "__main__":
    if sys.argv[1] == "setup":
        setup()
    else:
        sys.path.insert(0, str(ROOT / "scripts"))
        run(int(sys.argv[2]), int(sys.argv[3]))
