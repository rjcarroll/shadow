"""Build the shadow for ONE draw under an ablation W-spec, by pointing fp_rerun's
validated (nb06-faithful) run() at the spec's model. Honors the model's per-mode
feat_cols, so the FP uses only the spec's channels. Writes to data/interim/ablation/.

Usage:  python scripts/ablate_shadow.py <cy> <ud> <spec>
"""
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "scripts"))
import fp_rerun  # noqa: E402  (reuse its validated FP + aggregation)

ABL = ROOT / "data" / "interim" / "ablation"


def main(cy, ud, spec):
    if (ABL / f"cy_shadow_{spec}_{cy}_{ud}.parquet").exists():
        print(f"skip shadow {spec} {cy}_{ud} (cached)", flush=True)
        return
    fp_rerun.run(cy, ud,
                 model_path=ABL / f"sl_model_{spec}_{cy}_{ud}.pkl",
                 oof_path=ABL / f"sl_oof_{spec}_{cy}_{ud}.parquet",
                 out_dir=ABL, tag=spec)


if __name__ == "__main__":
    main(int(sys.argv[1]), int(sys.argv[2]), sys.argv[3])
