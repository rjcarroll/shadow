#!/usr/bin/env bash
# Stage-1 grouped-CV diagnostic: retrain the super-learner (iso_chain, full FP)
# under StratifiedGroupKFold by HOST-YEAR on 5 draws, to measure how much the
# straight-10-fold CV inflates the §2 measurement stats. Writes to
# data/interim/groupcv/ (reads canonical dd_spat via symlink); does NOT touch
# canonical. Detached-friendly. Marker: results/spike/GROUPCV_COMPLETE.
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p logs data/interim/groupcv
export SHADOW_INTERIM="$PWD/data/interim/groupcv" GROUPCV=1
export OMP_NUM_THREADS=3 MKL_NUM_THREADS=3 OPENBLAS_NUM_THREADS=3 RF_JOBS=3
export FP_MAX_ITER=5 FP_TOL=1e-4
DRAWS="1 1|2 2|3 3|4 4|5 5"   # diagonal fan: one UD per CY-imputation

# canonical dd_spat needed as input (grouped run reuses the SAME features)
for d in ${DRAWS//|/ }; do :; done
for cy in 1 2 3 4 5; do ln -sf "$PWD/data/interim/dd_spat_${cy}_${cy}.parquet" data/interim/groupcv/; done

echo "=== groupcv fan start $(date) ==="
echo "$DRAWS" | tr '|' '\n' | xargs -P 5 -I{} sh -c \
  'set -- {}; GROUPCV=1 SHADOW_INTERIM="'"$PWD"'/data/interim/groupcv" \
     .venv/bin/python scripts/ablate_stage1.py $1 $2 iso_chain canonical'
echo "=== groupcv fan done $(date); reading metrics ==="

.venv/bin/python - <<'PY'
import pandas as pd, glob, numpy as np
from sklearn.metrics import log_loss, roc_auc_score, average_precision_score
def prl3(y,P):
    nb=np.clip(np.bincount(y,minlength=3)/len(y),1e-9,None)
    return (-np.log(nb[y]).mean()-log_loss(y,P,labels=[0,1,2]))/(-np.log(nb[y]).mean())
rows=[]
for tag,d in [("grouped(host-year)","data/interim/groupcv"),("straight(current)","data/interim")]:
    for cy in range(1,6):
        f=f"{d}/sl_oof_{cy}_{cy}.parquet"
        try: o=pd.read_parquet(f)
        except: continue
        y=o.intervention.astype(int).values; P=o[["p_none","p_gov","p_opp"]].values
        rows.append(dict(cv=tag,draw=f"{cy}_{cy}",prl=prl3(y,P)*100,
                         auc=roc_auc_score((y>0).astype(int),P[:,1:].sum(1)),
                         aucpr=average_precision_score((y>0).astype(int),P[:,1:].sum(1))))
R=pd.DataFrame(rows)
print(R.groupby("cv")[["prl","auc","aucpr"]].mean().round(3).to_string())
R.to_parquet("results/spike/groupcv_diag.parquet",index=False)
PY
date > results/spike/GROUPCV_COMPLETE
echo "=== GROUPCV COMPLETE $(date) ==="
