#!/usr/bin/env bash
# Chained drop-column importance runs: waits for the Stage-2 suite to finish
# (so it doesn't oversubscribe cores against the suite's RF LOCO), then runs
# logit (primary, ~1h) and RF (confirmatory robustness, ~5h). Detached-friendly.
# Drops results/spike/DROPCOL_COMPLETE when done.
#   nohup caffeinate -s ./scripts/run_dropcol.sh >> logs/dropcol.log 2>&1 < /dev/null & disown
set -uo pipefail
cd "$(dirname "$0")/.."
export OMP_NUM_THREADS=1
log(){ echo "=== $* $(date '+%H:%M:%S') ==="; }

log "waiting for results/spike/SUITE_COMPLETE"
while [ ! -e results/spike/SUITE_COMPLETE ]; do sleep 60; done
log "suite done; starting drop-column"

log "START dropcol logit"
.venv/bin/python scripts/dropcol_importance.py logit && log "OK dropcol logit" || log "FAILED dropcol logit"

log "START dropcol rf (confirmatory)"
.venv/bin/python scripts/dropcol_importance.py rf && log "OK dropcol rf" || log "FAILED dropcol rf"

date > results/spike/DROPCOL_COMPLETE
log "DROPCOL COMPLETE"
