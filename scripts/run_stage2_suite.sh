#!/usr/bin/env bash
# Full Stage-2 suite on the promoted clean pipeline, then re-export numbers/tables
# and a compile check.  Detached-friendly: run under nohup; drops
# results/spike/SUITE_COMPLETE when done.  Order: fast direction scripts first
# (early partials for check-ins), heavy LOCO next, dependents, then export.
# Plan: i-went-home-so-wiggly-quill.md.  Run:
#   nohup caffeinate -s ./scripts/run_stage2_suite.sh >> logs/stage2_suite.log 2>&1 < /dev/null & disown
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p logs
PY=.venv/bin/python
rm -f results/spike/SUITE_COMPLETE
log(){ echo "=== $* $(date '+%H:%M:%S') ==="; }

run(){  # run <script> [args]; non-fatal, logged
  log "START $1"
  if $PY "scripts/$1" ${2:-} ; then log "OK $1"; else log "FAILED $1 (continuing)"; echo "$1" >> logs/stage2_suite_failures.txt; fi
}

: > logs/stage2_suite_failures.txt

# ---- Phase 1: fast direction / sign scripts (minutes each) ----
run net_tilt_direction.py      # direction_signs
run net_tilt_txp.py            # direction_txp (T×P CIs)
run joint_test_mi.py           # direction_joint + direction_coefs_full
run direction_by_type.py       # direction_by_type (§3.4 direction)
run channel_signs.py           # per-channel signs (transparency)

# ---- Phase 2: heavy LOCO gates (each parallelizes internally; run serial) ----
run stage2_oos_final.py        # oos_loco_metrics + oos_loco_preds (Baseline/Entrants/Full, logit+rf)
run oos_metric_cis.py          # metric bootstrap CIs (needs oos_loco_preds above)
run subsumption_loco.py        # subsumption_loco (proxies vs shadow)
run stage2_oos_channels.py     # oos_loco_channel_metrics (§3.4 per-type gates)
run stage2_oos_topfit.py       # oos_loco_topfit (logit topfit specs)
run stage2_oos_topfit_rf.py    # oos_loco_topfit_rf

# ---- Phase 3: validation correlations + figures ----
run gm_correlation.py          # CorrLake / CorrGM convergent validity

# ---- Phase 4: export + compile ----
log "START export_numbers"
if $PY scripts/export_numbers.py ; then log "OK export_numbers"; else log "FAILED export_numbers"; fi
log "START compile"
( cd paper && latexmk -pdf -interaction=nonstopmode shadow.tex >/dev/null 2>&1 && echo "compile exit 0" || echo "compile NONZERO (prose likely references changed framing — expected pre-rewrite)" )

date > results/spike/SUITE_COMPLETE
log "SUITE COMPLETE"
echo "failures: $(cat logs/stage2_suite_failures.txt | tr '\n' ' ')"
