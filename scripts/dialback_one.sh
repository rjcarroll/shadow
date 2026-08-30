#!/usr/bin/env bash
# One dial-back combo, CANONICAL output: retrain Stage-1 then rebuild the shadow.
# Arg: "<cy> <ud> <spec>"
set -uo pipefail
cd "$(dirname "$0")/.."
read -r cy ud spec <<< "$1"
.venv/bin/python scripts/ablate_stage1.py "$cy" "$ud" "$spec" canonical && \
  .venv/bin/python scripts/fp_rerun.py "$cy" "$ud"
