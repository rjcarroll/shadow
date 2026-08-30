#!/usr/bin/env bash
# One ablation combo: train Stage-1 then build the shadow. Arg: "<cy> <ud> <spec>"
set -uo pipefail
cd "$(dirname "$0")/.."
read -r cy ud spec <<< "$1"
.venv/bin/python scripts/ablate_stage1.py "$cy" "$ud" "$spec" && \
  .venv/bin/python scripts/ablate_shadow.py "$cy" "$ud" "$spec"
