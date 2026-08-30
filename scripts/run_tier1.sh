#!/bin/zsh
# Tier-1 fan-out: universal FP on the hypothetical-war-patched inputs, all 25
# draws (cached draws skip), then the probe suite. Plan: i-went-home-so-wiggly-quill.md
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p logs
echo "tier1 fan-out start $(date)"
for cy in 1 2 3 4 5; do for ud in 1 2 3 4 5; do echo "$cy $ud"; done; done \
  | xargs -P 5 -L 1 sh -c '.venv/bin/python scripts/tier1_counterfactual.py run $0 $1 >> logs/tier1_fanout.log 2>&1'
echo "tier1 FP done $(date)"
.venv/bin/python scripts/tier1_probes.py 2>&1 | tee logs/tier1_probes.log
echo "tier1 complete $(date)"
