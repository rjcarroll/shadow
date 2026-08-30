#!/bin/zsh
# Driver: nested Stage-1 spot-check over 5 diagonal draws, sequential (each draw
# saturates the machine), then the evaluation. See scripts/nested_spotcheck.py.
set -e
cd "$(dirname "$0")/.."
mkdir -p logs
for d in "1 1" "2 2" "3 3" "4 4" "5 5"; do
  echo "=== draw $d $(date) ==="
  .venv/bin/python scripts/nested_spotcheck.py ${=d}
done
echo "=== eval $(date) ==="
.venv/bin/python scripts/nested_spotcheck_eval.py
echo "=== done $(date) ==="
