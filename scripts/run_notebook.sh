#!/usr/bin/env bash
# Usage: ./scripts/run_notebook.sh notebooks/06-stage1-predictions.ipynb
set -euo pipefail

NB="${1:?Usage: run_notebook.sh <notebook.ipynb>}"
NB_NAME="$(basename "$NB" .ipynb)"
LOG_DIR="$(dirname "$0")/../logs"
mkdir -p "$LOG_DIR"
LOGFILE="$LOG_DIR/${NB_NAME}_$(date +%Y%m%d_%H%M%S).log"

echo "Running $NB — log at $LOGFILE"

caffeinate -i .venv/bin/jupyter-nbconvert \
  --to notebook --execute --inplace \
  --ExecutePreprocessor.timeout=7200 \
  --ExecutePreprocessor.kernel_name=python3 \
  "$NB" \
  >"$LOGFILE" 2>&1

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
  osascript -e "display notification \"$NB_NAME finished successfully\" with title \"Notebook Done\""
  echo "SUCCESS: $NB_NAME" | tee -a "$LOGFILE"
else
  osascript -e "display notification \"$NB_NAME FAILED (exit $EXIT_CODE) — check $LOGFILE\" with title \"Notebook FAILED\" sound name \"Basso\""
  echo "FAILED: $NB_NAME (exit $EXIT_CODE)" | tee -a "$LOGFILE"
fi

exit $EXIT_CODE
