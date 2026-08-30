#!/usr/bin/env bash
# Usage: ./scripts/run_notebook.sh notebooks/06-stage1-predictions.ipynb
set -euo pipefail

NB="${1:?Usage: run_notebook.sh <notebook.ipynb>}"
NB_NAME="$(basename "$NB" .ipynb)"
LOG_DIR="$(dirname "$0")/../logs"
mkdir -p "$LOG_DIR"
LOGFILE="$LOG_DIR/${NB_NAME}_$(date +%Y%m%d_%H%M%S).log"

echo "Running $NB — log at $LOGFILE"

# ── Machine / runtime log (how things ran on this machine) ─────────────────
RUNTIMES="$LOG_DIR/runtimes.md"
if [ ! -f "$RUNTIMES" ]; then
  CPU="$(sysctl -n machdep.cpu.brand_string 2>/dev/null || sysctl -n hw.model 2>/dev/null || uname -m)"
  CORES="$(sysctl -n hw.logicalcpu 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo '?')"
  RAM_GB="$(( $(sysctl -n hw.memsize 2>/dev/null || echo 0) / 1073741824 ))"
  {
    echo "# Notebook run log — $(hostname)"
    echo
    echo "- **Machine:** ${CPU}, ${CORES} logical cores, ${RAM_GB} GB RAM"
    echo "- **OS:** $(sw_vers -productName 2>/dev/null) $(sw_vers -productVersion 2>/dev/null)"
    echo "- **Python:** $(.venv/bin/python -V 2>&1 | awk '{print $2}')"
    echo
    echo "| started | notebook | wall clock | exit |"
    echo "|---|---|---|---|"
  } > "$RUNTIMES"
fi

START_EPOCH=$(date +%s)
START_HUMAN=$(date "+%Y-%m-%d %H:%M:%S")

set +e
caffeinate -i .venv/bin/jupyter-nbconvert \
  --to notebook --execute --inplace \
  --ExecutePreprocessor.timeout=-1 \
  --ExecutePreprocessor.kernel_name=python3 \
  "$NB" \
  >"$LOGFILE" 2>&1
EXIT_CODE=$?
set -e

DUR=$(( $(date +%s) - START_EPOCH ))
printf '| %s | %s | %dh%02dm%02ds | %d |\n' \
  "$START_HUMAN" "$NB_NAME" $((DUR/3600)) $(((DUR%3600)/60)) $((DUR%60)) "$EXIT_CODE" >> "$RUNTIMES"

if [ $EXIT_CODE -eq 0 ]; then
  osascript -e "display notification \"$NB_NAME finished in $((DUR/60))m\" with title \"Notebook Done\"" 2>/dev/null || true
  echo "SUCCESS: $NB_NAME (${DUR}s)" | tee -a "$LOGFILE"
else
  osascript -e "display notification \"$NB_NAME FAILED (exit $EXIT_CODE) — check $LOGFILE\" with title \"Notebook FAILED\" sound name \"Basso\"" 2>/dev/null || true
  echo "FAILED: $NB_NAME (exit $EXIT_CODE, ${DUR}s)" | tee -a "$LOGFILE"
fi

exit $EXIT_CODE
