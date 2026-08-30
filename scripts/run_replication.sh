#!/usr/bin/env bash
# Replication runner for "Fighting in the Shadow of Intervention"
# (PSRM). Regenerates every number, table, and figure in the manuscript
# from the shipped data, compares against the shipped exhibit files, and
# writes a timestamped log.
#
# Usage:   bash scripts/run_replication.sh
# Output:  replication-YYYYMMDD-HHMMSS.log in the working directory.
#
# Exit status 0 means every regenerated number and table is byte-for-byte
# identical to the shipped (published) versions. Figures are regenerated
# but compared by inspection (PDF bytes embed timestamps).

set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
PY="${PYTHON:-.venv/bin/python}"
LOG="replication-$(date +%Y%m%d-%H%M%S).log"
exec > >(tee "$LOG") 2>&1

echo "=== Replication run: Fighting in the Shadow of Intervention ==="
date
echo "Python:   $($PY -V 2>&1)"
echo "Platform: $(uname -sm)"
echo

# --- 1. Snapshot the shipped exhibits (the published reference) --------
REF="$(mktemp -d)"
cp -R paper/generated "$REF/generated"
cp -R paper/tables    "$REF/tables"
cp -R paper/figures   "$REF/figures"
echo "Shipped exhibits snapshotted to $REF"
echo

# --- 2. Regenerate every number and table ------------------------------
echo "--- Regenerating numbers.tex and all tables (export_numbers.py)"
$PY scripts/export_numbers.py || { echo "FATAL: export_numbers.py failed"; exit 1; }
echo

# --- 3. Regenerate the figures -----------------------------------------
echo "--- Regenerating figures (fig_shadow_ts.py, fig_appendix.py)"
$PY scripts/fig_shadow_ts.py  || { echo "FATAL: fig_shadow_ts.py failed"; exit 1; }
$PY scripts/fig_appendix.py   || { echo "FATAL: fig_appendix.py failed"; exit 1; }
echo

# --- 4. Auxiliary check: hand-coded appendix table vs raw data ---------
echo "--- Verifying appendix tab:post1999 against the raw CSV"
$PY scripts/verify_post1999_table.py || { echo "FATAL: post-1999 check failed"; exit 1; }
echo

# --- 5. Compare regenerated exhibits to the shipped versions -----------
echo "=== Comparison: regenerated vs shipped ==="
fail=0
for f in "$REF"/generated/*.tex "$REF"/tables/*.tex; do
  rel="${f#"$REF"/}"
  if cmp -s "$f" "paper/$rel"; then
    echo "IDENTICAL  paper/$rel"
  else
    echo "DIFFERS    paper/$rel"
    fail=1
  fi
done
echo
echo "Figures. On the original platform, regenerated PDFs are byte-identical"
echo "up to the embedded creation timestamp (MATCH). On other platforms the"
echo "PDF byte layout differs (fonts, compression) even for identical plotted"
echo "content — reported as REGEN; compare visually. The plotted data comes"
echo "from the same inputs verified byte-exactly above."
figs_regen=0
for name in fig-shadow-ts fig-fp-convergence fig-sl-calibration fig-shadow-kde; do
  old="$REF/figures/$name.pdf"; new="paper/figures/$name.pdf"
  if [ ! -f "$new" ] || [ "$(wc -c < "$new")" -lt 5000 ]; then
    echo "MISSING    $name.pdf (absent or degenerate)"; fail=1; continue
  fi
  so=$(wc -c < "$old"); sn=$(wc -c < "$new")
  nb=$(cmp -l "$old" "$new" 2>/dev/null | wc -l | tr -d ' ')
  if [ "$so" -eq "$sn" ] && [ "$nb" -le 64 ]; then
    echo "MATCH      $name.pdf ($sn bytes; $nb timestamp bytes differ)"
  else
    echo "REGEN      $name.pdf (shipped $so B, regenerated $sn B; compare visually)"
    figs_regen=1
  fi
done
echo

# --- 6. Verdict --------------------------------------------------------
if [ "$fail" -eq 0 ]; then
  echo "RESULT: SUCCESS — every regenerated number and table is"
  echo "byte-identical to the shipped (published) versions."
  if [ "$figs_regen" -eq 0 ]; then
    echo "Figures: byte-identical up to embedded timestamps."
  else
    echo "Figures: regenerated; PDF byte layout differs from the shipped"
    echo "files (expected across platforms) — compare visually."
  fi
else
  echo "RESULT: FAILURE — see DIFFERS/MISSING lines above."
fi
date
echo "Log written to $LOG"
exit "$fail"
