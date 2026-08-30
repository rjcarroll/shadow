#!/usr/bin/env bash
# Assemble the PSRM Dataverse replication package.
#
# Usage:    bash scripts/build_package.sh [outdir]     (default: package-build/)
# Produces: <outdir>/shadow-replication/          the package tree
#           <outdir>/shadow-replication.zip       the single-zip deposit
#
# Code and docs come from `git archive HEAD` (tracked files only, so
# nothing internal can leak); data files are copied per the manifest in
# REPLICATION.md Section 4.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
OUT="${1:-$ROOT/package-build}"
PKG="$OUT/shadow-replication"
rm -rf "$PKG"
mkdir -p "$PKG"

# --- 1. Code + docs: the tracked tree ----------------------------------
git archive HEAD | tar -x -C "$PKG"

# --- 2. Prune repo-only content ----------------------------------------
rm -rf "$PKG/docs"                      # portfolio overview, not replication
rm -f  "$PKG/.gitignore"
# figures not referenced by the manuscript
for f in convergent-validity popplot prl-vs-weight proximate-year \
         shadow-hierarchy shadow-refinement shadow-vs-lit topfit; do
  rm -f "$PKG/paper/figures/fig-$f.pdf"
done
# the replication guide is the package README
mv "$PKG/REPLICATION.md" "$PKG/README.md"

# --- 3. Data manifest (REPLICATION.md Section 4) -----------------------
mkdir -p "$PKG/data/interim" "$PKG/data/raw/regan" \
         "$PKG/data/raw/cunningham" "$PKG/data/raw/gm" "$PKG/results/spike"
cp data/interim/country_year.parquet \
   data/interim/dd_int_1_1.parquet \
   data/interim/sl_model_meta.parquet \
   data/interim/sl_fp_diag.parquet \
   "$PKG/data/interim/"
cp data/interim/sl_oof_[0-9]_[0-9].parquet \
   data/interim/sl_oofpm_[0-9]_[0-9].parquet \
   data/interim/sl_spat_conv_[0-9]_[0-9].parquet \
   data/interim/cy_shadow_[0-9]_[0-9].parquet \
   "$PKG/data/interim/"
cp data/raw/regan/replication.10.26.01.dta \
   data/raw/regan/post1999_interventions.csv "$PKG/data/raw/regan/"
cp data/raw/cunningham/cunningham.dta "$PKG/data/raw/cunningham/"
cp data/raw/gm/conditionalInterventionProbs_replication.csv "$PKG/data/raw/gm/"
cp results/spike/*.parquet "$PKG/results/spike/"

# --- 3b. Author-run log (AUTHOR_LOG=path to a run_replication.sh log) --
if [ -n "${AUTHOR_LOG:-}" ] && [ -f "$AUTHOR_LOG" ]; then
  cp "$AUTHOR_LOG" "$PKG/author-replication-run.log"
  echo "included author-run log: $AUTHOR_LOG"
fi

# --- 4. Report + zip ---------------------------------------------------
echo "package tree: $(find "$PKG" -type f | wc -l | tr -d ' ') files, $(du -sh "$PKG" | cut -f1)"
( cd "$OUT" && rm -f shadow-replication.zip \
  && zip -qr shadow-replication.zip shadow-replication )
ls -lh "$OUT/shadow-replication.zip" | awk '{print "zip:          " $5, $9}'
