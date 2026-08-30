#!/usr/bin/env bash
# Word ledger for the PSRM 9,000-word limit (title-page figure in shadow.tex).
# The paper splits its bibliography via bibunits: the MAIN reference list
# (bu1.bbl) holds works cited in the main text; the ONLINE APPENDIX reference
# list (bu2.bbl) holds the rest and is excluded per PSRM's online-appendix rule.
# GOVERNING COUNT = body (five main sections; \input'ed generated tables are not
# followed, matching the diet-era ledger) + the main reference list.
# Requires a prior build: bu1.bbl / bu2.bbl are latexmk products.
set -euo pipefail
cd "$(dirname "$0")/../paper"
TEXCOUNT=$(command -v texcount || echo /Library/TeX/texbin/texcount)
DETEX=$(command -v detex || echo /Library/TeX/texbin/detex)
[ -f bu1.bbl ] || { echo "bu1.bbl missing -- run 'latexmk -pdf shadow.tex' first"; exit 1; }
B=$("$TEXCOUNT" -sum -brief -q -total sections/introduction.tex \
    sections/motivation.tex sections/constructing.tex \
    sections/decision.tex sections/conclusion.tex | tail -1)
B=${B%%:*}
MAIN=$("$DETEX" bu1.bbl | wc -w | tr -d ' ')
SI=$("$DETEX" bu2.bbl | wc -w | tr -d ' ')
echo "body=$B  main-refs(bu1.bbl)=$MAIN  online-appendix-refs(bu2.bbl)=$SI"
echo "GOVERNING (main manuscript, through the main reference list) = $((B + MAIN)) / 9000"
echo "  online-appendix reference list ($SI words) excluded per PSRM rule"
