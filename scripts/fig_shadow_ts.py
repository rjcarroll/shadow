"""Regenerate fig-shadow-ts for the three clean, heavily-internationalized
civil-war showcases (Angola, Ethiopia, Afghanistan).

Replaces the previous panel set, which paired an inflated, mislabeled
North-Vietnam series (ccode 816, plotted as "South Vietnam") and an inflated
Egypt series (651) -- both in the diffuse-floor regime documented in
notes/shadow-inflation-analysis.md. Mirrors notebook 09 cell "Figure 3"; uses
the shared paper style so it matches the other figures.
"""
import sys
from pathlib import Path

import pandas as pd
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.utils.plots import apply_style, GOV, OPP, SHADE  # noqa: E402

apply_style()
DATA = ROOT / "data" / "interim"
FIG = ROOT / "paper" / "figures"; FIG.mkdir(exist_ok=True)

files = sorted(DATA.glob("cy_shadow_*.parquet"))
assert len(files) == 25, f"expected 25 cy_shadow files, found {len(files)}"
cy = (pd.concat([pd.read_parquet(f) for f in files])
        .groupby(["ccode", "year"]).mean(numeric_only=True).reset_index())

CASES = {"540": "Angola", "530": "Ethiopia", "700": "Afghanistan"}
WAR_SPANS = {                                            # COW Intra-State Wars v5.1
    "540": [(1976, 1991), (1992, 1995), (1998, 2003)],   # Angola
    "530": [(1963, 1965), (1975, 1991)],                 # Ethiopia
    "700": [(1978, 1981), (1989, 2001), (2014, 2014)],   # Afghanistan
}
# Each panel spans its conflict era; this also drops the pre-conflict early-Cold-War
# years where long-standing, well-connected states show diffuse-floor inflation
# (Ethiopia 1951-53 in particular). See notes/shadow-inflation-analysis.md.
WINDOW = {"540": (1975, 2003), "530": (1972, 1992), "700": (1975, 2014)}

fig, axes = plt.subplots(len(CASES), 1, figsize=(6.5, 1.8 * len(CASES)), sharex=False)
for ax, (ccode, name) in zip(axes, CASES.items()):
    w0, w1 = WINDOW[ccode]
    sub = cy[(cy["ccode"] == ccode) & cy["year"].between(w0, w1)].sort_values("year")
    ax.set_xlim(w0, w1)
    for start, end in WAR_SPANS.get(ccode, []):
        ax.axvspan(start - 0.5, end + 0.5, alpha=0.15, color=SHADE, zorder=0)
    ax.plot(sub["year"], sub["E_gov"], label="$E^G$", color=GOV, linewidth=1.2)
    ax.plot(sub["year"], sub["E_opp"], label="$E^O$", color=OPP, linewidth=1.2, linestyle="--")
    ax.set_ylabel("Expected\ninterveners", fontsize=8)
    ax.set_title(name, fontsize=10)
    ax.tick_params(labelsize=8)
    if ccode == "540":                                   # legend on the first panel
        ax.legend(frameon=False, fontsize=8, loc="upper left")
axes[-1].set_xlabel("Year")
fig.tight_layout(h_pad=0.8)
fig.savefig(FIG / "fig-shadow-ts.pdf", bbox_inches="tight")
print(f"saved {FIG / 'fig-shadow-ts.pdf'}")
for ccode, name in CASES.items():
    sub = cy[cy["ccode"] == ccode]
    print(f"  {name} ({ccode}): {len(sub)} yrs {int(sub.year.min())}-{int(sub.year.max())}, "
          f"peak E^G {sub.E_gov.max():.2f} @ {int(sub.loc[sub.E_gov.idxmax(),'year'])}")
