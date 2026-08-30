"""
Regenerate the three data-driven figures on the current (baseline+chain) pipeline
outputs, extracted from notebooks/09-figures.ipynb (cells 3, 5, 7, 9):
  fig-fp-convergence.pdf  (Stage-1 burnout convergence; sl_spat_conv_*)
  fig-sl-calibration.pdf  (Stage-1 reliability diagrams; sl_oof_*)
  fig-shadow-kde.pdf      (E^G / E^O density; cy_shadow_*)
All vector PDFs.  Run:  .venv/bin/python scripts/fig_appendix.py
"""
import sys, ast
from pathlib import Path
import numpy as np, pandas as pd
import matplotlib.pyplot as plt
import pyarrow.parquet as pq
from sklearn.calibration import calibration_curve
from scipy.stats import gaussian_kde

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.utils.plots import apply_style, GOV, OPP, SHADE

apply_style()
DATA = ROOT / "data" / "interim"
FIG = ROOT / "paper" / "figures"
FIG.mkdir(exist_ok=True)
SINGLE = (4.8, 4.0)

# ── fig-fp-convergence (nb09 cell 3) ──────────────────────────────
curves = []
for f in sorted(DATA.glob("sl_spat_conv_*.parquet")):
    cols = pq.read_schema(f).names
    read_cols = ["cy", "ud"] + [c for c in ["n_iters", "final_delta", "deltas"] if c in cols]
    df = pd.read_parquet(f, columns=read_cols)
    cy, ud = int(df["cy"].iloc[0]), int(df["ud"].iloc[0])
    if "deltas" in cols:
        curves.append({"cy": cy, "ud": ud, "deltas": ast.literal_eval(str(df["deltas"].iloc[0]))})
    else:
        curves.append({"cy": cy, "ud": ud, "deltas": [float(df["final_delta"].iloc[0])],
                       "n_iters_only": int(df["n_iters"].iloc[0])})
fig, ax = plt.subplots(figsize=SINGLE)
for c in curves:
    d = c["deltas"]
    if "n_iters_only" in c:
        ax.scatter(c["n_iters_only"], d[-1], color=GOV, s=20, alpha=0.5, zorder=5)
    else:
        iters = list(range(1, len(d) + 1))
        ax.plot(iters, d, color=GOV, alpha=0.3, linewidth=1, zorder=3)
        ax.scatter(iters[-1], d[-1], color=GOV, s=15, alpha=0.5, zorder=5)
ax.axhline(5e-4, color=OPP, linestyle="--", linewidth=0.8, label="Tolerance (5×10⁻⁴)")
ax.set_yscale("log"); ax.set_xlabel("Iteration"); ax.set_ylabel("Mean |Δ spatial lag|")
ax.set_title("Stage 1 Burnout Convergence (25 draws)")
ax.legend(frameon=False, fontsize=9); ax.set_xlim(0.5, 12)
fig.tight_layout(); fig.savefig(FIG / "fig-fp-convergence.pdf", bbox_inches="tight"); plt.close(fig)
n_full = sum(1 for c in curves if "n_iters_only" not in c)
print(f"fig-fp-convergence: {len(curves)} draws, {n_full} with full trajectories, {len(curves)-n_full} endpoint-only")

# ── fig-sl-calibration (nb09 cell 5) ──────────────────────────────
oof_files = sorted(DATA.glob("sl_oof_*.parquet"))
assert len(oof_files) == 25, f"Expected 25, found {len(oof_files)}"
oof = pd.concat([pd.read_parquet(f) for f in oof_files], ignore_index=True)
CLASSES = {0: ("No intervention", "p_none"), 1: ("Government-biased", "p_gov"), 2: ("Opposition-biased", "p_opp")}
fig, axes = plt.subplots(1, 3, figsize=(6.5, 2.8))
for ax, (k, (label, pcol)) in zip(axes, CLASSES.items()):
    y_bin = (oof["intervention"] == k).astype(int).values
    p = oof[pcol].values
    frac_pos, mean_pred = calibration_curve(y_bin, p, n_bins=10, strategy="quantile")
    ax.plot(mean_pred, frac_pos, "o-", color=GOV, markersize=4, linewidth=1.2, zorder=5)
    lo = min(mean_pred.min(), frac_pos.min()); hi = max(mean_pred.max(), frac_pos.max())
    pad = (hi - lo) * 0.12; lo = max(0, lo - pad); hi = min(1, hi + pad)
    ax.set_xlim(lo, hi); ax.set_ylim(lo, hi)
    ax.plot([lo, hi], [lo, hi], "--", color=OPP, linewidth=0.8, zorder=1)
    ax_hist = ax.inset_axes([0.0, 0.0, 1.0, 0.22])
    ax_hist.hist(p, bins=50, range=(lo, hi), color=SHADE, alpha=0.6, edgecolor="none")
    ax_hist.set_xlim(lo, hi); ax_hist.set_xticks([]); ax_hist.set_yticks([]); ax_hist.patch.set_alpha(0)
    for spine in ax_hist.spines.values():
        spine.set_visible(False)
    ax.set_title(label, fontsize=9); ax.set_xlabel("Mean predicted prob.", fontsize=8); ax.tick_params(labelsize=7)
axes[0].set_ylabel("Observed frequency", fontsize=8)
fig.tight_layout(w_pad=1.5); fig.savefig(FIG / "fig-sl-calibration.pdf", bbox_inches="tight"); plt.close(fig)
print(f"fig-sl-calibration: pooled {len(oof)} OOF rows")

# ── fig-shadow-kde (nb09 cells 7 + 9) ─────────────────────────────
def load_cy_shadow():
    files = sorted(DATA.glob("cy_shadow_*.parquet"))
    assert len(files) == 25, f"Expected 25, found {len(files)}"
    return pd.concat([pd.read_parquet(f) for f in files]).groupby(["ccode", "year"]).mean(numeric_only=True).reset_index()

cy = load_cy_shadow()
fig, ax = plt.subplots(figsize=SINGLE)
x_grid = np.linspace(0, cy[["E_gov", "E_opp"]].max().max() * 1.05, 500)
for col, label, color, ls in [("E_gov", "$E^G$", GOV, "-"), ("E_opp", "$E^O$", OPP, "--")]:
    vals = cy[col].dropna().values
    ax.plot(x_grid, gaussian_kde(vals, bw_method=0.15)(x_grid), color=color, linestyle=ls, linewidth=1.5, label=label)
ax.fill_between(x_grid, gaussian_kde(cy["E_gov"].dropna().values, bw_method=0.15)(x_grid), alpha=0.2, color=GOV)
ax.set_xlabel("Expected interveners"); ax.set_ylabel("Density")
ax.set_title("Distribution of $E^G$ and $E^O$ (all country-years)")
ax.legend(frameon=False, fontsize=9)
fig.tight_layout(); fig.savefig(FIG / "fig-shadow-kde.pdf", bbox_inches="tight"); plt.close(fig)
print(f"fig-shadow-kde: {len(cy)} country-years")
print("done")
