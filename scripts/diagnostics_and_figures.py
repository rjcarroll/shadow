"""
A+B: diagnostics off the LOCO preds (NO new fitting) + vector-PDF figures.

A1. Proximate timing trajectories t-2..t+2 (OOS predicted-prob, all 6 models), as ratios.
A2. Heterogeneity: DELTA-AUC (shadow - baseline) by ERA and by REGION, cluster-bootstrap CIs.
B.  pp-scatter figures (log-log, 45-deg line; never/eventually/onset coloring).

ADDITIVE & NON-DESTRUCTIVE: reads results/spike/*preds.parquet + data/interim/country_year.parquet,
writes tables to results/spike/ and VECTOR PDFs to results/spike/fig/ (NOT paper/figures/ — promote later).

Usage:  .venv/bin/python scripts/diagnostics_and_figures.py
"""
import sys, warnings; sys.path.insert(0, "scripts"); sys.path.insert(0, "src"); warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np, pandas as pd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from sklearn.metrics import roc_auc_score

try:
    from shadow.utils.plots import apply_style, GOV, OPP, ACCENT, SHADE
except Exception:  # self-contained fallback mirroring the house style
    GOV, OPP, ACCENT, SHADE = "#000000", "#888888", "#555555", "#cccccc"
    def apply_style():
        plt.rcParams.update({"figure.dpi": 150, "font.family": "sans-serif",
                             "axes.spines.top": False, "axes.spines.right": False})
RED = "#c1272d"   # onset points (exploratory; swap to GOV+marker for grayscale-final)
apply_style()

SPIKE = Path("results/spike"); FIG = SPIKE / "fig"; FIG.mkdir(parents=True, exist_ok=True)
MODELS6 = ["logit_Baseline", "logit_Entrants", "logit_Full", "rf_Baseline", "rf_Entrants", "rf_Full"]

# ---- load & assemble ----
preds = pd.read_parquet(SPIKE / "oos_loco_preds.parquet")
chan = pd.read_parquet(SPIKE / "oos_loco_channel_preds.parquet").drop(columns="onset")
cy = pd.read_parquet("data/interim/country_year.parquet", columns=["ccode", "year", "region"])
df = preds.merge(chan, on=["ccode", "year"], how="left").merge(cy, on=["ccode", "year"], how="left")
ever = set(df.loc[df.onset == 1, "ccode"].unique())
print(f"rows={len(df)}  onsets={int(df.onset.sum())}  ever-onset countries={len(ever)}")

# ============================================================ A1. TIMING
DTS = [-2, -1, 0, 1, 2]
onset_keys = df.loc[df.onset == 1, ["ccode", "year"]]
gmean = {m: df[m].mean() for m in MODELS6}
rows = []
for dt in DTS:
    k = onset_keys.copy(); k["year"] = k["year"] + dt
    j = k.merge(df, on=["ccode", "year"], how="inner")
    rec = {"dt": dt, "n": len(j)}
    for m in MODELS6:
        rec[m + "_ratio"] = j[m].mean() / gmean[m]
    rows.append(rec)
T = pd.DataFrame(rows)
T.to_parquet(SPIKE / "diag_timing.parquet", index=False)
print("\n=== A1. TIMING: mean OOS predicted prob / model's global mean (ratio) ===")
print("  (dt<0 run-up, dt=0 onset year, dt>0 aftermath -- t+1/t+2 mechanical & selected on short wars)")
print(T.round(2).to_string(index=False))

# ============================================================ A2/A3. HETEROGENEITY
def boot_delta_auc(sub, spec, base="logit_Baseline", B=1000, seed=20260609):
    sub = sub.dropna(subset=[spec, base])
    y = sub.onset.values.astype(int); ps = sub[spec].values; pb = sub[base].values; cc = sub.ccode.values
    n_on, n_cc = int(y.sum()), int(pd.unique(cc).size)
    if n_on < 3 or (len(y) - n_on) < 3:
        return dict(spec=spec, point=np.nan, lo=np.nan, hi=np.nan, n_onset=n_on, n_country=n_cc)
    point = roc_auc_score(y, ps) - roc_auc_score(y, pb)
    uniq = np.unique(cc); pos = {c: np.where(cc == c)[0] for c in uniq}; rng = np.random.default_rng(seed)
    out = []
    for _ in range(B):
        idx = np.concatenate([pos[c] for c in rng.choice(uniq, size=uniq.size, replace=True)])
        yy = y[idx]
        if 0 < yy.sum() < len(yy):
            out.append(roc_auc_score(yy, ps[idx]) - roc_auc_score(yy, pb[idx]))
    lo, hi = (np.percentile(out, [2.5, 97.5]) if out else (np.nan, np.nan))
    return dict(spec=spec, point=point, lo=lo, hi=hi, n_onset=n_on, n_country=n_cc)

def het_table(groups, label):
    recs = []
    for name, sub in groups:
        for spec in ["logit_Entrants", "logit_Full"]:
            r = boot_delta_auc(sub, spec); r[label] = name; recs.append(r)
    R = pd.DataFrame(recs)[[label, "spec", "point", "lo", "hi", "n_onset", "n_country"]]
    return R

df["era"] = np.where(df.year <= 1989, "ColdWar 46-89", "Post 90-14")
era_R = het_table([(n, df[df.era == n]) for n in ["ColdWar 46-89", "Post 90-14"]], "era")
REGIONS = ["africa", "asia", "mena", "latinAmerica", "eeurope"]  # western dropped (n_onset=1)
reg_R = het_table([(n, df[df.region == n]) for n in REGIONS], "region")
era_R.to_parquet(SPIKE / "diag_het_era.parquet", index=False)
reg_R.to_parquet(SPIKE / "diag_het_region.parquet", index=False)
print("\n=== A2. DELTA-AUC (spec - logit_Baseline) by ERA, cluster-boot 95% CI ===")
print(era_R.round(3).to_string(index=False))
print("\n=== A3. DELTA-AUC by REGION (western dropped, n_onset=1) ===")
print(reg_R.round(3).to_string(index=False))

# ============================================================ FIGURES
def style_ax(ax):
    for s in ("top", "right"): ax.spines[s].set_visible(False)

# ---- FIG timing ----
fig, ax = plt.subplots(figsize=(4.2, 3.3))
curves = [("logit_Baseline", SHADE, "-", "Baseline"),
          ("logit_Entrants", OPP, "-", "Entrants"),
          ("logit_Full", GOV, "-", "Full"),
          ("rf_Full", GOV, "--", "Full (RF)")]
for m, c, ls, lab in curves:
    ax.plot(T.dt, T[m + "_ratio"], ls, color=c, marker="o", ms=4, lw=1.8, label=lab)
ax.axvspan(0.5, 2.5, color=SHADE, alpha=0.35, lw=0)
ax.axhline(1.0, color=ACCENT, lw=0.8, ls=":")
ax.text(1.5, ax.get_ylim()[1] * 0.96, "aftermath\n(mechanical)", ha="center", va="top", fontsize=7, color=ACCENT)
ax.set_xticks(DTS); ax.set_xlabel("years relative to onset"); ax.set_ylabel("OOS pred. prob ÷ model mean")
ax.set_title("Risk trajectory around onset", fontsize=10); ax.legend(frameon=False, fontsize=8, loc="upper left")
style_ax(ax); fig.tight_layout(); fig.savefig(FIG / "fig-timing.pdf"); plt.close(fig)

# ---- FIG heterogeneity (era + region) ----
fig, axes = plt.subplots(1, 2, figsize=(8.4, 3.4))
for ax, R, ttl in [(axes[0], era_R, "by era"), (axes[1], reg_R, "by region")]:
    full = R[R.spec == "logit_Full"].reset_index(drop=True)
    bins = full[era_R.columns[0] if R is era_R else "region"].tolist() if False else full.iloc[:, 0].tolist()
    yy = np.arange(len(full))[::-1]
    ax.errorbar(full.point, yy, xerr=[full.point - full.lo, full.hi - full.point],
                fmt="o", color=GOV, ecolor=ACCENT, capsize=3, ms=5, lw=1.2)
    ax.axvline(0, color=ACCENT, lw=0.8, ls=":")
    ax.set_yticks(yy); ax.set_yticklabels([f"{b}\n(n={n})" for b, n in zip(full.iloc[:, 0], full.n_onset)], fontsize=8)
    ax.set_xlabel("Δ AUC  (Full − Baseline)"); ax.set_title(ttl, fontsize=10); style_ax(ax)
fig.suptitle("Where the shadow lifts onset discrimination (OOS)", fontsize=10)
fig.tight_layout(); fig.savefig(FIG / "fig-heterogeneity.pdf"); plt.close(fig)

# ---- FIG pp-scatter (3 panels) ----
def cat_colors(sub):
    c = np.where(sub.onset == 1, "onset", np.where(sub.ccode.isin(ever), "eventual", "never"))
    return c
# 2x2 on PERCENTILE RANKS (calibration-free -> isolates discrimination/re-ranking; avoids RF's
# probability inflation contaminating the cross-model-class comparison, per Rob's catch).
PAIRS = [("logit_Baseline", "logit_Entrants", "base question: aggregate shadow"),
         ("logit_Baseline", "logit_Full", "what we bought: full shadow"),
         ("logit_Entrants", "rf_Entrants", "nonlinearity: forest re-ranks (aggregate)"),
         ("logit_Full", "rf_Full", "…and it's gone once disaggregated")]
need = sorted(set([a for a, _, _ in PAIRS] + [b for _, b, _ in PAIRS]))
RNK = {m: df[m].rank(pct=True).values for m in need}
catall = cat_colors(df)
fig, axes = plt.subplots(2, 2, figsize=(8.6, 8.6)); axes = axes.ravel()
for ax, (xv, yv, ttl) in zip(axes, PAIRS):
    xr, yr = RNK[xv], RNK[yv]
    for cat, col, sz, al, z, mk, ew in [("never", SHADE, 5, .30, 1, "o", 0),
                                        ("eventual", OPP, 8, .50, 2, "o", 0),
                                        ("onset", GOV, 60, 1.0, 3, "*", 0.4)]:
        mm = catall == cat
        ax.scatter(xr[mm], yr[mm], s=sz, c=col, alpha=al, marker=mk, lw=ew, edgecolors="white",
                   zorder=z, clip_on=False,
                   label={"never": "never onset", "eventual": "eventually onsets", "onset": "onset year"}[cat])
    ax.plot([-0.04, 1.04], [-0.04, 1.04], color=ACCENT, lw=0.9, ls="--", zorder=4)
    ax.set_xlim(-0.04, 1.04); ax.set_ylim(-0.04, 1.04); ax.set_aspect("equal")
    lab = lambda v: v.replace("logit_", "").replace("rf_", "RF ")
    ax.set_xlabel(f"risk rank: {lab(xv)}"); ax.set_ylabel(f"risk rank: {lab(yv)}")
    ax.set_title(ttl, fontsize=9); style_ax(ax)
axes[0].legend(frameon=False, fontsize=7, loc="upper left", markerscale=1.4)
fig.suptitle("Re-ranking of onset risk across specifications (OOS percentile rank)", fontsize=10)
fig.tight_layout(); fig.savefig(FIG / "fig-ppscatter.pdf"); plt.close(fig)

print(f"\nwrote vector PDFs -> {FIG}/  (fig-timing, fig-heterogeneity, fig-ppscatter)")
print("tables -> results/spike/diag_timing.parquet, diag_het_era.parquet, diag_het_region.parquet")
