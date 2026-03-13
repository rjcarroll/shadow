"""
Correlate our shadow measure with Gibilisco & Montero (2024) structural estimates.

G&M estimate a 6-player game (Rebel + 5 P5 members) and produce:
  1. Conditional intervention probabilities: Pr(k intervenes | conflict) per country
  2. Counterfactual peace probabilities: Pr(peace) under data / noInt / allInt / no-k / all-k

Both are cross-sectional (150 countries, averaged 1950-1999).

We compare these to our shadow measure's P5-specific and aggregate quantities,
averaged over the same period. High correlation = convergent validity between
a fully structural approach and our learned-proxy approach.

Requires: cy_shadow files from nb06 (run after nb05/06 complete).
"""

import sys
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats

ROOT = Path(__file__).resolve().parent.parent
INTERIM = ROOT / "data" / "interim"
GM_DIR = ROOT / "replication"

# ── Paths to G&M replication data ──────────────────────────────────────────
# These CSVs are inside the zip; extract first or adjust paths.
GM_INTERV_PROBS = Path("/tmp/gm_replication/Gibilisco_Montero_Intervention/"
                       "matlab/baseline_model/"
                       "conditionalInterventionProbs_replication.csv")
GM_CF_PEACE = Path("/tmp/gm_replication/Gibilisco_Montero_Intervention/"
                   "matlab/baseline_model/"
                   "counterfactuals_prpeace_replication.csv")
GM_LAKE = Path("/tmp/gm_replication/Gibilisco_Montero_Intervention/"
               "r/Lake_HIR_Country_yearreplication.dta")

# P5 COW codes (matching our pipeline)
P5_CODES = {"002", "200", "220", "365", "710"}  # US, UK, FR, RU/USSR, CHN
# G&M use 364 for Russia in some periods; our fix_ccode maps 365->364
# Their ccode column should use COW standard


def load_shadow_mean(year_range=(1950, 1999)):
    """Load cy_shadow files and average over draws and years."""
    frames = []
    for cy in range(1, 6):
        for ud in range(1, 6):
            path = INTERIM / f"cy_shadow_{cy}_{ud}.parquet"
            if not path.exists():
                raise FileNotFoundError(
                    f"Missing {path}. Run nb05 + nb06 first."
                )
            df = pd.read_parquet(path)
            df = df[(df["year"] >= year_range[0]) & (df["year"] <= year_range[1])]
            frames.append(df)

    all_draws = pd.concat(frames, ignore_index=True)

    # Average across all 25 draws and all years -> one row per country
    e_cols = [c for c in all_draws.columns
              if c.startswith("E_") and not c.endswith("_asinh")]
    agg_cols = e_cols + [c + "_asinh" for c in e_cols
                         if c + "_asinh" in all_draws.columns]

    shadow_mean = (
        all_draws
        .groupby("ccode")[agg_cols]
        .mean()
        .reset_index()
    )
    # Convert ccode to string with zero-padding for merge
    shadow_mean["ccode_str"] = shadow_mean["ccode"].astype(str).str.zfill(3)
    return shadow_mean


def load_gm_intervention_probs():
    """Load G&M conditional intervention probabilities."""
    gm = pd.read_csv(GM_INTERV_PROBS)
    # Sum across P5 members for total intervention exposure
    gm["total_P5"] = gm[["US", "UK", "FRN", "RUS", "CHN"]].sum(axis=1)
    # Western bloc (US+UK+FR) vs Eastern (RUS+CHN)
    gm["western"] = gm[["US", "UK", "FRN"]].sum(axis=1)
    gm["eastern"] = gm[["RUS", "CHN"]].sum(axis=1)
    return gm


def load_gm_counterfactuals():
    """Load G&M counterfactual peace probabilities."""
    cf = pd.read_csv(GM_CF_PEACE)
    # Compute intervention effects on peace
    cf["effect_allInt"] = cf["allInt"] - cf["noInt"]
    cf["effect_US"] = cf["allUS"] - cf["noUS"]
    cf["effect_RUS"] = cf["allRUS"] - cf["noRUS"]
    return cf


def load_lake_hierarchy():
    """Load Lake (2009) U.S. security hierarchy, averaged 1950-1999."""
    lake = pd.read_stata(GM_LAKE)
    lake = lake[lake["year"] <= 1999]
    lake_avg = (
        lake.groupby("ccode")["us_SH1995"]
        .mean()
        .reset_index()
        .rename(columns={"us_SH1995": "lake_hierarchy"})
    )
    return lake_avg


def correlate(x, y, label_x, label_y):
    """Compute and print Pearson and Spearman correlations."""
    mask = np.isfinite(x) & np.isfinite(y)
    x_clean, y_clean = x[mask], y[mask]
    n = len(x_clean)
    if n < 10:
        print(f"  {label_x} vs {label_y}: too few observations ({n})")
        return None

    r_pearson, p_pearson = stats.pearsonr(x_clean, y_clean)
    r_spearman, p_spearman = stats.spearmanr(x_clean, y_clean)
    print(f"  {label_x} vs {label_y} (n={n}):")
    print(f"    Pearson  r={r_pearson:+.3f}  p={p_pearson:.4f}")
    print(f"    Spearman r={r_spearman:+.3f}  p={p_spearman:.4f}")
    return {"x": label_x, "y": label_y, "n": n,
            "pearson_r": r_pearson, "pearson_p": p_pearson,
            "spearman_r": r_spearman, "spearman_p": p_spearman}


def main():
    print("=" * 70)
    print("  Shadow Measure vs G&M (2024) Structural Estimates")
    print("=" * 70)

    # Load data
    try:
        shadow = load_shadow_mean()
    except FileNotFoundError as e:
        print(f"\n{e}")
        print("Run nb05 + nb06 first, then re-run this script.")
        sys.exit(1)

    gm_int = load_gm_intervention_probs()
    gm_cf = load_gm_counterfactuals()
    lake = load_lake_hierarchy()

    # ── Merge on ccode ─────────────────────────────────────────────────────
    # G&M ccode is numeric COW code. Our ccode is string (zero-padded).
    gm_int["ccode_str"] = gm_int["ccode"].astype(str).str.zfill(3)
    gm_cf["ccode_str"] = gm_cf["ccode"].astype(str).str.zfill(3)
    lake["ccode_str"] = lake["ccode"].astype(str).str.zfill(3)

    merged = shadow.merge(gm_int, on="ccode_str", how="inner", suffixes=("", "_gm"))
    merged = merged.merge(gm_cf[["ccode_str", "data", "noInt", "allInt",
                                  "effect_allInt", "effect_US", "effect_RUS"]],
                          on="ccode_str", how="left")
    merged = merged.merge(lake[["ccode_str", "lake_hierarchy"]],
                          on="ccode_str", how="left")

    print(f"\nMerged dataset: {len(merged)} countries")
    print(f"  Shadow countries: {len(shadow)}")
    print(f"  G&M countries: {len(gm_int)}")
    print(f"  Lake hierarchy countries: {lake['lake_hierarchy'].notna().sum()}")

    results = []

    # ── 1. Aggregate shadow vs G&M total intervention probability ──────────
    print("\n" + "-" * 70)
    print("1. Aggregate intervention exposure")
    print("-" * 70)

    # E_gov + E_opp (total intervention exposure) vs G&M total
    if "E_gov_trim" in merged.columns:
        merged["E_total_trim"] = merged["E_gov_trim"] + merged["E_opp_trim"]
        results.append(correlate(
            merged["E_total_trim"].values, merged["total_P5"].values,
            "E_total_trim (shadow)", "total_P5 (G&M)"))

    if "E_gov" in merged.columns:
        merged["E_total"] = merged["E_gov"] + merged["E_opp"]
        results.append(correlate(
            merged["E_total"].values, merged["total_P5"].values,
            "E_total (shadow)", "total_P5 (G&M)"))

    # ── 2. P5-specific shadow vs G&M P5 ───────────────────────────────────
    print("\n" + "-" * 70)
    print("2. P5-specific intervention exposure")
    print("-" * 70)

    if "E_P5_gov_trim" in merged.columns:
        merged["E_P5_total_trim"] = merged["E_P5_gov_trim"] + merged["E_P5_opp_trim"]
        results.append(correlate(
            merged["E_P5_total_trim"].values, merged["total_P5"].values,
            "E_P5_total_trim (shadow)", "total_P5 (G&M)"))

    if "E_major_gov_trim" in merged.columns:
        merged["E_major_total_trim"] = merged["E_major_gov_trim"] + merged["E_major_opp_trim"]
        results.append(correlate(
            merged["E_major_total_trim"].values, merged["total_P5"].values,
            "E_major_total_trim (shadow)", "total_P5 (G&M)"))

    # ── 3. Direction-specific: gov vs opp ─────────────────────────────────
    print("\n" + "-" * 70)
    print("3. Direction-specific correlations")
    print("-" * 70)

    # G&M's "direction" robustness (if available from separate CSV)
    # For now, correlate shadow gov/opp separately with total G&M
    if "E_P5_gov_trim" in merged.columns:
        results.append(correlate(
            merged["E_P5_gov_trim"].values, merged["western"].values,
            "E_P5_gov_trim (shadow)", "western_P5 (G&M)"))
        results.append(correlate(
            merged["E_P5_opp_trim"].values, merged["eastern"].values,
            "E_P5_opp_trim (shadow)", "eastern_P5 (G&M)"))

    # ── 4. Shadow vs G&M peace counterfactuals ─────────────────────────────
    print("\n" + "-" * 70)
    print("4. Shadow measure vs G&M peace probability")
    print("-" * 70)

    if "E_gov_trim_asinh" in merged.columns and "data" in merged.columns:
        results.append(correlate(
            merged["E_gov_trim_asinh"].values, merged["data"].values,
            "E_gov_trim_asinh (shadow)", "Pr(peace|data) (G&M)"))
        results.append(correlate(
            merged["E_total_trim"].values, merged["effect_allInt"].values,
            "E_total_trim (shadow)", "effect_allInt (G&M)"))

    # ── 5. Shadow vs Lake hierarchy ────────────────────────────────────────
    print("\n" + "-" * 70)
    print("5. Shadow measure vs Lake (2009) U.S. security hierarchy")
    print("-" * 70)

    if "E_gov_trim" in merged.columns:
        results.append(correlate(
            merged["E_gov_trim"].values, merged["lake_hierarchy"].values,
            "E_gov_trim (shadow)", "Lake hierarchy"))

    if "E_P5_gov_trim" in merged.columns:
        results.append(correlate(
            merged["E_P5_gov_trim"].values, merged["lake_hierarchy"].values,
            "E_P5_gov_trim (shadow)", "Lake hierarchy"))

    # G&M's US probability vs Lake (replicating their Fig 1 right panel)
    results.append(correlate(
        merged["US"].values, merged["lake_hierarchy"].values,
        "Pr(US intervenes) (G&M)", "Lake hierarchy"))

    # ── 6. Cold War subsample ──────────────────────────────────────────────
    print("\n" + "-" * 70)
    print("6. G&M Pr(peace) counterfactual effects")
    print("-" * 70)
    print("  (Positive effect = intervention increases peace)")

    if "effect_US" in merged.columns:
        print(f"\n  G&M mean effect of US always intervening: "
              f"{merged['effect_US'].mean():.3f}")
        print(f"  G&M mean effect of RUS always intervening: "
              f"{merged['effect_RUS'].mean():.3f}")
        print(f"  G&M mean effect of ALL always intervening: "
              f"{merged['effect_allInt'].mean():.3f}")
        print(f"  G&M mean Pr(peace|no intervention): "
              f"{merged['noInt'].mean():.3f}")
        print(f"  G&M mean Pr(peace|data): "
              f"{merged['data'].mean():.3f}")

    # ── Summary table ──────────────────────────────────────────────────────
    print("\n" + "=" * 70)
    print("  Summary of all correlations")
    print("=" * 70)
    results_clean = [r for r in results if r is not None]
    if results_clean:
        summary = pd.DataFrame(results_clean)
        print(summary[["x", "y", "n", "pearson_r", "spearman_r"]].to_string(index=False))

        # Save
        out_path = ROOT / "results" / "gm_correlations.csv"
        out_path.parent.mkdir(parents=True, exist_ok=True)
        summary.to_csv(out_path, index=False)
        print(f"\nSaved to {out_path}")

    return merged, results_clean


if __name__ == "__main__":
    merged, results = main()
