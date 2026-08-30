"""
Per-draw RF predictive-significance tests (measurement-honest version of the
phase-1 spike). For EACH of the 25 shadow draws: permutation tests for E_gov,
E_opp, and the JOINT pair on that draw's frame — propagating Stage-1
measurement uncertainty that the averaged-frame run hides (the M&M-style test
knows nothing about generated regressors; running it per draw and reporting
the distribution of verdicts is our Gill-register adaptation).

Reuses perm_test / data machinery from spike_rf_significance verbatim.
Writes results/spike/rfsig_perdraw.parquet. Marker: RFSIG_PERDRAW_COMPLETE.
Runtime ~1.5h (3 tests x 25 draws x ~75s).
"""
import sys, warnings
sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import pandas as pd

from spike_rf_significance import (BASELINE_VARS, SHADOW, ENTRANTS, INTERIM, SPIKE,
                                   load_analysis_data, perm_test)

def main():
    data = load_analysis_data()
    base_cols = list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))
    base_df = data[base_cols].copy()
    gov_i, opp_i = ENTRANTS.index("E_gov_asinh"), ENTRANTS.index("E_opp_asinh")

    rows = []
    for cy in range(1, 6):
        for ud in range(1, 6):
            sh = pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                                 columns=["ccode", "year"] + SHADOW)
            sub = base_df.merge(sh, on=["ccode", "year"], how="left")[["onset"] + ENTRANTS].dropna()
            y = sub["onset"].values.astype(int)
            X = sub[ENTRANTS].values.astype(float)
            tag = f"{cy}_{ud}"
            for label, idx in [("E_gov", [gov_i]), ("E_opp", [opp_i]),
                               ("E_gov+E_opp", [gov_i, opp_i])]:
                r = perm_test(X, y, idx, f"{label} {tag}")
                r["draw"], r["test"] = tag, label
                rows.append(r)
            # checkpoint after every draw so a crash loses nothing
            pd.DataFrame(rows).to_parquet(SPIKE / "rfsig_perdraw.parquet", index=False)

    R = pd.DataFrame(rows)
    print("\n=== PER-DRAW SUMMARY (25 draws, n_perm=99) ===")
    for t, g in R.groupby("test"):
        p = g["p"]
        print(f"  {t:14s} median p={p.median():.2f}  p<=0.05 in {(p <= 0.05).sum()}/25  "
              f"p<=0.10 in {(p <= 0.10).sum()}/25  [min {p.min():.2f}, max {p.max():.2f}]")

if __name__ == "__main__":
    main()
