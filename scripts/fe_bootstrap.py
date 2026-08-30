"""Standalone FE T×P bootstrap — runs outside notebook to avoid timeout."""
import sys, time
sys.path.insert(0, str(__import__('pathlib').Path(__file__).resolve().parent.parent))

import numpy as np
import pandas as pd
import statsmodels.api as sm
from statsmodels.discrete.conditional_models import ConditionalLogit
from pathlib import Path
import zipfile

ROOT    = Path(__file__).resolve().parent.parent
INTERIM = ROOT / "data" / "interim"
RESULTS = ROOT / "results"

# ── Config ──────────────────────────────────────────────────────────────
T_DRAWS = 25
P_REPS  = 100  # reduced from 200 for clogit speed
rng = np.random.default_rng(seed=20260304)

BASELINE_VARS = [
    "polity2_lag", "lgdp_lag", "lpop_lag",
    "lmtnest", "ncontig", "oil",
    "nwstate", "instab_lag", "prior_war",
    "ethfrac", "relfrac", "year",
]
FE_TIME_VARS = [
    "polity2_lag", "lgdp_lag", "lpop_lag",
    "nwstate", "instab_lag", "prior_war", "year",
]
SHADOW_VARS = ["E_gov_asinh", "E_opp_asinh"]
fe_covars = FE_TIME_VARS + SHADOW_VARS


def _build_preperiod():
    vdem_zip = ROOT / "data" / "raw" / "vdem" / "V-Dem-CY-FullOthers-v15_csv.zip"
    with zipfile.ZipFile(vdem_zip) as z:
        csv_name = [n for n in z.namelist() if n.endswith(".csv")][0]
        vdem = pd.read_csv(z.open(csv_name),
            usecols=["COWcode", "year", "e_polity2", "e_pt_coup"], low_memory=False)
    vdem = vdem[vdem["year"].between(1942, 1945)].copy()
    vdem = vdem.rename(columns={"COWcode": "ccode", "e_polity2": "polity2"})
    vdem["ccode"] = vdem["ccode"].astype("Int64").astype(str).str.zfill(3)
    vdem.loc[vdem["ccode"] == "365", "ccode"] = "364"
    vdem = vdem.sort_values(["ccode", "year"])
    vdem["polity2_lag3"] = vdem.groupby("ccode")["polity2"].shift(3)
    v45 = vdem[vdem["year"] == 1945].copy()
    delta3 = (v45["polity2"] - v45["polity2_lag3"]).abs()
    coup = v45["e_pt_coup"].fillna(0).astype(bool)
    v45["instab"] = ((delta3 >= 3) | coup).astype(float)
    v45.loc[v45["polity2"].isna() & ~coup, "instab"] = np.nan
    return v45[["ccode", "year", "polity2", "instab"]].copy()


def _fit_logit(y, X, maxiter=1000):
    try:
        return sm.Logit(y, X).fit(disp=False, maxiter=maxiter)
    except np.linalg.LinAlgError:
        return sm.Logit(y, X).fit(disp=False, method="bfgs", maxiter=maxiter)


# ── Load data ───────────────────────────────────────────────────────────
print("Loading data...")
cy_frames = [pd.read_parquet(INTERIM / f"cy_imputed_{i}.parquet") for i in range(1, 6)]
avg_cols = ["onset", "polity2", "lgdp_lag", "lpop_lag",
            "lmtnest", "ncontig", "oil", "nwstate", "instab",
            "prior_war", "ethfrac", "relfrac"]
cy_avg = pd.concat(cy_frames).groupby(["ccode", "year"])[avg_cols].mean().reset_index()
cy_avg["onset"] = (cy_avg["onset"] >= 0.5).astype(int)

pre = _build_preperiod()
cy_aug = pd.concat([pre, cy_avg], ignore_index=True).sort_values(["ccode", "year"])
cy_aug["polity2_lag"] = cy_aug.groupby("ccode")["polity2"].shift(1)
cy_aug["instab_lag"] = cy_aug.groupby("ccode")["instab"].shift(1)
base_df = cy_aug[cy_aug["year"] >= 1946].copy()

baseline_cols = list(dict.fromkeys(["ccode", "year", "onset"] + BASELINE_VARS))
base_df = base_df[baseline_cols].copy()
print(f"Base data: {len(base_df):,} rows")

# ── Naive FE result (averaged shadow) for comparison ────────────────────
sh_all = pd.concat([
    pd.read_parquet(INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
                    columns=["ccode", "year"] + SHADOW_VARS)
    for cy in range(1, 6) for ud in range(1, 6)
])
sh_avg = sh_all.groupby(["ccode", "year"])[SHADOW_VARS].mean().reset_index()
df_naive = base_df.merge(sh_avg, on=["ccode", "year"], how="left")
sub_naive = df_naive[list(dict.fromkeys(["onset", "ccode"] + fe_covars))].dropna()
onset_by_cc = sub_naive.groupby("ccode")["onset"].agg(["sum", "count"])
varying = onset_by_cc[(onset_by_cc["sum"] > 0) & (onset_by_cc["sum"] < onset_by_cc["count"])].index
sub_v = sub_naive[sub_naive["ccode"].isin(varying)]
naive_res = ConditionalLogit(
    sub_v["onset"].values, sub_v[fe_covars].astype(float).values,
    groups=sub_v["ccode"].values
).fit(disp=False)
print(f"Naive FE fit done: {len(sub_v):,} obs, {len(varying)} countries")

# ── T×P bootstrap ──────────────────────────────────────────────────────
print(f"\nStarting T×P bootstrap: {T_DRAWS} × {P_REPS} = {T_DRAWS*P_REPS:,}")
t0 = time.time()

all_coefs = []
draw_labels = []
draw_idx = 0
n_failed = 0

for cy in range(1, 6):
    for ud in range(1, 6):
        draw_idx += 1
        draw_key = f"{cy}_{ud}"
        t1 = time.time()

        sh = pd.read_parquet(
            INTERIM / f"cy_shadow_{cy}_{ud}.parquet",
            columns=["ccode", "year"] + SHADOW_VARS,
        )
        df_t = base_df.merge(sh, on=["ccode", "year"], how="left")
        sub_t = df_t[list(dict.fromkeys(["onset", "ccode"] + fe_covars))].dropna()

        onset_by_cc = sub_t.groupby("ccode")["onset"].agg(["sum", "count"])
        varying = onset_by_cc[
            (onset_by_cc["sum"] > 0) & (onset_by_cc["sum"] < onset_by_cc["count"])
        ].index
        sub_v = sub_t[sub_t["ccode"].isin(varying)].copy()

        y_full = sub_v["onset"].values
        X_full = sub_v[fe_covars].astype(float).values
        countries = sub_v["ccode"].values
        unique_cc = np.unique(countries)
        cc_idx = {cc: np.where(countries == cc)[0] for cc in unique_cc}

        # Point estimate
        try:
            res_t = ConditionalLogit(y_full, X_full, groups=countries).fit(disp=False)
            all_coefs.append(res_t.params)
            draw_labels.append(draw_key)
        except Exception:
            n_failed += 1

        # Bootstrap reps
        for p in range(P_REPS):
            cc_sample = rng.choice(unique_cc, size=len(unique_cc), replace=True)
            boot_idx = np.concatenate([cc_idx[cc] for cc in cc_sample])

            y_b = y_full[boot_idx]
            X_b = X_full[boot_idx]
            g_b = countries[boot_idx]

            # Filter to groups with onset variation
            onset_check = pd.Series(y_b).groupby(g_b).agg(["sum", "count"])
            vary_g = onset_check[
                (onset_check["sum"] > 0) & (onset_check["sum"] < onset_check["count"])
            ].index
            if len(vary_g) == 0:
                n_failed += 1
                continue
            mask = pd.Series(g_b).isin(vary_g).values

            try:
                res_b = ConditionalLogit(y_b[mask], X_b[mask], groups=g_b[mask]).fit(disp=False)
                all_coefs.append(res_b.params)
                draw_labels.append(draw_key)
            except Exception:
                n_failed += 1

        elapsed = time.time() - t1
        total_elapsed = time.time() - t0
        print(f"  Draw {draw_idx}/{T_DRAWS} ({draw_key}): "
              f"{elapsed:.0f}s this draw, {total_elapsed:.0f}s total, "
              f"{len(all_coefs):,} vectors, {n_failed} failed")

total_time = time.time() - t0
print(f"\nDone in {total_time:.0f}s ({total_time/60:.1f} min)")
print(f"Total coefficient vectors: {len(all_coefs):,}, failed: {n_failed}")

# ── Summarise ──────────────────────────────────────────────────────────
fe_boot_arr = np.array(all_coefs)
fe_boot_df = pd.DataFrame(fe_boot_arr, columns=fe_covars)
fe_boot_df["draw"] = draw_labels

print(f"\nT×P Bootstrap results (FE Entrants model):")
print(f"{'Variable':>25s}  {'Mean':>8s}  {'SD':>8s}  "
      f"{'CI_lo':>8s}  {'CI_hi':>8s}  {'Naive SE':>8s}  {'Inflation':>8s}")

for i, var in enumerate(fe_covars):
    mean_b = fe_boot_arr[:, i].mean()
    sd_b   = fe_boot_arr[:, i].std()
    ci_lo  = np.percentile(fe_boot_arr[:, i], 2.5)
    ci_hi  = np.percentile(fe_boot_arr[:, i], 97.5)
    naive_se = naive_res.bse[i]
    inflation = sd_b / naive_se
    sig = " *" if (ci_lo > 0 or ci_hi < 0) else ""
    print(f"{var:>25s}  {mean_b:+8.4f}  {sd_b:8.4f}  "
          f"{ci_lo:+8.4f}  {ci_hi:+8.4f}  {naive_se:8.4f}  {inflation:8.2f}{sig}")

# Variance decomposition
print("\n── Variance decomposition (shadow vars, FE model) ──")
for var in SHADOW_VARS:
    within = fe_boot_df.groupby("draw")[var].var().mean()
    between = fe_boot_df.groupby("draw")[var].mean().var()
    total = fe_boot_df[var].var()
    pct_between = between / total * 100
    print(f"  {var}: total={total:.4f}, "
          f"within={within:.4f} ({100-pct_between:.1f}%), "
          f"between={between:.4f} ({pct_between:.1f}%)")

# Save
fe_boot_summary = pd.DataFrame({
    "variable": fe_covars,
    "mean":     fe_boot_arr.mean(axis=0),
    "sd":       fe_boot_arr.std(axis=0),
    "ci_lo":    np.percentile(fe_boot_arr, 2.5, axis=0),
    "ci_hi":    np.percentile(fe_boot_arr, 97.5, axis=0),
    "naive_se": naive_res.bse,
    "inflation": fe_boot_arr.std(axis=0) / naive_res.bse,
})
fe_boot_summary.to_parquet(RESULTS / "stage2_bootstrap_fe.parquet", index=False)
print(f"\nSaved: {RESULTS / 'stage2_bootstrap_fe.parquet'}")
