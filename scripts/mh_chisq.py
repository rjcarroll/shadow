"""
Good-faith implementation of the McAlexander & Mentch (2020) predictive-
significance test: the Mentch & Hooker (2016) chi-square prediction-difference
test with subsampled tree ensembles, applied to the Entrants onset model.

Procedure (M&M 2020 "Hypothesis testing with random forests"; MH 2016):
  1. REDUCED dataset: focal column(s) permuted (their robust variant — permute,
     don't drop; joint tests permute the whole set with one row permutation).
  2. Original + reduced trees built on PAIRED subsamples of size K drawn
     without replacement; the tree-pair prediction DIFFERENCE at the N test
     points is one draw of the difference kernel D(x) = T_orig(x) - T_red(x).
  3. D_hat = mean difference over M independent subsample pairs.
  4. Sigma_D = (K^2 / n_train) * Sigma_1 + (1/M) * Sigma_k, with Sigma_1 the
     covariance of the conditional expectation given one shared observation
     (MH2016 fixed-point estimator: NZ fixed points x NMC trees each) and
     Sigma_k the raw kernel covariance (from the M main pairs).
  5. Statistic D'S^{-1}D ~ chi2_N under H0 (focal is not predictively
     significant); p = P(chi2_N >= stat).

Faithful choices, per M&M footnote 4 unless noted:
  * K=75, NZ(nx1)=50, NMC=1000, minsplit=3, N=20 test points.
  * Test points are HELD OUT of all training subsamples (their "Replication"
    section stresses tests are conducted on held-out data) and drawn from
    observed rows — the empirical distribution is the density weighting.
  * Like them, tuning is validated on CALIBRATION ANCHORS FIRST: a pure-noise
    focal must produce ~uniform p across replications; a synthetic strong
    focal (noised copy of the label) must reject. Only then unblind the
    shadow tests.
  * Trees are deterministic given the subsample (max_features=None): the
    ensemble is a clean infinite-order U-statistic ("subbagging"); the MH2016
    random-kernel extension would also permit feature subsampling.

Usage:
  .venv/bin/python scripts/mh_chisq.py smoke               # tiny params, ~1 min
  .venv/bin/python scripts/mh_chisq.py calibrate [NMC]     # noise x20 + synthetic
  .venv/bin/python scripts/mh_chisq.py focal [NMC]         # the real tests + anchors
Writes results/spike/mh_chisq_{calib,focal}.parquet. Single-threaded by design
(plays nice with concurrently running fan-outs).
"""
import os, sys, time, warnings
os.environ.setdefault("OMP_NUM_THREADS", "1")
sys.path.insert(0, "scripts"); warnings.filterwarnings("ignore")
import numpy as np
import pandas as pd
from scipy.stats import chi2
from sklearn.tree import DecisionTreeClassifier

from spike_rf_significance import ENTRANTS, load_analysis_data, SPIKE

SEED = 20260713
K, NZ, NMC, M, NTEST, MINSPLIT = 75, 50, 1000, 2000, 20, 3


def prob1(t, X):
    """P(onset) from a tree that may have seen only one class."""
    P = t.predict_proba(X)
    if P.shape[1] == 1:
        return np.full(len(X), float(t.classes_[0]))
    return P[:, 1]


def pair_diff(X, Xr, y, s, Xtest):
    """One draw of the difference kernel: paired trees on subsample s."""
    t1 = DecisionTreeClassifier(min_samples_split=MINSPLIT).fit(X[s], y[s])
    t2 = DecisionTreeClassifier(min_samples_split=MINSPLIT).fit(Xr[s], y[s])
    return prob1(t1, Xtest) - prob1(t2, Xtest)


def mh_test(X, y, focal, label, seed, nmc=NMC, m=M, nz=NZ, ntest=NTEST, ridge=0.0):
    """MH2016 chi-square prediction-difference test for the focal column set."""
    t0 = time.time()
    rng = np.random.default_rng(seed)
    n = len(y)

    # held-out test points, from observed rows (dense by construction)
    test_idx = rng.choice(n, ntest, replace=False)
    pool = np.setdiff1d(np.arange(n), test_idx)
    Xtest = X[test_idx]

    # reduced dataset: permute the focal set jointly (one row permutation)
    perm = rng.permutation(n)
    Xr = X.copy()
    Xr[:, focal] = X[np.ix_(perm, focal)]

    # (3) main paired ensembles -> D_hat and Sigma_k
    D = np.empty((m, ntest))
    for j in range(m):
        s = rng.choice(pool, K, replace=False)
        D[j] = pair_diff(X, Xr, y, s, Xtest)
    Dhat = D.mean(axis=0)
    Sk = np.cov(D, rowvar=False)

    # (4) Sigma_1 via the fixed-common-observation estimator
    fixed = rng.choice(pool, nz, replace=False)
    Ybar = np.empty((nz, ntest))
    for i, z in enumerate(fixed):
        rest = pool[pool != z]
        acc = np.zeros(ntest)
        for _ in range(nmc):
            s = np.append(rng.choice(rest, K - 1, replace=False), z)
            acc += pair_diff(X, Xr, y, s, Xtest)
        Ybar[i] = acc / nmc
    S1 = np.cov(Ybar, rowvar=False)

    Sd = (K * K / len(pool)) * S1 + Sk / m
    if ridge:
        Sd = Sd + ridge * np.trace(Sd) / ntest * np.eye(ntest)
    stat = float(Dhat @ np.linalg.pinv(Sd) @ Dhat)
    p = float(chi2.sf(stat, df=ntest))
    dt = time.time() - t0
    cond = float(np.linalg.cond(Sd))
    print(f"  [{label:22s}] stat={stat:9.2f} df={ntest}  p={p:.4f}  "
          f"|Dhat|max={np.abs(Dhat).max():.4f}  cond(Sd)={cond:.1e}  ({dt:.0f}s)", flush=True)
    return dict(test=label, stat=stat, df=ntest, p=p, dmax=float(np.abs(Dhat).max()),
                cond=cond, k=K, nz=nz, nmc=nmc, m=m, seconds=dt)


def get_Xy():
    data = load_analysis_data()
    sub = data[["onset"] + ENTRANTS].dropna()
    y = sub["onset"].values.astype(int)
    X = sub[ENTRANTS].values.astype(float)
    print(f"frame: n={len(y)}, onsets={y.sum()} ({y.mean()*100:.1f}%); "
          f"expected events per subsample ~{K*y.mean():.1f}", flush=True)
    return X, y


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "smoke"
    nmc = int(sys.argv[2]) if len(sys.argv) > 2 else NMC
    X, y = get_Xy()
    gov_i, opp_i = ENTRANTS.index("E_gov_asinh"), ENTRANTS.index("E_opp_asinh")
    lgdp_i, lpop_i = ENTRANTS.index("lgdp_lag"), ENTRANTS.index("lpop_lag")
    rng = np.random.default_rng(SEED)
    p_feat = X.shape[1]  # appended-column index for noise/synthetic

    if mode == "smoke":
        # tiny params: correctness only, no inference
        Xn = np.column_stack([X, rng.standard_normal(len(y))])
        r = mh_test(Xn, y, [p_feat], "smoke:noise", seed=SEED, nmc=30, m=200, nz=10, ntest=10)
        Xs = np.column_stack([X, y + rng.normal(0, 0.5, len(y))])
        r2 = mh_test(Xs, y, [p_feat], "smoke:synthetic", seed=SEED, nmc=30, m=200, nz=10, ntest=10)
        print("smoke done (no inference intended at these params)")
        return

    if mode == "calibrate":
        # M&M footnote 4 discipline: noise focal must give ~uniform p across reps
        rows = []
        for rep in range(20):
            Xn = np.column_stack([X, np.random.default_rng(SEED + 100 + rep).standard_normal(len(y))])
            rows.append(mh_test(Xn, y, [p_feat], f"noise rep{rep:02d}", seed=SEED + 1000 + rep, nmc=nmc))
        Xs = np.column_stack([X, y + rng.normal(0, 0.5, len(y))])
        rows.append(mh_test(Xs, y, [p_feat], "synthetic (MUST reject)", seed=SEED + 2000, nmc=nmc))
        R = pd.DataFrame(rows)
        R.to_parquet(SPIKE / "mh_chisq_calib.parquet", index=False)
        pn = R[R.test.str.startswith("noise")]["p"]
        print(f"\nnoise p: mean={pn.mean():.2f} (want ~0.5)  min={pn.min():.2f}  "
              f"frac<=0.05={(pn <= 0.05).mean():.2f} (want ~0.05)")
        print(f"synthetic p={R.iloc[-1]['p']:.4f} (want <=0.05)")
        return

    if mode == "focal":
        rows = [
            mh_test(X, y, [gov_i], "E_gov", seed=SEED + 1, nmc=nmc),
            mh_test(X, y, [opp_i], "E_opp", seed=SEED + 2, nmc=nmc),
            mh_test(X, y, [gov_i, opp_i], "E_gov+E_opp (joint)", seed=SEED + 3, nmc=nmc),
            mh_test(X, y, [lgdp_i], "GDP (control)", seed=SEED + 4, nmc=nmc),
            mh_test(X, y, [lpop_i], "Population (ctrl)", seed=SEED + 5, nmc=nmc),
        ]
        # anchors at identical settings, reported alongside
        Xn = np.column_stack([X, rng.standard_normal(len(y))])
        rows.append(mh_test(Xn, y, [p_feat], "noise (NOT reject)", seed=SEED + 6, nmc=nmc))
        Xs = np.column_stack([X, y + rng.normal(0, 0.5, len(y))])
        rows.append(mh_test(Xs, y, [p_feat], "synthetic (MUST reject)", seed=SEED + 7, nmc=nmc))
        R = pd.DataFrame(rows)
        R.to_parquet(SPIKE / "mh_chisq_focal.parquet", index=False)
        print("\nsaved:", SPIKE / "mh_chisq_focal.parquet")
        return

    raise SystemExit(f"unknown mode {mode!r}")


if __name__ == "__main__":
    main()
