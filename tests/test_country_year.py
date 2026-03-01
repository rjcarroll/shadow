"""
Tests for the country-year pipeline output.

These tests validate the *data contracts* on the outputs of
notebooks/01-country-year.ipynb rather than the notebook itself,
so they run fast and are repeatable without re-executing the notebook.

The new pipeline uses COW Intra-State Wars v5.1 (WarTypes 4+5) and
V-Dem v15, covering 1940–2014 instead of the original F&L 1945–1999.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

INTERIM = Path(__file__).parents[1] / "data" / "interim"
CY_PATH = INTERIM / "country_year.parquet"


@pytest.fixture(scope="module")
def cy():
    if not CY_PATH.exists():
        pytest.skip("country_year.parquet not found — run notebook 01 first")
    return pd.read_parquet(CY_PATH)


@pytest.fixture(scope="module")
def incy(cy):
    """State-years in the full analysis sample (1946–2014)."""
    return cy[cy["inCY"] == True]


@pytest.fixture(scope="module")
def regan(cy):
    """State-years in the Regan training period (1946–1999)."""
    return cy[cy["inCY"] & cy["regan_period"]]


# ---------------------------------------------------------------------------
# Structure and uniqueness
# ---------------------------------------------------------------------------

def test_no_duplicate_cyears(cy):
    """Each cyear key appears at most once."""
    dups = cy["cyear"].duplicated().sum()
    assert dups == 0, f"Found {dups} duplicate cyear values"


def test_year_range(cy):
    """Years span 1940–2014 (pre-period for lags + full COW v5.1 coverage)."""
    assert cy["year"].min() == 1940, f"Expected min year 1940, got {cy['year'].min()}"
    assert cy["year"].max() == 2014, f"Expected max year 2014, got {cy['year'].max()}"


def test_incy_covers_1946_to_2014(incy):
    """inCY sample runs 1946–2014."""
    assert incy["year"].min() == 1946, f"inCY min year: {incy['year'].min()}"
    assert incy["year"].max() == 2014, f"inCY max year: {incy['year'].max()}"


def test_regan_period_within_incy(regan, incy):
    """regan_period is a subset of inCY (1946–1999)."""
    assert len(regan) < len(incy)
    assert regan["year"].max() == 1999, f"regan max year: {regan['year'].max()}"


def test_ccode_format(cy):
    """All non-null ccodes are 3-character zero-padded strings."""
    cc = cy["ccode"].dropna()
    bad = cc[~cc.str.match(r"^\d{3}$")]
    assert len(bad) == 0, f"Malformed ccodes: {bad.unique()[:10]}"


# ---------------------------------------------------------------------------
# Civil war onset (COW v5.1, WarTypes 4+5, high threshold)
# ---------------------------------------------------------------------------

def test_onset_binary(incy):
    """onset is binary (0 or 1) with no other values."""
    vals = set(incy["onset"].dropna().unique())
    assert vals <= {0, 1, 0.0, 1.0}, f"Non-binary onset values: {vals}"


def test_onset_count_regan_plausible(regan):
    """COW v5.1 onsets 1946–1999: expect roughly 100–200.

    COW v5.1 has 153 phase-starts 1946–1999 (each war restart counts as a new onset).
    F&L had 111 using a different coding convention (one onset per war lifetime).
    The numbers are not directly comparable.
    """
    total = regan["onset"].sum()
    assert 80 <= total <= 220, (
        f"COW v5.1 onset count 1946–1999 out of plausible range [80, 220]: {total}"
    )


def test_onset_count_full_period_plausible(incy):
    """1946–2014 should have more onsets than 1946–1999 alone (~190 expected)."""
    total = incy["onset"].sum()
    assert total > 100, f"Suspiciously few onsets for 1946–2014 (got {total})"
    assert total < 350, f"Suspiciously many onsets for 1946–2014 (got {total})"


# ---------------------------------------------------------------------------
# Polity / regime
# ---------------------------------------------------------------------------

def test_polity2_range(cy):
    """polity2 must lie in [-10, 10] wherever it is not missing."""
    pol = cy["polity2"].dropna()
    assert pol.between(-10, 10).all(), (
        f"polity2 out of range: min={pol.min()}, max={pol.max()}"
    )


def test_instab_binary(incy):
    """instab should be 0/1 (or float equivalent), no other values."""
    vals = incy["instab"].dropna().unique()
    assert set(vals) <= {0, 1, 0.0, 1.0}, f"Non-binary instab values: {vals}"


def test_v2x_polyarchy_range(incy):
    """v2x_polyarchy must lie in [0, 1] wherever it is not missing."""
    poly = incy["v2x_polyarchy"].dropna()
    assert poly.between(0, 1).all(), (
        f"v2x_polyarchy out of [0,1]: min={poly.min()}, max={poly.max()}"
    )


# ---------------------------------------------------------------------------
# Economic variables
# ---------------------------------------------------------------------------

def test_lgdp_plausible(incy):
    """log GDP per capita: V-Dem e_gdppc is in thousands of 2011 USD.
    Range should be roughly [-1, 6] → ~$370 (Liberia wartime) to ~$400k.
    """
    lgdp = incy["lgdp"].dropna()
    assert lgdp.min() > -2, f"lgdp min implausibly low: {lgdp.min()}"
    assert lgdp.max() < 8, f"lgdp max implausibly high: {lgdp.max()}"


def test_oil_binary(incy):
    """oil must be binary (0/1) or NA."""
    vals = set(incy["oil"].dropna().unique())
    assert vals <= {0, 1, 0.0, 1.0}, f"Non-binary oil values: {vals}"


# ---------------------------------------------------------------------------
# Ethnic exclusion (EPR)
# ---------------------------------------------------------------------------

def test_eth_excl_frac_range(incy):
    """eth_excl_frac must lie in [0, 1] wherever it is not missing."""
    excl = incy["eth_excl_frac"].dropna()
    assert excl.between(0, 1).all(), (
        f"eth_excl_frac out of [0,1]: min={excl.min()}, max={excl.max()}"
    )


def test_eth_excl_frac_has_coverage(incy):
    """eth_excl_frac should be non-null for the majority of inCY rows."""
    coverage = incy["eth_excl_frac"].notna().mean()
    assert coverage > 0.6, f"Low EPR coverage: {coverage:.1%}"


# ---------------------------------------------------------------------------
# Imputed files
# ---------------------------------------------------------------------------

def test_imputed_files_exist():
    """Five imputed parquet files must exist."""
    missing = [
        INTERIM / f"cy_imputed_{i}.parquet"
        for i in range(1, 6)
        if not (INTERIM / f"cy_imputed_{i}.parquet").exists()
    ]
    assert not missing, f"Missing imputed files: {missing}"


def test_imputed_no_missing():
    """Imputed files should have no missing values in numeric columns."""
    for i in range(1, 6):
        path = INTERIM / f"cy_imputed_{i}.parquet"
        if not path.exists():
            pytest.skip(f"cy_imputed_{i}.parquet not found")
        imp = pd.read_parquet(path)
        num = imp.select_dtypes(include=np.number)
        missing = num.isna().sum().sum()
        assert missing == 0, (
            f"cy_imputed_{i}.parquet has {missing} missing numeric values"
        )
