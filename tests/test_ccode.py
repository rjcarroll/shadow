"""Tests for COW country code utilities (src/shadow/data/ccode.py)."""

import numpy as np
import pandas as pd
import pytest

from shadow.data.ccode import (
    _ALWAYS_DROP,
    cc_series,
    cy_series,
    fix_ccode,
    make_cc,
    make_cy,
)


# ---------------------------------------------------------------------------
# make_cc
# ---------------------------------------------------------------------------

class TestMakeCC:
    def test_pads_single_digit_int(self):
        assert make_cc(2) == "002"

    def test_pads_two_digit_int(self):
        assert make_cc(51) == "051"

    def test_three_digit_int_unchanged(self):
        assert make_cc(840) == "840"

    def test_string_input(self):
        assert make_cc("51") == "051"

    def test_already_padded_string(self):
        assert make_cc("840") == "840"

    def test_float_input(self):
        assert make_cc(840.0) == "840"

    def test_none_returns_none(self):
        assert make_cc(None) is None

    def test_nan_returns_none(self):
        assert make_cc(float("nan")) is None


# ---------------------------------------------------------------------------
# make_cy
# ---------------------------------------------------------------------------

class TestMakeCY:
    def test_basic(self):
        assert make_cy(840, 1990) == "840_1990"

    def test_pads_ccode(self):
        assert make_cy(2, 1945) == "002_1945"

    def test_none_ccode_returns_none(self):
        assert make_cy(None, 1990) is None

    def test_string_inputs(self):
        assert make_cy("840", "1990") == "840_1990"


# ---------------------------------------------------------------------------
# cc_series / cy_series
# ---------------------------------------------------------------------------

class TestSeriesFunctions:
    def test_cc_series_pads(self):
        s = pd.Series([2, 51, 840])
        result = cc_series(s)
        assert list(result) == ["002", "051", "840"]

    def test_cc_series_propagates_na(self):
        s = pd.Series([840, None, 2])
        result = cc_series(s)
        assert result[0] == "840"
        assert pd.isna(result[1])
        assert result[2] == "002"

    def test_cy_series_basic(self):
        cc = pd.Series([840, 2])
        yr = pd.Series([1990, 1945])
        result = cy_series(cc, yr)
        assert list(result) == ["840_1990", "002_1945"]


# ---------------------------------------------------------------------------
# fix_ccode — always-drop codes
# ---------------------------------------------------------------------------

class TestFixCcodeDrops:
    def _fix(self, ccode_list, year_list):
        cc = cc_series(pd.Series(ccode_list))
        yr = pd.Series(year_list)
        return fix_ccode(cc, yr)

    def test_always_drop_returns_na(self):
        result = self._fix(["031"], [1975])
        assert pd.isna(result.iloc[0])

    def test_valid_code_passes_through(self):
        result = self._fix([840], [1990])
        assert result.iloc[0] == "840"

    def test_all_drop_codes_are_dropped(self):
        for code in _ALWAYS_DROP:
            result = self._fix([code], [1975])
            assert pd.isna(result.iloc[0]), f"Expected {code} to be dropped"


# ---------------------------------------------------------------------------
# fix_ccode — Germany
# ---------------------------------------------------------------------------

class TestFixCcodeGermany:
    def _fix(self, ccode, year):
        cc = cc_series(pd.Series([ccode]))
        yr = pd.Series([year])
        return fix_ccode(cc, yr).iloc[0]

    def test_west_germany_1945_1948_recoded_to_255(self):
        assert self._fix(260, 1946) == "255"

    def test_east_germany_1945_1948_recoded_to_255(self):
        assert self._fix(265, 1947) == "255"

    def test_west_germany_1949_valid(self):
        assert self._fix(260, 1949) == "260"

    def test_west_germany_1954_is_na(self):
        assert pd.isna(self._fix(260, 1954))

    def test_unified_germany_1990_is_na(self):
        assert pd.isna(self._fix(255, 1990))

    def test_unified_germany_1991_valid(self):
        assert self._fix(255, 1991) == "255"


# ---------------------------------------------------------------------------
# fix_ccode — other temporal rules
# ---------------------------------------------------------------------------

class TestFixCcodeTemporalRules:
    def _fix(self, ccode, year):
        cc = cc_series(pd.Series([ccode]))
        yr = pd.Series([year])
        return fix_ccode(cc, yr).iloc[0]

    def test_austria_occupied_is_na(self):
        assert pd.isna(self._fix(305, 1950))

    def test_austria_after_occupation_valid(self):
        assert self._fix(305, 1955) == "305"

    def test_syria_uar_years_na(self):
        assert pd.isna(self._fix(652, 1959))
        assert pd.isna(self._fix(652, 1960))

    def test_syria_before_and_after_uar_valid(self):
        assert self._fix(652, 1958) == "652"
        assert self._fix(652, 1961) == "652"

    def test_japan_occupied_is_na(self):
        assert pd.isna(self._fix(740, 1948))

    def test_japan_after_occupation_valid(self):
        assert self._fix(740, 1952) == "740"

    def test_bhutan_before_1971_is_na(self):
        assert pd.isna(self._fix(760, 1965))

    def test_bhutan_1971_valid(self):
        assert self._fix(760, 1971) == "760"

    def test_jamaica_before_independence_is_na(self):
        assert pd.isna(self._fix(51, 1961))

    def test_jamaica_after_independence_valid(self):
        assert self._fix(51, 1962) == "051"

    def test_oman_before_1971_is_na(self):
        assert pd.isna(self._fix(698, 1970))

    def test_oman_1971_valid(self):
        assert self._fix(698, 1971) == "698"

    def test_ussr_365_recoded_to_364(self):
        assert self._fix(365, 1975) == "364"

    def test_russia_365_valid_after_1991(self):
        assert self._fix(365, 1992) == "365"

    def test_pakistan_770_recoded_to_769_before_1972(self):
        assert self._fix(770, 1965) == "769"

    def test_pakistan_770_valid_after_1971(self):
        assert self._fix(770, 1972) == "770"

    def test_north_vietnam_816_recoded_to_818_from_1977(self):
        assert self._fix(816, 1980) == "818"

    def test_north_vietnam_816_valid_before_1977(self):
        assert self._fix(816, 1975) == "816"

    def test_yugoslavia_345_recoded_to_347_from_1992(self):
        assert self._fix(345, 1995) == "347"

    def test_yugoslavia_345_valid_before_1992(self):
        assert self._fix(345, 1991) == "345"

    def test_ethiopia_529_in_1993_is_na(self):
        assert pd.isna(self._fix(529, 1993))

    def test_ethiopia_530_from_1993_recoded_to_529(self):
        assert self._fix(530, 1994) == "529"

    def test_ethiopia_530_before_1993_valid(self):
        assert self._fix(530, 1992) == "530"

    def test_us_always_valid(self):
        for year in range(1945, 2000):
            assert self._fix(840, year) == "840"


# ---------------------------------------------------------------------------
# fix_ccode FL mode — Germany
# ---------------------------------------------------------------------------

class TestFixCcodeFLMode:
    """FL mode uses different Germany rules (type='FL' in fn-ccodeCleaner.R)."""

    def _fix(self, code, yr, **kw):
        s = fix_ccode(cc_series(pd.Series([code])),
                      pd.Series([yr]), fl_mode=True)
        return s.iloc[0]

    def test_fl_germany_before_1949_is_na(self):
        assert pd.isna(self._fix(260, 1945))
        assert pd.isna(self._fix(260, 1948))

    def test_fl_germany_1949_valid(self):
        assert self._fix(260, 1949) == "260"

    def test_fl_germany_1954_valid(self):
        # FL mode does NOT drop 260_1954 (unlike COW mode)
        assert self._fix(260, 1954) == "260"

    def test_fl_germany_1990_valid(self):
        # FL mode does NOT drop 255_1990 (unlike COW mode)
        assert self._fix(255, 1990) == "255"

    def test_fl_west_germany_1991_recoded_to_255(self):
        assert self._fix(260, 1991) == "255"

    def test_fl_west_germany_1999_recoded_to_255(self):
        assert self._fix(260, 1999) == "255"
