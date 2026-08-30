"""
COW country code utilities.

Correlates of War (COW) country codes are 3-digit numeric strings
("840" = United States, "002" = Canada, etc.). These functions standardize
raw codes, form country-year identifiers, and apply the temporal validity
rules that encode when states were recognized as independent sovereign actors
in the COW system.

All public functions accept either scalar values or pandas Series.

Reference: fn-ccodeCleaner.R in the original R pipeline.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# Non-independent / colonial / dependent entities — always dropped.
# These codes appear in some source data but are never valid COW members.
# (Qatar, 694, was removed from this list on 2026-06-17: it is a valid COW
#  member that does intervene — e.g., Libya 2011 — and so must remain a
#  candidate intervener and country in the panel.)
# ---------------------------------------------------------------------------
_ALWAYS_DROP: frozenset[str] = frozenset({
    "031", "053", "054", "055", "056", "057", "058",
    "060", "080", "115", "212", "221", "223", "232",
    "331", "338", "395", "402", "403", "411", "511",
    "581", "591", "781", "835", "935", "940",
    "946", "955", "970", "983", "986", "987", "990",
})


# ---------------------------------------------------------------------------
# Scalar helpers
# ---------------------------------------------------------------------------

def make_cc(code: int | str | float | None) -> str | None:
    """
    Zero-pad a COW numeric country code to exactly 3 characters.

    Examples
    --------
    >>> make_cc(2)
    '002'
    >>> make_cc("51")
    '051'
    >>> make_cc("840")
    '840'
    >>> make_cc(None) is None
    True
    """
    if code is None or (isinstance(code, float) and np.isnan(code)):
        return None
    try:
        return f"{int(float(str(code).strip())):03d}"
    except (ValueError, TypeError):
        return None


def make_cy(ccode: int | str | float | None, year: int | str | float) -> str | None:
    """
    Form a country-year identifier string: '<ccode>_<year>'.

    Examples
    --------
    >>> make_cy(840, 1990)
    '840_1990'
    >>> make_cy(2, 1945)
    '002_1945'
    >>> make_cy(None, 1990) is None
    True
    """
    cc = make_cc(ccode)
    if cc is None:
        return None
    return f"{cc}_{int(year)}"


# ---------------------------------------------------------------------------
# Vectorized Series operations
# ---------------------------------------------------------------------------

def cc_series(codes: pd.Series) -> pd.Series:
    """Apply make_cc to every element of a Series."""
    return codes.map(make_cc)


def cy_series(ccodes: pd.Series, years: pd.Series) -> pd.Series:
    """Form country-year identifier strings from two Series."""
    cc = cc_series(ccodes)
    yr = years.astype("Int64").astype(str)
    # cat handles NA propagation: if cc is None the result is NA
    return cc.where(cc.notna()).str.cat(yr, sep="_")


# ---------------------------------------------------------------------------
# Temporal validity rules
# ---------------------------------------------------------------------------

def fix_ccode(
    ccode: pd.Series,
    year: pd.Series,
    fl_mode: bool = False,
) -> pd.Series:
    """
    Apply COW temporal validity rules to (ccode, year) Series pairs.

    Returns a corrected ccode Series. A value of ``pd.NA`` means the
    state did not exist as a COW member in that year — observations with
    NA codes should be dropped from the analysis frame.

    Parameters
    ----------
    ccode : pd.Series of str
        Raw COW country codes, already zero-padded (run cc_series first).
    year : pd.Series of int
        Corresponding calendar years.
    fl_mode : bool, default False
        Apply Fearon-Laitin–specific Germany corrections instead of the
        default COW corrections.  In FL mode:
          • 255/260/265 before 1949 → NA  (FL never includes occupied Germany)
          • 260 in 1991+ → 255            (West Germany code continues as
                                            unified Germany in the FL dataset)
        In COW mode (default):
          • 255/260/265 in 1945–1948 → 255 (recode to single occupied entity)
          • 255 or 260 in 1954 → NA        (missing Polity year)
          • 255 in 1990 → NA               (reunification transition year)

    Rules applied (in order):
      1. Always-invalid entity codes → NA
      2. Germany harmonisation (COW mode or FL mode, see above)
      3. Austria: occupied 1945–1954 → NA
      4. Syria: merged into UAR 1959–1960 → NA
      5. Japan: occupied 1946–1951 → NA
      6. Bhutan: not independent until 1971 → NA
      7. Bangladesh (771): independence-year edge case 1971 → NA
      8. Jamaica (051): not independent until 1962 → NA
      9. Oman (698): not independent until 1971 → NA
      10. Russia/USSR: 365 → 364 during Soviet era (1940–1991)
      11. Pakistan: 770 (pre-Bangladesh) → 769 (1971 and earlier)
      12. Vietnam: North Vietnam 816 → unified 818 from 1977 onward
      13. Yugoslavia: 345 → 347 (Serbia & Montenegro) from 1992
      14. Ethiopia: 530 (pre-Eritrea) → 529 from 1993; 529 in 1993 itself → NA

    Returns
    -------
    pd.Series of str | pd.NA
    """
    cc = ccode.copy().astype(object)  # object dtype supports mixed str/NA
    yr = year.astype(int)

    # 1. Always-invalid entities
    cc[cc.isin(_ALWAYS_DROP)] = pd.NA

    # 2. Germany
    german = cc.isin({"255", "260", "265"})
    if fl_mode:
        # FL dataset keeps West Germany as 260 throughout, then renames 260→255
        # after reunification; pre-1949 occupied Germany is dropped entirely.
        cc[german & (yr < 1949)] = pd.NA
        cc[(cc == "260") & (yr >= 1991)] = "255"
    else:
        # COW: harmonise 1945–1948 occupation period; mark anomalous years NA
        cc[german & yr.between(1945, 1948)] = "255"
        cc[cc.isin({"255", "260"}) & (yr == 1954)] = pd.NA
        cc[(cc == "255") & (yr == 1990)] = pd.NA

    # 3. Austria: Allied occupation ends 1955; drop 1945–1954
    cc[(cc == "305") & yr.between(1945, 1954)] = pd.NA

    # 4. Syria: merged with Egypt as UAR; rejoined COW system 1961
    cc[(cc == "652") & yr.isin([1959, 1960])] = pd.NA

    # 5. Japan: US occupation; returned to COW system 1952
    cc[(cc == "740") & yr.between(1946, 1951)] = pd.NA

    # 6. Bhutan: British-controlled foreign affairs until 1971
    cc[(cc == "760") & (yr <= 1970)] = pd.NA

    # 7. Bangladesh (771): recorded as entering in 1971 but independence
    #    declared late in the year — the 1971 observation is unstable
    cc[(cc == "771") & (yr == 1971)] = pd.NA

    # 8. Jamaica (051): independent from UK in 1962
    cc[(cc == "051") & (yr <= 1961)] = pd.NA

    # 9. Oman (698): sultanate modernised; entered COW system 1971
    cc[(cc == "698") & (yr <= 1970)] = pd.NA

    # 10. Russia/USSR: COW uses 364 for the Soviet Union
    cc[(cc == "365") & yr.between(1940, 1991)] = "364"

    # 11. Pakistan: 770 = undivided Pakistan (with East Pakistan/Bangladesh)
    #     until Bangladesh's independence in 1971
    cc[(cc == "770") & (yr <= 1971)] = "769"

    # 12. Vietnam: North Vietnam (816) → unified Vietnam (818) from 1977
    cc[(cc == "816") & (yr >= 1977)] = "818"

    # 13. Yugoslavia → Serbia & Montenegro from 1992 breakup
    cc[(cc == "345") & (yr >= 1992)] = "347"

    # 14. Ethiopia: 530 includes Eritrea; after Eritrean independence (1993)
    #     the remaining state takes code 529.  1993 itself is a transition
    #     year with no clean observation for either code.
    cc[(cc == "529") & (yr == 1993)] = pd.NA
    cc[(cc == "530") & (yr >= 1993)] = "529"

    return cc
