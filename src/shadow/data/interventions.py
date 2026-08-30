"""Code the intervention outcome variable.

Replaces R script 08-addInterventions.R.

Maps Regan (2000) intervention records onto directed-dyad-year observations.
Each dyad-year with a civil war onset is coded as:
  0 = no intervention
  1 = government-biased intervention
  2 = opposition-biased intervention

Output: adds `intervention` column to interim dyad files.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pyreadstat

from shadow.data.ccode import cc_series, fix_ccode


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _apply_coding_corrections(df: pd.DataFrame) -> pd.DataFrame:
    """
    Apply per-conflict target/ccode corrections verbatim from R script
    08-addInterventions.R (lines 58–203).

    Applied to raw Regan data *before* ccode standardisation and year
    conversion, matching the R script's order of operations.

    All corrections preserve the original R inline comments, citing the
    corresponding R line numbers.
    """
    df = df.copy()

    # R line 64 — conflict 3 (Second Congolese Civil War, 1997):
    # France coded neutral but provided military support to the Cobra militia
    # (opposition-biased, same side as Angola/ccode 540).
    df.loc[(df['conflict'] == 615) & (df['thrdpar'] == 220), 'target'] = 2

    # R line 81 — conflict 15 (Permesta revolution, Indonesia, 1958):
    # USA supported the anti-communist opposition (CIA Operation Archipelago).
    df.loc[(df['conflict'] == 823) & (df['thrdpar'] == 2), 'target'] = 2

    # R line 84 — conflict 17: West Germany miscoded as 255; correct to 260.
    df.loc[(df['conflict'] == 835) & (df['thrdpar'] == 255), 'thrdpar'] = 260

    # R lines 92–99 — conflict 18 (Congo Crisis):
    # DRC (ccode 490) better represents this conflict than RoC (484).
    df.loc[df['conflict'] == 838, 'ccode'] = 490
    # In 1964 the sides effectively switched; flip Belgium's target.
    flip = (df['conflict'] == 838) & (df['yrintv'] == 64)
    df.loc[flip, 'target'] = df.loc[flip, 'target'].map({1.0: 2, 2.0: 1})

    # R line 138 — conflict 49 (Ethiopian Civil War):
    # Somalia (520) generally supported the opposition despite one gov record.
    df.loc[(df['conflict'] == 898) & (df['thrdpar'] == 520), 'target'] = 2

    # R line 150 — conflict 55 (Cambodian Civil War):
    # Vietnam (DRV/816) supported the government throughout.
    df.loc[(df['conflict'] == 908) & (df['thrdpar'] == 816), 'target'] = 1

    # R line 158 — conflict 57 (Mozambique Civil War):
    # Zimbabwe (552) consistently supported FRELIMO; miscoded after 1982.
    df.loc[(df['conflict'] == 913) & (df['thrdpar'] == 552), 'target'] = 1
    # Malawi (553) supported both sides → neutral (will be dropped as target=3).
    df.loc[(df['conflict'] == 913) & (df['thrdpar'] == 553), 'target'] = 3

    # R line 167 — conflict 59 (Zimbabwe/South Africa):
    # South Africa helped both sides → neutral (will be dropped as target=3).
    df.loc[df['conflict'] == 920, 'target'] = 3

    # R lines 173–174 — conflict 63 (Sri Lankan Civil War):
    # UK miscoded; India should be government-biased.
    df.loc[(df['conflict'] == 930) & (df['thrdpar'] == 200), 'target'] = 2
    df.loc[(df['conflict'] == 930) & (df['thrdpar'] == 750), 'target'] = 1

    # R line 191 — conflict 77 (Iranian Civil War):
    # Iraq (645) favoured the government; miscoded.
    df.loc[(df['conflict'] == 963) & (df['thrdpar'] == 645), 'target'] = 1

    return df


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def build_intervention_table(path: Path) -> pd.DataFrame:
    """
    Load and clean the Regan intervention data into a directed-dyad lookup.

    Replicates 08-addInterventions.R (lines 42–259).

    Parameters
    ----------
    path : Path
        Path to ``replication.10.26.01.dta``.

    Returns
    -------
    pd.DataFrame with columns:
        ccode_A  – host country code (3-char str)
        ccode_B  – intervening state code (3-char str)
        year     – intervention year (int)
        target   – 1 = gov-biased, 2 = opp-biased
    One row per unique (ccode_A, ccode_B, year) military intervention.
    """
    raw, _ = pyreadstat.read_dta(path)

    # R lines 43-46: select columns and keep state interveners only.
    # thrdpar codes: 1=UN, 3-11=non-state organisations; exclude these.
    # Keeps thrdpar==2 (USA, COW code 002) and thrdpar>=12 (state ccodes).
    cols = [
        'time', 'ccode', 'yrintv', 'endate', 'start', 'opposing', 'thrdpar',
        'military', 'clash', 'economic', 'target', 'conflict',
        'group1', 'group2', 'group3', 'group4',
    ]
    df = raw[cols].copy()

    # Exclude non-state thrdpar codes: {1, 3, 4, 5, 6, 7, 8, 9, 10, 11}.
    # (Stata loads numeric as float, so compare as float.)
    non_state = {float(v) for v in [1, 3, 4, 5, 6, 7, 8, 9, 10, 11]}
    df = df[~df['thrdpar'].isin(non_state) & df['thrdpar'].notna()].copy()

    # Apply per-conflict coding corrections (verbatim from R, on raw codes).
    df = _apply_coding_corrections(df)

    # R lines 209-211: drop unused columns.
    df = df.drop(columns=['opposing', 'clash', 'group1', 'group2', 'group3', 'group4'])

    # Standardise ccode and thrdpar to 3-char strings.
    df['ccode_A'] = cc_series(df['ccode'])
    df['ccode_B'] = cc_series(df['thrdpar'])

    # Convert 2-digit year to 4-digit (yrintv values are 44–99 → 1944–1999).
    df['year'] = (1900 + df['yrintv']).astype(int)

    # Apply temporal validity rules (equivalent to R's fixCY(makeCY(...))).
    df['ccode_A'] = fix_ccode(df['ccode_A'], pd.Series(df['year'], index=df.index))
    df['ccode_B'] = fix_ccode(df['ccode_B'], pd.Series(df['year'], index=df.index))

    # R line 220: build ddyear identifier for later deduplication.
    # Use 'NA' literal for missing codes (matches R string comparison behaviour).
    df['ddyear'] = (
        df['ccode_A'].fillna('NA').astype(str) + '_' +
        df['ccode_B'].fillna('NA').astype(str) + '_' +
        df['year'].astype(str)
    )
    df = df.sort_values('ddyear').reset_index(drop=True)

    # R line 222: drop neutral interventions (target == 3).
    df = df[df['target'] != 3].copy()

    # R lines 224-227: keep only ddyears with at least one military intervention.
    has_mil = df.groupby('ddyear')['military'].transform(lambda x: (x > 0).any())
    df = df[has_mil].copy()

    # R line 259: drop rows where host ccode is invalid (no state that year).
    df = df[df['ccode_A'].notna() & (df['ccode_A'] != 'NA')].copy()

    # --- Resolve target inconsistencies (R lines 238-250) ---
    # Two directed dyads had conflicting targets across multiple records;
    # these are resolved explicitly before final deduplication.

    # R line 240-242: Georgia (372) intervened by Russia (365, post-1991) →
    # opposition-biased. Note: 365 stays as 365 post-1991 (not converted to 364).
    df.loc[(df['ccode_A'] == '372') & (df['ccode_B'] == '365'), 'target'] = 2

    # R line 246-249: Liberia (450) intervened by Sierra Leone (451) →
    # government-biased (member of ECOWAS-led ULIMO force, sided with Nigeria).
    df.loc[(df['ccode_A'] == '450') & (df['ccode_B'] == '451'), 'target'] = 1

    # R lines 252-254: deduplicate — one row per (ccode_A, ccode_B, year),
    # taking first(target). After corrections above, targets are consistent.
    # ccode_A, ccode_B, year are preserved as non-key aggregation columns.
    df = (
        df.sort_values('ddyear')
        .groupby('ddyear', sort=False)
        .first()
        .reset_index()
    )

    df['target'] = df['target'].astype(int)
    df['year']   = df['year'].astype(int)

    return df[['ccode_A', 'ccode_B', 'year', 'target', 'ddyear']].reset_index(drop=True)


def load_post1999_interventions(path: Path) -> pd.DataFrame:
    """
    Load post-1999 intervention codings from CSV.

    Parameters
    ----------
    path : Path
        Path to ``post1999_interventions.csv``.

    Returns
    -------
    pd.DataFrame with same schema as :func:`build_intervention_table` output:
        ccode_A, ccode_B, year, target, ddyear
    """
    df = pd.read_csv(path, comment='#')

    df['ccode_A'] = cc_series(df['host_ccode'])
    df['ccode_B'] = cc_series(df['intervener_ccode'])
    df['year'] = df['year'].astype(int)
    df['target'] = df['target'].astype(int)

    # Apply temporal ccode validity rules
    df['ccode_A'] = fix_ccode(df['ccode_A'], pd.Series(df['year'], index=df.index))
    df['ccode_B'] = fix_ccode(df['ccode_B'], pd.Series(df['year'], index=df.index))

    # Drop rows where either ccode resolved to NaN (e.g., state not in system)
    df = df[df['ccode_A'].notna() & df['ccode_B'].notna()].copy()

    df['ddyear'] = (
        df['ccode_A'].astype(str) + '_' +
        df['ccode_B'].astype(str) + '_' +
        df['year'].astype(str)
    )

    # Deduplicate: one row per directed dyad-year
    df = df.sort_values('ddyear').drop_duplicates(subset='ddyear', keep='first')

    return df[['ccode_A', 'ccode_B', 'year', 'target', 'ddyear']].reset_index(drop=True)


def code_interventions(dd: pd.DataFrame, int_table: pd.DataFrame) -> pd.DataFrame:
    """
    Add an ``intervention`` column to a directed-dyad DataFrame.

    Matching logic (verbatim from 08-addInterventions.R, lines 266–297):
      - Directed dyad must match on (ccode_A, ccode_B).
      - Year window: intervention year V matched to onset year Y if
            V − 2 ≤ Y ≤ V + 5
        (R: ``inRange(year, int$year[i], ub=5, lb=2)`` where year is the
        DD onset year and int$year is the intervention year.)

    Column values:
      - ``NaN``  non-onset rows (onset_A != 1)
      - ``0``    onset row with no matching military intervention
      - ``1``    onset row with a government-biased intervention
      - ``2``    onset row with an opposition-biased intervention

    Parameters
    ----------
    dd : pd.DataFrame
        A single directed-dyad parquet file (output of notebook 02).
    int_table : pd.DataFrame
        Output of :func:`build_intervention_table`.

    Returns
    -------
    dd with an ``intervention`` column appended.
    """
    dd = dd.copy()
    dd['intervention'] = np.nan

    onset_mask = dd['onset_A'] == 1
    if onset_mask.sum() == 0:
        return dd

    onset = dd.loc[onset_mask, ['ddyear', 'ccode_A', 'ccode_B', 'year']].copy()

    # Cross-join onset rows with intervention table on directed dyad.
    merged = onset.merge(
        int_table[['ccode_A', 'ccode_B', 'year', 'target']].rename(
            columns={'year': 'year_int'}
        ),
        on=['ccode_A', 'ccode_B'],
        how='inner',
    )

    # Year window: V − 2 ≤ Y ≤ V + 5  (Y = onset year, V = intervention year).
    in_window = (
        (merged['year_int'] - 2 <= merged['year']) &
        (merged['year'] <= merged['year_int'] + 5)
    )
    matched = merged[in_window].copy()

    # Map target onto onset rows; default 0 (no intervention).
    if len(matched) > 0:
        # Bug 2 fix: a host with two onsets close together (e.g., Libya 2011 &
        # 2014) would otherwise have a single intervention land inside both
        # windows. Assign each intervention record (ccode_A, ccode_B, year_int)
        # to its SINGLE nearest onset, so it cannot bleed into a second onset.
        matched['dist'] = (matched['year'] - matched['year_int']).abs()
        matched = (
            matched.sort_values(['ccode_A', 'ccode_B', 'year_int', 'dist', 'year'])
            .groupby(['ccode_A', 'ccode_B', 'year_int'], sort=False)
            .first()
            .reset_index()
        )
        # Each onset row then takes the first target assigned to it.
        matched = matched.sort_values(['ddyear', 'year_int'])
        best = matched.groupby('ddyear', sort=False)['target'].first()
        intervention = onset['ddyear'].map(best).fillna(0).astype(int)
    else:
        intervention = pd.Series(0, index=onset.index)

    # Write back: onset rows get 0/1/2, non-onset rows stay NaN.
    onset_idx = onset.index
    dd.loc[onset_idx, 'intervention'] = intervention.values

    return dd
