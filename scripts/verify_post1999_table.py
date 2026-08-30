"""Verify appendix tab:post1999 against the pipeline's own loader.

The table (paper/sections/appendix.tex, accepted version) hand-lists 32
coded interventions. The generated macro \\PostExtInterventions counts
len(load_post1999_interventions(csv)) = 31. Reconcile: raw CSV -> loader
output -> table rows, reporting every drop and mismatch.

Run: .venv/bin/python scripts/verify_post1999_table.py
"""
import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from shadow.data.interventions import load_post1999_interventions

# The 32 rows of tab:post1999 exactly as printed (host, year, intervener, side).
TABLE = [
    ("Sierra Leone", 2000, "United Kingdom", "Gov."),
    ("Ivory Coast", 2002, "France", "Gov."),
    ("Liberia", 2002, "Guinea", "Opp."),
    ("Pakistan", 2004, "United States", "Gov."),
    ("Chad", 2005, "France", "Gov."),
    ("Chad", 2005, "Sudan", "Opp."),
    ("Pakistan", 2005, "United States", "Gov."),
    ("Somalia", 2006, "Ethiopia", "Gov."),
    ("Chad", 2007, "France", "Gov."),
    ("Chad", 2007, "Sudan", "Opp."),
    ("Pakistan", 2007, "United States", "Gov."),
    ("Somalia", 2009, "Ethiopia", "Gov."),
    ("Iraq", 2010, "United States", "Gov."),
    ("Ivory Coast", 2011, "France", "Opp."),
    ("Libya", 2011, "United States", "Opp."),
    ("Libya", 2011, "United Kingdom", "Opp."),
    ("Libya", 2011, "France", "Opp."),
    ("Libya", 2011, "Qatar", "Opp."),
    ("Libya", 2011, "UAE", "Opp."),
    ("Mali", 2012, "France", "Gov."),
    ("Mali", 2012, "Chad", "Gov."),
    ("Nigeria", 2013, "Chad", "Gov."),
    ("South Sudan", 2013, "Uganda", "Gov."),
    ("Ukraine", 2014, "Russia", "Opp."),
    ("Somalia", 2014, "Ethiopia", "Gov."),
    ("Somalia", 2014, "Uganda", "Gov."),
    ("Libya", 2014, "Egypt", "Gov."),
    ("Libya", 2014, "UAE", "Gov."),
    ("Yemen", 2014, "Saudi Arabia", "Gov."),
    ("Yemen", 2014, "UAE", "Gov."),
    ("Afghanistan", 2014, "United States", "Gov."),
    ("Afghanistan", 2014, "United Kingdom", "Gov."),
]
SIDE = {1: "Gov.", 2: "Opp.", 3: "Neutral"}

# Display names in the table vs pipeline cnames.
ALIAS = {
    "United States of America": "United States",
    "United Arab Emirates": "UAE",
    "Cote D'Ivoire": "Ivory Coast", "Cote d'Ivoire": "Ivory Coast",
    "Ivory Coast": "Ivory Coast",
}

cy = pd.read_parquet(ROOT / "data/interim/country_year.parquet",
                     columns=["ccode", "cname"])
NAME = (cy.assign(ccode=cy.ccode.astype(int))
          .drop_duplicates("ccode").set_index("ccode")["cname"]
          .map(lambda s: ALIAS.get(s, s)).to_dict())

csv_path = ROOT / "data/raw/regan/post1999_interventions.csv"
raw = pd.read_csv(csv_path, comment="#")
post = load_post1999_interventions(csv_path)

print(f"raw CSV rows:      {len(raw)}")
print(f"loader output:     {len(post)}   (= \\PostExtInterventions)")
print(f"table rows:        {len(TABLE)}")

# Loader output as comparable tuples.
loaded = {
    (NAME.get(int(r.ccode_A), f"?{int(r.ccode_A)}"), int(r.year),
     NAME.get(int(r.ccode_B), f"?{int(r.ccode_B)}"), SIDE.get(int(r.target)))
    for r in post.itertuples()
}
table = set(TABLE)

print("\n-- in TABLE but not in loader output:")
for t in sorted(table - loaded):
    print("   ", t)
print("-- in loader output but not in TABLE:")
for t in sorted(loaded - table):
    print("   ", t)

# Diagnose drops raw -> loader: replay the loader's steps on the raw CSV.
from shadow.data.ccode import cc_series, fix_ccode
d = raw.copy()
d["ccode_A"] = fix_ccode(cc_series(d["host_ccode"]), d["year"])
d["ccode_B"] = fix_ccode(cc_series(d["intervener_ccode"]), d["year"])
nan_drop = d[d.ccode_A.isna() | d.ccode_B.isna()]
print(f"\n-- raw rows dropped by fix_ccode (state not in system): {len(nan_drop)}")
for r in nan_drop.itertuples():
    print(f"    host={r.host_ccode} intervener={r.intervener_ccode} "
          f"year={r.year} target={r.target}")
d = d[d.ccode_A.notna() & d.ccode_B.notna()].copy()
d["ddyear"] = (d.ccode_A.astype(str) + "_" + d.ccode_B.astype(str) + "_"
               + d.year.astype(str))
dups = d[d.duplicated("ddyear", keep=False)].sort_values("ddyear")
print(f"-- raw rows collapsed by ddyear dedup: "
      f"{len(d) - d.ddyear.nunique()}")
for r in dups.itertuples():
    print(f"    ddyear={r.ddyear} host={r.host_ccode} "
          f"intervener={r.intervener_ccode} year={r.year} target={r.target}")

ok = (table == loaded) and len(post) == len(TABLE)
print(f"\nVERDICT: {'MATCH' if ok else 'MISMATCH — reconcile above'}")
