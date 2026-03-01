# Data Sources

Raw data files live in `data/raw/` and are **not tracked in git**.
Download each source and place the file at the path shown.

---

## Civil War Onset

### UCDP/PRIO Armed Conflict Dataset (v23+) — *primary onset measure*
- **Coverage:** 1946–2023
- **Download:** https://ucdp.uu.se/downloads/
- **File:** `data/raw/ucdp/UcdpPrioConflict_v23_1.csv`
- **Notes:** Replaces Fearon & Laitin (2003) `repdata.dta` for post-1999 coverage.
  F&L data kept at `data/raw/fl/repdata.dta` for replication of baseline models.

---

## Intervention

### Regan (2000) — *primary intervention data, 1945–1999*
- **Coverage:** 1944–1999
- **File:** `data/raw/regan/replication.10.26.01.dta`
- **Notes:** 118 biased military interventions across 52 civil wars.

### Post-1999 intervention data — *open research question*
Candidates (to be decided):
- **UCDP External Support Dataset** (Högbladh et al.) — https://ucdp.uu.se/downloads/
- **ACLED** (Armed Conflict Location and Event Data) — https://acleddata.com/

---

## State Characteristics

### Polity5
- **Coverage:** 1800–2018
- **Download:** https://www.systemicpeace.org/polityproject.html
- **File:** `data/raw/polity/p5v2018.csv`

### COW National Material Capabilities (v6.0)
- **Coverage:** 1816–2016
- **Download:** https://correlatesofwar.org/data-sets/national-material-capabilities/
- **File:** `data/raw/nmc/NMC-60-abridged.csv`

### COW Interstate Wars (v4.0)
- **File:** `data/raw/cow/Inter-StateWarData_v4.0.csv`

### COW Militarized Interstate Disputes (v4.01)
- **File:** `data/raw/cow/MIDB_4.01.csv`

### COW Intrastate Wars (v4.1)
- **File:** `data/raw/cow/Intra-StateWarData_v4.1.csv`

### COW National Trade (v4.0)
- **File:** `data/raw/cow/national_trade_4.0.csv`

### COW Major Power Status (2011)
- **File:** `data/raw/cow/majors2011.csv`

---

## Dyadic Variables

### Alliance Treaty Obligations and Provisions (ATOP v5.0)
- **Download:** http://www.atopdata.org/
- **File:** `data/raw/atop/atop5_0dy.csv`

### COW Contiguity (v3.2)
- **Download:** https://correlatesofwar.org/data-sets/contiguity/
- **File:** `data/raw/cow/contdird.csv`

### COW Bilateral Trade (v4.0)
- **Download:** https://correlatesofwar.org/data-sets/bilateral-trade/
- **File:** `data/raw/cow/Dyadic_COW_4.0.csv`

### Dispute Outcome Expectations / DOE Scores (Carroll & Kenkel 2018)
- **Download:** https://bkenkel.com/data/doe.html
- **File:** `data/raw/doe/doe-scores.csv`

### Crisis-Density Rivalries (Hewitt 2005)
- **File:** `data/raw/rivalries/rivalries.csv`

### ICOW Territorial Claims
- **Download:** https://icow.la.utexas.edu/
- **File:** `data/raw/icow/ICOW_v11.csv`

---

## Ethnic / Linguistic / Religious

### Ellingsen (2000) / "witches brew"
- **File:** `data/raw/ethnic/witchesbrew1945-2002.dta`

---

## Replication Data (kept for baseline comparison)

| File | Source |
|------|--------|
| `data/raw/fl/repdata.dta` | Fearon & Laitin (2003) — original F&L replication data |
| `data/raw/cunningham/cunningham.dta` | Cunningham (2016) — US security hierarchy variable |
