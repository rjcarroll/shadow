# Next Session TODO

## Setup (mac mini M1)

- Python 3.14, venv at `.venv/`, `pip install -e ".[dev]"` done
- `brew install libomp` done (required by LightGBM/miceforest)
- `brew install typst` done; compile from `paper/` with `typst compile shadow.typ`
- Kernel fix applied: `.venv/share/jupyter/kernels/python3/kernel.json` uses full venv path
- Tresorit filters configured: `.tresorit/Filters/roaming.filter` excludes `.venv/`, `data/`, `results/`, etc.

---

## Pipeline status

Notebooks 01–04 ✓ completed and committed.

### Notebook 05 — Stage 1 ensemble training (IN PROGRESS at commit time)

**Was running as background process when committed** — check whether it finished:

```bash
ls data/interim/sl_model_*.pkl | wc -l   # should be 25
ls data/interim/sl_cv_metrics.parquet
```

If not done, re-run:

```bash
.venv/bin/jupyter-nbconvert --to notebook --execute --inplace \
  --ExecutePreprocessor.timeout=7200 \
  --ExecutePreprocessor.kernel_name=python3 \
  notebooks/05-stage1-training.ipynb 2>&1
```

**What nb05 does**: Nash fixed-point training loop — for each of 25 imputation combinations
(5 CY × 5 UD), trains RF + elastic-net + multinomial logit + MLP, stacks with NNLS
super-learner, iterates spatial-lag updates until convergence (typically 2–3 passes).

**Outputs**: `sl_oof_{cy}_{ud}.parquet`, `sl_weights_{cy}_{ud}.parquet`,
`sl_model_{cy}_{ud}.pkl`, `sl_cv_metrics.parquet`

**Validation**: look for "✓ converged" in output; check PRL > 0 for super_learner rows in
`sl_cv_metrics.parquet`; check ensemble weights sum to ~1.

---

### Notebook 06 — Stage 1 predictions (PENDING nb05)

```bash
.venv/bin/jupyter-nbconvert --to notebook --execute --inplace \
  --ExecutePreprocessor.timeout=7200 \
  --ExecutePreprocessor.kernel_name=python3 \
  notebooks/06-stage1-predictions.ipynb 2>&1
```

**What it does**: Nash universal fixed-point prediction — predicts for ALL directed
dyad-years (onset and non-onset), iterates spatial-lag updates until convergence.
Overwrites onset rows with OOF predictions. Aggregates to country-year shadow measure.

**Outputs**: `shadow_{cy}_{ud}.parquet` (25 files), `shadow_avg.parquet`

---

### Notebook 07 — Stage 2 onset logits (PENDING nb06)

```bash
.venv/bin/jupyter-nbconvert --to notebook --execute --inplace \
  --ExecutePreprocessor.timeout=3600 \
  --ExecutePreprocessor.kernel_name=python3 \
  notebooks/07-stage2-onset.ipynb 2>&1
```

**What it does**: F&L baseline logit + Entrants model augmented with shadow measure;
in/out-of-sample fit comparison; Vuong test.

---

## Paper: sections with empirical TODOs

Once nb05–07 complete, fill in:

- `decision.typ` — fit-comparison table, annual performance plot, variable importance,
  cutpoint selection result
- `conclusion.typ` — key empirical findings (shadow sign, magnitude, Cold War heterogeneity)
- `shadow.typ` abstract — update with final numbers
- `constructing.typ` — actual fixed-point iteration counts, actual PRL numbers

**Also**: verify and correct `@gibilisco2021` BibTeX entry in `references.bib` — currently
a placeholder with dummy title/journal. Check Zotero for the actual Gibilisco & Monteiro
structural intervention paper.

---

## Paper: intellectual state (important for framing edits)

The paper was substantially reframed this session. Key themes to preserve in all future
editing:

**1. Measurement paper, not theory-test paper.**
Theory (Cetinyan 2002, Cunningham 2016) establishes that intervention expectations matter;
the paper's contribution is building a faithful measure of them. The model is a faithfulness
discipline, not a theoretical claim.

**2. Fixed-point has dual justification — statistical first, game-theoretic second.**
(a) *Statistical*: without it, spatial lag inputs are inconsistent with predicted outputs,
creating deterministic bias of unknown sign and magnitude — especially dangerous for
counterfactual pre-conflict years where the out-of-sample world may differ substantially
from the training-period equilibrium.
(b) *Game-theoretic*: the self-consistency condition is the Nash equilibrium condition for
the equivalence class of games that share the same best-response correspondence.
The two justifications coincide — that's the argument.

**3. Best-response correspondence is the primitive, not the game.**
M estimates BR* directly from data. Payoff functions that generate BR* are unidentified and
unneeded. Agnosticism is over payoff representations, not over games. Games are partitioned
into equivalence classes under "generates the same BR correspondence"; σ* is Nash for every
game in the class M estimates.

**4. Ensemble as implicit specification test of the parametric game family.**
MNL component = parametric anchor (linear-utility/softmax game, the writable-down formal
model). RF/MLP/elastic-net = non-parametric exploration of departures from it. NNLS weight
on MNL ≈ how well the parametric game approximates the true intervention game. Worth a
sentence in Stage 1 results once weights are available.

**5. "I argue that" language removed.**
Abstract and intro use "theory predicts" / "the literature establishes." Model is framed
as a faithfulness discipline. Three contributions reordered: measure first, model second
(as discipline), test third (as validation).

---

## What was done this session (March 2026)

- **Notebooks 03 + 04** re-run and confirmed ✓
- **Notebook 05** re-running with Nash fixed-point code (previous run was pre-Nash, killed)
- **Nash fixed-point** implemented in `spatial.py` (`update_spatial_lags_proba`,
  `_build_W_cache`, `_spat_for_onset_proba`); integrated into nb05 training loop
  (onset-only, model refit each iter) and nb06 prediction loop (all rows, model fixed)
- **Bug fixes**: `multi_class` removed from LogisticRegression (sklearn 1.5); Arrow dtype
  fix in `get_feature_cols`; Vuong test refitted on common non-missing index;
  `sklearn.base.clone` replacing manual instantiation
- **Paper reframed**: abstract, introduction, constructing section revised per above themes;
  fixed-point paragraph restructured (statistical bias argument → self-consistency fix →
  BR correspondence / equivalence class → MNL parametric fiber in footnote)
- **`@gibilisco2021`** placeholder added to references.bib (needs verification)
- **PDF** compiles cleanly

---

## Project context

- Repo: https://github.com/rjcarroll/shadow (private)
- Working dir: `/Users/rjc/portfolio/shadow`
- Install: `pip install -e ".[dev]"` (already done — activate `.venv`)
- Data: `data/` is gitignored; raw files must already be present on the machine
- Memory file: `.claude/projects/…/memory/MEMORY.md` (auto-loaded by Claude Code)
