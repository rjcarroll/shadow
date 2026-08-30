// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}


// syntax highlighting functions from skylighting:
/* Function definitions for syntax highlighting generated by skylighting: */
#let EndLine() = raw("\n")
#let Skylighting(fill: none, number: false, start: 1, sourcelines) = {
   let blocks = []
   let lnum = start - 1
   let bgcolor = rgb("#f1f3f5")
   for ln in sourcelines {
     if number {
       lnum = lnum + 1
       blocks = blocks + box(width: if start + sourcelines.len() > 999 { 30pt } else { 24pt }, text(fill: rgb("#aaaaaa"), [ #lnum ]))
     }
     blocks = blocks + ln + EndLine()
   }
   block(fill: bgcolor, width: 100%, inset: 8pt, radius: 2pt, blocks)
}
#let AlertTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let AnnotationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let AttributeTok(s) = text(fill: rgb("#657422"),raw(s))
#let BaseNTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let BuiltInTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let CharTok(s) = text(fill: rgb("#20794d"),raw(s))
#let CommentTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let CommentVarTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ConstantTok(s) = text(fill: rgb("#8f5902"),raw(s))
#let ControlFlowTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let DataTypeTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DecValTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DocumentationTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ErrorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let ExtensionTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let FloatTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let FunctionTok(s) = text(fill: rgb("#4758ab"),raw(s))
#let ImportTok(s) = text(fill: rgb("#00769e"),raw(s))
#let InformationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let KeywordTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let NormalTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let OperatorTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let OtherTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let PreprocessorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let RegionMarkerTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let SpecialCharTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let SpecialStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let StringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let VariableTok(s) = text(fill: rgb("#111111"),raw(s))
#let VerbatimStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let WarningTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))



#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: none,
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Set document metadata for PDF accessibility
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()
  set par(
    justify: true,
    leading: linestretch * 0.65em
  )
  set text(lang: lang,
           region: region,
           size: fontsize)
  set text(font: font) if font != none
  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont) if codefont != none

  set heading(numbering: sectionnumbering)

  show link: set text(fill: rgb(content-to-string(linkcolor))) if linkcolor != none
  show ref: set text(fill: rgb(content-to-string(citecolor))) if citecolor != none
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
   }

  let has-title-block = title != none or (authors != none and authors != ()) or date != none or abstract != none
  if has-title-block {
    place(
      top,
      float: true,
      scope: "parent",
      clearance: 4mm,
      block(below: 1em, width: 100%)[

        #if title != none {
          align(center, block(inset: 2em)[
            #set par(leading: heading-line-height) if heading-line-height != none
            #set text(font: heading-family) if heading-family != none
            #set text(weight: heading-weight)
            #set text(style: heading-style) if heading-style != "normal"
            #set text(fill: heading-color) if heading-color != black

            #text(size: title-size)[#title #if thanks != none {
              footnote(thanks, numbering: "*")
              counter(footnote).update(n => n - 1)
            }]
            #(if subtitle != none {
              parbreak()
              text(size: subtitle-size)[#subtitle]
            })
          ])
        }

        #if authors != none and authors != () {
          let count = authors.len()
          let ncols = calc.min(count, 3)
          grid(
            columns: (1fr,) * ncols,
            row-gutter: 1.5em,
            ..authors.map(author =>
                align(center)[
                  #author.name \
                  #author.affiliation \
                  #author.email
                ]
            )
          )
        }

        #if date != none {
          align(center)[#block(inset: 1em)[
            #date
          ]]
        }

        #if abstract != none {
          block(inset: 2em)[
          #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
          ]
        }
      ]
    )
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  doc
}

#set table(
  inset: 6pt,
  stroke: none
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

#show: doc => article(
  title: [Measuring the Shadow of Intervention],
  subtitle: [A two-stage machine learning pipeline for latent geopolitical quantities],
  authors: (
    ( name: [Robert J. Carroll],
      affiliation: [],
      email: [] ),
    ),
  toc_title: [Table of contents],
  toc_depth: 3,
  doc,
)

= The problem
<the-problem>
Theory in international relations predicts that #emph[anticipated] third-party military intervention shapes whether civil wars start. If a rebel group expects powerful states to back the government, fighting looks less attractive; if outside support for the opposition is likely, it looks more attractive. The quantity that matters---the #strong[shadow of intervention]---is an expectation about the international environment, not a realized event.

No one has measured it. Existing approaches either use a static proxy (the US security hierarchy) or estimate a structural game restricted to the five permanent Security Council members. But P5 states account for fewer than half of all military interventions; neighbors, regional rivals, and coethnics make up the rest.

This project constructs a measure of expected intervention that covers all \~190 potential interveners, varies annually with shifting alliances and capabilities, and distinguishes government-biased from opposition-biased intervention. The measure is then used to test whether the shadow actually deters and emboldens. The resulting paper is conditionally accepted at #emph[Political Science Research and Methods].

= Pipeline architecture
<pipeline-architecture>
The pipeline has two stages, bridged by a game-theoretic equilibrium computation.

#Skylighting(([#NormalTok("Raw data (COW, V-Dem, UNGA, trade, alliances, ...)");],
[#NormalTok("    │");],
[#NormalTok("    ▼");],
[#NormalTok("Multiple imputation (miceforest, 5 CY × 5 UD = 25 datasets)");],
[#NormalTok("    │");],
[#NormalTok("    ▼");],
[#NormalTok("109 directed-dyad-year features (97 base + 12 spatial)");],
[#NormalTok("    │");],
[#NormalTok("    ▼");],
[#NormalTok("┌─────────────────────────────────────────────────┐");],
[#NormalTok("│  STAGE 1: Super-learner ensemble                │");],
[#NormalTok("│                                                 │");],
[#NormalTok("│  9 learners (RF, 2×HGB, ridge, elastic-net,     │");],
[#NormalTok("│  lasso, multinomial logit, 2×MLP) × 3 feature   │");],
[#NormalTok("│  sets = 27 candidates → 10-fold CV              │");],
[#NormalTok("│  → NNLS stacking on multinomial log-loss         │");],
[#NormalTok("│                                                 │");],
[#NormalTok("│  Spatial lags ←──── Nash fixed-point iteration   │");],
[#NormalTok("│  (predictions feed back as inputs until          │");],
[#NormalTok("│   self-consistent)                               │");],
[#NormalTok("└──────────────────────┬──────────────────────────┘");],
[#NormalTok("                       │");],
[#NormalTok("                       ▼");],
[#NormalTok("              Dyad-level P(intervention | direction)");],
[#NormalTok("                       │");],
[#NormalTok("                       ▼");],
[#NormalTok("              Aggregate to country-year shadow:");],
[#NormalTok("              E_G = Σ P(gov-biased),  E_O = Σ P(opp-biased)");],
[#NormalTok("                       │");],
[#NormalTok("                       ▼");],
[#NormalTok("┌─────────────────────────────────────────────────┐");],
[#NormalTok("│  STAGE 2: Onset logit                           │");],
[#NormalTok("│                                                 │");],
[#NormalTok("│  Does the shadow predict civil war onset?        │");],
[#NormalTok("│  T×P bootstrap (25 draws × 200 cluster reps)    │");],
[#NormalTok("│  propagates measurement uncertainty into SEs     │");],
[#NormalTok("└─────────────────────────────────────────────────┘");],));
== Why the fixed point?
<why-the-fixed-point>
Interveners don't act in isolation. When one state intervenes on the government side, rivals face pressure to back the opposition. The classifier uses spatial lags---summaries of other states' behavior---as features. But out of sample, those lags should reflect the model's own predictions, not historical observations. The #strong[Nash fixed-point iteration] requires predictions to be self-consistent inputs to themselves:

$ sigma^(*) \( B \, A \, t \) = hat(M) #scale(x: 120%, y: 120%)[\(] X \( B \, A \, t \, sigma^(*) \) #scale(x: 120%, y: 120%)[\)] quad forall #h(0em) \( B \, A \, t \) $

This is both a measurement correction (eliminating a deterministic discrepancy between lag inputs and probability outputs) and a game-theoretic equilibrium condition. Convergence is rapid---two to three passes---across all 25 imputation datasets.

= Data architecture
<data-architecture>
The unit of analysis for the classifier is a #strong[directed dyad-year]: an ordered pair (potential intervener $B$, potential host $A$, year $t$). For each of \~190 states in the international system across 69 years (1946--2014), we ask: would $B$ intervene militarily in $A$'s civil conflict, and if so, on which side?

Getting there requires assembling, aligning, and imputing data from a dozen sources at three levels of analysis.

== Three levels, one pipeline
<three-levels-one-pipeline>
#strong[Country-year] features describe each state in isolation: material capabilities (GDP, military personnel, iron/steel production from COW National Material Capabilities v6), regime type (Polity II, V-Dem liberal democracy index, instability indicators), ethnic fractionalization, oil wealth, terrain, and foreign policy positioning (UNGA ideal points). These come from separate datasets with different country coding schemes, temporal coverage, and missingness patterns.

#strong[Undirected-dyad] features describe relationships between pairs of states: alliance commitments, bilateral trade, geographic distance, contiguity, colonial history, shared IGO membership, territorial disputes, and peace quality. Each source uses its own dyad identifier format and covers a different time span.

#strong[Directed-dyad] features assign roles: state $A$ is the potential civil war host, state $B$ the potential intervener. This doubles many features (GDP of $A$ vs GDP of $B$, regime type of $A$ vs regime type of $B$) and adds asymmetric quantities like the capability ratio $B \/ A$. The final feature matrix has #strong[109 columns] per directed-dyad-year observation: 97 base features plus 12 spatial lags.

== Staged imputation
<staged-imputation>
Missing data is pervasive---early Cold War GDP figures, pre-1965 ideal points, incomplete alliance records. A complete-case analysis would discard most of the sample.

The imputation uses #strong[miceforest] (MICE with random forests), but the staging matters:

+ #strong[Country-year first.] GDP, regime type, and other state-level variables are imputed at the country-year level, producing 5 complete CY panels. This ensures that a country's GDP is the same value in every dyad involving that country---imputing at the dyad level would break this consistency.

+ #strong[Undirected-dyad second.] Bilateral variables (trade, alliance similarity, ideal-point distance) are imputed conditional on the already-imputed CY variables, producing 5 UD draws per CY draw = 25 complete datasets.

+ #strong[Directed expansion last.] The undirected dyad $\( A \, B \)$ is expanded into two directed rows $\( A arrow.r B \)$ and $\( B arrow.r A \)$, with features assigned by role. No imputation here---just reshaping.

The result is 25 complete directed-dyad-year datasets, stored as parquet throughout.

== Spatial weight matrices
<spatial-weight-matrices>
Twelve spatial-lag features capture the strategic context: what are other potential interveners doing in the same host country? These are weighted averages across all states $C eq.not B$ of their intervention behavior toward $A$, using a polity-similarity weight matrix (states with similar regime types get higher weight, reflecting ideological alignment in intervention decisions).

The weights are constructed separately for each of the 25 imputation datasets, since the polity scores feeding the similarity calculation are themselves imputed. This produces 25 distinct spatial weight matrices---each \~92 MB---that propagate imputation uncertainty through the spatial structure.

= Stage 1 results
<stage-1-results>
The ensemble achieves a #strong[proportional reduction in log-loss (PRL) of 39.6%] and a ROC-AUC of 0.958 on out-of-fold predictions, for a three-class rare-event problem where interventions account for \~0.8% of observations. Average precision tells the sharper story: AUC-PR reaches 0.32 for any intervention (0.22 government-biased, 0.25 opposition-biased) against a no-skill baseline near 0.01 --- the classifier earns its keep by finding needles, which matters, because sending all intervention probabilities to zero is exactly what a standard onset model without shadow variables already does.

#strong[Ensemble weights] (NNLS, averaged across the 25 draws) concentrate on the nonlinear learners: the random forest leads at 53%, the deep MLP takes 20%, and the multinomial logit --- the softmax best-response function assumed by structural game-theoretic models --- earns 7%. The ensemble's edge over the best purely logistic candidate is #strong[8.7 points of PRL]: the nonlinear advantage that motivates learning the proxy rather than specifying it.

The 27 candidates also span three feature sets --- base features only (X), spatial lags only (W), and both (XW) --- and the stacker spreads real weight across them (57% / 5% / 38%): the strategic-context features carry signal beyond the monadic and dyadic fundamentals.

= The shadow measure
<the-shadow-measure>
Aggregating dyad-level predictions to the country-year level produces two time-varying measures: expected government-biased intervention ($E^G$) and expected opposition-biased intervention ($E^O$).

#figure([
#box(image("../paper/figures/fig-shadow-ts.pdf", width: 95.0%))
], caption: figure.caption(
position: bottom, 
[
Shadow measure time series for five countries with well-documented intervention environments.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


South Vietnam shows the cleanest signal: both measures ramp through the 1960s to peak intervention expectations during the war's height. Afghanistan captures the regime-switching dynamic of the late 1970s. Egypt---which experiences no civil war in our data---provides a pure out-of-sample check: the shadow tracks the Six Day War, the Soviet expulsion, and the Gulf War purely from counterfactual predictions.

Against the two existing proxies, the measure correlates sensibly while covering far more of the world: $r = 0.29$ with the Lake security-hierarchy measure ($n = 162$) and $r = 0.47$ with the Gibilisco--Montero structural P5 intervention probabilities ($n = 146$) --- related, but far from redundant.

= Stage 2: Does the shadow predict onset?
<stage-2-does-the-shadow-predict-onset>
Prediction gates everything. Under leave-one-country-out cross-validation, adding the shadow pair to a standard onset baseline raises out-of-sample AUC-PR from 0.057 to 0.074; resolving the shadow by intervener type raises it to 0.143 --- two and a half times the baseline. The measure earns its place in the onset model before any coefficient is read.

Reading direction takes more care. The two channels are nearly collinear ($r approx 0.94$), so their individual coefficients swing from measurement draw to measurement draw and cannot be trusted. What the data can speak to is the pair's two axes: a #strong[common intensity] ($E^O + E^G$ --- deeper into the shadow of both kinds of intervention at once) and a #strong[net tilt] ($E^O - E^G$ --- the balance shifting toward one side). Those are precisely the coordinates of the theory's two-axis predictions, and the estimates behave accordingly: government-biased expectations deter consistently across draws, while the raw opposition channel is left unsigned by the theory itself --- a race between the better odds outside backing brings and the heavier war it portends.

== Honest uncertainty
<honest-uncertainty>
The T×P bootstrap pools measurement spread (variation across the 25 imputation draws) and sampling uncertainty into every reported interval, and measurement dominates: the binding constraint is not "do we have enough civil wars?" but "how precisely can we measure the shadow?" Wide intervals from honest propagation are the contribution --- narrow ones would pretend a learned proxy is observed data.

= Technical stack
<technical-stack>
- #strong[Language]: Python 3.14
- #strong[ML]: scikit-learn (ensemble, logistic, MLP), LightGBM (HGB)
- #strong[Imputation]: miceforest (multiple imputation by chained equations with random forests)
- #strong[Data]: pandas, pyarrow/parquet throughout
- #strong[Paper]: LaTeX
- #strong[Pipeline]: Jupyter notebooks plus a script suite; every reported number and table is generated by #NormalTok("scripts/export_numbers.py");

= Code
<code>
The full pipeline lives in nine notebooks:

#table(
  columns: (41.67%, 58.33%),
  align: (left,left,),
  table.header([Notebook], [What it does],),
  table.hline(),
  [01], [Country-year panel construction + multiple imputation],
  [02], [Directed-dyad expansion + bilateral features],
  [03], [Intervention coding (Regan 1944--1999 + post-1999 extension)],
  [04], [Spatial weight matrices (25 datasets × polity-similarity weights)],
  [05], [Stage 1 training (super-learner, 10-fold CV, Nash fixed-point)],
  [06], [Stage 1 predictions (universal application + aggregation)],
  [07], [Stage 2 onset analysis (logit, specifications, T×P bootstrap)],
  [09], [Exploratory figures],
)
Source code for data loading, feature engineering, and model utilities is in #NormalTok("src/shadow/");.

Paper exhibits are generated by scripts, not notebooks: #NormalTok("scripts/export_numbers.py"); writes every reported statistic as a LaTeX macro and every table as a generated file, and #NormalTok("scripts/fig_shadow_ts.py"); and #NormalTok("scripts/fig_appendix.py"); produce the figures. The manuscript #NormalTok("\\input");s these, so the page cannot diverge from the pipeline --- verified against the accepted version by byte-identical regeneration and a seeded 25-item random audit of reported values (25/25). The extended analyses behind the paper's Section 3 (out-of-sample gates, subsumption, direction and channels, drop-column importance, predictive-significance tests) also live in #NormalTok("scripts/");\; the replication package design is in #NormalTok("REPLICATION.md");.
