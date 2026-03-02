// ============================================================
// Paper style template
// ============================================================

#let paper(title: [], author: [], abstract: [], body) = {

  set document(title: title, author: author.text)

  set page(
    paper:        "us-letter",
    margin:       (x: 1in, y: 1in),
    numbering:    "1",
    number-align: center,
  )

  set text(
    font:   "Linux Libertine",
    size:   12pt,
    lang:   "en",
  )

  set par(
    justify:      true,
    leading:      1em,       // ~double-space
    first-line-indent: 1.5em,
  )

  set heading(numbering: "1.")
  set math.equation(numbering: "(1)")

  // Section headings
  show heading.where(level: 1): it => block(
    above: 2em, below: 1em,
    text(size: 13pt, weight: "bold", it),
  )
  show heading.where(level: 2): it => block(
    above: 1.5em, below: 0.75em,
    text(size: 12pt, weight: "bold", style: "italic", it),
  )

  // ---- title block ----
  align(center)[
    #v(1in)
    #text(size: 16pt, weight: "bold", title)
    #v(1em)
    #text(size: 12pt, author)
    #v(0.5em)
    #text(size: 10pt, datetime.today().display("[month repr:long] [year]"))
    #v(1.5em)
  ]

  // ---- abstract ----
  block(
    width:   85%,
    inset:   (x: 0pt, y: 0pt),
  )[
    #align(center)[*Abstract*]
    #v(0.5em)
    #set par(first-line-indent: 0pt)
    #abstract
  ]

  v(2em)
  line(length: 100%, stroke: 0.5pt)
  v(2em)

  body
}

// ---- table helper (booktabs-style) ----
#let booktab(columns: (), headers: (), rows: ()) = {
  set text(size: 10pt)
  table(
    columns:      columns,
    stroke:       none,
    inset:        (x: 6pt, y: 4pt),
    // top rule
    table.hline(y: 0, stroke: 1.5pt),
    // header row
    ..headers.map(h => table.cell(align: center, text(weight: "bold", h))),
    // mid rule
    table.hline(y: 1, stroke: 0.75pt),
    // data rows
    ..rows.flatten(),
    // bottom rule
    table.hline(stroke: 1.5pt),
  )
}
