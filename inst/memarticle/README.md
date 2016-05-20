# A memoir-based article template for markdown

This pandoc template and Makefile makes it possible to generate an article-length document in PDF, making use of some of the layout and formatting capacities of the `memoir` package. The included Makefile allows us to govern xelatex and biber with a single `make` (relying on latexmk to determine the number of passes needed).

The included `memoir-article.latex` template adds support for a few extra metadata variables:

`anon`: Remove author information (including from PDF metadata) for anonymous submission.

`thanks`: An unnumbered note is added to the first package with the contents of this variable (which can be any LaTeX).

For font settings, the `mainfont`, `mainfontoptions`, etc. metadata variables work as in the pandoc default.

## Chicago Style

To use the marvelous `biblatex-chicago` package for marvelous *Chicago Manual of Style* bibliography generation, set:

```yaml
biblatex: true
biblatex-chicago: true
biblatexoptions: notes
bibliography: ../mybib.bib
```

Note that the bibliography path is relative to the `out` directory that is created by the Makefile. The `biblatexoptions` are given to `biblatex-chicago` (e.g. you might try `[notes,short,noibid]`).

Footnotes are also formatted according to Chicago's recommendations. To use widely spaced ellipses, specify `chicago-ellipses: true`.

