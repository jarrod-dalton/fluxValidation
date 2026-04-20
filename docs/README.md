# Docs

This folder is intended to contain rendered HTML copies of the package vignettes.

To render locally (from the package root):

```r
Rscript tools/render_docs.R
```

That will compile `vignettes/*.Rmd` into `docs/*.html` using the lightweight CSS in `docs/fluxDocs.css`.
