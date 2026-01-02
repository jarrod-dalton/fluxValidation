#!/usr/bin/env Rscript

# Render package vignettes to /docs as standalone HTML files.
#
# Usage (from package root):
#   Rscript tools/render_docs.R
#
suppressPackageStartupMessages({
  library(rmarkdown)
})

pkg_root <- normalizePath(".")
vig_dir  <- file.path(pkg_root, "vignettes")
out_dir  <- file.path(pkg_root, "docs")

if (!dir.exists(vig_dir)) stop("Missing vignettes/: ", vig_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rmds <- list.files(vig_dir, pattern = "\\.Rmd$", full.names = TRUE)
if (length(rmds) == 0) stop("No .Rmd files found in ", vig_dir)

message("Rendering ", length(rmds), " vignette(s) to ", out_dir)

for (rmd in rmds) {
  out_file <- paste0(tools::file_path_sans_ext(basename(rmd)), ".html")
  message("- ", basename(rmd), " -> docs/", out_file)

  rmarkdown::render(
    input = rmd,
    output_dir = out_dir,
    output_file = out_file,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}

message("Done.")
