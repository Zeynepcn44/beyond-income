# Render the same canonical Quarto content through R Markdown's HTML backend.
# This avoids Quarto's user-profile Sass cache on restricted computers.
project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
stopifnot(file.exists("report.qmd"))
if (!rmarkdown::pandoc_available()) {
  quarto_path <- Sys.which("quarto")
  if (nzchar(quarto_path)) {
    possible_pandoc <- file.path(dirname(quarto_path), "tools")
    if (dir.exists(possible_pandoc)) Sys.setenv(RSTUDIO_PANDOC = possible_pandoc)
  }
}
stopifnot(rmarkdown::pandoc_available())
html_source <- readLines("report.qmd", warn = FALSE, encoding = "UTF-8")
html_source <- gsub("{{< pagebreak >}}", '<div class="pagebreak"></div>', html_source, fixed = TRUE)
intermediate <- file.path(project_dir, ".report_html_build.Rmd")
writeLines(html_source, intermediate, useBytes = TRUE)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, out.width = "100%")
rmarkdown::render(intermediate,
  output_format = rmarkdown::html_document(theme = NULL, highlight = NULL, mathjax = NULL,
    toc = FALSE, self_contained = TRUE, css = file.path(project_dir, "report.css")),
  output_file = "report.html", output_dir = project_dir,
  knit_root_dir = project_dir, clean = TRUE, quiet = TRUE,
  envir = new.env(parent = globalenv()))
unlink(intermediate) # Only this script's known temporary derived source.
