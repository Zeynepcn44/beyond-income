# One-time setup. Run manually; analysis never installs software on its own.
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "patchwork",
              "sf", "leaflet", "htmlwidgets", "htmltools", "knitr", "rmarkdown")
needed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(needed)) install.packages(needed, repos = "https://cloud.r-project.org")
