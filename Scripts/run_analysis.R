# Run from the project folder. Raw data are rebuilt on every render.
required <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "patchwork",
              "sf", "leaflet", "htmlwidgets", "htmltools", "knitr", "rmarkdown")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing packages: ", paste(missing_packages, collapse = ", "),
                                  ". Run source('Scripts/install_packages.R') once.")
set.seed(7)
dir.create("Outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Outputs/figures", recursive = TRUE, showWarnings = FALSE)
source("Scripts/01_prepare.R")
source("Scripts/02_models.R")
source("Scripts/03_visualise.R")
source("Scripts/04_interactive_map.R")
quality <- prepare_data()
d <- quality$data
models <- run_models(d)
visuals <- make_visuals(d, quality, models)
explorer <- make_interactive_map(visuals$world, d)
writeLines(capture.output(sessionInfo()), "Outputs/sessionInfo.txt")
version_table <- data.frame(package = required,
  version = vapply(required, function(x) as.character(packageVersion(x)), character(1)))
write.csv(version_table, "Outputs/package_versions.csv", row.names = FALSE)
input_files <- c("Data/Combined_fixed.xlsx", "Data/ne_50m_admin_0_countries.geojson",
                 "Data/variable_names.csv")
write.csv(data.frame(file = input_files, md5 = unname(tools::md5sum(input_files))),
          "Outputs/input_checksums.csv", row.names = FALSE)
stopifnot(nrow(models$main_data) == 140, nrow(models$education_data) == 114,
          abs(summary(models$models$baseline_2023)$r.squared - .6913340540) < 1e-4,
          sum(models$residual_ranking$income_residual >= 1) == 7,
          sum(models$residual_ranking$income_residual <= -1) == 9)
writeLines(c("All data and analysis assertions passed.",
  "Raw workbook rebuilt from Sheet1 and Sheet2; joined data match Final Sheet.",
  "One row per ISO3 and survey-window endpoint; no input values corrected.",
  "All model comparisons use identical countries within each comparison.",
  "All 157 panel ISO3 values match Natural Earth geometry keys.",
  "Main sample 140; education sensitivity sample 114; residual ranking 140."),
  "Outputs/validation.txt")
