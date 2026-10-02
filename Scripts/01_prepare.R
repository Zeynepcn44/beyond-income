# Import the supplied sources, reconstruct their join and fail on silent changes.
prepare_data <- function(data_dir = "Data", table_dir = "Outputs/tables") {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Install readxl; see README.md.")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  book <- file.path(data_dir, "Combined_fixed.xlsx")
  mapping <- read.csv(file.path(data_dir, "variable_names.csv"), check.names = FALSE)
  stopifnot(!anyDuplicated(mapping$original), !anyDuplicated(mapping$short))
  rename_checked <- function(x) {
    stopifnot(all(names(x) %in% mapping$original))
    names(x) <- mapping$short[match(names(x), mapping$original)]
    text_cols <- c("country", "country_wb", "iso3", "continent")
    for (nm in setdiff(names(x), text_cols)) {
      before <- x[[nm]]
      after <- suppressWarnings(as.numeric(before))
      if (any(!is.na(before) & is.na(after))) stop("Non-numeric input in ", nm)
      x[[nm]] <- after
    }
    x
  }
  sheet_names <- readxl::excel_sheets(book)
  stopifnot(setequal(sheet_names, c("Final Sheet", "Sheet1", "Sheet2")))
  raw <- setNames(lapply(sheet_names, function(s) as.data.frame(readxl::read_excel(
    book, sheet = s, na = c("", "NA", "N/A", ".."), col_types = "text",
    .name_repair = "minimal"), check.names = FALSE)), sheet_names)
  sheet_profile <- data.frame(sheet = sheet_names,
    rows = sapply(raw, nrow), columns = sapply(raw, ncol), row.names = NULL)
  final <- rename_checked(raw[["Final Sheet"]])
  whr <- rename_checked(raw[["Sheet1"]])
  wdi <- rename_checked(raw[["Sheet2"]])
  key <- function(z) paste(z$iso3, z$year, sep = "_")
  stopifnot(!anyDuplicated(key(final)), !anyDuplicated(key(whr)),
            !anyDuplicated(key(wdi)), !anyNA(final[c("iso3", "year")]))
  whr <- whr[whr$year %in% 2016:2023, ]
  wdi <- wdi[wdi$year %in% 2016:2023, ]
  unmatched_whr <- whr[!key(whr) %in% key(wdi), c("iso3", "year", "country")]
  unmatched_wdi <- wdi[!key(wdi) %in% key(whr), c("iso3", "year", "country_wb")]

  # Required merge: one country and one window-ending year per source row.
  joined <- merge(whr, wdi, by = c("iso3", "year"), all = FALSE, sort = FALSE)
  # Geography metadata are supplied by the partner; their original source is unrecorded.
  geography <- unique(final[c("iso3", "continent", "longitude", "latitude")])
  stopifnot(!anyDuplicated(geography$iso3))
  d <- merge(joined, geography, by = "iso3", all.x = TRUE, sort = FALSE)
  d <- d[order(d$iso3, d$year), names(final)]
  expected <- final[order(final$iso3, final$year), ]
  rownames(d) <- rownames(expected) <- NULL
  stopifnot(isTRUE(all.equal(d, expected, tolerance = 1e-10, check.attributes = FALSE)))
  stopifnot(nrow(d) == 1173, ncol(d) == 23, length(unique(d$iso3)) == 157,
            all(d$life_sat >= 0 & d$life_sat <= 10),
            all(d$life_exp > 0 & d$life_exp <= 120),
            all(d$urban_pct >= 0 & d$urban_pct <= 100),
            all(d$gdp_pc[!is.na(d$gdp_pc)] > 0),
            all(d$health_exp[!is.na(d$health_exp)] > 0))
  has_ci <- complete.cases(d[c("ci_lower", "ci_upper")])
  stopifnot(all(d$ci_lower[has_ci] <= d$life_sat[has_ci]),
            all(d$ci_upper[has_ci] >= d$life_sat[has_ci]))
  original_names <- names(d)
  d$log_gdp <- log(d$gdp_pc)
  d$log_health <- log(d$health_exp)
  complete_panel <- names(which(table(d$iso3) == 8))
  endpoint_panel <- intersect(d$iso3[d$year == 2016], d$iso3[d$year == 2023])
  d$balanced_panel <- d$iso3 %in% complete_panel
  d$endpoint_panel <- d$iso3 %in% endpoint_panel
  profile <- data.frame(variable = original_names,
    original = mapping$original[match(original_names, mapping$short)],
    type = vapply(d[original_names], function(x) class(x)[1], character(1)),
    missing = sapply(d[original_names], function(x) sum(is.na(x))),
    missing_pct = sapply(d[original_names], function(x) 100 * mean(is.na(x))),
    row.names = NULL)
  vars <- c("life_sat", "gdp_pc", "life_exp", "health_exp", "educ_exp", "urban_pct")
  missing_year <- do.call(rbind, lapply(2016:2023, function(y) {
    s <- d[d$year == y, ]
    data.frame(year = y, variable = vars, n = nrow(s),
      available = sapply(s[vars], function(x) sum(!is.na(x))),
      missing_pct = sapply(s[vars], function(x) 100 * mean(is.na(x))), row.names = NULL)
  }))
  # Flags invite source review. No flagged value is deleted or overwritten.
  d$life_exp_change <- ave(d$life_exp, d$iso3, FUN = function(x) c(NA, diff(x)))
  year_gap <- ave(d$year, d$iso3, FUN = function(x) c(NA, diff(x)))
  d$life_exp_change[is.na(year_gap) | year_gap != 1] <- NA_real_
  flags <- d[(d$life_exp < 40 | (!is.na(d$life_exp_change) & abs(d$life_exp_change) > 5) |
    (!is.na(d$educ_exp) & d$educ_exp < 0.001)),
    c("iso3", "country", "year", "life_exp", "life_exp_change", "educ_exp")]
  components <- grep("^ex_", names(d), value = TRUE)
  cc <- complete.cases(d[components])
  decomposition <- data.frame(complete_rows = sum(cc),
    max_absolute_difference = max(abs(d$life_sat[cc] - rowSums(d[cc, components]))))
  coverage <- data.frame(year = 2016:2023, countries = as.integer(table(d$year)),
                        balanced_countries = length(complete_panel))
  write_table <- function(x, name) write.csv(x, file.path(table_dir, name), row.names = FALSE)
  write_table(sheet_profile, "workbook_sheets.csv")
  write_table(profile, "column_profile.csv")
  write_table(missing_year, "missingness_by_year.csv")
  write_table(coverage, "year_coverage.csv")
  write_table(unmatched_whr, "unmatched_whr.csv")
  write_table(unmatched_wdi, "unmatched_wdi.csv")
  write_table(flags, "source_quality_flags.csv")
  write_table(decomposition, "whr_decomposition_check.csv")
  write_table(d, "analysis_data.csv")
  saveRDS(d, file.path(dirname(table_dir), "analysis_data.rds"))
  list(data = d, sheets = sheet_profile, profile = profile,
       missing = missing_year, coverage = coverage, flags = flags,
       unmatched_whr = unmatched_whr, unmatched_wdi = unmatched_wdi,
       decomposition = decomposition, balanced_n = length(complete_panel),
       endpoint_n = length(endpoint_panel))
}
