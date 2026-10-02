# Simple, transparent country-level models. This file uses base R only.
# The main analysis is one cross-section, avoiding pooled repeated-country SEs.
# Call: model_results <- run_models(d, "Outputs/tables")

run_models <- function(d, table_dir = "Outputs/tables") {
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  required <- c("year", "iso3", "country", "life_sat", "gdp_pc", "log_gdp",
                "life_exp", "urban_pct", "health_exp", "log_health", "educ_exp")
  stopifnot(all(required %in% names(d)))
  d <- as.data.frame(d)
  stopifnot(!anyDuplicated(d[c("iso3", "year")]))

  save_table <- function(x, filename) {
    write.csv(x, file.path(table_dir, paste0(filename, ".csv")),
              row.names = FALSE, na = "")
    invisible(x)
  }
  sample_for <- function(year_value, variables) {
    z <- d[d$year == year_value, , drop = FALSE]
    z <- z[complete.cases(z[variables]), , drop = FALSE]
    z <- z[order(z$iso3), , drop = FALSE]
    rownames(z) <- NULL
    stopifnot(nrow(z) > length(variables), !anyDuplicated(z$iso3))
    z
  }

  # HC3 uses squared residuals adjusted for leverage. It changes uncertainty,
  # not the OLS coefficient estimates; it is a sensitivity check, not a new model.
  vcov_hc3 <- function(model) {
    X <- model.matrix(model)
    stopifnot(qr(X)$rank == ncol(X), all(hatvalues(model) < 1))
    bread <- solve(crossprod(X))
    adjusted_residual <- residuals(model) / (1 - hatvalues(model))
    meat <- crossprod(X * as.numeric(adjusted_residual))
    bread %*% meat %*% bread
  }
  coefficient_table <- function(model, model_name) {
    beta <- coef(model)
    ordinary_se <- sqrt(diag(vcov(model)))
    robust_se <- sqrt(diag(vcov_hc3(model)))
    critical <- qt(.975, df.residual(model))
    mf <- model.frame(model)
    standardized <- rep(NA_real_, length(beta))
    names(standardized) <- names(beta)
    for (term in setdiff(names(beta), "(Intercept)")) {
      standardized[term] <- beta[term] * sd(mf[[term]]) / sd(model.response(mf))
    }
    data.frame(
      model = model_name, term = names(beta), estimate = unname(beta),
      std_estimate = unname(standardized),
      ordinary_se = unname(ordinary_se),
      ordinary_ci_low = unname(beta - critical * ordinary_se),
      ordinary_ci_high = unname(beta + critical * ordinary_se),
      ordinary_p = unname(2 * pt(-abs(beta / ordinary_se), df.residual(model))),
      hc3_se = unname(robust_se),
      hc3_ci_low = unname(beta - critical * robust_se),
      hc3_ci_high = unname(beta + critical * robust_se),
      hc3_p = unname(2 * pt(-abs(beta / robust_se), df.residual(model))),
      df_residual = df.residual(model), row.names = NULL
    )
  }
  performance <- function(model, model_name) {
    s <- summary(model)
    e <- residuals(model)
    # PRESS identity gives exact leave-one-country-out prediction errors for OLS.
    loo_errors <- e / (1 - hatvalues(model))
    data.frame(model = model_name, n = nobs(model),
               predictors = length(coef(model)) - 1,
               r_squared = s$r.squared, adjusted_r_squared = s$adj.r.squared,
               training_rmse = sqrt(mean(e^2)),
               loo_rmse = sqrt(mean(loo_errors^2)),
               residual_sd = s$sigma)
  }
  vif_table <- function(model, model_name) {
    X <- model.matrix(model)[, -1, drop = FALSE]
    vifs <- vapply(seq_len(ncol(X)), function(j) {
      if (ncol(X) == 1) return(1)
      other <- as.data.frame(X[, -j, drop = FALSE])
      1 / (1 - summary(lm(X[, j] ~ ., data = other))$r.squared)
    }, numeric(1))
    data.frame(model = model_name, variable = colnames(X), vif = vifs)
  }
  diagnostic_summary <- function(model, model_name) {
    X <- as.data.frame(model.matrix(model)[, -1, drop = FALSE])
    # Studentized BP-type check: n R-squared of squared residuals on predictors.
    auxiliary <- lm(I(residuals(model)^2) ~ ., data = X)
    statistic <- nobs(model) * summary(auxiliary)$r.squared
    bp_df <- ncol(X)
    data.frame(model = model_name, bp_statistic = statistic, bp_df = bp_df,
               bp_approx_p = pchisq(statistic, df = bp_df, lower.tail = FALSE),
               cooks_cutoff = 4 / nobs(model),
               n_cooks_flagged = sum(cooks.distance(model) > 4 / nobs(model)),
               leverage_cutoff = 2 * length(coef(model)) / nobs(model),
               n_leverage_flagged = sum(hatvalues(model) >
                                          2 * length(coef(model)) / nobs(model)),
               maximum_cooks_distance = max(cooks.distance(model)))
  }

  core_variables <- c("life_sat", "log_gdp", "life_exp", "urban_pct")
  main_data <- sample_for(2023, core_variables)
  health_data <- sample_for(2023, c(core_variables, "log_health"))
  education_data <- sample_for(2022, c(core_variables, "educ_exp"))
  ranking_data <- sample_for(2023, c("life_sat", "log_gdp"))

  models <- list(
    baseline_2023 = lm(life_sat ~ log_gdp, data = main_data),
    main_2023 = lm(life_sat ~ log_gdp + life_exp + urban_pct, data = main_data),
    health_baseline_2023 = lm(life_sat ~ log_gdp, data = health_data),
    health_main_2023 = lm(life_sat ~ log_gdp + life_exp + urban_pct,
                          data = health_data),
    health_expanded_2023 = lm(life_sat ~ log_gdp + life_exp + urban_pct + log_health,
                              data = health_data),
    education_base_2022 = lm(life_sat ~ log_gdp + life_exp + urban_pct,
                             data = education_data),
    education_expanded_2022 = lm(life_sat ~ log_gdp + life_exp + urban_pct + educ_exp,
                                 data = education_data)
  )
  coefficients <- do.call(rbind, Map(coefficient_table, models, names(models)))
  metrics <- do.call(rbind, Map(performance, models, names(models)))
  vifs <- do.call(rbind, Map(vif_table, models, names(models)))
  diagnostics <- do.call(rbind, Map(diagnostic_summary, models, names(models)))
  rownames(coefficients) <- rownames(metrics) <- rownames(vifs) <-
    rownames(diagnostics) <- NULL

  comparisons <- do.call(rbind, lapply(list(
    c("baseline_2023", "main_2023"),
    c("health_main_2023", "health_expanded_2023"),
    c("education_base_2022", "education_expanded_2022")
  ), function(pair) {
    lower <- models[[pair[1]]]
    upper <- models[[pair[2]]]
    stopifnot(nobs(lower) == nobs(upper),
              identical(model.response(model.frame(lower)),
                        model.response(model.frame(upper))))
    conventional <- anova(lower, upper)
    data.frame(base_model = pair[1], expanded_model = pair[2], n = nobs(lower),
               delta_r_squared = summary(upper)$r.squared - summary(lower)$r.squared,
               ordinary_nested_f_p = conventional$`Pr(>F)`[2])
  }))

  # Rank residuals from the GDP-only model, never from the multivariable model.
  ranking_model <- lm(life_sat ~ log_gdp, data = ranking_data)
  interval <- predict(ranking_model, interval = "confidence")
  residual_ranking <- ranking_data[, c("iso3", "country", "life_sat", "gdp_pc",
                                     "life_exp", "urban_pct", "health_exp", "educ_exp")]
  residual_ranking$income_predicted <- unname(interval[, "fit"])
  residual_ranking$income_residual <- as.numeric(residuals(ranking_model))
  residual_ranking$loo_residual <- as.numeric(residuals(ranking_model) /
                                               (1 - hatvalues(ranking_model)))
  residual_ranking$residual_rank <- rank(-residual_ranking$income_residual,
                                        ties.method = "min")
  residual_ranking$loo_residual_rank <- rank(-residual_ranking$loo_residual,
                                            ties.method = "min")
  # +/-1 point is a transparent descriptive threshold, not a significance test.
  residual_ranking$band <- ifelse(residual_ranking$income_residual >= 1,
                                  "At least 1 point above income prediction",
                                  ifelse(residual_ranking$income_residual <= -1,
                                         "At least 1 point below income prediction",
                                         "Within 1 point of income prediction"))
  residual_ranking <- residual_ranking[order(residual_ranking$residual_rank), ]
  rownames(residual_ranking) <- NULL
  extreme_profiles <- residual_ranking[abs(residual_ranking$income_residual) >= 1, ]

  # Dropping a country is a sensitivity check only; every row remains in main results.
  influence_rows <- function(model, model_name, data_used) {
    data.frame(model = model_name, iso3 = data_used$iso3, country = data_used$country,
               fitted = as.numeric(fitted(model)), residual = as.numeric(residuals(model)),
               standardized_residual = as.numeric(rstandard(model)),
               leverage = as.numeric(hatvalues(model)),
               cooks_distance = as.numeric(cooks.distance(model)),
               cooks_flag = as.numeric(cooks.distance(model)) > 4 / nobs(model),
               leverage_flag = as.numeric(hatvalues(model)) >
                 2 * length(coef(model)) / nobs(model))
  }
  influence <- rbind(influence_rows(models$baseline_2023, "baseline_2023", main_data),
                     influence_rows(models$main_2023, "main_2023", main_data))
  influence_keep <- cooks.distance(models$main_2023) <= 4 / nobs(models$main_2023)
  influence_reduced <- lm(life_sat ~ log_gdp + life_exp + urban_pct,
                          data = main_data[influence_keep, , drop = FALSE])
  influence_comparison <- rbind(coefficient_table(models$main_2023, "Main: all countries"),
                                coefficient_table(influence_reduced,
                                                  "Sensitivity: exclude Cook flags"))
  influence_removed <- main_data[!influence_keep, c("iso3", "country")]

  original_residuals <- as.numeric(residuals(ranking_model))
  original_ranks <- rank(-original_residuals)
  leave_one_out <- do.call(rbind, lapply(seq_len(nrow(ranking_data)), function(i) {
    fit_i <- lm(life_sat ~ log_gdp, data = ranking_data[-i, , drop = FALSE])
    # Score all original countries against each leave-one-out line, even the omitted one.
    residual_i <- ranking_data$life_sat - predict(fit_i, newdata = ranking_data)
    data.frame(omitted_iso3 = ranking_data$iso3[i],
               omitted_country = ranking_data$country[i],
               gdp_slope = unname(coef(fit_i)["log_gdp"]),
               doubling_gdp_association = unname(coef(fit_i)["log_gdp"]) * log(2),
               rank_spearman = cor(original_residuals, residual_i, method = "spearman"),
               maximum_rank_shift = max(abs(original_ranks - rank(-residual_i))))
  }))
  rank_robustness <- data.frame(
    n = nrow(ranking_data),
    original_gdp_slope = unname(coef(ranking_model)["log_gdp"]),
    doubling_gdp_association = unname(coef(ranking_model)["log_gdp"]) * log(2),
    leave_one_out_slope_min = min(leave_one_out$gdp_slope),
    leave_one_out_slope_max = max(leave_one_out$gdp_slope),
    minimum_leave_one_out_rank_spearman = min(leave_one_out$rank_spearman),
    maximum_leave_one_out_rank_shift = max(leave_one_out$maximum_rank_shift),
    press_vs_original_rank_spearman = cor(residual_ranking$income_residual,
                                          residual_ranking$loo_residual,
                                          method = "spearman"),
    press_maximum_rank_shift = max(abs(residual_ranking$residual_rank -
                                       residual_ranking$loo_residual_rank))
  )

  # Show who is retained by complete-case selection instead of hiding missingness.
  coverage <- do.call(rbind, lapply(c(2022, 2023), function(y) {
    z <- d[d$year == y, , drop = FALSE]
    data.frame(year = y, available_countries = nrow(z),
               gdp_complete = sum(complete.cases(z[c("life_sat", "log_gdp")])),
               main_complete = sum(complete.cases(z[core_variables])),
               health_complete = sum(complete.cases(z[c(core_variables, "log_health")])),
               education_observed = sum(!is.na(z$educ_exp)),
               education_complete = sum(complete.cases(z[c(core_variables, "educ_exp")]))
    )
  }))
  education_selection <- do.call(rbind, lapply(c(2022, 2023), function(y) {
    z <- d[d$year == y, , drop = FALSE]
    z$education_observed <- !is.na(z$educ_exp)
    do.call(rbind, lapply(c(TRUE, FALSE), function(observed) {
      zz <- z[z$education_observed == observed, , drop = FALSE]
      data.frame(year = y, group = if (observed) "Education observed" else "Education missing",
                 n = nrow(zz),
                 mean_life_sat = mean(zz$life_sat, na.rm = TRUE),
                 median_gdp_pc = median(zz$gdp_pc, na.rm = TRUE),
                 mean_life_exp = mean(zz$life_exp, na.rm = TRUE),
                 mean_urban_pct = mean(zz$urban_pct, na.rm = TRUE))
    }))
  }))

  save_table(coefficients, "model_coefficients")
  save_table(metrics, "model_performance")
  save_table(comparisons, "same_sample_model_comparisons")
  save_table(vifs, "model_vif")
  save_table(diagnostics, "model_diagnostic_summary")
  save_table(influence, "country_model_diagnostics")
  save_table(influence_comparison, "influence_sensitivity_coefficients")
  save_table(influence_removed, "influence_sensitivity_excluded_countries")
  save_table(residual_ranking, "income_adjusted_ranking_2023")
  save_table(extreme_profiles, "income_residual_extreme_profiles_2023")
  save_table(leave_one_out, "income_ranking_leave_one_out")
  save_table(rank_robustness, "income_ranking_robustness")
  save_table(coverage, "model_sample_coverage")
  save_table(education_selection, "education_missingness_selection")

  result <- list(models = models, coefficients = coefficients, performance = metrics,
                 comparisons = comparisons, vif = vifs, diagnostic_summary = diagnostics,
                 main_data = main_data, health_data = health_data,
                 education_data = education_data, ranking_data = ranking_data,
                 ranking_model = ranking_model, residual_ranking = residual_ranking,
                 extreme_profiles = extreme_profiles, country_diagnostics = influence,
                 influence_reduced_model = influence_reduced,
                 influence_coefficients = influence_comparison,
                 influence_excluded = influence_removed, leave_one_out = leave_one_out,
                 rank_robustness = rank_robustness, coverage = coverage,
                 education_selection = education_selection)
  saveRDS(result, file.path(table_dir, "model_results.rds"))
  result
}
