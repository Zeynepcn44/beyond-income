# One figure function, one consistent visual language, explicit samples.
make_visuals <- function(d, quality, models, figure_dir = "Outputs/figures",
                         table_dir = "Outputs/tables", data_dir = "Data") {
  suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(tidyr); library(patchwork)})
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  theme_set(theme_minimal(base_size = 11, base_family = "sans") + theme(
    plot.title = element_text(face = "bold", size = 13, colour = "#17324D"),
    plot.subtitle = element_text(size = 10, colour = "#526474"),
    plot.caption = element_text(size = 8, colour = "#526474", hjust = 0),
    panel.grid.minor = element_blank(), legend.position = "bottom",
    strip.text = element_text(face = "bold"), plot.title.position = "plot"))
  blue <- "#2878A1"; teal <- "#138A80"; red <- "#B74B58"; grey <- "#D8DFE5"
  continent_col <- c(Africa = "#B87733", Asia = "#4C89A9", Europe = "#56765A",
                     `North America` = "#916B9E", `South America` = "#C46D69", Oceania = "#5C9691")
  save_plot <- function(p, name, width = 8, height = 4.5) {
    ggsave(file.path(figure_dir, paste0(name, ".png")), p, width = width,
           height = height, dpi = 220, bg = "white")
  }
  s <- d[d$year == 2023, ]
  labels <- c(life_sat = "Life satisfaction", log_gdp = "Log GDP per capita",
    life_exp = "Life expectancy", log_health = "Log health expenditure",
    educ_exp = "Education expenditure", urban_pct = "Urban population")
  vars <- names(labels)
  descriptive <- do.call(rbind, lapply(c("life_sat", "gdp_pc", "life_exp", "health_exp", "educ_exp", "urban_pct"), function(v) {
    x <- s[[v]]
    data.frame(variable = v, n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE), min = min(x, na.rm = TRUE),
      q25 = unname(quantile(x, .25, na.rm = TRUE)), median = median(x, na.rm = TRUE),
      q75 = unname(quantile(x, .75, na.rm = TRUE)), max = max(x, na.rm = TRUE))
  }))
  write.csv(descriptive, file.path(table_dir, "descriptive_2023.csv"), row.names = FALSE)
  missing <- quality$missing
  missing$variable <- factor(missing$variable,
    levels = c("educ_exp", "health_exp", "gdp_pc", "urban_pct", "life_exp", "life_sat"),
    labels = c("Education expenditure", "Health expenditure", "GDP per capita",
               "Urban population", "Life expectancy", "Life satisfaction"))
  p <- ggplot(missing, aes(factor(year), variable, fill = missing_pct)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.0f%%", missing_pct)), size = 3.5) +
    scale_fill_gradient(low = "#EEF4F8", high = "#DF9D5B", limits = c(0, 50)) +
    labs(x = "Survey-window ending year", y = NULL, fill = "Missing (%)",
      title = "Education has the largest coverage gap") + theme(panel.grid = element_blank())
  save_plot(p, "01_missingness", height = 3.25)
  cont_order <- s |> group_by(continent) |> summarise(m = median(life_sat)) |> arrange(m) |> pull(continent)
  s$continent <- factor(s$continent, levels = cont_order)
  p <- ggplot(s, aes(life_sat, continent)) + geom_boxplot(width = .5, outlier.shape = NA, fill = "#E8EFF4") +
    geom_point(aes(colour = continent), position = position_jitter(height = .12, width = 0, seed = 7),
      alpha = .65, size = 1.8) + scale_colour_manual(values = continent_col, guide = "none") +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    labs(x = "National life satisfaction (0-10)", y = NULL,
      title = "Regional differences coexist with substantial overlap",
      subtitle = "2023 window endpoint; each point is one country (n = 141)")
  save_plot(p, "02_continent_distribution", height = 3.8)
  trends <- d |> group_by(year) |> summarise(all_mean = mean(life_sat),
    balanced_mean = mean(life_sat[balanced_panel]), n = n(), .groups = "drop")
  write.csv(trends, file.path(table_dir, "global_trends.csv"), row.names = FALSE)
  p1 <- trends |> pivot_longer(c(all_mean, balanced_mean), names_to = "sample", values_to = "mean") |>
    mutate(sample = recode(sample, all_mean = "All available countries", balanced_mean = "Same 131 countries")) |>
    ggplot(aes(year, mean, colour = sample)) + geom_line(linewidth = .9) + geom_point(size = 2) +
    scale_colour_manual(values = c("#8895A0", blue)) +
    scale_x_continuous(breaks = 2016:2023) + scale_y_continuous(limits = c(4.8, 6.2)) +
    labs(x = NULL, y = "Country-average score", colour = NULL,
      title = "Compare trends on a stable set of countries")
  regional <- d |> filter(balanced_panel) |> group_by(continent, year) |>
    summarise(mean = mean(life_sat), countries = n(), .groups = "drop")
  p2 <- ggplot(regional, aes(year, mean, colour = continent)) + geom_line(linewidth = .8) +
    geom_point(size = 1.5) + facet_wrap(~continent, nrow = 2) +
    scale_colour_manual(values = continent_col, guide = "none") +
    scale_x_continuous(breaks = c(2016, 2019, 2023), expand = expansion(mult = .12)) +
    scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
    labs(x = "Survey-window ending year", y = "Country-average score",
      title = "Regional trends use the same vertical scale") +
    theme(panel.spacing.x = grid::unit(1.2, "lines"))
  save_plot(p1 / p2 + patchwork::plot_layout(heights = c(1, 1.65)), "03_time_trends", height = 6.5)
  # Country spellings can change between windows; join by ISO, never by display name.
  endpoint <- merge(d[d$year == 2016, c("iso3", "life_sat")],
                    d[d$year == 2023, c("iso3", "country", "life_sat")], by = "iso3", suffixes = c("_2016", "_2023"))
  endpoint$change <- endpoint$life_sat_2023 - endpoint$life_sat_2016
  endpoint <- endpoint[order(endpoint$change), ]
  write.csv(endpoint, file.path(table_dir, "endpoint_changes.csv"), row.names = FALSE)
  extremes <- rbind(head(endpoint, 6), tail(endpoint, 6))
  p <- ggplot(extremes, aes(change, reorder(country, change))) +
    geom_vline(xintercept = 0, colour = "#7F8C97") +
    geom_segment(aes(x = 0, xend = change, yend = reorder(country, change)), colour = grey, linewidth = 1) +
    geom_point(aes(colour = change > 0), size = 3) +
    scale_colour_manual(values = c(red, teal), guide = "none") +
    labs(x = "Change in score: 2023 window minus 2016 window", y = NULL,
      title = "The largest observed changes among 137 matched countries",
      subtitle = "Six largest increases and six largest decreases; descriptive, not causal")
  save_plot(p, "04_endpoint_changes", height = 4.2)
  g <- models$main_data
  p1 <- ggplot(g, aes(gdp_pc, life_sat)) + geom_point(alpha = .55, colour = blue, size = 1.6) +
    scale_x_continuous(labels = scales::label_dollar(scale = .001, suffix = "k")) +
    scale_y_continuous(limits = c(0, 10)) + labs(x = "GDP per capita (current US$)", y = "Life satisfaction (0-10)", title = "Income on its original scale")
  prediction_grid <- data.frame(log_gdp = seq(min(g$log_gdp), max(g$log_gdp), length.out = 150))
  prediction_grid$fit <- predict(models$models$baseline_2023, newdata = prediction_grid)
  p2 <- ggplot(g, aes(log_gdp, life_sat)) + geom_point(alpha = .55, colour = blue, size = 1.6) +
    geom_line(data = prediction_grid, aes(y = fit), colour = "#17324D", linewidth = .9) +
    scale_x_continuous(breaks = log(c(500, 2000, 10000, 50000)), labels = c("$0.5k", "$2k", "$10k", "$50k")) +
    scale_y_continuous(limits = c(0, 10)) + labs(x = "GDP per capita (log spacing)", y = NULL,
      title = "Log income supports a simple linear fit")
  save_plot(p1 + p2, "05_gdp_relationship", height = 3.8, width = 9)
  corr <- cor(s[vars], use = "pairwise.complete.obs")
  pair_n <- outer(vars, vars, Vectorize(function(a, b) sum(complete.cases(s[c(a,b)]))))
  dimnames(pair_n) <- list(vars, vars)
  write.csv(corr, file.path(table_dir, "correlations_2023.csv"))
  write.csv(pair_n, file.path(table_dir, "correlation_pair_counts_2023.csv"))
  cd <- as.data.frame(as.table(corr)); names(cd) <- c("x", "y", "r")
  corr_labels <- c("Life\nsatisfaction", "Log GDP", "Life\nexpectancy",
                   "Log health\nspending", "Education\nspending", "Urban share")
  cd$x <- factor(cd$x, levels = vars,
    labels = c("Score", "Log GDP", "Life exp.", "Log health", "Education", "Urban %"))
  cd$y <- factor(cd$y, levels = rev(vars), labels = rev(corr_labels))
  p <- ggplot(cd, aes(x, y, fill = r)) + geom_tile(colour = "white", linewidth = .6) +
    geom_text(aes(label = sprintf("%.2f", r)), size = 4.2) +
    scale_fill_gradient2(low = red, mid = "white", high = blue, limits = c(-1, 1), midpoint = 0) +
    coord_equal() + labs(x = NULL, y = NULL, fill = "Pearson r",
      title = "Strong correlation does not imply\nunique explanatory value",
      subtitle = "2023 cross-section; pairwise complete observations (n = 77-141)") +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10))
  save_plot(p, "06_correlations", width = 7, height = 5.3)
  relations <- s |> select(life_sat, life_exp, urban_pct, educ_exp, log_health) |>
    pivot_longer(-life_sat, names_to = "factor", values_to = "value") |>
    filter(!is.na(value)) |> mutate(factor = factor(factor,
      levels = c("life_exp", "urban_pct", "educ_exp", "log_health"),
      labels = c("Life expectancy (years)", "Urban population (%)", "Education expenditure (% GDP)", "Log health expenditure (PPP int$)")))
  p <- ggplot(relations, aes(value, life_sat)) + geom_point(colour = blue, alpha = .55, size = 1.5) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#17324D", linewidth = .7) +
    facet_wrap(~factor, scales = "free_x", nrow = 2) + scale_y_continuous(limits = c(0, 10)) +
    labs(x = NULL, y = "Life satisfaction (0-10)", title = "Positive bivariate patterns need an income-adjusted check",
      subtitle = "2023; available countries in each panel; lines are descriptive fits")
  save_plot(p, "07_socioeconomic_relationships", height = 4.8)
  ct <- models$coefficients |> filter(model == "main_2023", term != "(Intercept)")
  ct$scale <- c(log(2), 10, 10)[match(ct$term, c("log_gdp", "life_exp", "urban_pct"))]
  ct$label <- c("GDP per capita: doubling", "Life expectancy: +10 years", "Urban population: +10 percentage points")[match(ct$term, c("log_gdp", "life_exp", "urban_pct"))]
  p <- ggplot(ct, aes(estimate * scale, reorder(label, estimate * scale))) +
    geom_vline(xintercept = 0, colour = "#7F8C97", linetype = 2) +
    geom_segment(aes(x = hc3_ci_low * scale, xend = hc3_ci_high * scale,
      yend = reorder(label, estimate * scale)), linewidth = .9, colour = blue) +
    geom_point(size = 3, colour = blue) +
    labs(x = "Adjusted association with life-satisfaction score", y = NULL,
      title = "Income retains an association after adjustment",
      subtitle = "2023 main model; HC3 95% confidence intervals; n = 140")
  save_plot(p, "08_model_coefficients", height = 2.9)
  diag <- models$country_diagnostics |> filter(model == "main_2023")
  p1 <- ggplot(diag, aes(fitted, residual)) + geom_hline(yintercept = 0, colour = grey) +
    geom_point(alpha = .6, colour = blue, size = 1.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = red, linewidth = .6) +
    labs(x = "Fitted score", y = "Residual", title = "Residuals vs fitted")
  p2 <- ggplot(diag, aes(sample = standardized_residual)) + stat_qq(colour = blue, alpha = .6, size = 1.5) +
    stat_qq_line(colour = red, linewidth = .6) + labs(x = "Normal theoretical quantile", y = "Standardized residual", title = "Normal Q-Q")
  p3 <- ggplot(diag, aes(fitted, sqrt(abs(standardized_residual)))) +
    geom_point(alpha = .6, colour = blue, size = 1.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = red, linewidth = .6) +
    labs(x = "Fitted score", y = "Sqrt |std. residual|", title = "Scale-location")
  p4 <- ggplot(diag, aes(leverage, cooks_distance)) + geom_point(alpha = .6, colour = blue, size = 1.5) +
    geom_hline(yintercept = 4 / nrow(diag), linetype = 2, colour = red) +
    geom_text(data = diag[order(-diag$cooks_distance)[1:3], ], aes(label = iso3), nudge_y = .008, size = 3) +
    labs(x = "Leverage", y = "Cook's distance", title = "Influence (line = 4/n)")
  save_plot((p1 + p2) / (p3 + p4), "09_diagnostics", height = 5.5)
  rank <- models$residual_ranking
  extreme <- rbind(head(rank[order(rank$income_residual), ], 8), tail(rank[order(rank$income_residual), ], 8))
  p <- ggplot(extreme, aes(income_residual, reorder(country, income_residual))) +
    geom_vline(xintercept = 0, colour = "#7F8C97") +
    geom_vline(xintercept = c(-1, 1), colour = grey, linetype = 2) +
    geom_segment(aes(x = 0, xend = income_residual, yend = reorder(country, income_residual)), colour = grey, linewidth = 1) +
    geom_point(aes(colour = income_residual > 0), size = 2.7) +
    scale_colour_manual(values = c(red, teal), guide = "none") +
    labs(x = "Observed score minus income-predicted score", y = NULL,
      title = "Countries can differ markedly from the income benchmark",
      subtitle = "Eight largest positive and negative residuals, 2023; dashed lines mark +/-1 point")
  save_plot(p, "10_income_residuals", height = 5.1)
  # Natural Earth may contain several features per country; dissolve before joining.
  suppressPackageStartupMessages(library(sf))
  world <- st_read(file.path(data_dir, "ne_50m_admin_0_countries.geojson"), quiet = TRUE)
  # Split the dateline before projection; union projected features to avoid
  # spherical seam artefacts in Russia, Canada and other multi-part countries.
  world <- world |> filter(ADMIN != "Antarctica") |>
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) |>
    st_transform("+proj=robin") |> st_make_valid() |>
    group_by(ISO_A3_EH) |> summarise(geometry = st_union(geometry), .groups = "drop")
  stopifnot(!anyDuplicated(world$ISO_A3_EH), all(unique(d$iso3) %in% world$ISO_A3_EH))
  world <- world |> left_join(s |> select(iso3, country, life_sat), by = c("ISO_A3_EH" = "iso3")) |>
    # `band` is the +/-1 point classification already defined in 02_models.R.
    # Carrying it here means the map and the ranking table always agree.
    left_join(rank |> select(iso3, gdp_pc, income_predicted, income_residual, band),
              by = c("ISO_A3_EH" = "iso3"))
  maptheme <- theme_void(base_size = 10) + theme(legend.position = "bottom",
    plot.title = element_text(face = "bold", colour = "#17324D", size = 12))
  p1 <- ggplot(world) + geom_sf(aes(fill = life_sat), colour = "white", linewidth = .08) +
    coord_sf(crs = "+proj=robin", datum = NA) +
    scale_fill_gradient(low = "#E4EFF5", high = "#165376", limits = c(0, 10), na.value = grey) +
    labs(title = "Life satisfaction (141 countries)", fill = "Score (0-10)") + maptheme
  p2 <- ggplot(world) + geom_sf(aes(fill = income_residual), colour = "white", linewidth = .08) +
    coord_sf(crs = "+proj=robin", datum = NA) +
    scale_fill_gradient2(low = red, mid = "#FAFAF7", high = teal, midpoint = 0,
      limits = c(-2.5, 2.5), na.value = grey) +
    labs(title = "Income-adjusted life satisfaction (140 countries)", fill = "Residual (points)") + maptheme
  save_plot(p1 / p2, "11_maps", height = 7, width = 9)
  write.csv(data.frame(iso3 = world$ISO_A3_EH, score_available = !is.na(world$life_sat),
    residual_available = !is.na(world$income_residual)), file.path(table_dir, "geographic_join_audit.csv"), row.names = FALSE)

  # --------------------------------------------------------------------------
  # Figures 12-16 restore exploratory analyses from the first version of this
  # project. The methods are unchanged from that version; only the theme and
  # the saving helper were adapted to match the rest of this script.
  # --------------------------------------------------------------------------

  # 12. Shape of the outcome across all country-windows. Histogram, density and
  # a rug so that every observation is visible, not only a summary.
  p <- ggplot(d, aes(life_sat)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "#E8EFF4", colour = "white") +
    geom_density(colour = teal, linewidth = .8) +
    geom_rug(alpha = .12, colour = "#526474") +
    geom_vline(xintercept = mean(d$life_sat), linetype = "dashed", colour = red) +
    scale_x_continuous(breaks = 1:8) +
    labs(x = "Life satisfaction (0-10)", y = "Density",
         title = "The distribution has two peaks",
         subtitle = sprintf("All %s country-windows, 2016-2023. Mean %.2f, median %.2f (dashed line = mean)",
                            format(nrow(d), big.mark = ","), mean(d$life_sat), median(d$life_sat)))
  save_plot(p, "12_distribution", height = 3.6)

  # 13. Named countries at each end of the 2023 distribution.
  ends <- rbind(
    transform(s[order(-s$life_sat), ][1:15, ], group = "15 highest"),
    transform(s[order(s$life_sat), ][1:15, ],  group = "15 lowest"))
  p <- ggplot(ends, aes(life_sat, reorder(country, life_sat), fill = continent)) +
    geom_col(width = .72) +
    geom_text(aes(label = sprintf("%.2f", life_sat)), hjust = -.15, size = 2.7,
              colour = "#17324D") +
    facet_wrap(~ group, scales = "free_y") +
    scale_x_continuous(expand = expansion(mult = c(0, .16))) +
    scale_fill_manual(values = continent_col) +
    labs(x = "Life satisfaction (0-10)", y = NULL, fill = NULL,
         title = "The highest and lowest ranked countries in 2023",
         subtitle = sprintf("%s countries reported in 2023", nrow(s)))
  save_plot(p, "13_top_bottom", height = 4.6, width = 9)

  # 14. Do the two peaks in figure 12 correspond to income groups? Countries are
  # split into four income groups and each group is drawn separately.
  q <- d[!is.na(d$gdp_pc), ]
  q$income_group <- factor(dplyr::ntile(q$gdp_pc, 4), levels = 1:4,
    labels = c("Q1 lowest income", "Q2", "Q3", "Q4 highest income"))
  p <- ggplot(q, aes(life_sat)) +
    geom_density(aes(colour = income_group), linewidth = .85) +
    geom_density(linetype = "dashed", colour = "#526474", linewidth = .7) +
    geom_rug(alpha = .07, colour = "#526474") +
    scale_colour_manual(values = c("#B74B58", "#DF9D5B", "#7FB2C8", "#17324D")) +
    labs(x = "Life satisfaction (0-10)", y = "Density", colour = NULL,
         title = "Each income group has a single peak",
         subtitle = sprintf("Income quartiles, %s country-windows. Dashed grey = the overall distribution from figure 12",
                            format(nrow(q), big.mark = ",")))
  save_plot(p, "14_income_groups", height = 3.6)

  # 15. Country paths over time. All countries in grey; the three largest rises
  # and three largest falls between the 2016 and 2023 windows are highlighted.
  mv <- endpoint[order(-endpoint$change), ]
  pick <- c(head(mv$iso3, 3), tail(mv$iso3, 3))
  hi <- d[d$iso3 %in% pick, ]
  lab <- hi[hi$year == 2023, ]
  p <- ggplot(mapping = aes(year, life_sat)) +
    geom_line(data = d, aes(group = iso3), colour = grey, linewidth = .3) +
    geom_line(data = hi, aes(group = iso3, colour = country), linewidth = .9) +
    geom_text(data = lab, aes(label = country, colour = country), hjust = -.1,
              size = 2.9, show.legend = FALSE) +
    scale_x_continuous(breaks = 2016:2023, limits = c(2016, 2025.6)) +
    scale_colour_brewer(palette = "Dark2", guide = "none") +
    labs(x = "Survey-window ending year", y = "Life satisfaction (0-10)",
         title = "The flat global average hides large country movements",
         subtitle = "All countries in grey; the three largest rises and falls between the 2016 and 2023 windows")
  save_plot(p, "15_trajectories", height = 4.2, width = 9)

  # 16. Latitude looks related to life satisfaction, but the relationship is
  # tested again against the income residual from the GDP-only model.
  lat <- merge(rank[, c("iso3", "life_sat", "income_residual")],
               s[, c("iso3", "latitude", "continent")], by = "iso3")
  lat_long <- rbind(
    transform(lat[, c("latitude", "continent")], value = lat$life_sat,
              panel = "Life satisfaction"),
    transform(lat[, c("latitude", "continent")], value = lat$income_residual,
              panel = "After accounting for income (residual)"))
  lat_long$panel <- factor(lat_long$panel,
    levels = c("Life satisfaction", "After accounting for income (residual)"))
  zero <- data.frame(panel = factor("After accounting for income (residual)",
                                    levels = levels(lat_long$panel)))
  r_raw <- cor(lat$latitude, lat$life_sat)
  r_res <- cor(lat$latitude, lat$income_residual)
  p <- ggplot(lat_long, aes(latitude, value)) +
    geom_hline(data = zero, aes(yintercept = 0), colour = "#B0BCC6") +
    geom_point(aes(colour = continent), alpha = .75, size = 1.6) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = "#17324D", linewidth = .8) +
    facet_wrap(~ panel, scales = "free_y") +
    scale_colour_manual(values = continent_col) +
    labs(x = "Latitude (degrees; negative = southern hemisphere)", y = NULL,
         colour = NULL, title = "The latitude pattern is largely an income pattern",
         subtitle = sprintf("2023, %d countries. Correlation with latitude: %.2f for the score, %.2f for the income residual",
                            nrow(lat), r_raw, r_res))
  save_plot(p, "16_latitude", height = 4.0, width = 9)
  write.csv(data.frame(latitude_vs_score = r_raw, latitude_vs_residual = r_res,
                       latitude_vs_log_gdp = cor(lat$latitude, s$log_gdp[match(lat$iso3, s$iso3)],
                                                 use = "complete.obs"), n = nrow(lat)),
            file.path(table_dir, "latitude_checks.csv"), row.names = FALSE)

  list(descriptive = descriptive, trends = trends, regional = regional,
       endpoint = endpoint, correlations = corr, pair_counts = pair_n, world = world)
}
