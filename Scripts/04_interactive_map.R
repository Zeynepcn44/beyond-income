# Chapter of choice: an offline, layered choropleth with country time profiles.
make_interactive_map <- function(world, d, output_file = "Outputs/country_explorer.html") {
  stopifnot(requireNamespace("leaflet", quietly = TRUE),
            requireNamespace("htmlwidgets", quietly = TRUE))
  # Simplification is for screen performance only. Analytical values are unchanged.
  map_data <- sf::st_transform(world, 3857)
  map_data <- sf::st_simplify(map_data, dTolerance = 1500, preserveTopology = TRUE)
  map_data <- sf::st_transform(map_data, 4326)
  safe <- function(x) as.character(htmltools::htmlEscape(x))
  fmt <- function(x, digits = 2) ifelse(is.na(x), "Not available", formatC(x, digits = digits, format = "f", big.mark = ","))
  sparkline <- function(iso) {
    z <- d[d$iso3 == iso, c("year", "life_sat")]
    if (nrow(z) == 0) return("")
    z <- z[order(z$year), ]
    pts <- paste(sprintf("%.1f,%.1f", 12 + (z$year - 2016) / 7 * 180,
                         72 - z$life_sat / 10 * 60), collapse = " ")
    paste0('<svg viewBox="0 0 205 94" width="250" role="img" aria-label="Life satisfaction from 2016 to 2023, scale zero to ten">',
      '<path d="M12 12V72H192" stroke="#bdcbd5" fill="none"/>',
      '<polyline points="', pts, '" fill="none" stroke="#2878a1" stroke-width="2.5"/>',
      '<text x="0" y="16" font-size="9">10</text><text x="1" y="74" font-size="9">0</text>',
      '<text x="12" y="88" font-size="10">2016</text><text x="169" y="88" font-size="10">2023</text></svg>')
  }
  popups <- vapply(seq_len(nrow(map_data)), function(i) {
    nm <- ifelse(is.na(map_data$country[i]), "Not in analysis sample", map_data$country[i])
    income_text <- if (is.na(map_data$gdp_pc[i])) "Not available" else paste0("$", fmt(map_data$gdp_pc[i], 0), " current US$")
    paste0('<div style="font-family:Arial,sans-serif;min-width:245px">',
      '<strong style="font-size:17px">', safe(nm), '</strong><br>2023 window endpoint<br><br>',
      'Life satisfaction: <b>', fmt(map_data$life_sat[i]), '</b> / 10<br>',
      'GDP per capita: ', income_text, '<br>',
      'Income-predicted score: ', fmt(map_data$income_predicted[i]), '<br>',
      'Income residual: <b>', fmt(map_data$income_residual[i]), '</b> points<br>',
      sparkline(map_data$ISO_A3_EH[i]),
      '<small>Windows overlap. Line joins available observations.<br>',
      'Residuals are descriptive, not causal effects.<br>Sources: WHR, WDI; boundaries: Natural Earth.</small></div>')
  }, character(1))
  # Two colour scales. The score runs light to dark because it has no natural
  # middle; the residual runs red - white - green because zero is meaningful.
  pal_score <- leaflet::colorNumeric(c("#E4EFF5", "#165376"), domain = c(0, 10), na.color = "#D8DFE5")
  pal_resid <- leaflet::colorNumeric(c("#B74B58", "#FAFAF7", "#138A80"), domain = c(-2.5, 2.5), na.color = "#D8DFE5")

  # ---- The residual-band filter -------------------------------------------
  # Every country already carries the +/-1 point `band` label produced by
  # 02_models.R, so the map uses exactly the same three groups as the ranking
  # table. Each band is then drawn as its own map layer. Because a layer can be
  # switched on and off in the layer control, unticking a band removes exactly
  # those countries from the map. Countries with no residual are given their own
  # group, so missing data stays visible instead of disappearing unexplained.
  band <- ifelse(is.na(map_data$band), "No comparable data", map_data$band)
  band_order <- c("At least 1 point above income prediction",
                  "Within 1 point of income prediction",
                  "At least 1 point below income prediction",
                  "No comparable data")
  stopifnot(all(band %in% band_order))
  band_order <- band_order[band_order %in% band]
  score_group <- "Life satisfaction (all countries)"
  highlight <- leaflet::highlightOptions(weight = 2, color = "#17324D", bringToFront = TRUE)
  country_label <- ~ifelse(is.na(country), "Not in analysis sample", country)

  # The score layer is added first so that it sits underneath the band layers.
  widget <- leaflet::leaflet(map_data, width = "100%", height = 520,
      options = leaflet::leafletOptions(minZoom = 1, maxZoom = 6, zoomSnap = .25, worldCopyJump = TRUE)) |>
    leaflet::addPolygons(fillColor = ~pal_score(life_sat), fillOpacity = .95, color = "white",
      weight = .5, group = score_group, popup = popups,
      label = country_label, highlightOptions = highlight)

  # One layer per band. Subsetting the data here is what makes the filter work.
  for (b in band_order) {
    keep <- band == b
    widget <- leaflet::addPolygons(widget, data = map_data[keep, , drop = FALSE],
      fillColor = ~pal_resid(income_residual), fillOpacity = .95, color = "white",
      weight = .5, group = b, popup = popups[keep],
      label = country_label, highlightOptions = highlight)
  }

  widget <- widget |>
    leaflet::addLayersControl(overlayGroups = c(band_order, score_group),
      options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::addLegend(pal = pal_resid, values = c(-2.5, 2.5), position = "bottomright",
      title = "Income residual (points)", na.label = "No data", opacity = 1) |>
    leaflet::addLegend(pal = pal_score, values = c(0, 10), position = "bottomright",
      title = "Life satisfaction (0-10)", na.label = "No data", opacity = 1) |>
    leaflet::addControl(htmltools::HTML('<b>Beyond income</b><br>Tick a band to show or hide those countries.<br>Click a country for its figures.<br>Grey: no comparable data.<br>Natural Earth boundaries; WHR/WDI values.'), position = "bottomleft") |>
    leaflet::fitBounds(-180, -55, 180, 80) |>
    # The three bands start visible; the score layer starts hidden so the map
    # opens on the income-adjusted view that answers the research question.
    leaflet::hideGroup(score_group)
  widget <- htmlwidgets::onRender(widget, 'function(el, x) { el.style.background = "#F4F7FA"; }')
  # No tile server or API key is needed. Dependencies are embedded in the HTML.
  htmlwidgets::saveWidget(widget, file = output_file, selfcontained = TRUE,
                         title = "Beyond income - country explorer")
  widget
}
