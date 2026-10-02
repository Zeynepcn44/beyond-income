# Source and anomaly verification for the final report

Audit date: 11 September 2026. The four flagged observations were read directly from the supplied `analysis_data.rds`. Raw values are preserved. A value being unusual is not evidence of a transcription error.

## Happiness year definition

Use **survey-window ending year**, not WHR reporting year, for the supplied `year` field. The mapping is recorded in `WHR_year_mapping.csv` and supported by the linked annual report chapters. The local top-ranked values are consistent with that mapping: Norway 7.537 in data year 2016 is the WHR 2017 result; Finland 7.741 in data year 2023 is the WHR 2024 result. Thus the 2023 cross-section pairs the 2021-2023 life-evaluation average with 2023 WDI indicators. Describe this as an approximation to time alignment. Where a country lacks surveys within one or more years of the window, WHR averages the available surveys; a three-year label does not guarantee three annual surveys.

Source: [WHR 2017, Chapter 2](https://files.worldhappiness.report/WHR17_Ch02.pdf), printed pp. 18-22, and [WHR 2024, Chapter 2](https://www.worldhappiness.report/ed/2024/happiness-of-the-younger-the-older-and-those-in-between/), “Ranking of Happiness 2021-2023”. Adjacent windows overlap, so time-trend observations are not independent annual outcomes.

## Missing decomposition fields

All supplied GDP-contribution values are missing in 2016, 2017 and 2018. Their absence is a limitation of the supplied combined data. The original WHR 2017 Figure 2.2 already includes the six contribution components and the Dystopia-plus-residual segment; it also displays confidence intervals. Do not claim that these quantities were not published in early editions. The original import process is not available, so the particular cause of missing values in the combined file cannot be identified.

Source: [WHR 2017, Figure 2.2](https://files.worldhappiness.report/WHR17_Ch02.pdf), printed pp. 20-22. The report's printed pp. 18-19 explain that the outcome is the survey mean, and that the components including the residual sum to that mean. The six contributions alone do not exactly reconstruct the outcome. Excluding them from the main models avoids reusing WHR's fitted decomposition and keeps the interpretation in original socioeconomic units.

## Flagged original values

All four flagged observations agree with the official World Bank API, retrieved on 11 September 2026; its response metadata states a last update of 13 July 2026. The exact original responses are saved as JSON and the checked rows are recorded in `anomaly_source_verification.csv`.

| Country | Year | Variable | Supplied value | Official WDI value | Decision |
|---|---:|---|---:|---:|---|
| Central African Republic | 2019 | Life expectancy at birth, years | 31.530 | 31.530 | Retain |
| South Sudan | 2016 | Life expectancy at birth, years | 36.720 | 36.720 | Retain |
| South Sudan | 2017 | Life expectancy at birth, years | 35.351 | 35.351 | Retain |
| Somalia | 2018 | Government education expenditure, percent of GDP | 0.00000400990834307 | 0.00000400990834307 | Retain |

There is no verified source correction to apply. Keep a sensitivity flag for the three unusually low life-expectancy values and the near-zero expenditure measure where these enter pooled descriptive analyses. A sensitivity comparison may exclude these rows for that calculation while preserving them in the prepared data. None of the four observations belongs to the main 2023 or education-sensitivity 2022 model samples, so these particular flags cannot affect those fits. Do not claim that the source verification explains why the values are so unusual, or that their independent demographic or fiscal validity has been established.

Official source URLs: [CAF life expectancy](https://api.worldbank.org/v2/country/CAF/indicator/SP.DYN.LE00.IN?date=2016:2023&format=json&per_page=100), [SSD life expectancy](https://api.worldbank.org/v2/country/SSD/indicator/SP.DYN.LE00.IN?date=2016:2023&format=json&per_page=100), [SOM education](https://api.worldbank.org/v2/country/SOM/indicator/SE.XPD.TOTL.GD.ZS?date=2018&format=json&per_page=100).

The World Bank identifies [life expectancy](https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=CF) as drawing on UN World Population Prospects and national/Eurostat sources, and [education expenditure](https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS?locations=SO) as drawing on the UNESCO Institute for Statistics. Confirming a value against WDI confirms provenance, not independent validity of the underlying estimate.

## Scope of audit

The supplied workbook's historical extraction date is unknown. Current API results can confirm agreement or identify a source-vintage difference, but should not be described as proof of the data version originally downloaded. Original values and exact retrieved responses should remain available alongside the audit decision. No time interpolation, rescaling or plausible-value substitution is warranted without specific source evidence.
