# Additional source checks

On 11 September 2026, an expanded World Bank API query checked the life-expectancy
values in all 15 initially flagged rows. All 15 current values exactly matched
WDI. The response update was 13 July 2026. The exact response and comparison table
are archived beside this note. This supplements the four specific anomaly checks
described in the report; Somalia's near-zero education value was separately verified.

The initial comparison found a difference for Eswatini's change into 2023, but
not for its 2023 value. Eswatini is absent from the joined panel in 2022. The
initial panel difference therefore compared 2023 (64.123) with 2021 (58.228),
whereas the API annual comparison used 2022 (63.028). This was a gap in the joined
panel, not a source-value disagreement. The preparation script now calculates
life-expectancy change flags only for consecutive years. The final flag table has
14 rows. No original value was altered, and no model uses this change helper.

The archived 15-row comparison deliberately preserves the original audit evidence,
including the now-explained Eswatini change mismatch. It is not a list of confirmed
data errors. Matching WDI establishes provenance, not independent demographic validity.
