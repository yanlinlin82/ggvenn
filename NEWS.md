# ggvenn NEWS

## Version 0.1.19 (2025-10-08, a quick fix)

- Fix: Resolve count and percentage display issues in Venn diagrams
- Enhancement: Improve README documentation with comprehensive usage examples and feature descriptions

## Version 0.1.18 (2025-10-08)

- Feature: Extend Venn diagram support from 4 to 5 sets (with geometry functions for up to 8 sets)
- Feature: Add `get_venn_table()` function to extract Venn diagram intersection data
- Feature: Add text truncation and element display options with `max_elements` and `text_truncate` parameters
- Feature: Remove lifecycle dependency and enhance parameter flexibility
- Fix: Correct intersection calculation and display with `auto_scale=TRUE`
- Fix: Improve element display logic and label positioning
- Refactor: Consolidate data conversion functions and improve code organization

## Version 0.1.17 (2025-10-06)

- Feature: Add `show_set_totals` and `show_stats` arguments for enhanced display flexibility
- Feature: Add data conversion utility functions (`data_frame_to_list`, `list_to_data_frame`)
- Feature: Add comma separation option for large numbers (#42)
- Fix: Allow individual stroke color/alpha settings for each set (#33)
- Fix: Replace deprecated `size` aesthetic with `linewidth` for ggplot2 3.4.0 compatibility
- Refactor: Improve `geom_venn` function structure and readability

## Version 0.1.10 (2023-03-31)

- Adapt to CRAN policies

## Version 0.1.9 (2023-03-31)

- Omit NA in list (#35)

## Version 0.1.8 (2021-01-10)

- Solve notes/warnings for CRAN check

## Version 0.1.7 (2020-12-24)

- Fix issues: Allow run ggvenn() without loading the package (#12)
- Add @importFrom declarations (#12)

## Version 0.1.6 (2020-12-14)

- Allow to change precision for percentages (#11)

## Version 0.1.5 (2020-09-04)

- Allow to customize label sep (#9)

## Version 0.1.4 (2020-08-16)

- Correct usage for old dplyr-1.0.0 (#8)
- Fix error from masked dplyr::count (#7)

## Version 0.1.3 (2020-03-13)

- Allow hiding percentage (#5, #6)
- Use option 'show_percentage' instead of 'value_type'

## Version 0.1.2 (2020-02-27)

- Allow to show set elements (#4)

## Version 0.1.1 (2019-12-17)

- Fix error in count (A & B & !C & D) part (#3)
- Adapt to using 'data.table'
- Add code to check such typos

## Version 0.1.0 (2019-12-17)

- Allow to set colors and other attributes (#2)

## Version 0.0.0.9000 (2019-12-17)

- Initial release with basic implementation
- Implement ggplot (layer) grammar
- Basic venn diagram functionality
