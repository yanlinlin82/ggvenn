# ggvenn

## Version 0.1.16 (Development)

* Added a `NEWS.md` file to track changes to the package.
* Refactor: Improve fill color handling and set name extraction (#2)
* Fix: Count outside elements in percentages in show_outside=None (#53)
* Refactor: Replace deprecated `size` aesthetic with `linewidth` for lines in ggplot2 3.4.0
* Feature: Add `show_set_totals` Argument to Display Set Totals
* Feature: Adds some padding to allow long labels to be fully visible
* Refactor: Change dependencies and imports (#45)
* Test: Add comprehensive test suite (#44)
* Documentation: Improve documentation, deprecate `show_percentage`, and update `geom_venn`
* Feature: Add `show_stats` Argument to `ggvenn` for enhanced display flexibility
* Feature: Added option to add comma as separation for large numbers (#42)
* Documentation: Add links to DESCRIPTION (#39)

## Version 0.1.10 (2023-03-31)

* Adapt to CRAN policies

## Version 0.1.9 (2023-03-31)

* Omit NA in list (#35)

## Version 0.1.8 (2021-01-10)

* Solve notes/warnings for CRAN check

## Version 0.1.7 (2020-12-24)

* Fix issues: Allow run ggvenn() without loading the package (#12)
* Add @importFrom declarations (#12)

## Version 0.1.6 (2020-12-14)

* Allow to change precision for percentages (#11)

## Version 0.1.5 (2020-09-04)

* Allow to customize label sep (#9)

## Version 0.1.4 (2020-08-16)

* Correct usage for old dplyr-1.0.0 (#8)
* Fix error from masked dplyr::count (#7)

## Version 0.1.3 (2020-03-13)

* Allow hiding percentage (#5, #6)
* Use option 'show_percentage' instead of 'value_type'

## Version 0.1.2 (2020-02-27)

* Allow to show set elements (#4)

## Version 0.1.1 (2019-12-17)

* Fix error in count (A & B & !C & D) part (#3)
* Adapt to using 'data.table'
* Add code to check such typos

## Version 0.1.0 (2019-12-17)

* Allow to set colors and other attributes (#2)

## Version 0.0.0.9000 (2019-12-17)

* Initial release with basic implementation
* Implement ggplot (layer) grammar
* Basic venn diagram functionality
