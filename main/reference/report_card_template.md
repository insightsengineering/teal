# Template function for `TealReportCard` creation and customization

This function generates a report card with a title, an optional
description, and the option to append the filter state list.

## Usage

``` r
report_card_template(
  title,
  label,
  description = NULL,
  with_filter,
  filter_panel_api
)
```

## Arguments

- title:

  (`character(1)`) title of the card (unless overwritten by label)

- label:

  (`character(1)`) label provided by the user when adding the card

- description:

  (`character(1)`) optional, additional description

- with_filter:

  (`logical(1)`) flag indicating to add filter state

- filter_panel_api:

  (`FilterPanelAPI`) object with API that allows the generation of the
  filter state in the report

## Value

(`TealReportCard`) populated with a title, description and filter state.
