library(scda)

ADSL <- synthetic_cdisc_data("rcd_2021_05_05")$adsl #nolint
ADSL$xxx <- "1"

# instead of return
ADSL
