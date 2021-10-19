library(scda)

ADSL <- synthetic_cdisc_data("latest")$adsl #nolint
ADSL$xxx <- "1"

# instead of return
ADSL
