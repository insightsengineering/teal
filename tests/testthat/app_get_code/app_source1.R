ADSL <- synthetic_cdisc_data("rcd_2021_05_05")$adsl #nolint
ADTTE <- synthetic_cdisc_data("rcd_2021_05_05")$adtte #nolint

tern::keys(ADSL) <- c("USUBJID", "STUDYID")
tern::keys(ADTTE) <- c("USUBJID", "STUDYID", "PARAMCD")
