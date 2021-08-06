
# code ADSL ADTTE>
ADSL <- synthetic_cdisc_data("rcd_2021_05_05")$adsl # nolint
tern::keys(ADSL) <- c("USUBJID", "STUDYID")
# <ADSL ADTTE code

# code ADTTE>
ADTTE <- synthetic_cdisc_data("rcd_2021_05_05")$adtte # nolint
tern::keys(ADTTE) <- c("USUBJID", "STUDYID", "PARAMCD")
# <ADTTE code
