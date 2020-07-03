
# code ADSL ADTTE>
ADSL <- radsl(N = 600) # nolint
tern::keys(ADSL) <- c("USUBJID", "STUDYID")
# <ADSL ADTTE code

# code ADTTE>
ADTTE <- radtte(ADSL, event.descr = c("STUDYID", "USUBJID", "PARAMCD")) # nolint
tern::keys(ADTTE) <- c("USUBJID", "STUDYID", "PARAMCD")
# <ADTTE code
