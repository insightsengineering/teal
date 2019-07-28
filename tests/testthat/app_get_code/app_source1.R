ADSL <- radsl(N = 600) #nolint
ADTTE <- radtte(ADSL, event.descr = c("STUDYID", "USUBJID", "PARAMCD")) #nolint

tern::keys(ADSL) <- c("USUBJID", "STUDYID")
tern::keys(ADTTE) <- c("USUBJID", "STUDYID", "PARAMCD")
