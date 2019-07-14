ASL <- radsl(N = 600) #nolint
ADTE <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD")) #nolint

tern::keys(ASL) <- c("USUBJID", "STUDYID")
tern::keys(ADTE) <- c("USUBJID", "STUDYID", "PARAMCD")
