ASL <- radsl(N = 600) #nolint
adte <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))

tern::keys(ASL) <- c("USUBJID", "STUDYID")
tern::keys(adte) <- c("USUBJID", "STUDYID", "PARAMCD")
