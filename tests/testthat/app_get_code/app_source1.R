ASL <- radsl(N = 600) #nolint
adte <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))

library(tern)
keys(ASL) <- c("USUBJID", "STUDYID")
keys(adte) <- c("USUBJID", "STUDYID", "PARAMCD")
