asl <- radsl(N = 600)
adte <- radtte(asl, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))

keys(asl) <- c("USUBJID", "STUDYID")
keys(adte) <- c("USUBJID", "STUDYID", "PARAMCD")
