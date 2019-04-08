ASL <- radsl(N = 600)  %>% dplyr::mutate(A = 1)
ADTE <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))

ASL_FILTERED <- ASL
ADTE_FILTERED_ALONE <- ADTE
ADTE_FILTERED <- merge(x = ASL_FILTERED[, c("USUBJID", "STUDYID")], y = ADTE_FILTERED_ALONE,
    by = c("USUBJID", "STUDYID"), all.x = FALSE, all.y = FALSE)

# Data filtering from Encoding

response_data <- ADTE_FILTERED %>% dplyr::filter(PARAMCD == "OS") %>% dplyr::select(ADTE.BMRKR1 = BMRKR1, USUBJID, STUDYID)

regressor_data <- ASL_FILTERED %>% dplyr::select(ASL.AGE = AGE, USUBJID, STUDYID)

# Data merge

datasets <- list(response_data = response_data, regressor_data = regressor_data)

merged_dataset <- purrr::reduce(.x = lapply(datasets, function(dataset) dataset), .f = safe_join, by = c("USUBJID", "STUDYID"
    ))

fit <- lm(ADTE.BMRKR1 ~ ASL.AGE, data = merged_dataset)
summary <- summary(fit)
plot(fit$model[, 2:1])
