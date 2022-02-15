ADSAMP <- ADSL %>% # nolint
  dplyr::filter(SEX == "M") %>%
  dplyr::left_join(ADVS, by = c("USUBJID", "STUDYID"))
