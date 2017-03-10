
#' Generate a Sample Dataset
#'
#' This dataset generator function is useful to produce
#'
#'
#' @param type case insensitiv, specify which data to generate
#'
#' @export
#'
#' @return a data.frame
#'
#' @examples
#'
#' ASL <- generate_sample_data('ASL')
generate_sample_data <- function(type = c('asl', 'ars')) {

  type <- tolower(type)
  type <- match.arg(type)

  if (length(type) != 1) stop("specify only one dataset name")

  switch(
    type,
    'asl' = {

      # x <- read.csv("~/Desktop/stream_csv/asl.csv", header = FALSE)
      # dput(as.character(x[,4]))
      vars <-  c( "USUBJID", "STUDYID", "SUBJID",
                  "AEWITHFL",
                  "AGEGRP", "AGExx", "ALIVDT", "BAGE", "BAGEU", "BBMI", "BHT",
                  "BHTU", "BTEMP", "BTEMPU", "BWT", "BWTU", "COMPSDT", "COMPSFU",
                  "COMPSTUD", "COMPTRT", "DISCAE", "DISCDEAT", "DISCSTUD", "DISTRTFL",
                  "IBRTHDT", "IBRTHDTF", "INFCODT", "ITTFL", "RANDDT", "REGION",
                  "SAFFL", "STDDRS", "STDSSDT", "TRT01A", "TRT01AN", "TRT01P",
                  "TRT01PN", "TRT0xA", "TRT0xAN", "TRT0xP", "TRT0xPN", "TRTDRS",
                  "TRTDUR", "TRTEDTM", "TRTETMF", "TRTSDTC", "TRTSDTM", "TRTSEQA",
                  "TRTSEQAN", "TRTSEQP", "TRTSEQPN", "TRTSTMF")
      labels <- c("Unique Subject Identifier",
                 "Study Identifier",
                 "Subject Identifier for the Study",
                 "Actual Sequence of Treatments",
                 "Actual Sequence of Treatments(N)", "Actual Treatment for Period 01",
                 "Actual Treatment for Period 01(N)", "Actual Treatment for Period 0x",
                 "Actual Treatment for Period 0x(N)", "AE Leading to Drug Withdrawal Flag",
                 "Age Group", "Age Group xx", "Baseline Age", "Baseline Age Unit",
                 "Baseline Body Mass Index (kg/m2)", "Baseline Height", "Baseline Height Unit",
                 "Baseline Temp", "Baseline Temp Unit", "Baseline Weight", "Baseline Weight Unit",
                 "Date Last Known to be Alive (N)", "Date of First Exposure to Treatment (C)",
                 "Date of Randomization ", "Datetime of First Exposure to Treatment",
                 "Datetime of Last Exposure to Treatment", "Duration of Exposure(days)",
                 "First Exposure Time Imputation Flag", "Imputed Birth Date",
                 "Imputed Birth Date Flag", "Informed Consent Date", "Intent-To-Treat Population Flag",
                 "Last Exposure Time Imputation Flag", "Patient Completed Study",
                 "Patient Completed Treatment ", "Patient Complete Safety Follow-Up",
                 "Patient Died", "Patient Discontinued Study", "Patient Discontinued Study Due to AE",
                 "Patient Discontinued Treatment ", "Planned Sequence of Treatments",
                 "Planned Sequence of Treatments(N)", "Planned Treatment for Period 01",
                 "Planned Treatment for Period 01(N)", "Planned Treatment for Period 0x",
                 "Planned Treatment for Period 0x(N)", "Reason for Study Discontinuation",
                 "Reason for Treatment Discontinuation", "Region", "Safety Population Flag",
                 "Study Completion Date", "Study Discontinuation Date")
      type <- c("Char", "Char","Char",
                "Char", "Char", "Date", "Char", "Num", "Char", "Num", "Char",
                "Num", "Char", "Num", "Char", "Num", "Char", "Char", "Num", "Num",
                "Char", "Num", "Char", "Num", "Char", "Num", "Char", "Char",
                "Char", "Num", "Num", "Char", "Char", "Char", "Char", "Char",
                "Char", "Char", "Num", "Char", "Num", "Char", "Num", "Char",
                "Num", "Char", "Num", "Char", "Num", "Char", "Char", "Num")


      N <- 100

      df <- data.frame(
        USUBJID = structure(paste0("id-", 1:N), label=labels[1]),
        STUDYID = structure(sample(LETTERS[1:5], N, replace = TRUE), label=labels[2]),
        USUBJID = structure(paste0("id-", 1:N), label=labels[3]),
        stringsAsFactors = FALSE
      )

      Map(function(name, label, type) {

        df[[name]] <<- structure(
          switch(
            type,
            Char = {
              sample(paste("Character Data:", LETTERS[1:20]), N, replace = TRUE)
            },
            Date = {
              Sys.Date() + floor(runif(N) * 200)
            },
            Num = {
              round(rnorm(N, 100, 20), 2)
            },
            rep("unknown", N)
          ),
          label = label
        )

        NULL

      }, vars[-(1:3)], labels[-(1:3)], type[-(1:3)])

      df

    },
    'ars' = {
      data.frame(
        STUDYID = rep(LETTERS[1:4], each = 10),
        USUBJID = paste0("id-", 1:40),
        stringsAsFactors = FALSE
      )
    },
    NULL
  )

}
