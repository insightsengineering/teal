
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
#' \dontrun{
#' file <- "~/Desktop/STREAM_DAP_M3_AnalysisDataDefinitions.xls"
#' STREAM_DAP_M3 <- Map(function(i) {
#'   df <- read_excel(file, i)
#'   df[1:max(which(!is.na(df$`VARIABLE NAME`))), ]
#' }, 10:28)
#' names(STREAM_DAP_M3) <- readxl::excel_sheets(file)[10:28]
#' devtools::use_data(STREAM_DAP_M3)
#' }
#'
#' ASL <- generate_sample_data('ASL')
#' head(ASL)
#'
#' ATX <- generate_sample_data('ATX')
#' head(ATX)
#'
#' ATE <- generate_sample_data('ATE')
#' head(ATE)
generate_sample_data <- function(type = c("ATX", "ASL", "AAG", "AAE", "XAAE", "ACM", "ADD", "AEG", "XAEG",
                                          "AEX", "AHY", "AHYTE", "ALB", "XALB", "AMH", "AVS", "XAVS", "ARS",
                                          "ATE"),
                                 N = 100) {

  type <- match.arg(type)

  if (length(type) != 1) stop("specify only one dataset name")


  tvn <- STREAM_DAP_M3[[type]]$`VARIABLE NAME`

  df_raw <-  subset(STREAM_DAP_M3[[type]], !is.na(tvn) & toupper(tvn) == tvn,
                select=c("VARIABLE NAME", "VARIABLE LABEL", "VARIABLE TYPE"))


  names(df_raw) <- gsub(" ", "_", names(df_raw))


  is_dupli <- duplicated(df_raw$VARIABLE_NAME)

  df_clean_tmp <- Reduce(rbind, Map(function(x) {
    switch(
      x$VARIABLE_NAME,
      "USUBJID" = {x$VARIABLE_LABEL <- ""; x$VARIABLE_TYPE <- "Char"},
      "STUDYID" = {x$VARIABLE_LABEL <- "Study Identifier"; x$VARIABLE_TYPE <- "Char"},
      "SUBJID" = {x$VARIABLE_LABEL <- "Subject Identifier for the Study"; x$VARIABLE_TYPE <- "Char"}
    )

    if (is.na(x$VARIABLE_TYPE)) x$VARIABLE_TYPE <- "Char"

    x
  } , split(df_raw[!is_dupli, ], 1:sum(!is_dupli))))

  df_clean <- df_clean_tmp[!df_clean_tmp$VARIABLE_NAME %in% c("USUBJID", "STUDYID", "SUBJID"), ]

  df <- data.frame(
    USUBJID = structure(paste0("id-", 1:N), label="Unique Subject Identifier"),
    STUDYID = structure(sample(LETTERS[1:5], N, replace = TRUE), label="Study Identifie"),
    USUBJID = structure(paste0("id-", 1:N), label="Subject Identifier for the Study"),
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

  }, df_clean$VARIABLE_NAME, df_clean$VARIABLE_LABEL, df_clean$VARIABLE_TYPE)


  df
}





