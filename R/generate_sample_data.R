
#' Generate a Sample Dataset
#'
#' This dataset generator function is useful to produce
#'
#'
#' @param type case insensitiv, specify which data to generate
#'
#' @export
#'
#' @importFrom dplyr %>% arrange
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

  STREAM_DAP_M3[["ASL"]]$`VARIABLE NAME`[which( STREAM_DAP_M3[["ASL"]]$`VARIABLE NAME`=="SUBJID ")] <- "SUBJID"
  tvn <- STREAM_DAP_M3[[type]]$`VARIABLE NAME`

  df_raw <-  subset(STREAM_DAP_M3[[type]], !is.na(tvn) & toupper(tvn) == tvn,
                    select=c("VARIABLE NAME", "VARIABLE LABEL", "VARIABLE TYPE"))


  names(df_raw) <- gsub(" ", "_", names(df_raw))


  is_dupli <- duplicated(df_raw$VARIABLE_NAME)

  df_clean_tmp <- Reduce(rbind, Map(function(i) {

    x <- df_raw[!is_dupli,][i,]

    switch(
      x$VARIABLE_NAME,
      "USUBJID" = {x$VARIABLE_LABEL <- "Unique Subject Identifier"; x$VARIABLE_TYPE <- "Char"},
      "STUDYID" = {x$VARIABLE_LABEL <- "Study Identifier"; x$VARIABLE_TYPE <- "Char"},
      "SUBJID" = {x$VARIABLE_LABEL <- "Subject Identifier for the Study"; x$VARIABLE_TYPE <- "Char"}
    )

    if (is.na(x$VARIABLE_TYPE)) x$VARIABLE_TYPE <- "Char"

    x
  } , 1:sum(!is_dupli)))

  df_clean <- df_clean_tmp[!df_clean_tmp$VARIABLE_NAME %in% c("USUBJID", "STUDYID", "SUBJID"), ]


  USUBJID <- paste0("id-", 1:N)
  STUDYID <- sample(LETTERS[1:5], N, replace = TRUE)
  SUBJID <- paste0("id-", 1:N)

  nvisits <- round(rexp(N, 0.25)) + 1 # Needed for ARS
  T <- 10 + vapply(rnorm(N)*100, function(x) max(x,0), numeric(1)) # Needed for ASL and ARS

  if (type == "ASL") {

    BAGE <- sample(18:80, N, replace = TRUE)
    BWT <- sample(50:125, N, replace = TRUE)
    BHT <- sample(100:200, N, replace = TRUE)
    TRTSDT <- as.Date("2010-01-01")
    COMPSDT <- TRTSDT + max(T)
    TRT01P <- sample(c("Drug 5mg", "Drug 10mg", "Placebo"), N, replace = TRUE)
    TRT01PN <- factor(TRT01P, labels = c(1, 2, 3))
    TRT01A <- sample(c("D5mg", "D10mg", "Pb"), N, replace = TRUE)
    TRT01AN <- factor(TRT01A, labels = c(1, 2, 3))

    df <- data.frame(STUDYID = STUDYID,
                     USUBJID = USUBJID,
                     SUBJID = USUBJID,
                     SEX = sample(c('M', 'F', 'U', 'UNDIFFERENTIATED'), N, replace=TRUE, prob = c(.46,.46,.04,.04)),
                     AGE = rbinom(N, 60, 0.5) + 20,
                     RACE = sample(c("BLACK OR AFRICAN AMERICAN", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN", "WHITE"), N, replace=TRUE),
                     BAGE = BAGE,
                     BAGEU = "YEARS",
                     AGE65 = ifelse(BAGE >= 65, ">= 65", "< 65"),
                     AGEGRP = ifelse(BAGE >= 65, ">= 65", "< 65"),
                     IBRTHDT = TRTSDT - BAGE*365,
                     IBRTHDTF = "",
                     BWT = BWT,
                     BWTU = "kg",
                     BHT = BHT,
                     BHTU = "cm",
                     BTEMP = sample(36:38, N, replace = TRUE),
                     BTEMPU = "C",
                     BBMI = BWT / (BHT/100)^2,
                     ITTFL = "Y",
                     SAFFL = "Y",
                     TRTSDT = TRTSDT,
                     TRTSDTM = paste0(TRTSDT, " 00:00:00"),
                     TRTSTMF = "HMS",
                     INFCODT = TRTSDT - 1,
                     RANDDT = TRTSDT - 1,
                     TRTEDTM = paste0(TRTSDT + T, " 00:00:00"),
                     TRTETMF = "HMS",
                     TRTDUR = T,
                     COMPSTUD = "Y",
                     DISCSTUD = "N",
                     STDDRS = NA,
                     STDSSDT = NA,
                     COMPSDT = COMPSDT,
                     DISCDEAT = "N",
                     DISCAE = "N",
                     COMPSFU = "Y",
                     COMPTRT = "Y",
                     DISTRTFL = "",
                     TRTDRS = NA,
                     TRT01P = TRT01P,
                     TRT01PN = TRT01PN,
                     TRT01A = TRT01A,
                     TRT01AN = TRT01AN,
                     TRTSEQP = TRT01P,
                     TRTSEQPN = TRT01PN,
                     TRTSEQA = TRT01A,
                     TRTSEQAN = TRT01AN,
                     REGION = "CANADA",
                     AEWITHFL = "",
                     ALIVDT = COMPSDT,
                     stringsAsFactors = F, row.names = NULL)

    df <- df %>% arrange(STUDYID, USUBJID, SUBJID)

    Map(function(name) {

      lab <- df_clean_tmp$VARIABLE_LABEL[match(name, df_clean_tmp$VARIABLE_NAME)[1]]

      df[[name]] <<- structure(
        df[[name]],
        label = lab
      )
    }, names(df))

  } else if (type == "ARS") {

    paramcd_lookup <- setNames(c("Best Confirmed Overall Response - Investigator",
                                 "Best Confirmed Overall Response - IRF Vendor 1",
                                 "Overall Response by Visit - Investigator ",
                                 "Overall Response by Visit - IRF Vendor 1 (Reader 1)",
                                 "Sum of Longest Diameter by Investigator",
                                 "Sum of Longest Diameter - Change from Baseline by Investigator",
                                 "Sum of Longest Diameter - Percent Change from Baseline by Investigator"),
                               c("BESRSPI", "BESRSPI1", "OVRINV", "OVRRAD11", "SLD", "CHSLD", "PCHSLD"))

    out <- Map(function(studyid, usubjid, nvisit, t) {

      ADY <- round(runif(nvisit) * t)
      ADT <- as.character(as.Date("2010-01-01") + ADY)
      ADTM <- paste0(ADT, " 00:00:00")

      AVISIT <- paste0("VISIT ", 1:nvisit)
      AVISITN <- 1:nvisit

      AVAL <- cumsum(round(runif(length(ADY), -1.5, 1.5)))

      AVALC <- sample(c("CR", "PR", "SD", "PD", "NE"), nvisit, replace = TRUE)

      baseline <- rexp(1, 2)*100

      ovrinv <- data.frame(STUDYID = studyid,
                           USUBJID = usubjid,
                           PARAMCAT = "Tumor Response",
                           PARAM = paramcd_lookup["OVRINV"],
                           PARAMCD = "OVRINV",
                           AVAL = AVAL,
                           AVALC = AVALC,
                           ADT = ADT,
                           ADTF = "",
                           ADTM = ADTM,
                           ATMF = "HMS",
                           ADY = ADY,
                           APERIOD = 1,
                           APERIODC = "Treatment Period",
                           AVISIT = AVISIT,
                           AVISITN = AVISITN,
                           ANLFL = "Y",
                           DERV = "",
                           stringsAsFactors = F, row.names = NULL)

      ovrrad11 <- ovrinv
      ovrrad11$PARAMCD <- "OVRRAD11"
      ovrrad11$PARAM <- paramcd_lookup["OVRRAD11"]

      besrspi <- data.frame(STUDYID = studyid,
                            USUBJID = usubjid,
                            PARAMCAT = "Tumor Response",
                            PARAM = paramcd_lookup["BESRSPI"],
                            PARAMCD = "BESRSPI",
                            AVAL = tail(AVAL, 1),
                            AVALC = tail(AVALC, 1),
                            ADT = tail(ADT, 1),
                            ADTF = "",
                            ADTM = tail(ADTM, 1),
                            ATMF = "HMS",
                            ADY = tail(ADY, 1),
                            APERIOD = 1,
                            APERIODC = "Treatment Period",
                            AVISIT = tail(AVISIT, 1),
                            AVISITN = tail(AVISITN, 1),
                            ANLFL = "Y",
                            DERV = "Y",
                            stringsAsFactors = F, row.names = NULL)

      besrspi1 <- besrspi
      besrspi1$PARAMCD <- "BESRSPI1"
      besrspi1$PARAM <- paramcd_lookup["BESRSPI1"]

      sld <- data.frame(STUDYID = studyid,
                        USUBJID = usubjid,
                        PARAMCAT = "Tumor Response",
                        PARAM = paramcd_lookup["CHSLD"],
                        PARAMCD = "CHSLD",
                        AVAL = baseline + cumsum(AVAL),
                        AVALC = NA,
                        ADT = NA,
                        ADTF = "",
                        ADTM = NA,
                        ATMF = "HMS",
                        ADY = ADY,
                        APERIOD = NA,
                        APERIODC = "",
                        AVISIT = NA,
                        AVISITN = NA,
                        ANLFL = "Y",
                        DERV = "Y",
                        stringsAsFactors = F, row.names = NULL)

      chsld <- sld
      chsld$PARAMCD <- "CHSLD"
      chsld$PARAM <- paramcd_lookup["CHSLD"]
      chsld$AVAL <- cumsum(AVAL)

      pchsld <- chsld
      pchsld$PARAMCD <- "PCHSLD"
      pchsld$PARAM <- paramcd_lookup["PCHSLD"]
      pchsld$AVAL <- cumsum(AVAL) / baseline

      rbind(ovrinv, ovrrad11, besrspi, besrspi1, sld, chsld, pchsld)

    }, STUDYID, USUBJID, nvisits, T)

    df <- do.call(rbind, out) %>% arrange(STUDYID, USUBJID, PARAMCD, APERIOD, ADTM, AVISITN)

    Map(function(name) {

      lab <- df_clean_tmp$VARIABLE_LABEL[match(name, df_clean_tmp$VARIABLE_NAME)[1]]

      df[[name]] <<- structure(
        df[[name]],
        label = lab
      )
    }, names(df))

  } else {

    df <- data.frame(
      USUBJID = structure(USUBJID, label="Unique Subject Identifier"),
      STUDYID = structure(STUDYID, label="Study Identifie"),
      SUBJID = structure(SUBJID, label="Subject Identifier for the Study"),
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

  }

  structure(
    df,
    md5sum = setNames("-", paste0("generate_sample_data('", type, "')")),
    import = paste0("generate_sample_data('", type, "')")
  )
}
