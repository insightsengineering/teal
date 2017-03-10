
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
      data.frame(
        STUDYID = rep(LETTERS[1:4], each = 10),
        USUBJID = paste0("id-", 1:40),
        stringsAsFactors = FALSE
      )
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
