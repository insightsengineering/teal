library(rlang)
library(purrr)
library(stringr)
library(stringi)
library(magrittr)
devtools::load_all("teal.devel", export_all = FALSE)

rm(list = ls())

#-- Function definition --
global_chunks <<- chunks$new()
locate_variables <- function(string, pattern){
  tryCatch(
      stringr::str_extract_all(string, pattern, simplify = TRUE) %>% unique %>% stri_remove_empty,
      error = function(e){
        NULL
      }
      )
}
length_check <- function(x){
  length(x)>0
}

is_teal_data <- function(x){

  if (is.data.frame(x)) {
    # Teal data || merged_data
    is(x, "FilteredData") || !is.null(attr(x, "keys")) || !is.null(attr(x, "source"))
  }else{
    FALSE
  }

}

get_variables_from_call <- function(input_call, envir = parent.frame()){
  variables_to_define <- lapply(ls(envir),
      locate_variables,
      string = as.character(input_call)
  )
  variables_to_define <- Filter(f = length_check, variables_to_define)
  variables_to_define <- map(variables_to_define, ~ unique(.))

  variables_to_define %<>% map(~ get(., envir)) %>% setNames(variables_to_define)

  teal_ds_in_variables <- Filter(is_teal_data, variables_to_define) %>% names()

  if (length(teal_ds_in_variables) == 1){

    variables_to_define$dataset <- variables_to_define[[teal_ds_in_variables]]
    variables_to_define$dataname <- teal_ds_in_variables

  }

  return(variables_to_define)

}

#' @importFrom rlang call2
#' @importFrom magrittr %>%
pipe_chunk <- function()
{

  function(lhs,rhs){
    pipe <- as.character(match.call()[[1]]) %>% gsub(pattern="%", replacement = "")

    chunk_name <- match.call()[[2]] %>% as.character
    right_hand_side <- match.call()[[3]]

    # chunk with assignment
    if (pipe == "<chunk>"){
      p <- parent.frame()
      attr(rhs, "from_chunk") <- TRUE
      p[[chunk_name]] <- rhs
      right_hand_side <- call2("<-",sym(chunk_name),right_hand_side)
    }

    variable_list <- get_variables_from_call(right_hand_side, parent.frame())

    if("dataset" %in% names(variable_list) && "dataname" %in% names(variable_list)){
      right_hand_side <- right_hand_side %>% deparse() %>%
          gsub(pattern = variable_list$dataname, replacement = "dataset")  %>%
          parse_expr()
    }

    set_chunk(
        id = chunk_name,
        expr = right_hand_side,
        vars = variable_list,
        chunks = global_chunks
        )

  }
}
`%chunk>%` <- pipe_chunk()

`%<chunk>%` <- pipe_chunk()

# Evaluation as user sees it:

merged_dataset <- data.frame(x = c(1,2), y = c(2,2))

attr(merged_dataset, "keys") <- "x"

fit %<chunk>%
  lm(as.formula("x ~ x"), data = merged_dataset)

#fit <- lm(as.formula("x ~ x"), data = merged_dataset)

plot_type <- "Response vs Regressor"

i <- which(plot_type == c(
        "Residuals vs Fitted",
        "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
        "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
    ))

if (plot_type == "Response vs Regressor") {
  if (ncol(fit$model) > 1) {
    validate(need(dim(fit$model)[2] < 3, "Response vs Regressor is not provided for >2 Regressors"))
    plot %chunk>%
        plot(fit$model[, 2:1])
  } else {
    plot %chunk>% {
      plot_data <- data.frame(fit$model[, 1], fit$model[, 1])
      names(plot_data) <- rep(names(fit$model), 2)
      plot(plot_data)
      abline(merged_dataset)
    }
  }
} else {
  plot %chunk>%
      plot(merged_dataset, which = i, id.n = NULL)
}

# EVALUATION PERFORMED THAT SHOULD GIVE PLOT
global_chunks$code_chunks$plot$eval()
print(global_chunks$code_chunks$fit$eval())
print(global_chunks$code_chunks$fit$get_rcode())
