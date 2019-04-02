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


my_env <- new.env()

my_chunk1 <- rlang::call2("<-",rlang::sym("x"),1)

my_chunk2 <- rlang::expr(x * 2)

eval(my_chunk1, envir = my_env)

eval(my_chunk2, envir = my_env)

#---- My Chunks ----
library(magrittr)
rm(list = ls())

source("teal.devel/R/chunks.R")


my_chunks <- chunks$new()

y <- 3

init_chunk_environment(chunks = my_chunks)

set_chunk(id="one",expression = rlang::call2("<-",rlang::sym("x"), 2), chunks = my_chunks)
set_chunk(expression = rlang::expr(x * y), chunks = my_chunks)

my_chunks$pop()

stopifnot(length(my_chunks$get_chunks()) == 1)

set_chunk(expression = rlang::expr(x * y), chunks = my_chunks)

set_chunk(id="three", expression = rlang::expr(x * y), chunks = my_chunks)

stopifnot(eval_chunk("one", my_chunks) == 2)

testthat::expect_error(eval_chunk("three", my_chunks), "order")

stopifnot(length(my_chunks$.__enclos_env__$private$remaining) == 2)

stopifnot(my_chunks$.__enclos_env__$private$envir$x == 2)

testthat::expect_error(eval_chunk(chunks = my_chunks), "remaining")

stopifnot(eval_remaining(chunks = my_chunks) == 6)
stopifnot(my_chunks$get_env()$x == 2)

testthat::expect_error(my_chunks$pop())


#----------------------------------------------------- PART2 - PIPES
rm(list = ls())
source("teal.devel/R/chunks.R")
source("teal.devel/R/chunks_pipe.R")


ANL <- data.frame(x = c(1,2),y=c(2,2))
x <- "x"
y <- "y"
session <<- new.env()
session$userData <- new.env()
session$userData$chunks <- chunks$new()

renew_chunk_environment(chunks = session$userData$chunks)

form %<chunk_env%
    as.formula(paste(y,
    paste(x,
        collapse = " + "
    ),
    sep = " ~ "
))

# GOOD
fit %<chunk% rlang::expr(lm(!!form, data = ANL))
session$userData$chunks$get_remaining()
#fit$eval(chunks = session$userData$chunks)

eval_chunk(fit$id, chunks = session$userData$chunks)
session$userData$chunks$get_remaining()
# BAD
fit %<chunk% lm(form, data = ANL)

fit$eval(chunks = session$userData$chunks)

rm(list = ls())

ANL <- data.frame(x = c(1,2),y=c(2,2))
x <- "x"
y <- "y"
my_chunks <- chunks$new()

# Init chunk environment without form
init_chunk_environment(chunks = my_chunks)

form <-
    paste(y,
        paste(x,
            collapse = " + "
        ),
        sep = " ~ "
    )

# substitute form
fit %<chunk% lm(form, data = ANL) %substitute%
    list(form = as.formula(form))

stopifnot(is(eval_remaining(my_chunks),"lm"))
