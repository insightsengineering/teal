#----------------------------------------------------- PART1 - set chunks
library(magrittr)
rm(list = ls())

source("../R/chunks.R")

my_chunks <- chunks$new()

y <- 3

renew_chunk_environment(chunks = my_chunks)

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
source("../R/chunks.R")
source("../R/chunks_pipe.R")


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

stopifnot(class(fit$eval(chunks = session$userData$chunks)) == "lm")
stopifnot(
    get_code_chunk(
        session$userData$chunks$latest,
        chunks=session$userData$chunks) == "lm(y ~ x, data = ANL)")


stopifnot(is.null(eval_chunk(fit$id, chunks = session$userData$chunks)))

stopifnot(length(session$userData$chunks$get_remaining()) == 0)
# BAD
fit %<chunk% lm(form, data = ANL)
stopifnot(
    get_code_chunk(
        session$userData$chunks$latest,
        chunks=session$userData$chunks) == "lm(form, data = ANL)")

stopifnot(class(fit$eval(chunks = session$userData$chunks)) == "lm")


#----------------------------------------------------- PART3 - PIPES WITH SUBS
rm(list = ls())

ANL <- data.frame(x = c(1,2),y=c(2,2))
x <- "x"
y <- "y"
source("../R/chunks.R")
source("../R/chunks_pipe.R")
session <<- new.env()
session$userData <- new.env()
session$userData$chunks <- chunks$new()

# Init chunk environment without form
renew_chunk_environment(chunks = session$userData$chunks)

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

#----------------------------------------------------- PART3.1 - SUBS without pipes
rm(list = ls())

ANL <- data.frame(x = c(1,2),y=c(2,2))
x <- "x"
y <- "y"
source("../R/chunks.R")
source("../R/chunks_pipe.R")
session <<- new.env()
session$userData <- new.env()
session$userData$chunks <- chunks$new()

# Init chunk environment without form
renew_chunk_environment(chunks = session$userData$chunks)

form <-
    paste(y,
        paste(x,
            collapse = " + "
        ),
        sep = " ~ "
    )

# substitute form
fit %<chunk% substituteDirect(
    lm(form, data = ANL),
    list(
        form = form)
    )

stopifnot(class(fit$eval(chunks = session$userData$chunks)) == "lm")
