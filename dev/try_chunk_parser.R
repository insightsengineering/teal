#----------------------------------------------------- PART1 - set chunks
library(magrittr)
library(R6)
rm(list = ls())

source("../R/chunks.R")

my_chunks <- chunks$new()

y <- 3

renew_chunk_environment(chunks = my_chunks)

# Add one Chunk with ID
my_chunks$push(id = "one",
    expression = rlang::call2("<-",rlang::sym("x"), 2)
    )
# Try to overwrite this chunk - should fail
set_chunk(id="one",
    expression = rlang::call2("<-",rlang::sym("x"), 2),
    chunks = my_chunks)

# Set a chunk without ID - should get random ID
set_chunk(expression = rlang::expr(x * y), chunks = my_chunks)

#remove the last added chunk
my_chunks$pop()

# have one chunk left
stopifnot(length(my_chunks$get_chunks()) == 1)

# adding random chunk again
set_chunk(expression = rlang::expr(x * y), chunks = my_chunks)

# adding an id chunk again
set_chunk(id="three", expression = rlang::expr(x * y), chunks = my_chunks)

# eval the first chunk
stopifnot(eval_chunk("one", my_chunks) == 2)

# Expect an error if you try to run the third chunk
testthat::expect_error(eval_chunk("three", my_chunks), "order")

# Are two chunks not executed
stopifnot(length(my_chunks$.__enclos_env__$private$remaining) == 2)

# Check if the first chunk was executed and changed the value of x
stopifnot(my_chunks$.__enclos_env__$private$envir$x == 2)

# Evaluation of one chunk is not allowed without an ID
testthat::expect_error(eval_chunk(chunks = my_chunks), "remaining")

# Evaluate all reamining chunks and return the value of the last evaluated
stopifnot(eval_remaining(chunks = my_chunks) == 6)

# Check that x was not changed
stopifnot(my_chunks$get_env()$x == 2)

# You cannot delete a chunk anymore, as everything was evaluated already.
testthat::expect_error(my_chunks$pop())

# Set an environment variable, just a PRO-user function. NEST needs to be provided
my_chunks$set_env_var("x", 5, "NEST")

stopifnot(my_chunks$get_env()$x == 5)

#----------------------------------------------------- PART1.1 get_rcode

# get a list of all stacked code-chunks R-Code
my_chunks$get_rcode()

# get a single one - e.g. "one"
my_chunks$get_rcode_id("one")

rm(list=c("my_chunks"))

my_chunks <- chunks$new()

my_chunks$push("one",
    substitute(y <- x, list(x = 5))
    )

my_chunks$get_rcode()

# Do not replace x by 5
my_chunks <- chunks$new()
my_chunks$set_env_var("x", 5, "NEST")

my_chunks$push("one",
    bquote(y <- x)
    )
my_chunks$get_rcode()

# Get x replaced by 5 with rlang
my_chunks <- chunks$new()
x <- 5
my_chunks$set_env_var("x", 5, "NEST")
my_chunks$push(expression = rlang::expr({y <- !!x}))
my_chunks$get_rcode()


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

# chunk_env does exactly the same as set_env_var
# it also sets the variable in the current environment
form %<chunk_env%
    as.formula(paste(y,
    paste(x,
        collapse = " + "
    ),
    sep = " ~ "
))

# GOOD
# Fit is a link to the right chunk afterwards
fit %<chunk% rlang::expr(lm(!!form, data = ANL))
# get nice Rcode out from the session chunks
session$userData$chunks$get_rcode()

session$userData$chunks$get_remaining()

# test that the evaluation works from the return value
# wich is just a link
stopifnot(class(fit$eval(chunks = session$userData$chunks)) == "lm")

# get Rcode by chunk link
stopifnot(
    get_code_chunk(
        session$userData$chunks$latest,
        chunks=session$userData$chunks) == "lm(y ~ x, data = ANL)")


stopifnot(is.null(eval_chunk(fit$id, chunks = session$userData$chunks)))

stopifnot(length(session$userData$chunks$get_remaining()) == 0)

# BAD
# People can set variables like form, that are never shown
# as R-Code. So they can kill reproducibilty
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

# just set form outside
form <-
    paste(y,
        paste(x,
            collapse = " + "
        ),
        sep = " ~ "
    )

# substitute form
# equal to subsitute(lm(form, data = ANL), list(form = as.formula(form)))
# currently broken
fit %<chunk% lm(form, data = ANL) %substitute%
    list(form = as.formula(form))

fit$eval(chunks = session$userData$chunks)

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
