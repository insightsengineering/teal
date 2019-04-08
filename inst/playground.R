## testing
library(random.cdisc.data) #nolint
library(dplyr)
library(magrittr)
devtools::load_all()
source("inst/pr_wip_cdisc_data.R")
source("inst/pr_wip_cdisc_data.R")
file_path <- get_filename()
file_path <- "inst/app.R"
# get_preprocessing_code(files_path = file_path, include_current_file = FALSE, exclude_comments = FALSE) %>% cat

get_preprocessing_code(files_path = file_path, include_current_file = FALSE, exclude_comments = FALSE) %>% cat

x <- 10
y <- 20
z <- 30

adsl <- radsl(N = 100, seed = 1)
ate <- radtte(adsl)

init() # ERROR; just for automatic stop of reading file
# basic example
CDISC_data(ADSL = 1, x = 1, y = 2, z = 3)
# objects created before
CDISC_data(ADSL = 1, x = x, y = y, z = z)
# mix
CDISC_data(ADSL = 1, x = 1, y = y, z = 3)
CDISC_data(ADSL = 1, x = x, y = 2, z = z)
# function in argument call
CDISC_data(ADSL = 1, x = x, y = y, z = z + 2)
# preprocessing in string
CDISC_data(ADSL = xyz, x = x2, z = z2, code = "xyz <- 1; x2 <- 2; z2 <- 3")
# preprocessing in both automatic detection and string
CDISC_data(ADSL = y, x = x2, z = z2, code = paste0("x2 <- 2; z2 <- 3\n", get_preprocessing_code()))
# here error because 'y' is defined in current file and we do not parse current file
CDISC_data(ADSL = y, x = x2, z = z2, code = "x2 <- 2; z2 <- 3")

rm(x, y, z)
CDISC_data(ADSL = 1, y = 2, z = 3, x = x, code = "x <- 2")


# missing ADSL argument
CDISC_data(x = 1, y = 2, z = 3)
CDISC_data(x = 1, y = 2, z = 3, code = "xyz <- 1")
CDISC_data(x = 1, y = 2, z = 3, code = "ADSL <- 1")
CDISC_data()
CDISC_data(code = "xyz <- 1")
CDISC_data(code = "ADSL <- 1")

CDISC_data(adsl)
CDISC_data(adsl %>% mutate(AGE2 = AGE + 2), x = 3)
