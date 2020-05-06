#source("https://raw.github.roche.com/NEST/nest_on_bee/master/bee_nest_utils.R")
#bee_use_nest(release = "UAT_2020_04_09")

#source("/home/bceuser/mordigm/.Rprofile")
# uses my own R profile which refers to latest release

# uses my own R profile which refers to latest release
library(random.cdisc.data)

options(shiny.fullstacktrace = TRUE)
options(teal_logging = TRUE) # to log to console

ADSL <- radsl(cached = TRUE)
ADSL$logical_test <- sample(c(TRUE, FALSE, NA), size = nrow(ADSL), replace = TRUE)

ADSL$SEX[1:150] <- NA
#ADSL <- tern::df_explicit_na(ADSL)

ADAE <- radae(cached = TRUE)
ADLB <- radlb(cached = TRUE)

# debugging example ----

# todo: error: check for code argument below is not performed
#FilteredData$debug("set_filter_chars");
devtools::load_all("/home/bceuser/mordigm/scratch/teal"); app <- init(
  data = cdisc_data(
    cdisc_dataset(dataname = "ADSL", data = ADSL),
    cdisc_dataset(dataname = "ADAE", data = ADAE),
    cdisc_dataset(dataname = "ADLB", data = ADLB),
    code = "
    # todo: there is an error, data is not checked to agree with current data
ADSL <- radsl(cached = TRUE)
ADAE <- radae(cached = TRUE)
ADLB <- radlb(cached = TRUE)
"),
  modules = root_modules(
    #teal:::filter_calls_module()
    teal:::bookmark_module()
  ),
  initial_filter_states = list(ADSL = list(SEX = list(choices = "M", keep_na = TRUE))),
  header = "Simple teal app",
  footer = tags$p(class = "text-muted", "Source: agile-R website")
); shinyApp(app$ui, app$server, enableBookmarking = "url")
# withr::with_options(list(warn = 2), shinyApp(app$ui, app$server))
# options(warn = 2); shinyApp(app$ui, app$server)
# options(warn = 0)

debugonce(datasets$print_filter_info)


# need to call this after devtools::load_all() and before creating object of this class
FilteredData$debug("set_filter_state")

# many modules example ----

# test nested teal modules
devtools::load_all("/home/bceuser/mordigm/scratch/teal"); app <- init(
  data = cdisc_data(
    cdisc_dataset(dataname = "ADSL", data = ADSL),
    cdisc_dataset(dataname = "ADAE", data = ADAE),
    cdisc_dataset(dataname = "ADLB", data = ADLB),
    code = "
    # todo: there is an error, data is not checked to agree with current data
ADSL <- radsl(cached = TRUE)
ADAE <- radae(cached = TRUE)
ADLB <- radlb(cached = TRUE)
"),
  # modules = teal:::filter_calls_module("D1"),
  modules = root_modules(
    teal:::bookmark_module("B1"),
    # modules(label = "A1", teal:::filter_calls_module("D1.1", active_datanames = c("ADSL", "ADAE")), teal:::filter_calls_module("D1.2")),
    # modules(label = "Reset", teal:::reset_filters_module("R1", active_datanames = c("ADSL", "ADLB"))),
    # modules(label = "A2", teal:::filter_calls_module("D2", active_datanames = c("ADSL"))),
    teal:::filter_calls_module("D3", active_datanames = c("ADAE"))
  ),
  # modules = root_modules(
  #   teal:::filter_calls_module("D1")
  # ),
  #initial_filter_states = list(ADSL = list(SEX = NULL)),
  #initial_filter_states = list(ADSL = list(SEX = list(choices = "M", keep_na = TRUE))),
  #initial_filter_states = list(ADSL = list(AGE = "default", SEX = list(choices = "M", keep_na = TRUE))),
  #initial_filter_states = list(ADSL = list(AGE = "default", SEX = list(choices = "M", keep_na = TRUE)), ADAE = list(AETOXGR = "default")),
  header = "Simple teal app",
  footer = tags$p(class = "text-muted", "Source: agile-R website")
); shinyApp(app$ui, app$server, enableBookmarking = "server")
# old: isolate(app$ui_datasets$set_filter_state("ADSL", "SEX", list(choices = "M", keep_na = TRUE)));


# delayed loading example ----

cdisc_data_global <- cdisc_data(
  cdisc_dataset(dataname = "ADSL", data = ADSL),
  cdisc_dataset(dataname = "ADAE", data = ADAE),
  cdisc_dataset(dataname = "ADLB", data = ADLB),
  code = "
    # todo: there is an error, data is not checked to agree with current data
ADSL <- radsl(cached = TRUE)
ADAE <- radae(cached = TRUE)
ADLB <- radlb(cached = TRUE)
")
# ignores seed, todo
radsl_fast <- function(...) radsl(cached = TRUE) # todo: error: function not found, i.e. evaluated in wrong environment?
devtools::load_all("../teal"); app <- init(
  data = rcd_cdisc_data(
    rcd_dataset("ADSL", radsl, cached = FALSE),
    #rcd_dataset("ADSL", radsl_fast, cached = FALSE),
    code = "ADSL$SEX <- as.factor(ADSL$SEX)"
  ),
  modules = root_modules(
    teal:::filter_calls_module(),
    teal:::bookmark_module(),
    teal:::debug_browser_module()
  ),
  #filter = NULL,
  header = tags$h1("Sample App")
); shinyApp(app$ui, app$server, enableBookmarking = "url")




devtools::load_all();
toString(teal:::filter_calls_module("D3"), indent = 2)
debugonce(toString.teal_modules)
modules(label = "A1", teal:::filter_calls_module("D1.1"), teal:::filter_calls_module("D1.2"))

modules(label = "A2", teal:::filter_calls_module("D2"))
root_modules(
  modules(label = "A1", teal:::filter_calls_module("D1.1"), teal:::filter_calls_module("D1.2")),
  modules(label = "A2", teal:::filter_calls_module("D2")),
  teal:::filter_calls_module("D3")
)

debugonce(toString.teal_modules)
toString(modules(label = "A1", teal:::filter_calls_module("D1.1"), teal:::filter_calls_module("D1.2")))



shiny:::hasCurrentRestoreContext
shiny:::ShinySession$public_methods$doBookmark
