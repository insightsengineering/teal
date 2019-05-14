# code>
library(teal) #nocode
library(teal.devel) #nolint
library(random.cdisc.data) #nolint
require(magrittr)
library(teal)
set.seed(1)
source("app_source1.R")
source("app_source2.R")
"# this is not a comment" #this is a comment
# "this is a comment"
# <code

x <- teal::init(
  data = cdisc_data(
    ASL = ASL,
    ADTE = adte,
    code = get_code("app.R", exclude_comments = TRUE),
    check = TRUE
  ),
  modules = root_modules(
      tm_made_up_lm(
      label = "Regression",
      dataname = c("ASL", "ADTE"),
      response = list(adte_extracted),
      regressor = list(
        asl_extracted,
        adte_extracted1
      )
    )
  )
)

shinyApp(x$ui, x$server)
