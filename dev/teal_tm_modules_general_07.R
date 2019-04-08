library(random.cdisc.data) #nolint
library(magrittr)
asl  <- radsl(N = 600)
adte <- radtte(asl, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))

adte_filters <- filter_spec(
    vars = c("PARAMCD"), # only key variables are allowed
    sep = " - ",
    choices = c("OS", "PFS", "EFS"),
    selected = "OS",
    multiple = TRUE, # if multiple, then a spread is needed
    label = "Choose endpoint"
)


adte_extracted1 <- data_extract_spec(
    dataname = "ADTE",
    filter = adte_filters,
    columns = columns_spec(
        choices = c("AVAL", "AVALU", "BMRKR1", "SITEID"),
        selected = c("AVAL"),
        multiple = TRUE,
        fixed = FALSE, # Whether the user can select the item (optional)
        label = "Column" # Label the column select dropdown (optional)
    )
)

adte_extracted <- data_extract_spec(
    dataname = "ADTE",
    filter = adte_filters,
    columns = columns_spec(
        choices = c("AVAL", "BMRKR1"),
        selected = c("AVAL"),
        multiple = FALSE,
        fixed = FALSE, # Whether the user can select the item
        label = "" # Label the column select dropdown (optional)
    )
)

asl_extracted <- data_extract_spec(
    dataname = "ASL",
    columns = columns_spec(
        choices = c("SEX", "AGE"),
        selected = c("AGE"),
        multiple = TRUE,
        fixed = FALSE
    )
)
# @end_code

x <- teal::init(
    data = cdisc_data(
        ASL = data_for_teal(
            asl,
            keys = c("USUBJID", "STUDYID"),
            source = get_code("teal_tm_modules_general_07.R")
        ),
        ADTE = data_for_teal(
            adte,
            keys = c("USUBJID", "STUDYID", "PARAMCD"),
            source = get_code("teal_tm_modules_general_07.R")
        )
    ),
    modules = root_modules(
        teal.modules.general::tm_g_regression(
            label = "Regression",
            dataname = c("ASL","ADTE"),
            response = list(adte_extracted),
            regressor = list(
                asl_extracted,
                adte_extracted1
            )
        )
    )
)


shinyApp(x$ui, x$server)
