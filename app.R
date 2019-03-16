
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#------------------ App Code -----------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
tryCatch(unloadNamespace("teal.modules.general"))
tryCatch(unloadNamespace("teal.modules.clinical"))
tryCatch(unloadNamespace("teal.devel"))
tryCatch(unloadNamespace("teal"))
devtools::load_all("../teal")
devtools::load_all("../teal.devel")
library(magrittr)
library(random.cdisc.data)
library(dplyr)


ASL <- radsl(N = 600)

ADTE <- radaette(ASL)

modified_data <- ASL %>% mutate(A = 1) 

adte_filters <- keys_filter_from_sep(
    vars = c("PARAMCD", "BMRKR2"), # only key variables are allowed
    sep = " - ",
    choices = c("AETTE1 - LOW", "AETTE1 - LOW", "AETTE2 - MEDIUM"),
    selected = "AETTE1 - LOW", 
    multiple = FALSE # if multiple, then a spread is needed
)

# Instead of column_filter overwrite choices_selected
# backwards compatible with new arguments show and label
adte_extracted1 <- data_extract(
    dataname = "ADTE", 
    keys_filtering = adte_filters,
    columns = choices_selected(
            choices =  c("AVAL", "AVALU"),
            selected = c("AVAL"),
            multiple = FALSE,
        # optional
        # - choices
        # - selected
        # - multiple
        show = FALSE, # Whether the user can select the item (optional)
        label = "" # Label the column select dropdown (optional)
    )
)

adte_extracted <- data_extract(
    dataname = "ADTE", 
    keys_filtering = adte_filters,
    columns =choices_selected(
            choices =  c("AVAL"),
            selected = c("AVAL"),
            multiple = FALSE,
        show = FALSE, # Whether the user can select the item
        label = "" # Label the column select dropdown (optional)
    )
)

asl_extracted <- data_extract(
    dataname = "ASL", 
    columns =choices_selected(
            choices =  c("SEX", "AGE"),
            selected = c("AGE"),
            multiple = FALSE,
            show = TRUE
        )
    )

x <- teal::init(
    data = CDISC_data(
        ASL = data_for_teal(
            modified_data,
            c("USUBJID", "STUDYID"),
            "radsl(N = 600)  %>% dplyr::mutate(A = 1)"),
        ADTE = data_for_teal(
            ADTE,
            c("USUBJID", "STUDYID", "PARAMCD", "BMRKR2"),
            "radaette(radsl(N = 600))")
    ),
    modules = root_modules(
        teal.modules.general::tm_data_table("Data Table"),
        tm_made_up(
            label = "Qplot",
            dataname = c("ASL","ADTE"),
            response = adte_extracted,
            regressor = list(
                adte_extracted1,
                asl_extracted
            )
        )
    )
)

shinyApp(x$ui, x$server)