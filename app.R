
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

ADTE <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))

modified_data <- ASL %>% mutate(A = 1) 

adte_filters <- keys_filter_from_sep(
    vars = c("PARAMCD"), # only key variables are allowed
    sep = " - ",
    choices = c("OS", "PFS", "EFS"),
    selected = "OS", 
    multiple = TRUE, # if multiple, then a spread is needed
    label = "Choose endpoint"
)

# Instead of column_filter overwrite choices_selected
# backwards compatible with new arguments show and label
adte_extracted1 <- data_extract(
    dataname = "ADTE", 
    keys_filtering = adte_filters,
    columns = choices_selected(
            choices =  c("AVAL", "AVALU", "BMRKR1", "SITEID"),
            selected = c("AVAL"),
            multiple = FALSE,
        # optional
        # - choices
        # - selected
        # - multiple
        show = TRUE, # Whether the user can select the item (optional)
        label = "Column" # Label the column select dropdown (optional)
    )
)

adte_extracted <- data_extract(
    dataname = "ADTE", 
    keys_filtering = adte_filters,
    columns =choices_selected(
            choices =  c("AVAL", "BMRKR1"),
            selected = c("AVAL"),
            multiple = FALSE,
        show = TRUE, # Whether the user can select the item
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
            c("USUBJID", "STUDYID", "PARAMCD"),
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