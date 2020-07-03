# code ADTTE>
# comment in source 3
adtte_extracted1 <- data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = c("AVAL", "AVALU", "BMRKR1", "SITEID"),
    selected = c("AVAL"),
    multiple = TRUE,
    fixed = FALSE, # Whether the user can select the item (optional)
    label = "Column" # Label the column select dropdown (optional)
  )
)

adtte_extracted <- data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = c("AVAL", "BMRKR1"),
    selected = c("AVAL"),
    multiple = FALSE,
    fixed = FALSE, # Whether the user can select the item
    label = "" # Label the column select dropdown (optional)
  )
)
# <ADTTE code

# code ADSL>
adsl_extracted <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = c("SEX", "AGE"),
    selected = c("AGE"),
    multiple = TRUE,
    fixed = FALSE
  )
)
# <ADSL code
