app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

# Input '`teal_modules_root.Data_Table-ASL-data_table_rows_current`' was set, but doesn't have an input binding.
# Input '`teal_modules_root.Data_Table-ASL-data_table_rows_all`' was set, but doesn't have an input binding.
app$setInputs(`teal_modules.root` = "White Data Table")
app$setInputs(`teal_modules_root.White_Data_Table-variable2-ADTE-filter` = c("OS", "PFS"))
app$setInputs(`teal_modules_root.White_Data_Table-variable2-ds` = "ASL")
app$setInputs(`teal_modules_root.White_Data_Table-variable2-ASL-column` = "SEX")
app$setInputs(`teal_modules_root.White_Data_Table-variable2-ASL-column` = "AGE")
app$snapshot()
