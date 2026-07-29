# In-App Data

## Building data in the App

Typically the data that is passed into a `teal` application is available
before the app is run. However, this is not always true and in some
cases the data will be built only after the app has started. A good
example is pulling the data from an external repository, like a
database, or uploading a file. Additional authentication may be
required.

### `teal_data_module`

Preprocessing actions can be performed in-app using the
`teal_data_module`. Rather than passing a `teal_data` object to the app,
one may pass a *`shiny` module* that *returns* a `teal_data` object
(wrapped in a reactive expression). This allows the app developer to
include user actions data creation, fetching, and even pre-filtering
modification.

## Further reading

A complete explanation of using the `teal_data_module` can be found in
[this `teal`
vignette](https://insightsengineering.github.io/teal/articles/data-as-shiny-module.md)
