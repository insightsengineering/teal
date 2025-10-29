# teal `v1.1.0` Migration Guide for modules

`teal` `v1.1.0` introduces several new features that may require minor updates to existing modules.
These changes simplify module development, improve reproducibility, and provide a unified reporting interface.

### Key changes

#### 🆕 A new `teal_reporter` object is passed to the module server functions

> Purpose: Enables modules to record outputs and custom content for reproducible reports.

- The new class inherits from `teal_data`, keeping all existing functionality and adds a `teal_card` slot to store a report (`teal_card` object).
- The conversion from `teal_data` to `teal_reporter` happens internally in `teal`, so app and module developers do not need to worry about it.
- Outputs from the code evaluation are captured in order and displayed in the reporter.
- It allows modules to add manual content to the report cards, such as titles and descriptions.

#### 🆕 `teal_card` object is now used to represent report cards in the reporter

> Purpose: Simplify the structure and management of report cards by directly storing R objects without a wrapper class.

- This new class stores a list of R objects that make up the report card, such as plots, tables, and text.
- Each object of the list is an R object, such as `character`, `ggplot`, `data.frame`, etc.

The previous `ReportCard` R6 class is now deprecated and will be removed in a future release.

See the section ["Adding arbitrary markdown content to the reporter"](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/teal-report-class.html#adding-content-to-the-teal_report) for more details on how to use the new `teal_card` class.

#### 🆕 `teal` always displays the Reporter buttons in the main UI

> Purpose: Provide a clear and consistent API for defining reporting features.

- The enabling of the reporter functionality is now determined by the `teal::init(reporter = NULL)` argument.
  - and not by the presence of `reporter` argument in the module's server function.
- To disable it for specific modules while keeping the UI consistent, wrap them with `teal::disable_report()`.

#### 🆕 Code reproducibility is now handled automatically by `teal`

> Purpose: Promote code reproducibility in modules and simplify module development.

- The `Show R code` button is always displayed, but becomes active only when the module returns a `qenv`-based object (such as `teal_data` or `teal_reporter`).
- To disable this globally use R option `options(teal.show_src = FALSE)`.
  - Alternatively, use the wrapper function `teal::disable_src()` around a single module or group.

### Backward compatibility

Existing modules will continue to function as before.
The new `Add to Report` and `Show R code` buttons will appear in the UI but remain disabled until the module is updated.

### Migration steps for module developers

Follow these steps to update your custom modules and take advantage of the new reporter and code reproducibility features (only required for modules that need reporter or reproducibility integration).

To take advantage of the new features module developers may need to make the following adjustments:

1. Return the modified `data` argument at the end of the server function  and make sure it's reactive
2. Remove `Add to Report` and `Show R code` buttons from module's UI
3. Remove `reporter` and `filter_panel_api` arguments from the module's server function
4. Add required title and extra content to the reporter during code evaluation with the help of `teal_card` function
5. Ensure that the code evaluation generates outputs

Here is an example of how the `tm_a_regression` module was updated  in `teal.modules.general` package:

### 1. Return the modified `data` argument at the end of the server function

For new modules this is the only required step to enable the new features.

```diff
@@ srv_a_regression <- function(id,
       paste(utils::capture.output(summary(fitted()))[-1], collapse = "\n")
     })
+    set_chunk_dims(pws, decorated_output_q)
   })
 }
```

### 2. Remove "Add to Report" and "Show R code" buttons from module's UI

```diff
@@ ui_a_regression <- function(id, ...) {
       tags$div(verbatimTextOutput(ns("text")))
     )),
     encoding = tags$div(
-      ### Reporter
-      teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
-      tags$br(), tags$br(),
-      ###
       tags$label("Encodings", class = "text-primary"), tags$br(),
       teal.transform::datanames_input(args[c("response", "regressor")]),
       teal.transform::data_extract_ui(
@@ ui_a_regression <- function(id, ...) {
         )
       )
     ),
-    forms = tagList(
-      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
-    ),
     pre_output = args$pre_output,
     post_output = args$post_output
   )
```

### 3. Remove the `reporter` and `filter_panel_api` arguments in the server

Remove the arguments and their usage:

```diff
@@ ui_a_regression <- function(id, ...) {
 # Server function for the regression module
 srv_a_regression <- function(id,
                              data,
-                             reporter,
-                             filter_panel_api,
                              response,
                              regressor,
                              plot_height,
@@ srv_a_regression <- function(id,
                              ggplot2_args,
                              default_outlier_label,
                              decorators) {
-  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
-  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
   checkmate::assert_class(data, "reactive")
   checkmate::assert_class(isolate(data()), "teal_data")
   moduleServer(id, function(input, output, session) {
```

The server logic that handles the reporter and R code buttons should also be removed.

```diff
@@ srv_a_regression <- function(id,
       paste(utils::capture.output(summary(fitted()))[-1], collapse = "\n")
     })

-    # Render R code.
-    source_code_r <- reactive(teal.code::get_code(req(decorated_output_q())))
-
-    teal.widgets::verbatim_popup_srv(
-      id = "rcode",
-      verbatim_content = source_code_r,
-      title = "R code for the regression plot",
-    )
-
-    ### REPORTER
-    if (with_reporter) {
-      card_fun <- function(comment, label) {
-        card <- teal::report_card_template(
-          title = "Linear Regression Plot",
-          label = label,
-          with_filter = with_filter,
-          filter_panel_api = filter_panel_api
-        )
-        card$append_text("Plot", "header3")
-        card$append_plot(plot_r(), dim = pws$dim())
-        if (!comment == "") {
-          card$append_text("Comment", "header3")
-          card$append_text(comment)
-        }
-        card$append_src(source_code_r())
-        card
-      }
-      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
-    }
-    ###
   })
```

Note that in the released `tm_a_regression` we are saving the plot size with a private utility function `set_chunk_dims()`.
This takes the plot dimension size from the `plot_with_settings` widget (`teal.widgets::plot_with_settings_srv()`) and stores it in the metadata of the last `teal_card()` element.

### 4. Add required title and extra content to the reporter during code evaluation

Note that we are adding a header named `Module's output(s)` to the report card using the `teal_card` function.

```diff
@@ srv_a_regression <- function(id,
       )
     })

-    qenv <- reactive(
-      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr")') # nolint quotes
-    )
+    qenv <- reactive({
+      obj <- data()
+      teal.reporter::teal_card(obj) <-
+        c(
+          teal.reporter::teal_card(obj),
+          teal.reporter::teal_card("## Module's output(s)")
+        )
+      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr")') # nolint: quotes
+    })

     anl_merged_q <- reactive({
       req(anl_merged_input())
```
`teal.reporter::teal_card(obj)` is reused as it already contains data filtering code and the code passed to `teal_data` object, before it was used in `teal::init(data = teal_data)`
### 5. Ensure that the code evaluation generates outputs

Here we modify the code evaluation to store the summary output in a variable `fit_summary` that is then returned.

We also add a header named `Plot` to the report card using the `teal_card` function as the next code evaluation generates a plot (in the decorators logic).

```diff
@@ srv_a_regression <- function(id,
             )
           }
         })) %>%
-        teal.code::eval_code(quote(summary(fit)))
+        teal.code::eval_code(quote({
+          fit_summary <- summary(fit)
+          fit_summary
+        }))
+      teal.reporter::teal_card(anl_fit) <- c(teal.reporter::teal_card(anl_fit), "### Plot")
+      anl_fit
     })
```

### Summary

The new features simplify how modules generate and manage reportable outputs.
By updating existing modules as described above, developers gain automatic integration with reproducibility and reporting features.

To understand how to use the new `teal_reporter` and `teal_card` classes in more detail, please refer to the following documentation:

- [`teal_reporter` class](https://insightsengineering.github.io/teal.reporter/main/articles/teal-report-class.html) vignette
- [Creating custom modules](https://insightsengineering.github.io/teal/latest-main/articles/creating-custom-modules.html) vignette
- [Adding Support for Reporting to Custom Modules](https://insightsengineering.github.io/teal/latest-main/articles/adding-support-for-reporting.html) vignette

For detailed API documentation, see:

- [`teal_reporter` reference](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_report.html)
- [`teal_card` reference](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/teal_card.html)
