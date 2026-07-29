# Validates no intersection between two vectors

This function is a wrapper for
[`shiny::validate`](https://rdrr.io/pkg/shiny/man/validate.html).

## Usage

``` r
validate_no_intersection(x, y, msg)
```

## Arguments

- x:

  vector

- y:

  vector

- msg:

  (`character(1)`) message to display if `x` and `y` intersect

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEATKKShzFFqxiXN47AdOm0z0gLzSC7AIyI-gAMuNL+gQBMoeFBwTy4rtIAzqTMVt7SjHCo7H58YACCBWEFAEIl0gUAwgUJ0nBQBAAWmSH8EB0CKrQ2ytQ9ZgAKUADmcC4Qbslw1HBiAJIQqCqkLmDZSv6VBQDKs-OkKWmWUP7SpES+RDCoLHAlSW4tRLQEcMmZ+UU7YBV4VTAtTACSeKQOYjgnh8BWKYCSoKm4Lmi2Wq3Wm0iv32KKOqXSUEiFyuxFu90eSOezVe70+Pm+cNKf1+wMRbmmEPI0MB-wRiSRUkY9EstBgujgAA9SAB5VYrNYFZIqGAwFisOoCLoQGaMQV9VTqcRaITysJEOWrHQgJLm0jygAkSpVar62QgZhE4ql7GtlJSsFQcwA+ucFNI7FB7R5gBHHScMl4fCbVvbNv4ALpg5IB4NEsOx6Ox-GnbxJtGkVNwJSRTMQMESbgeSxwIMQIhBoTkRgzDRaMHTHMt-xhbO3XP89nsxUqegAK0OnwA7rRSM0hMcCecoO6N6ciQRt22jkRBdQ0AUwR1J3dUnBgpNJ24CgA5ZX0ETSIhKFIz+didwQIgWRViIFDvBc2SWPAZBeJUcwQKMq7sKOgZDmyj4FNcZKMJo2oQY0pDQaQsEAvBiHNMhg5BpEV7srRAC+HT0QItDfuwnYiE04hSNoVpJMka4QKwhToOwPQjiIgpMQIYD0emQA)

## Examples

``` r
data <- data.frame(
  id = c(1:10, 11:20, 1:10),
  strata = rep(c("A", "B", "C"), each = 10)
)

ui <- fluidPage(
  selectInput("ref1", "Select strata1 to compare",
    choices = c("A", "B", "C"),
    selected = "A"
  ),
  selectInput("ref2", "Select strata2 to compare",
    choices = c("A", "B", "C"),
    selected = "B"
  ),
  verbatimTextOutput("summary")
)

server <- function(input, output) {
  output$summary <- renderText({
    sample_1 <- data$id[data$strata == input$ref1]
    sample_2 <- data$id[data$strata == input$ref2]

    validate_no_intersection(
      sample_1, sample_2,
      "subjects within strata1 and strata2 cannot overlap"
    )
    paste0(
      "Number of subject in: reference treatment=", length(sample_1),
      " comparions treatment=", length(sample_2)
    )
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
