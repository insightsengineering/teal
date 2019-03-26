context("get_rcode_header")

test_that("git_pkgs_check_rcd works", {
  tc1 <- git_pkgs_check_rcd(
    git_repo = "https://github.roche.com",
    git_pkgs = list("NEST/random.cdisc.data"), needs_rcd = FALSE
  )


  expect_equal(
    tc1,
    list(
      git_repo = "https://github.roche.com",
      git_pkgs = list("NEST/random.cdisc.data")
    ),
    info = "Couldn't render code for unnamed git repos."
  )

  tc2 <- git_pkgs_check_rcd(
    git_repo = "https://github.roche.com",
    git_pkgs = list("NEST/random.cdisc.data"), needs_rcd = TRUE
  )
  expect_equal(
    tc2,
    list(
      git_repo = "https://github.roche.com",
      git_pkgs = list("NEST/random.cdisc.data")
    ),
    info = "Couldn't render code for unnamed git repos + RCD."
  )

  tc3 <- git_pkgs_check_rcd(
    git_repo = "https://github.roche.com",
    git_pkgs = list(), needs_rcd = TRUE
  )
  expect_equal(
    tc3,
    list(
      git_repo = c("https://github.roche.com", roche = "https://github.roche.com"),
      git_pkgs = list(roche = "NEST/random.cdisc.data")
    ),
    info = "Could not render rcode for empty packages with needed rcd."
  )

  tc4 <- git_pkgs_check_rcd(
    git_repo = c(roche = "https://github.roche.com"),
    git_pkgs = list(), needs_rcd = TRUE
  )
  expect_equal(
    tc4,
    list(
      git_repo = c(roche = "https://github.roche.com"),
      git_pkgs = list(roche = "NEST/random.cdisc.data")
    ),
    info = "Cannot add random cdisc data repo if Roche repo is given already."
  )

  tc5 <- git_pkgs_check_rcd(
    git_repo = c(roche = "https://roche.com"),
    git_pkgs = list(), needs_rcd = TRUE
  )
  expect_equal(
    tc5,
    list(
      git_repo = c(roche = "https://github.roche.com"),
      git_pkgs = list(roche = "NEST/random.cdisc.data")
    ),
    info = "Problem adding 'roche'=github.roche.com repo."
  )

  tc6 <- git_pkgs_check_rcd(
    git_repo = c(roche = "https://roche.com"),
    git_pkgs = list("NEST/teal"), needs_rcd = TRUE
  )
  expect_equal(
    tc6,
    list(
      git_repo = c(roche = "https://github.roche.com"),
      git_pkgs = list("NEST/teal", roche = "NEST/random.cdisc.data")
    ),
    info = "Problem with given repo and adding rcd."
  )
  tc7 <- git_pkgs_check_rcd(
    git_repo = c(roche = "https://github.roche.com"),
    git_pkgs = list(roche = "NEST/teal"), needs_rcd = TRUE
  )

  expect_equal(
    tc7,
    list(
      git_repo = c(roche = "https://github.roche.com"),
      git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data"))
    ),
    info = "Problem adding rcd to existing 'roche' package list."
  )
})

test_that("get_rcode_git_pkgs", {
  tc1 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com"),
    git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data"))
  ) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc1,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)"
    ),
    info = "Problem parsing simple roche github placed packages."
  )

  tc2 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com", "global" = "http://github.com"),
    git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data"), global = "hadley/strict")
  ) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc2,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"hadley/strict\")",
      "library(strict)"
    ),
    info = "Problem parsing global and internal github packages at the same time."
  )

  tc3 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com", "global" = "http://github.com"),
    git_pkgs = list(
      roche = c("NEST/teal", "NEST/random.cdisc.data"),
      global = c("hadley/strict", "Roche/rtables")
    )
  ) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc3,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"hadley/strict\")",
      "library(strict)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"Roche/rtables\")",
      "library(rtables)"
    ),
    info = "Combination of many global and enterprise repos does not work."
  )

  tc4 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com", "global" = "http://github.com"),
    git_pkgs = list(
      roche = c("NEST/teal", "NEST/random.cdisc.data"),
      global = c("hadley/strict", "Roche/rtables")
    ), needs_rcd = TRUE
  ) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc4,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"hadley/strict\")",
      "library(strict)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"Roche/rtables\")",
      "library(rtables)"
    ),
    info = "Test of random cdisc data included in package listing."
  )

  tc5 <- get_rcode_git_pkgs(
    git_repo = c("http://github.com"),
    git_pkgs = list(c("hadley/strict", "Roche/rtables")), needs_rcd = TRUE
  ) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc5,
    c(
      "devtools::install_github(host = \"https://api.github.com\", repo = \"hadley/strict\")",
      "library(strict)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"Roche/rtables\")",
      "library(rtables)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)"
    ),
    info = "test of adding random.cdisc.data to the list of packages."
  )

  tc6 <- get_rcode_git_pkgs(git_repo = "https://github.roche.com/NEST/teal", needs_rcd = TRUE) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc6,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)"
    ),
    info = "Problem working with absolute package github path + RCD."
  )

  tc7 <- get_rcode_git_pkgs(git_repo = "https://github.roche.com/NEST/teal", needs_rcd = FALSE) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc7,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)"
    ),
    info = "Problem working with absolute package github path."
  )
})

test_that("has_source_attribute", {
  with_source <- "string"
  attr(with_source, "source") <- "any"

  without_source <- "string"

  tc1 <- list(a = with_source, b = without_source, c = without_source)

  expect_false(has_source_attribute(tc1), info = "check for non source attribute.")

  tc2 <- list(a = with_source, b = with_source, c = with_source)

  expect_true(has_source_attribute(tc2), info = "check for vector with source attribute.")
})

test_that("get_filter_txt", {
  ASL <- random.cdisc.data::radsl(600) # nolint
  ATE <- random.cdisc.data::radtte(ASL) # nolint

  attr(ASL, "source") <- "radsl(600)" # nolint
  attr(ATE, "source") <- "radtte(ASL)" # nolint

  d <- teal:::FilteredData$new()
  d$set_data("ASL", ASL) # nolint
  d$set_data("ATE", ATE) # nolint

  tc1 <- get_filter_txt("ASL", d)
  tc1_expect <- ""
  attr(tc1_expect, "unfiltered") <- "ASL"

  expect_equal(tc1, tc1_expect, info = "Simple filtered data test failed.")

  tc2 <- get_filter_txt("ATE", d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_true(
    tc2[1] == "", "Merged data check failed. [1]"
  )
  expect_true(
    tc2[2] == "ATE_FILTERED_ALONE <- ATE", "Merged data check failed. [2]"
  )
  expect_true(
    tc2[3] == "ATE_FILTERED <- merge(x = ASL[, c(\"USUBJID\", \"STUDYID\")], y = ATE_FILTERED_ALONE, ",
        "Merged data check failed. [3]"
  )
  expect_true(
    tc2[4] == "    by = c(\"USUBJID\", \"STUDYID\"), all.x = FALSE, all.y = FALSE)", "Merged data check failed. [4]"
  )

  d$set_filter_state("ASL", "ARM", "A: Drug X")

  tc3 <- get_filter_txt("ASL", d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc3,
    deparse(d$get_filter_call("ASL")),
    "Get filtered data for single data set."
  )

  # empty datanames argument shall lead to use ASL
  tc4 <- get_filter_txt(datanames = c(), datasets = d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc4,
    deparse(d$get_filter_call("ASL")),
    "Get filtering wihtout data names."
  )
})

test_that("get_rcode_datasets", {
  ASL <- random.cdisc.data::radsl(600) # nolint
  ATE <- random.cdisc.data::radtte(ASL) # nolint

  attr(ASL, "source") <- "radsl(600)" # nolint
  attr(ATE, "source") <- "radtte(ASL)" # nolint

  d <- teal:::FilteredData$new()
  d$set_data("ASL", ASL)
  d$set_data("ATE", ATE)

  tc1 <- get_rcode_datasets(d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws()

  expect_equal(
    tc1,
    c("ASL <- radsl(600)", "ATE <- radtte(ASL)"),
    info = "get source code of FilteredData failed."
  )

  tc2 <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"),
    ATE = structure(data.frame(a = 1),
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  ) %>%
    get_rcode_datasets() %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws()


  expect_equal(
    tc2,
    c(
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324"
    ),
    info = "MD5 sum test for data list failed."
  )

  expect_error(
    get_rcode_datasets(datasets <- list(one = structure(data.frame(a = 1)), two = data.frame(a = 1))),
    "source attribute",
    info = "source attribute of data test failed."
  )

  expect_error(
    get_rcode_datasets(datasets <- list(one = c(), two = data.frame(a = 1))),
    "FilteredData object or",
    info = "Filtered data set check failed."
  )
})

test_that("get_rcode_datalist", {
  tc1 <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  ) %>%
    get_rcode_data_list(datanames = "ASL") %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws()

  expect_equal(
    tc1,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "",
      ""
    ),
    info = "Two data sets with no filtering or pre-processing."
  )

  tc2 <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  ) %>%
    get_rcode_data_list(datanames = "ASL", code_data_processing = "ASL <- ASL %>% filter(AGE > 20)") %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws()
  expect_equal(
    tc2,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "ASL <- ASL %>% filter(AGE > 20)",
      "",
      ""
    ),
    info = "get_rcode_datalist: Two data sets with no filtering."
  )

  ASL <- random.cdisc.data::radsl(600) # nolint
  ATE <- random.cdisc.data::radtte(ASL) # nolint

  attr(ASL, "source") <- "radsl(600)" # nolint
  attr(ATE, "source") <- "radtte(ASL)" # nolint

  d <- teal:::FilteredData$new()
  d$set_data("ASL", ASL)
  d$set_data("ATE", ATE)

  tc3 <- get_rcode_data_list(d, datanames = c(), code_data_processing = "ASL <- ASL %>% filter(AGE > 20)") %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws()

  expect_equal(
    tc3,
    c(
      "",
      "ASL <- radsl(600)",
      "ATE <- radtte(ASL)",
      "",
      "ASL <- ASL %>% filter(AGE > 20)",
      "",
      ""
    ),
    info = "One data sets with teal filtering."
  )

  tc4 <- get_rcode_data_list(d,
    datanames = c("ASL", "ATE"),
    code_data_processing = "ASL <- ASL %>% filter(AGE > 20)"
  ) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws()

  filtered_strings <- lapply(d$get_filter_call("ATE"), function(x) x %>%
      deparse(width.cutoff = 80) %>%
      trimws()) %>%
    unlist()

  filtered_strings[[1]] <- ""

  expect_equal(
    tc4,
    c(
      "",
      "ASL <- radsl(600)",
      "ATE <- radtte(ASL)",
      "",
      "ASL <- ASL %>% filter(AGE > 20)",
      "",
      filtered_strings
    ),
    info = "two data sets with teal filtering."
  )
})

test_that("get_rcode_data", {
  tc1 <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  ) %>%
    get_rcode_data(datanames = "ASL") %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws() #

  expect_equal(
    tc1,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "",
      ""
    ),
    info = "get_rcode_data failed on using data + datanames argument"
  )

  data <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  )
  tc2 <- get_rcode_data(datanames = "ASL", datasets = data) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws() #
  expect_equal(
    tc2,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "",
      ""
    ),
    info = "get_rcode_data failed on using datasets + datanames argument"
  )

  expect_error(get_rcode_data(data = data, datasets = data))

  expect_equal(
    get_rcode_data(),
    NULL
  )

  tc3 <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  ) %>%
    get_rcode_data("ASL") %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws() #

  expect_equal(
    tc1,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "",
      ""
    ),
    info = "get_rcode_data failed on using just data argument"
  )

  data <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  )
  tc4 <- get_rcode_data(datasets = data) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws() #
  expect_equal(
    tc2,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "",
      ""
    ),
    info = "get_rcode_data failed on using just datasets argument"
  )

  data <- list(
    ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"), # nolint
    ATE = structure(data.frame(a = 1), # nolint
      source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
      md5sum = "32sdf32fds324"
    )
  )

  tc5 <- get_rcode_data(datanames = "ASL", datasets = data, code_data_processing = "") %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>%
    trimws() #

  expect_equal(
    tc5,
    c(
      "",
      "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
      "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324",
      "",
      "",
      "",
      ""
    ),
    info = "get_rcode_data failed on using datasets + datanames argument + core_data_processing arg."
  )
})


test_that("get_rcode_header", {
  expect_error(get_rcode_header())


  choose_index <- function(x, index) {
    x[index]
  }

  expect_equal(
    get_rcode_header(title = "title") %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      unname() %>%
      choose_index(c(1:3, 5:16)),
    c(
      "# title",
      "# ",
      paste("# Running on:", getwd(), ""),
      "# ",
      "# Not given",
      "# You can run this code interactively in http://r.roche.com",
      "",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"Roche/rtables\")",
      "library(rtables)",
      "",
      "devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")",
      "library(teal)",
      "",
      "devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")",
      "library(random.cdisc.data)"
    ),
    info = "get_rcode_header call with just title argument"
  )

  expect_match(
    get_rcode_header(title = "title") %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      unname() %>%
      choose_index(4),
    c(
      "Date\\:\\s.+\\:.+"
    ),
    info = "get_rcode_header call with just title argument 'Date' check."
  )

  expect_equal(
    get_rcode_header("title", git_hosts = NULL, git_repo = "https://github.com/zappingseb/RTest") %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      unname() %>%
      choose_index(c(10, 11)),
    c(
      "devtools::install_github(host = \"https://api.github.com\", repo = \"zappingseb/RTest\")",
      "library(RTest)"
    ),
    info = "Try single git repo intput - get_rcode_header."
  )

  expect_equal(
    get_rcode_header("title", libraries = c("dplyr")) %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      unname() %>%
      choose_index(c(18)),
    c(
      "library(dplyr)"
    ),
    info = "Try single library call - get_rcode_header."
  )

  expect_equal(
    get_rcode_header("title", description = "PKG") %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      unname() %>%
      choose_index(c(3)),
    c(
      "# PKG"
    ),
    info = "Try adding a description - get_rcode_header."
  )
})
