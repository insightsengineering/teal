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
    )
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
    )
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
    )
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
    )
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
    )
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
    )
  )
  tc7 <- git_pkgs_check_rcd(
    git_repo = c(roche = "https://roche.com"),
    git_pkgs = list(roche = "NEST/teal"), needs_rcd = TRUE
  )

  expect_equal(
    tc7,
    list(
      git_repo = c(roche = "https://github.roche.com"),
      git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data"))
    )
  )
})

test_that("get_rcode_git_pkgs", {
  tc1 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com"),
    git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data"))
  ) %>% strsplit(split = "\n") %>% unlist() %>% unname()

  expect_equal(
    tc1,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)"
    )
  )

  tc2 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com", "global" = "http://github.com"),
    git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data"), global = "hadley/strict")
  ) %>% strsplit(split = "\n") %>% unlist() %>% unname()

  expect_equal(
    tc2,
    c(
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/teal\")',
      "library(teal)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"hadley/strict\")",
      "library(strict)"
    )
  )

  tc3 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com", "global" = "http://github.com"),
    git_pkgs = list(
      roche = c("NEST/teal", "NEST/random.cdisc.data"),
      global = c("hadley/strict", "Roche/rtables")
    )
  ) %>% strsplit(split = "\n") %>% unlist() %>% unname()

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
    )
  )

  tc4 <- get_rcode_git_pkgs(
    git_repo = c(roche = "https://github.roche.com", "global" = "http://github.com"),
    git_pkgs = list(
      roche = c("NEST/teal", "NEST/random.cdisc.data"),
      global = c("hadley/strict", "Roche/rtables")
    ), needs_rcd = TRUE
  ) %>% strsplit(split = "\n") %>% unlist() %>% unname()

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
    )
  )

  tc5 <- get_rcode_git_pkgs(
    git_repo = c("http://github.com"),
    git_pkgs = list(c("hadley/strict", "Roche/rtables")), needs_rcd = TRUE
  ) %>% strsplit(split = "\n") %>% unlist() %>% unname()

  expect_equal(
    tc5,
    c(
      "devtools::install_github(host = \"https://api.github.com\", repo = \"hadley/strict\")",
      "library(strict)",
      "devtools::install_github(host = \"https://api.github.com\", repo = \"Roche/rtables\")",
      "library(rtables)",
      'devtools::install_github(host = \"https://github.roche.com/api/v3\", repo = \"NEST/random.cdisc.data\")',
      "library(random.cdisc.data)"
    )
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
    )
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
    )
  )
})

test_that("has_source_attribute", {
  with_source <- "string"
  attr(with_source, "source") <- "any"

  without_source <- "string"

  tc1 <- list(a = with_source, b = without_source, c = without_source)

  expect_false(has_source_attribute(tc1))

  tc2 <- list(a = with_source, b = with_source, c = with_source)

  expect_true(has_source_attribute(tc2))
})

test_that("get_filter_txt", {
  ASL <- random.cdisc.data::radsl(600)
  ATE <- random.cdisc.data::radtte(ASL)

  attr(ASL, "source") <- "radsl(600)"
  attr(ATE, "source") <- "radtte(ASL)"

  d <- teal:::FilteredData$new()
  d$set_data("ASL", ASL)
  d$set_data("ATE", ATE)

  tc1 <- get_filter_txt("ASL", d)

  expect_equal(tc1, "ASL_FILTERED <- ASL")

  tc2 <- get_filter_txt("ATE", d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc2,
    c(
      "ASL_FILTERED <- ASL",
      "ATE_FILTERED_ALONE <- ATE",
      "ATE_FILTERED <- merge(x = ASL_FILTERED[, c(\"USUBJID\", \"STUDYID\")], y = ATE_FILTERED_ALONE, ",
      "    by = c(\"USUBJID\", \"STUDYID\"), all.x = FALSE, all.y = FALSE)"
    )
  )
  d$set_filter_state("ASL", "ARM", "A: Drug X")
  tc3 <- get_filter_txt("ASL", d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc3,
    deparse(d$get_filter_call("ASL"))
  )

  # empty datanames argument shall lead to use ASL
  tc4 <- get_filter_txt(datanames = c(), datasets = d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname()

  expect_equal(
    tc4,
    deparse(d$get_filter_call("ASL"))
  )
})

test_that("get_rcode_datasets", {
  ASL <- random.cdisc.data::radsl(600)
  ATE <- random.cdisc.data::radtte(ASL)

  attr(ASL, "source") <- "radsl(600)"
  attr(ATE, "source") <- "radtte(ASL)"

  d <- teal:::FilteredData$new()
  d$set_data("ASL", ASL)
  d$set_data("ATE", ATE)

  tc1 <- get_rcode_datasets(d) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    unname() %>% trimws

  expect_equal(
      tc1,
      c("ASL <- radsl(600)","ATE <- radtte(ASL)")
      )

  tc2 <- list(
     ASL = structure(data.frame(a = 1), source = "haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')"),
     ATE = structure(data.frame(a = 1),
       source = "haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')",
       md5sum = "32sdf32fds324"
     )
   ) %>% get_rcode_datasets  %>%
   strsplit(split = "\n") %>%
   unlist() %>%
   unname() %>% trimws


   expect_equal(
       tc2,
       c(
           "ASL <- haven::read_sas('/opt/BIOSTAT/asl.sas7bdat')",
           "ATE <- haven::read_sas('/opt/BIOSTAT/ate.sas7bdat')  # md5sum at time of analysis: 32sdf32fds324"
           )
       )

  expect_error(
      get_rcode_datasets(datasets <- list(one = structure(data.frame(a=1)), two = data.frame(a=1)))
      ,"source attribute"
  )

  expect_error(
      get_rcode_datasets(datasets <- list(one = c(), two = data.frame(a=1)))
      ,"FilteredData object or"
  )
})
