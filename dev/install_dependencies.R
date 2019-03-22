# starting from nestbasertudio:v1 docker image

devtools::install_github(
  'NEST/random.cdisc.data',
  ref = "devel",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE
)
devtools::install_github(
  'NEST/teal',
  ref = "devel",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE
)
devtools::install_github(
  'NEST/teal.devel',
  ref = "devel",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE,
  auth_token = readLines("~/.github_token") # because private repo
)
devtools::install_github("hadley/strict")
devtools::install_github(
  'NEST/test.nest',
  ref = "devel",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE
)
devtools::install_github("Roche/rtables",
                         upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "NEST/tern",
  ref = "devel",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

from_source <- function(path_to_file) install.packages(path_to_file, repos = NULL, type="source")
download_dir <- "../scratch/install_packages"
from_source(file.path(download_dir, "teal.modules.clinical"))
install.packages("DT")
from_source(file.path(download_dir, "teal.modules.general"))

install.packages("styler")

# not working

# unloadNamespace("plyr")
#install.packages("automation.utils", repos = "http://3.121.104.44:8081/repository/NEST-R/")

#runApp("tests/testthat/app.R")

#install.packages(c("data.table", "ggmosaic"))





devtools::install_github(
  repo = "NEST/teal.modules.clinical",
  ref = "devel",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE,
  auth_token = readLines("~/.github_token") # because private repo
)

install_github(
  'NEST/teal.modules.general',
  ref = "master",
  host = 'https://github.roche.com/api/v3',
  upgrade_dependencies = FALSE,
  build_vignettes = TRUE,
  auth_token = readLines("~/.github_token") # because private repo
)
