# starting from https://github.roche.com/wolfs25/shinytestdocker docker Dockerfile
# Please change the identification with Roche github
system("git config user.name 'wolfs25'")
system("git config user.email 'sebastian.wolf.sw1@roche.com'")
user.name <- "wolfs25"
# Please get a github token and store it in ~/.github_token

#
############################################################################
#
install.packages(c("gridExtra","ggmosaic","forcats","ggplot2","lintr","roxygen2"))
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


git2r::clone(url="https://github.com/hadley/strict",local_path="/home/rstudio/strict")
devtools::install("/home/rstudio/strict")
devtools::install_github(
  repo = "NEST/test.nest",
  ref = "devel",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)


install_deps <- function(package_folder){
  package_desc <- desc::description$new(file = file.path(package_folder,"DESCRIPTION"))
  packages_to_install <- setdiff(package_desc$get_deps()$package,
                                   as.character(installed.packages()[,"Package"])
                                   )
  install.packages(packages_to_install)
}

from_source <- function(x){
  install.packages(devtools::build(x), type="source", repos=NULL)
}

clone_source <- function(repo = "NEST/teal.modules.general",
                         token = readLines("~/.github_token"),
                         download_dir = "/tmp",
                         github = "https://github.roche.com/",
                         ...){
  
  Sys.setenv("GITHUB_PAT"=token)
  
  package_name <- strsplit(repo,"/")[[1]][2]
  
  download_path <- file.path(download_dir, package_name)
  
  system(paste0("rm -rf ", download_path))
  
  git2r::clone(url = paste0("https://github.roche.com/",repo),
               local_path = download_path,
               credentials = git2r::cred_token(),
               ...
  )
    
  return(download_path)
  
  
}

download_package <- clone_source(repo = "NEST/teal.modules.general", branch = "devel", download_dir = "/home/rstudio")
install_deps(download_package)
from_source(download_package)


download_package <- clone_source(repo = "NEST/teal.modules.clinical", branch = "devel", download_dir = "/home/rstudio")
from_source(download_package)

