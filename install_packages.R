# This script writes packrat.lock for installing R packages to a docker image.
# It should be run from within the rocker/verse:3.6.0 container.
# For more info on installing R packages to docker images with
# packrat, see https://www.joelnitta.com/post/docker-and-packrat/

### Initialize packrat ###

# Don't let packrat try to find
# packages to install itself.

install.packages("packrat", repos = "https://cran.rstudio.com/")
packrat::init(
  infer.dependencies = FALSE,
  enter = TRUE,
  restart = FALSE)

### Setup repositories ###

# Install packages that install packages.
install.packages("remotes", repos = "https://cran.rstudio.com/")

# Set repos.
my_repos <- vector()
my_repos["CRAN"] <- "https://cran.rstudio.com/"
options(repos = my_repos)

### Install CRAN packages ###
cran_packages <- c(
  "assertr",
  "assertthat",
  "broom",
  "conflicted",
  "drake",
  "glue",
  "ggridges",
  "here",
  "jntools",
  "lubridate",
  "magrittr",
  "readxl",
  "scales",
  "tidyverse",
  "visNetwork"
)

install.packages(cran_packages)

### Install github packages ###
github_packages <- c(
  "joelnitta/jntools",
  "rstudio/gt",
  "thomasp85/patchwork"
)

remotes::install_github(github_packages)

### Take snapshot ###

packrat::snapshot(
  snapshot.sources = FALSE,
  ignore.stale = TRUE,
  infer.dependencies = FALSE)
