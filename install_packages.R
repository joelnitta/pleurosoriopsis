#' # Installing packages to a Docker image with packrat
#'
#' ## How this works
#'
#' ### Setup
#'
#' Keep `Dockerfile` and `install_packages.R` in the root of the
#' project directory.
#'
#' ### First step: make packrat/packrat.lock file
#'
#' Launch rocker/tidyverse with tag set to same version we will use in
#' the Dockerfile and this directory mounted, and run this script.
#'
#' ````
#' docker run --rm -e DISABLE_AUTH=true -it -v /Volumes/Transcend/Documents/Projects/pleurosoriopsis/:/home/rstudio/pleurosoriopsis rocker/verse:3.5.3 bash
#' ````
#'
#' Inside the docker container first install any dependencies
#' that aren't in rocker/verse. Then install R packages.
#'
#' ```
#' apt-get update
#' cd home/rstudio/pleurosoriopsis
#' Rscript install_packages.R
#' ```
#'
#' This will install current versions of all packages and deps
#' called by library() below to that container,
#' but the main reason is to write `packrat/packrat.lock`
#'
#' ### Second step: actually build the image
#'
#' Now we can use packrat.lock to restore (i.e., install) packages
#' during the next (real) docker build, using the `Dockerfile`.
#'
#' ```
#' docker build . -t joelnitta/pleurosoriopsis
#' ```
#'
#' ### Third step: rinse, repeat
#'
#' Edit the packages below as needed to add new packages
#' or update old ones (by installing), and repeat Steps 1 and 2.
#'
#' To only update new packages instead of running this entire script,
#' install new packages IN THE CONTAINER using e.g.
#' install.packages("visNetwork", lib = "/packrat/lib/x86_64-pc-linux-gnu/3.5.3", repos = "https://cran.rstudio.com/")
#' (The `lib` argument ensures it installs to the packrat library).
#' The run packrat::snapshot() again.
#' (You should still add the package to the package list below so that the
#' whole thing can be built from scratch if needed).

################################################################################

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
