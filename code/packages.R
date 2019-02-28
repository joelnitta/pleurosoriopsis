# Load the packages this workflow depends on

# Install ferncollectR (Fern collection data)
# This is updated frequently, so try to install 
# the newest package first. The pacakge is a private 
# github repo, so need have GITHUB_PAT defined in 
# .Renviron to access.

library(conflicted)
library(assertthat)
library(assertr)
library(drake)
library(glue)
library(here)
library(jntools)
library(lubridate)
library(magrittr)
library(readxl)
library(tidyverse)
library(broom)

# Resolve conflicting functions
collapse <- dplyr::collapse
count <- dplyr::count
expand <- tidyr::expand
extract <- magrittr::extract
filter <- dplyr::filter
gather <- tidyr::gather
intersect <- dplyr::intersect
here <- here::here
map <- purrr::map
select <- dplyr::select
setdiff <- dplyr::setdiff
