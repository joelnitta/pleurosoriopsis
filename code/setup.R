#' Load packages.
library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)
library(broom)
library(ggridges)
library(patchwork)
library(gt)
library(viridis)

#' Load custom functions.
source("code/functions.R")

#' Resolve namespace conflicts.
conflict_prefer("filter", "dplyr")
conflict_prefer("ggsave", "ggplot2")
conflict_prefer("intersect", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")
conflict_prefer("scale_discrete_manual", "ggplot2")
conflict_prefer("setdiff", "dplyr")
conflict_prefer("union", "dplyr")
