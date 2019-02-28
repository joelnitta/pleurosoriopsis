# make.R 

# Master script for running T_angustifrons analyses.
# This project uses drake to manage workflows.
# For more information about drake, see
# https://ropensci.github.io/drake/

# Setup ----

# Set working directory
setwd(here::here())

# Load packages
source("code/packages.R")

# Update drake settings
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# Load functions and plans  ----

# General functions
source("code/functions.R")
source("code/drake_functions.R")

# Data processing
source("code/plan.R")

# Run analyses ----
make(plan)
