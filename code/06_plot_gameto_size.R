#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Analyze distribution of gametophyte size (Fig. 6)
#'
#' This script analyzes the distribution of individual sizes
#' (length and width) of gametophytes of *Pleurosoriopsis makinoi* 
#' at Okutama, Japan.
#' 
#' There were not enough measurments to do proper statistics, but
#' we can visualize the distribution.
#' 
#' ### Setup
setwd(here::here())

source("code/setup.R")

#' ### Load pre-processed data, and clean up a bit more.
#' 
#' Note there is one outlier with length 46 mm, 
#' then the rest all less than 19 mm. We will remove this 
#' before plotting and make a note in the figure caption.
gameto_size <- read_csv("data/gameto_size.csv")

gameto_size %>% arrange(desc(length))

gameto_size <-
  gameto_size %>%
  # Change sex to uppercase for nicer plotting
  mutate(sex = stringr::str_to_title(sex)) %>%
  # Remove one outlier
  filter(length < 40)

gameto_size

#' Make jitter plots overlayed with box plots. Make two
#' subplots: one for width and one for length, then combine.

# Use colorbrewer palette: 3 diverging, color-blind friendly colors
cols <- c("#1b9e77","#d95f02","#7570b3")

subplots <- list()

subplots[["length"]] <- 
ggplot(gameto_size, aes(y = length, x = sex, color = sex)) +
  labs(
    subtitle = "a",
    y = "Length (mm)",
    x = "",
    color = "")

subplots[["width"]] <-
ggplot(gameto_size, aes(y = width, x = sex, color = sex)) +
  labs(
    subtitle = "b",
    y = "Width (mm)",
    x = "")
  
subplots <- 
  map(subplots,
      ~ . +
        geom_boxplot(
          outlier.shape = NA,
          fill = NA,
          color = "dark grey"
        ) +
        geom_jitter(size = 3) +
        scale_color_manual(values = cols) +
        theme(legend.position = "none")
  )

subplots[[1]] + subplots[[2]] + plot_layout(ncol = 2) 

ggsave(
  file = "results/fig7_size.pdf",
  height = 4,
  width = 7)
