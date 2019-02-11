#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Plot change in growth over time (Fig. 3)
#'
#' This script plots changes in growth (total area, 
#' number and length of gemmae) in a population of independent
#' gametophytes at Okutama, Japan.
#'
#' ### Setup
setwd(here::here())

source("code/setup.R")

#' ### Load pre-processed data
combined_morph <- read_csv("data/combined_morph.csv")

#' Set x-limits manually since the automatically set limits will vary
#' when we subset the data.
start_date <- combined_morph %>%
  pull(date) %>%
  min

end_date <- combined_morph %>%
  pull(date) %>%
  max

#' Make tibble of years for shading. This dataframe will be passed to `geom_rect()` when
#' plotting.
shading_dates <- 
  tibble(date = seq.Date(from = start_date, to = end_date, by = "day")) %>%
  mutate(year = year(date)) %>%
  # shade by alternate years
  filter(year %in% c(2009, 2011, 2013)) %>%
  group_by(year) %>%
  summarize(
    year_start = min(date),
    year_end = max(date)
  )

#' I've also made a very specific function for adding rectangles to show
#' the year:

shade_years

#' Make list of subplots. Build each one at a time, then add common
#' formatting. 
subplots <- list()

subplots[["cover"]] <- 
  combined_morph %>%
  select(starts_with("q"), date) %>%
  filter(complete.cases(.)) %>%
  gather(plot, area, -date) %>%
  ggplot(aes(x = date, y = area, linetype = plot)) %>%
  shade_years(shading_dates) +
  geom_line() +
  scale_linetype_manual(
    name="Quadrat",
    breaks=c("q_1", "q_2", "q_3", "q_4"),
    labels=c("1", "2", "3", "4"),
    values=c("solid", "dashed", "dotted", "dotdash")
  ) +
  labs(
    x = "",
    y = expression("Cover ("~cm^2~")"),
    subtitle = "a"
  ) +
  theme(legend.position = "bottom")

legend <- get_legend(subplots[["cover"]])

subplots[["cover"]] <- 
  subplots[["cover"]] +
  theme(legend.position = "none")


subplots[["gemmae_count"]] <- 
  combined_morph %>%
  select(count_mean, count_sd, date) %>%
  drop_na %>%
  ggplot(aes(x = date, y = count_mean)) %>%
  shade_years(shading_dates) +
  geom_errorbar(
    aes(ymin=count_mean-count_sd, 
        ymax=count_mean+count_sd), 
    width=.1,
    color = "dark grey") +
  geom_line() +
  geom_point(color = "black") +
  labs(
    x = "",
    y = expression("Gemmae count"),
    subtitle = "b"
  )

subplots[["gemmae_length"]] <- 
  combined_morph %>%
  select(length_mean, length_sd, date) %>%
  drop_na %>%
  ggplot(aes(x = date, y = length_mean)) %>%
  shade_years(shading_dates) +
  geom_errorbar(
    aes(ymin=length_mean-length_sd, 
        ymax=length_mean+length_sd), 
    width=.1,
    color = "dark grey") +
  geom_line() +
  geom_point(color = "black") +
  labs(
    x = "",
    y = expression(paste("Gemmae len. (", mu, "m)")),
    subtitle = "c"
  )

subplots <- subplots %>%
  map(~ . + 
        theme(
          axis.text.x = element_text(
            angle = 30, 
            hjust = 1, 
            vjust = 0.5, 
            margin=margin(-10,0,0,0)
          ),
          plot.margin = margin(0,10,0,10)
        ) +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "6 months",
          limits = c(start_date, end_date)
        )
  )

subplots[1:2] <- subplots[1:2] %>%
  map(~ . + theme(axis.text.x = element_blank()) 
  )

subplots[2:3] <- subplots[2:3] %>%
  map(~ . + theme(plot.margin = margin(-10,10,0,10)) 
  )

#' Combine subplots and write out
subplots[[1]] + subplots[[2]] + subplots[[3]] + 
  legend +
  plot_layout(ncol = 1, heights = c(1,1,1,0.2))

ggsave(
  file = "results/fig3_growth_bw.pdf",
  height = 7,
  width = 8)

#' Render this script as a report (keep the below code commented-out)
# rmarkdown::render(
#   "code/02_plot_growth.R",
#   output_file = glue::glue("02_plot_growth_{Sys.Date()}.html"),
#   output_dir = "results/",
#   knit_root_dir = here::here(),
#   clean = TRUE)