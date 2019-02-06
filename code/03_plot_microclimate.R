#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Plot change in microclimate over time (Fig. 4)
#'
#' This script plots changes in microclimate over time
#' at two *Pleurosoriopsis makinoi* sites in Japan: Okutama
#' and Uratakao. Okutama is home to a population of
#' independent gametophytes of *P. makinoi*. At Uratakao,
#' sporophytes and gametophytes grow together.
#'   
#' ### Setup
setwd(here::here())

source("code/setup.R")

#' ### Load pre-processed data.
daily_microclimate <- read_csv("data/daily_microclimate.csv")

#' Extract common start and end dates so x-axis are same across subplots.
start_date <- daily_microclimate %>% pull(date) %>% min
end_date <- daily_microclimate %>% pull(date) %>% max
all_days <- tibble( 
  date = seq(start_date, end_date, by = "day")
)

#' Make tibble of years for shading.
shading_dates <- 
  tibble(date = seq.Date(from = start_date, to = end_date, by = "day")) %>%
  mutate(year = year(date)) %>%
  # shade by odd years
  filter(year %in% c(2009, 2011, 2013)) %>%
  group_by(year) %>%
  summarize(
    year_start = min(date),
    year_end = max(date)
  )

#' PAR differs enough between sites that automatically set y-axis limits
#' would be different. Use the common min and max between them.
min_par <- daily_microclimate %>% pull(par_total) %>% min
max_par <- daily_microclimate %>% pull(par_total) %>% max

#' Insert NAs into microclimate for each site so that missing days
#' don't get connected by lines.
daily_microclimate_okutama <- filter(daily_microclimate, site == "okutama") %>%
  right_join(all_days)

daily_microclimate_takao <- filter(daily_microclimate, site == "uratakao") %>%
  right_join(all_days)

daily_microclimate_with_na <- bind_rows(
  daily_microclimate_okutama, daily_microclimate_takao)

#' Make list of subplots.
subplots <- list()

subplots[[1]] <- 
  daily_microclimate_okutama %>%
  ggplot(aes(x = date, y = par_total)) %>%
  shade_years(shading_dates) +
  geom_line() +
  scale_y_continuous(limits = c(min_par, max_par)) +
  labs(y = expression(paste("DLI (", mol~m^-2~day^-1, ")", sep = "") ),
       x = "",
       title = "Okutama",
       subtitle = "a")

subplots[[2]] <- 
  daily_microclimate_takao %>%
  ggplot(aes(x = date, y = par_total)) %>%
  shade_years(shading_dates) +
  geom_line() +
  scale_y_continuous(limits = c(min_par, max_par)) +
  labs(y = "",
       x = "",
       title = "Uratakao",
       subtitle = "b")

subplots[[3]] <- 
  daily_microclimate_okutama %>%
  ggplot(aes(x = date, y = rh_min)) %>%
  shade_years(shading_dates) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "Min. Rel. Humidity (%)",
       x = "",
       subtitle = "c")

subplots[[4]] <- 
  daily_microclimate_takao %>%
  ggplot(aes(x = date, y = rh_min)) %>%
  shade_years(shading_dates) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "",
       x = "",
       subtitle = "d")

subplots[[5]] <- 
  daily_microclimate_okutama %>%
  ggplot(aes(x = date, y = temp_mean)) %>%
  shade_years(shading_dates) +
  geom_line() +
  labs(y = "Mean Temp. (°C)",
       x = "",
       subtitle = "e")

subplots[[6]] <- 
  daily_microclimate_takao %>%
  ggplot(aes(x = date, y = temp_mean)) %>%
  shade_years(shading_dates) +
  geom_line() +
  labs(y = "",
       x = "",
       subtitle = "f")

#' Apply common formatting.
subplots <- subplots %>%
  map(~ . + 
        theme(
          axis.text.x = element_text(
            angle = 30, 
            hjust = 1, 
            vjust = 0.5, 
            margin=margin(-10,0,0,0)
          )
        ) +
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "6 months",
          limits = c(start_date, end_date)
        )
  )

#' Remove x-axis lables from upper plots.
subplots[1:4] <- subplots[1:4] %>%
  map(~ . + theme(axis.text.x = element_blank()) 
  )

#' Close gaps on top margins for lower plots.
subplots[c(3,5)] <- subplots[c(3,5)] %>%
  map(~ . + theme(plot.margin = margin(-10,10,0,10)) 
  )

#' Close gaps on left side for RHS plots.
subplots[c(2,4,6)] <- subplots[c(2,4,6)] %>%
  map(~ . + theme(plot.margin = margin(-10,10,0,-40)) 
  )

#' Combine plots and write out.
subplots[[1]] + subplots[[2]] + subplots[[3]] + 
  subplots[[4]] + subplots[[5]] + subplots[[6]] + 
  plot_layout(ncol = 2)

ggsave(
  file = "results/fig4_clim.pdf",
  height = 7,
  width = 8)

#' Render this script as a report (keep the below code commented-out)
# rmarkdown::render(
#   "code/03_plot_microclimate.R",
#   output_file = glue::glue("03_plot_microclimate_{Sys.Date()}.html"),
#   output_dir = "results/",
#   knit_root_dir = here::here(),
#   clean = TRUE)