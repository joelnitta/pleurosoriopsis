#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Growth Dynamics of the Independent Gametophytes of *Pleurorosiopsis makinoi* (Polypodiaceae)
#' 
#' Analyze correlation between microclimate and morphology in a colony of
#' independent gametophytes of *Pleurosoriopsis makinoi* from Japan.

library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)
library(broom)
# library(viridis)
library(ggridges)

source("code/functions.R")

# Load data ----

#' Read in raw microclimate data. 
#' 
#' PAR, rel. humidity, and temperature
#' measured once every 30 min. 
#' 
#' For Okutama, the raw data are in different columns
#' in the excel file, so read in each separately.

okutama_temp_raw <- read_excel(
  "data_raw/データロガー素データ.xlsm",
  sheet = 1,
  range = "A2:B28683",
  col_names = c("date_time", "temp"))

okutama_rh_raw <- read_excel(
  "data_raw/データロガー素データ.xlsm",
  sheet = 1,
  range = "C2:D28683",
  col_names = c("date_time", "rh"))

okutama_par_raw <- read_excel(
  "data_raw/データロガー素データ.xlsm",
  sheet = 1,
  range = "F2:G28204",
  col_names = c("date_time", "par"))

okutama_microclimate_raw <- 
  full_join(okutama_temp_raw, okutama_rh_raw) %>%
  full_join(okutama_par_raw)

okutama_microclimate_raw

#' Tidy Okutama microclimate data.
#' 
#' Add 12 hours if time is PM. 
#' Time units are in seconds when adding.
okutama_microclimate <- 
  okutama_microclimate_raw %>%
  mutate(
    pm = str_detect(date_time, "午後"),
    date_time = mdy_hms(date_time, tz=Sys.timezone()),
    date_time = case_when (
      pm == TRUE ~ date_time + 60*60*12,
      TRUE ~ date_time
    ),
    date = date(date_time)
  ) %>%
  select(-pm)

okutama_microclimate

#' Uratakao data have a single column for time of the three microclimate
#' variables, so these can be read-in all at once.

#' Read in raw data.
takao_microclimate_raw <- read_excel(
  "data_raw/データロガー素データ.xlsm",
  sheet = 2,
  range = "D2:G21459",
  col_names = c("date_time", "par", "temp", "rh"))

#' Clean up, correcting PM again like last time.
takao_microclimate <- takao_microclimate_raw %>%
  mutate(
    pm = str_detect(date_time, "午後"),
    date_time = mdy_hms(date_time, tz=Sys.timezone()),
    date_time = case_when (
      pm == TRUE ~ date_time + 60*60*12,
      TRUE ~ date_time
    ),
    date = date(date_time)
  ) %>%
  select(-pm)

#' In order to compare data across sites, need to use daily values since
#' the times are slightly different. Calculate daily min, mean,
#' max, sd, and total values. We won't use all of these but it's
#' easier to do them all at once using summarize_at(). Also add season.

daily_microclimate <-
  bind_rows(
    mutate(okutama_microclimate, site = "okutama"),
    mutate(takao_microclimate, site = "uratakao")
  ) %>%
  filter(complete.cases(.)) %>% # no missing values, so don't need na.rm
  group_by(date, site) %>%
  summarize_at(
    c("par", "temp", "rh"),
    funs(
      mean = mean,
      max = max,
      min = min,
      sd = sd,
      total = sum
  )) %>%
  ungroup %>%
  mutate(
    month = month(date),
    season = case_when(
      month >= 1 & month < 3 ~ "winter",
      month >= 3 & month < 6 ~ "spring",
      month >= 6 & month < 9 ~ "summer",
      month >= 9 & month < 12 ~ "fall",
      month == 12 ~ "winter"
    )
  )

daily_microclimate

#' ## Morphology data
#' 
#' ### Cover data
#'
#' Cover data was measured for four plots at a single site. 
#' Since the values for each plots is the number of filled squares, we can 
#' just sum them instead of looking at each separately or taking
#' the mean. This also makes more sense since each plot started with a different
#' number of filled squares, so not clear how to compare them.

cover_raw <- read_excel(
  "data_raw/カラクサシダ図表.xlsx",
  sheet = 1)

cover_raw

cover <- cover_raw %>%
  rename(date = "X__1", plot_1 = `No. 1`, plot_2 = `No. 2`, plot_3 = `No. 3`, plot_4 = `No. 4`) %>%
  mutate(date = lubridate::date(date)) %>%
  rowwise %>%
  mutate(total_cover = sum(plot_1, plot_2, plot_3, plot_4)) %>%
  mutate(
    month = month(date),
    year = year(date),
    month_year = paste(year, month, sep = "-"))

cover

#' ### Gemmae
#'
#' Read in pre-processed data (need to get raw data from Ebihara)

gemmae_count_raw <- read_xlsx(
  "data_raw/カラクサシダ図表.xlsx",
  sheet = 3,
  range = "A2:B62")

gemmae_count_raw

gemmae_length_raw <- read_xlsx(
  "data_raw/カラクサシダ図表.xlsx",
  sheet = 3,
  range = "E2:G62")

gemmae_length_raw

#' Tidy data. "month" column originally includes the year only for March 
#' the first time, then January.
#' 
#' Split this up and manually add year.
years <- c(rep(2009, 9),
           rep(2010, 12),
           rep(2011, 12,),
           rep(2012, 12),
           rep(2013, 12),
           rep(2014, 3))

gemmae_count <- 
  gemmae_count_raw %>%
  select(month = `採集月`, number = `数`) %>%
  mutate(
    month = str_remove_all(month, "月") %>% as.numeric,
    month = case_when(
      month > 20 & month < 40000 ~ 3,
      month > 20 ~ 1,
      TRUE ~ month
    ),
    year = years)

gemmae_length <- 
  gemmae_length_raw %>%
  select(month = `採集月`, length = `無性芽の平均の長さ(μm)`) %>%
  mutate(
    month = str_remove_all(month, "月") %>% as.numeric,
    month = case_when(
      month > 20 & month < 40000 ~ 3,
      month > 20 ~ 1,
      TRUE ~ month
    ),
    year = years)

gemmae_data <- full_join(gemmae_count, gemmae_length) %>%
  mutate(month_year = paste(year, month, sep = "-"))

gemmae_data

# Fig 1: Change in microclimate over time -----
daily_microclimate %>%
  select(-season, -month) %>%
  gather(var, value, -date, -site) %>% 
  ggplot(aes(x = date, y = value, color = site)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free")

# Fig 2: Change in morphology over time ----

# Fig 3: Compare microclimate between sites ----

#' For comparing microclimate between sites, subset the data to days only including
#' means from both sites.
both_days <-
  daily_microclimate %>%
  select(date, site, temp_mean) %>%
  spread(site, temp_mean) %>%
  drop_na() %>%
  pull(date)

paired_microclimate <- daily_microclimate %>%
  filter(date %in% both_days)

# Make plot including t-test on three microclimatic variables.
# Final plot includes multiple subplots. Each is a ridge plot showing 
# the distribution of one of the variables between the two sites, with the
# results of a paired t-test showing if they are different or not.

selected_vars <- c("rh_min", "par_total", "temp_mean")

# For looping across plots, make cross-product dataframe of all seasons and
# selected variables.
var_grid <-
  list(
    var = selected_vars,
    season = c("winter", "spring", "summer", "fall")
  ) %>% cross_df()

# Need to manually set x ranges to keep x-axis scale the same across plots
x_ranges <- tibble(var = selected_vars) %>%
  mutate(
    min = map_dbl(var, ~ pull(paired_microclimate, .) %>% min ),
    max = map_dbl(var, ~ pull(paired_microclimate, .) %>% max )
  )

# For looping across plots, make a nested dataframe by season.
nested_paired_microclimate <-
paired_microclimate %>%
  select(date, site, season, selected_vars) %>%
  group_by(season) %>%
  nest %>%
  right_join(var_grid)

# Make plots by mapping across nested datasets.
plot_results <-
  nested_paired_microclimate %>%
  right_join(x_ranges) %>%
  mutate(min = min - 0.05 * min,
         max = max + 0.05 * max,
         plot = pmap(list(var, data, x_min = min, x_max = max), 
                     plot_ridges, 
                     y_var = "site"),
         pval = map2(var, data, run_t_test),
         pval = case_when(
           pval < 0.0001 ~ "***",
           pval < 0.001 ~ "**",
           pval < 0.05 ~ "*",
           TRUE ~ ""
         ),
         plot = map2(
           plot, pval, 
           ~ .x + annotate("text", Inf, Inf, 
                           label = .y, hjust = 1, vjust = 1))
         
         # If needed, add annotation for season to make sure subplots
         # are positioned correctly
         
         # ,plot = map2(
         #   plot, season,
         #   ~ .x + annotate("text", -Inf, Inf,
         #                   label = .y, hjust = -1, vjust = 1)),
         # season = as_factor(season),
         # season = fct_relevel(season, "winter", "spring", "summer", "fall")
         
  ) %>% 
  arrange(season, var)

# Manually tweak some subplots to remove redundant labels, etc.
plot_results$plot[1:9] <- map(plot_results$plot[1:9], 
                             ~ . + theme(axis.title.x = element_blank()) )

plot_results$plot[[10]] <- plot_results$plot[[10]] +
  labs(x = expression("PAR (mmol per sq. cm)"))

plot_results$plot[[11]] <- plot_results$plot[[11]] +
  labs(x = expression("Rel. Humidity (%)"))

plot_results$plot[[12]] <- plot_results$plot[[12]] +
  labs(x = expression("Temp. (°C)"))

# Want to add y axis labels for season but this is not working.
plot_results$plot[[1]] <- plot_results$plot[[1]] +
  labs(y = "Winter")

plot_results$plot[[4]] <- plot_results$plot[[4]] +
  labs(y = "Spring")

plot_results$plot[[7]] <- plot_results$plot[[7]] +
  labs(y = "Summer")

plot_results$plot[[10]] <- plot_results$plot[[10]] +
  labs(y = "Fall")

main_plot <- plot_grid(plotlist = plot_results$plot, ncol = 3, align = "hv")

legend <- paired_microclimate %>%
  ggplot(aes(x = temp_mean, y = site, fill = site)) +
  geom_density_ridges() +
  theme(legend.position = "bottom")
legend <- get_legend(legend)

combined_plot <- plot_grid(main_plot, legend, ncol = 1, rel_heights = c(1, .1))

ggplot2::ggsave(plot = combined_plot, filename = "results/ridge_plots.pdf", height = 8, width = 6)

# Fig 4: Relationship between morphology and mean monthly climate ----
#' 
#' Run in a loop and plot each combination of climate vars as indep var
#' and morphology as dep var.
#' 
#' Calculate monthly means of the three selected variables (daily min RH, daily total PAR, daily
#' mean temp)

okutama_monthly <-
  okutama_microclimate %>%
  filter(complete.cases(.)) %>% # no missing values, so don't need na.rm
  group_by(date) %>%
  summarize_at(
    c("par", "temp", "rh"),
    funs(
      mean = mean,
      max = max,
      min = min,
      sd = sd,
      total = sum
    )) %>%
  ungroup %>%
  mutate(
    month = month(date),
    year = year(date),
    month_year = paste(year, month, sep = "-")) %>%
  group_by(month_year) %>%
  summarize_at(
    selected_vars,
    mean
  )

combined_morph <- 
full_join(
  select(cover, total_cover, month_year),
  select(gemmae_data, length, number, month_year, month, year)
)

combined_monthly_morph <- left_join(combined_morph, okutama_monthly) %>%
  ungroup %>% 
  mutate(month = as.factor(month))

combined_monthly_morph

#' ## Fit models

#' Loop through variables and make a model for each combination.
#' Note that since var_grid includes additional columns that aren't used
#' by the model function (y_lab and x_lab), need to include ... in 
#' the make_lm() function arguments.

#' Make dataframe of arguments to feed to the lm function. Includes all
#' combination of dependent (y) and independent (x) variables, and the
#' axis names to use for plotting.
var_grid <-
  list(
    x_var = selected_vars,
    y_var = c("length", "number", "total_cover")
  ) %>% cross_df() %>%
  mutate(
    y_lab = case_when(
      y_var == "length" ~ "Gemmae length (μm)", 
      y_var == "number" ~ "Gemmae number", 
      y_var == "total_cover" ~ "Total cover (sq. cm)" 
    ),
    x_lab = case_when(
      str_detect(x_var, "rh") ~ "Rel. Humidity (%)", 
      str_detect(x_var, "par") ~ "PAR (mmol per sq. cm)", 
      str_detect(x_var, "temp") ~ "Temp. (°C)" 
    )
  )

var_grid

#' Make tibble of data sets for looping. "data" column includes tibbles with
#'  x var, y var, month, and year for each set of variables.
model_data_sets <- pmap(
  var_grid, ~select(combined_monthly_morph, .x, .y, month, year) %>% drop_na)

model_data <-
  var_grid %>%
  mutate(data = model_data_sets)

model_data

#' Loop through the datasets and fit models to each. Tidy the output into
#' a single tibble with r.squared, p.value, and tibble of fits for each model.
model_results <-
  model_data %>%
  pmap(make_lm) %>%
  map_df(glance) %>%
  bind_cols(select(model_data, x_var, y_var)) %>%
  mutate(fits = 
           model_data %>%
           pmap(make_lm) %>%
           map(augment)
  )

model_results

#' ## Plotting

#' Loop through all combination of dep and indep variables and their models,
#' and make plots for each.
subplots <- 
  pmap(var_grid, 
       quick_plot_with_stats, 
       plot_data = combined_monthly_morph, 
       model_data = model_results)

#' Extract a legend to add to the combined plot.
legend <- quick_plot_with_stats(
  "rh_min", "length", 
  "minimum rh", "length", 
  combined_monthly_morph, model_results) + 
  labs(color = "Month") +
  theme(legend.position="bottom")

legend <- get_legend(legend)

#' Additional formatting for subplots: remove redundant subplot axis labels,
#' fix greek characters in labels.
for(i in 1:6) {
  subplots[[i]] <- subplots[[i]] + labs(x = "")
}

for(i in c(2,3,5,6,8,9) ) {
  subplots[[i]] <- subplots[[i]] + labs(y = "")
}

subplots[[1]] <- subplots[[1]] + labs(y = expression("Length ("~mu~"m)"))

#' Arrange plots are write out.
main_plot <- plot_grid(plotlist = subplots, align = "hv")

# Use NULL plots to center legend (need to have same number of columns).
legend_plot <- plot_grid(NULL, legend, NULL, nrow = 1)

okutama_model_plot <- plot_grid(main_plot, legend_plot, ncol = 1, rel_heights = c(1, .2))

# Save plot
ggsave(plot = okutama_model_plot,
       filename = "results/Pleurosoriopsis_combined_plot.pdf", width = 7, height = 9)

#'
# Render this script as a report
# rmarkdown::render("code/morph_clim_analysis.R", output_file = glue::glue("morph_clim_{Sys.Date()}.html"), output_dir = "reports/", knit_root_dir = here::here(), clean = TRUE)
