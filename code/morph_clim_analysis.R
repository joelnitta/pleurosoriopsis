#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' # Growth Dynamics of the Independent Gametophytes of *Pleurorosiopsis makinoi* (Polypodiaceae)
#' 
#' Analyze correlation between microclimate and growth in a colony of
#' independent gametophytes of *Pleurosoriopsis makinoi* from Japan.
#' 
#' Load packages and custom functions.
library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)
library(broom)
library(ggridges)
library(patchwork)
library(gt)
library(viridis)

source("code/functions.R")

# ----
#' ## Load and clean data
#' 
#' ### Data sources
#' 
#' The raw data are in `xlsx` files. Some of the data are pre-processed
#' (mean and sd of count and length of gemmae, 30 min averages of PPFD). 
#' 
#' The files include some additional analsyes and other data that won't 
#' be used here.
#'
#' ### Microclimate data
#'
#' Microclimate variables include PPFD (photon flux density, in μmol of 
#' light per sq m per sec), rel. humidity (%), and temperature (°C) 
#' measured once every 30 min. 
#' 
#' PPFD is 30 min-averages of values taken every 4 minutes
#' (raw 4 minute values not included).
#' 
#' There are two sites, Okutama (site of the independent gametophyte
#' colony) and Uratakao (site of the sporophyte population). Additional
#' sites were also measured, but not included in this analysis
#' because of too much missing data due to mechanical failures.
#' 
#' For Okutama, the raw data are in different columns
#' in the `xlsx` file, so read in each separately.
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
#' Add 12 hours if time is PM ("午後"). 
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
#' variables, so these can be read in all at once.
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

#' In order to compare data across sites, we need to use daily values since
#' the measurement times are slightly different between sites. 
#' 
#' There are many different ways to calculate
#' daily microclimate values (mean, max, min, sd, of temp, humidity, etc.). 
#' 
#' We will use three biologically relevant values: mean temp, min RH,
#' and DLI (daily light integral, the sum of all PAR measurements for one day).
#' 
#' https://en.wikipedia.org/wiki/Daily_light_integral
#' 
#' To calculate DLI (moles of light per sq m per day), we need to integrate daily PPFD 
#' (photon flux density, in μmol of light per sq m per sec) values.
#' 
#' PPFD was measured once every 4 minutes in then converted to a mean
#' value once every 30 minutes. The values in the raw data are the 30
#' minute averages. To get DLI:
#' 
#' μmol PAR per second * 1800 seconds in 30 min = μmol PAR in 30 min
#' 
#' sum of all μmol PAR in 30 min values in one day / 1,000,000 = total mol PAR for the day (DLI)
#' 
daily_microclimate <-
  bind_rows(
    mutate(okutama_microclimate, site = "okutama"),
    mutate(takao_microclimate, site = "uratakao")
  ) %>%
  # only include days with data at both sites
  filter(complete.cases(.)) %>% 
  group_by(date, site) %>%
  summarize(
    temp_mean = mean(temp, na.rm = TRUE),
    rh_min = min(rh, na.rm = TRUE),
    par_total = sum(1800*par) / 1000000
  ) %>%
  ungroup %>%
  # add season variable
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

#' ### Cover data
#'
#' Cover data (area of gametophytes in sq. cm) was measured once per month for 
#' four 10 x 10 cm plots at the Okutama site. 
#' 
#' Clean up and also calculate total cover, which will be used later.

cover_raw <- read_excel(
  "data_raw/カラクサシダ図表.xlsx",
  sheet = 1)

cover <- cover_raw %>%
  rename(date = "X__1", q_1 = `No. 1`, q_2 = `No. 2`, q_3 = `No. 3`, q_4 = `No. 4`) %>%
  mutate(date = lubridate::date(date)) %>%
  rowwise %>%
  mutate(total_cover = sum(q_1, q_2, q_3, q_4)) %>%
  mutate(
    month = month(date),
    year = year(date),
    month_year = paste(year, month, sep = "-"))

cover

#' ### Gemmae
#'
#' Gemmae count and length were measured in 10 individuals per
#' monthly census. The data provided are mean values with standard deviation
#' already cacluated.

gemmae_count_raw <- read_xlsx(
  "data_raw/カラクサシダ図表.xlsx",
  sheet = 3,
  range = "A2:C62")

gemmae_count_raw

gemmae_length_raw <- read_xlsx(
  "data_raw/カラクサシダ図表.xlsx",
  sheet = 3,
  range = "E2:G62",
  col_types = c("text", "numeric", "numeric"))

gemmae_length_raw

#' Tidy gemmae data. "month" column originally includes the year only for March 
#' the first time, then January.
#' 
#' Split this up and manually add year.
years <- c(rep(2009, 9),
           rep(2010, 12),
           rep(2011, 12),
           rep(2012, 12),
           rep(2013, 12),
           rep(2014, 3))

gemmae_count <- 
  gemmae_count_raw %>%
  select(month = `採集月`, count_mean = `数`, count_sd = `標準偏差`) %>%
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
  select(month = `採集月`, length_mean = `無性芽の平均の長さ(μm)`, length_sd = `標準偏差`) %>%
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

#' Combine all growth (morphological) varibles into a single tibble.
#' 
#' Since these variables were measured once per month, treat day as 1st of
#' each month for joining with monthly microclimate variables later.
combined_morph <- 
  full_join(
    select(cover, total_cover, month_year),
    gemmae_data
  ) %>%
  mutate(day = 01,
         date = paste(year, month, day, sep = "-") %>% as.Date)

combined_morph

# ----
#' ## Fig 3: Change in growth over time
#'
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
#' formatting. Use viridis for colorblind-friendly colors.
subplots <- list()

subplots[["cover"]] <- 
  cover %>%
  select(starts_with("q"), date) %>%
  gather(plot, area, -date) %>%
  ggplot(aes(x = date, y = area, color = plot)) %>%
  shade_years(shading_dates) +
  geom_line() +
  labs(
    x = "",
    y = expression("Cover ("~cm^2~")"),
    subtitle = "a"
  ) +
  scale_color_viridis(discrete=TRUE,
                      begin = 0.025,
                      end = 0.97,
                      name="Quadrat",
                      breaks=c("q_1", "q_2", "q_3", "q_4"),
                      labels=c("1", "2", "3", "4")) +
  theme(legend.position = "bottom")

# extract legend from first plot (to put at bottom), then drop
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
  geom_point(color = "blue") +
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
  geom_point(color = "blue") +
  labs(
    x = "",
    y = expression(paste("Gemmae len. (", mu, "m)")),
    subtitle = "c"
  )

#' Apply common formatting to all subplots: x-axis labels rotated 30 degrees,
#' add 10 pt to R, L margin to make room for two-line labels, scale x-axis to print
#' month every 6 months, and use common limits.
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
  file = "results/fig3_morph.pdf",
  height = 7,
  width = 8)

# ----
#' ## Fig 4: Change in microclimate over time
#'
#' Select microclimate variables of interest
selected_vars <- c("rh_min", "par_total", "temp_mean")

#' Extract common start and end dates so x-axis are same across subplots
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

#' Make list of subplots
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

#' Apply common formatting
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

#' Remove x-axis lables from upper plots
subplots[1:4] <- subplots[1:4] %>%
  map(~ . + theme(axis.text.x = element_blank()) 
  )

#' Close gaps on top margins for lower plots
subplots[c(3,5)] <- subplots[c(3,5)] %>%
  map(~ . + theme(plot.margin = margin(-10,10,0,10)) 
  )

#' Close gaps on left side for RHS plots
subplots[c(2,4,6)] <- subplots[c(2,4,6)] %>%
  map(~ . + theme(plot.margin = margin(-10,10,0,-40)) 
  )

#' Combine plots and write out
subplots[[1]] + subplots[[2]] + subplots[[3]] + 
  subplots[[4]] + subplots[[5]] + subplots[[6]] + 
  plot_layout(ncol = 2)

ggsave(
  file = "results/fig4_clim.pdf",
  height = 7,
  width = 8)

# ----
#' ## Fig 5 and Table 1: Compare microclimate between sites
#'
#' Subset the data to days only including means from both sites so they can
#' be compared.

# First get vector of days present in both sites, then use this to filter.
both_days <-
  daily_microclimate %>%
  select(date, site, temp_mean) %>%
  spread(site, temp_mean) %>%
  drop_na() %>%
  pull(date)

paired_microclimate <- daily_microclimate %>%
  filter(date %in% both_days)

paired_microclimate

#' Make a tibble of nested dataframes by season. We will loop across these
#' and calculate mean and sd of each selected variable, and do a paired
#' t-test between sites. 
#' 
# First make a cross-product dataframe including all seasons and
# selected variables.
var_grid <-
  list(
    var = selected_vars,
    season = c("winter", "spring", "summer", "fall")
  ) %>% cross_df()

var_grid

# Use `var_grid` to build a tibble of nested dataframes by season.
nested_paired_microclimate <-
  paired_microclimate %>%
  select(date, site, season, selected_vars) %>%
  group_by(season) %>%
  nest %>%
  right_join(var_grid)

nested_paired_microclimate

#' Calculate mean and sd for selected microclimate variables during each season,
#' then add these to results of t-test. Keep everything wide for outputting to
#' Table 1 in the paper.
means_by_site_wide <-
  paired_microclimate %>%
  select(-date, site, season, selected_vars) %>%
  group_by(site) %>%
  nest %>%
  mutate(
    means = map(
      data, ~
        group_by(., season) %>%
        summarize_at(
          vars(selected_vars),
          ~ mean(., na.rm = TRUE)) %>%
        gather(var, mean, -season)
    )
  ) %>%
  select(-data) %>%
  unnest(means) %>%
  spread(site, mean) %>%
  rename(okutama_mean = okutama, uratakao_mean = uratakao)

means_by_site_wide

sd_by_site_wide <-
  paired_microclimate %>%
  select(-date, site, season, selected_vars) %>%
  group_by(site) %>%
  nest %>%
  mutate(
    means = map(
      data, ~
        group_by(., season) %>%
        summarize_at(
          vars(selected_vars),
          ~sd(., na.rm = TRUE) ) %>%
        gather(var, mean, -season)
    )
  ) %>%
  select(-data) %>%
  unnest(means) %>%
  spread(site, mean) %>%
  rename(okutama_sd = okutama, uratakao_sd = uratakao)

sd_by_site_wide
  
t_test_results <-
  nested_paired_microclimate %>%
  mutate(
    results = map2(var, data, run_t_test, p_only = FALSE)
  ) %>%
  unnest(results) %>%
  rename(pval = p.value) %>%
  select(-data, -method, -alternative) %>%
  left_join(means_by_site_wide) %>%
  left_join(sd_by_site_wide)

t_test_results

#' Format Table 1 and output to RTF using `gt` library.

# order of variables for printing
order_vars <- c("par_total", "rh_min", "temp_mean")

table_1 <-
  t_test_results %>%
  arrange(factor(var, levels = order_vars)) %>%
  mutate(season = stringr::str_to_title(season)) %>%
  mutate(var = case_when(
    var == "rh_min" ~    "Min. Rel. Humidity",
    var == "par_total" ~ "DLI",
    var == "temp_mean" ~ "Mean Temp."
  )) %>%
  group_by(season) %>%
  gt() %>%
  fmt_number(
    columns = vars(estimate, statistic, conf.low, conf.high, 
                   okutama_mean, okutama_sd, 
                   uratakao_mean, uratakao_sd)
  ) %>%
  cols_move(
    columns = vars(okutama_mean, okutama_sd),
    after = vars(conf.high)
  ) %>%
  tab_spanner(
    label = "Okutama",
    columns = vars(okutama_mean, okutama_sd)
  ) %>%
  tab_spanner(
    label = "Uratakao",
    columns = vars(uratakao_mean, uratakao_sd)
  ) %>%
  cols_label(
    var = "",
    statistic = "t",
    pval = "p",
    parameter = "df",
    conf.low = "Lower",
    conf.high = "Upper",
    okutama_mean = "Mean",
    okutama_sd = "SD",
    uratakao_mean = "Mean",
    uratakao_sd = "SD"
  ) %>%
  tab_style(
    style = cells_styles(
      bkgd_color = "white"),
    locations = cells_data(
      columns = everything()
    )
  ) %>%
  tab_header(
    title = "Table 1"
  )

table_1

as_rtf(table_1) %>%
  write_lines("results/table1.rtf")

#' Extract p-values for plotting, and put into data frame
#' with xvals specifying season and yvals specifying height 
#' to plot asterisks.
p_vals <-
  t_test_results %>%
  mutate(pval = case_when(
    pval < 0.0001 ~ "***",
    pval < 0.001 ~ "**",
    pval < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  select(season, var, pval) %>%
  spread(var, pval) %>%
  rename(
    par_total_pval = par_total,
    rh_min_pval = rh_min,
    temp_mean_pval = temp_mean
  )

# Simple function to calculate maximum y-position for plotting asterisks.
# Should be just above the mean + error bar height (1 sd).
maxy <- function(x) {mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)}

pval_plot_data <- 
  paired_microclimate %>%
  select(-date, -month) %>%
  group_by(season, site) %>%
  summarize_all(
    maxy
  ) %>%
  group_by(season) %>%
  summarize_all(
    max
  ) %>%
  select(-site) %>%
  left_join(p_vals)

pval_plot_data

#' Format data for plotting: need columns to be mean and sd, by site and season.
paired_means <- 
  paired_microclimate %>%
  select(-date) %>%
  group_by(site, season) %>%
  summarize_all(
    list(~mean(., na.rm = TRUE),
         ~sd(., na.rm = TRUE))
  ) %>%
  mutate(
    season = factor(season, levels = c("winter", "spring", "summer", "fall"))
  )

paired_means

#' Make plot including t-test on three microclimatic variables.
#' 
#' The final plot includes multiple subplots. Each is a ridge plot showing 
#' the distribution of one of the variables between the two sites, with the
#' results of a paired t-test showing if they are different or not.
#' 
#' Need to manually set x ranges to keep x-axis scale the same across plots
x_ranges <- tibble(var = selected_vars) %>%
  mutate(
    min = map_dbl(var, ~ pull(paired_microclimate, .) %>% min ),
    max = map_dbl(var, ~ pull(paired_microclimate, .) %>% max )
  )

# light blue / light green colorblind-friendly from colorbrewer
# paired_colors <- c("#a6cee3", "#b2df8a")
# OR dark blue / dark green colorblind-friendly from colorbrewer
paired_colors <- c("#1f78b4", "#33a02c")

subplots <- list()

subplots[["par_total"]] <-
  ggplot(paired_means, aes(x = season, y = par_total_mean, fill = site)) +
  geom_errorbar(
    aes(
      ymin = par_total_mean - par_total_sd,
      ymax = par_total_mean + par_total_sd
    ),
    position=position_dodge(.9),
    width = 0.3) +
  annotate(
    "text",
    x = pval_plot_data$season,
    y = pval_plot_data$par_total * 1.05,
    label = pval_plot_data$par_total_pval
  ) +
  geom_bar(stat = "identity", position=position_dodge(), 
           color = "black") +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  labs(y = expression(paste("DLI (", mol~m^-2~day^-1, ")", sep = ""))) +
  labs(x = "") +
  scale_fill_manual(values = paired_colors,
                    breaks=c("okutama", "uratakao")) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank())

subplots[["rh_min"]] <-
  ggplot(paired_means, aes(x = season, y = rh_min_mean, fill = site)) +
  geom_errorbar(
    aes(
      ymin = rh_min_mean - rh_min_sd,
      ymax = rh_min_mean + rh_min_sd
    ),
    position=position_dodge(.9),
    width = 0.3) +
  annotate(
    "text",
    x = pval_plot_data$season,
    y = pval_plot_data$rh_min * 1.05,
    label = pval_plot_data$rh_min_pval
  ) +
  geom_bar(stat = "identity", position=position_dodge(), 
           color = "black") +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  labs(y = expression("Min. Rel. Humidity (%)")) +
  labs(x = "") +
  scale_fill_manual(values = paired_colors,
                    breaks=c("okutama", "uratakao")) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank())
  

subplots[["temp"]] <-
  ggplot(paired_means, aes(x = season, y = temp_mean_mean, fill = site)) +
  geom_errorbar(
    aes(
      ymin = ifelse(temp_mean_mean - temp_mean_sd > 0, temp_mean_mean - temp_mean_sd , 0),
      ymax = temp_mean_mean + temp_mean_sd
    ),
    position=position_dodge(.9),
    width = 0.3) +
  annotate(
    "text",
    x = pval_plot_data$season,
    y = pval_plot_data$temp_mean * 1.05,
    label = pval_plot_data$temp_mean_pval
  ) +
  geom_bar(stat = "identity", position=position_dodge(), 
           color = "black") +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  labs(y = expression("Mean Temp. (°C)")) +
  labs(x = "") +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("winter" = "Winter",
                     "spring" = "Spring",
                     "fall" = "Fall",
                     "summer" = "Summer")) +
  scale_fill_manual(values = paired_colors,
                     name="Site",
                     breaks=c("okutama", "uratakao"),
                     labels=c("Okutama", "Uratakao"))

subplots[[1]] +  subplots[[2]] +  subplots[[3]] + plot_layout(ncol = 1)

ggplot2::ggsave(
  filename = "results/fig5_climate_bars.pdf", 
  height = 8, width = 7)

#' Make plots by mapping across nested datasets.
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

subplots <- plot_results$plot

#' Manually some subplots to remove redundant labels, etc.
subplots[1:9] <- map(subplots[1:9], 
                             ~ . + theme(axis.title.x = element_blank(),
                                         axis.text.x = element_blank() ))


subplots[1:12] <- map(subplots[1:12], 
                     ~ . + scale_fill_manual(
                       values = paired_colors,
                       name="Site",
                       breaks=c("okutama", "uratakao"),
                       labels=c("Okutama", "Uratakao"))
)

subplots[[10]] <- subplots[[10]] +
  labs(x = expression(paste("DLI (", mol~m^-2~day^-1, ")", sep = "")) )

subplots[[11]] <- subplots[[11]] +
  labs(x = expression("Min. Rel. Humidity (%)"))

subplots[[12]] <- subplots[[12]] +
  labs(x = expression("Mean Temp. (°C)"))

#' Add titles.
subplots <- map(subplots, ~ . + labs(title = "") )

subplots[[2]] <- subplots[[2]] +
  labs(title = "Winter\n")

subplots[[5]] <- subplots[[5]] +
  labs(title = "Spring\n")

subplots[[8]] <- subplots[[8]] +
  labs(title = "Summer\n")

subplots[[11]] <- subplots[[11]] +
  labs(title = "Fall\n")

#' Extract legend
legend <- paired_microclimate %>%
  ggplot(aes(x = temp_mean, y = site, fill = site)) +
  geom_density_ridges() +
  theme(legend.position = "bottom") +
  scale_fill_manual(
    values = paired_colors,
    name="Site",
    breaks=c("okutama", "uratakao"),
    labels=c("Okutama", "Uratakao"))

legend <- get_legend(legend)

#' Combine and write out.
subplots[[1]] +  subplots[[2]] +  subplots[[3]] + 
subplots[[4]] +  subplots[[5]] +  subplots[[6]] + 
subplots[[7]] +  subplots[[8]] +  subplots[[9]] + 
subplots[[10]] + subplots[[11]] + subplots[[12]] +
plot_spacer() + legend + plot_spacer() + plot_layout(ncol = 3, heights = c(1, 1, 1, 1, 0.2))

ggplot2::ggsave(
  filename = "results/fig5_climate_diff.pdf", 
  height = 8, width = 7)

# ----
#' ## Fig 6: Growth vs. mean monthly climate
#' 
#' Run in a loop and plot each combination of climate vars as indep var
#' and morphology as dep var.
#' 
#' Calculate monthly mean and sd of the three selected microclimate variables 
#' (daily min RH, daily total PAR, daily
#' mean temp).
okutama_monthly <-
  okutama_microclimate %>%
  filter(complete.cases(.)) %>% # no missing values, so don't need na.rm
  group_by(date) %>%
  summarize(
    temp_mean = mean(temp, na.rm = TRUE),
    rh_min = min(rh, na.rm = TRUE),
    par_total = sum(1800*par) / 1000000
  ) %>%
  ungroup %>%
  mutate(
    month = month(date),
    year = year(date),
    month_year = paste(year, month, sep = "-")) %>%
  group_by(month_year) %>%
  summarize_at(
    selected_vars,
    list(
      ~ mean(., na.rm = TRUE),
      ~ sd(., na.rm = TRUE)
    )
  ) %>%
  # For clarity, drop extra "mean" from variable names.
  # Remember that we're dealing with monthly means though.
  rename(
    rh_min = rh_min_mean,
    temp_mean = temp_mean_mean,
    par_total = par_total_mean
  )

year_microclimate_start <- okutama_microclimate %>% pull(date) %>% year %>% min(na.rm = TRUE) %>% as.numeric
year_microclimate_end <- okutama_microclimate %>% pull(date) %>% year %>% max(na.rm = TRUE) %>% as.numeric

combined_monthly_morph <- left_join(combined_morph, okutama_monthly) %>%
  ungroup %>% 
  mutate(month = as.factor(month)) %>%
  filter(between(year, year_microclimate_start, year_microclimate_end))

combined_monthly_morph

#' #### Fit models
#'
#' Loop through variables and make a model for each combination.
#' Note that since var_grid includes additional columns that aren't used
#' by the model function (y_lab and x_lab), need to include ... in 
#' the make_lm() function arguments.
#'
#' Make dataframe of arguments to feed to the lm function. Includes all
#' combination of dependent (y) and independent (x) variables, and the
#' axis names to use for plotting.
#' 
#' For cover, since the values for each plot is the number of filled squares, 
#' use the total area of all them instead of taking the mean. This 
#' makes more sense since each plot started with a different
#' number of filled squares, so it is not clear how to compare them.
var_grid <-
  list(
    x_var = selected_vars,
    y_var = c("length_mean", "count_mean", "total_cover")
  ) %>% cross_df() %>%
  mutate(
    y_error = case_when(
      y_var == "length_mean" ~ "length_sd", 
      y_var == "count_mean" ~ "count_sd"
    ),
    x_error = case_when(
      x_var == selected_vars[[1]] ~ paste0(selected_vars[[1]], "_sd"),
      x_var == selected_vars[[2]] ~ paste0(selected_vars[[2]], "_sd"),
      x_var == selected_vars[[3]] ~ paste0(selected_vars[[3]], "_sd")
    )
  ) %>%
  mutate(y_var = factor(y_var, levels = c("total_cover", "count_mean", "length_mean")),
         x_var = factor(x_var, levels = sort(unique(x_var)))
         ) %>%
  arrange(y_var, x_var) %>%
  mutate(
    x_var = as.character(x_var),
    y_var = as.character(y_var)
  )

#' Make tibble of data sets for looping. "data" column includes tibbles with
#' x var, y var, month, and year for each set of variables.
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

#' Table 2: model results
cols <- colnames(model_results)
cols_numeric <- cols[map_lgl(model_results, is.numeric)]
no_fmt_num <- c("p.value", "df", "df.residual", "deviance")
cols_numeric <- cols_numeric[!cols_numeric %in% no_fmt_num]

table_2 <- 
  model_results %>%
  select(-fits) %>%
  arrange(factor(x_var, levels = order_vars)) %>%
  mutate(
    x_var = case_when(
      x_var == "rh_min" ~    "Min. Rel. Humidity",
      x_var == "par_total" ~ "DLI",
      x_var == "temp_mean" ~ "Mean Temp."
    ),
    y_var = case_when(
      y_var == "total_cover" ~ "Total cover",
      y_var == "count_mean" ~ "Gemmae count",
      y_var == "length_mean" ~ "Gemmae length"
    )
  ) %>%
  group_by(x_var) %>%
  gt() %>%
  cols_move_to_start(vars(y_var)) %>%
  cols_move(
    columns = vars(df.residual),
    after = vars(df)
  ) %>%
  fmt_number(
    columns = vars(cols_numeric)
  ) %>%
  fmt_number(
    columns = vars(p.value),
    decimals = 3
  ) %>%
  fmt_number(
    columns = vars(deviance),
    decimals = 0
  ) %>%
  cols_label(
    y_var = "",
    r.squared = "r-squared",
    adj.r.squared = "adj. r-squared",
    statistic = "t",
    p.value = "p",
    df.residual = "df residual"
    ) %>%
  tab_style(
    style = cells_styles(
      bkgd_color = "white"),
    locations = cells_data(
      columns = everything()
    )
  ) %>%
  tab_header(
    title = "Table 2"
  )

table_2

as_rtf(table_2) %>%
  write_lines("results/table2.rtf")

#' #### Plotting
#' 
#' Loop through all combination of dep and indep variables and their models,
#' and make plots for each.
subplots <- 
  pmap(var_grid, 
       quick_plot_with_stats, 
       plot_data = combined_monthly_morph, 
       model_data = model_results)

#' Add axis titles
#' 
#' First, remove all default titles, then add formatted titles.
subplots <- map(subplots, ~ . + labs(x = "", y = ""))
subplots[[1]] <- subplots[[1]] + labs(y = expression("Total cover ("~cm^2~")"))
subplots[[4]] <- subplots[[4]] + labs(y = "Gemmae count")
subplots[[7]] <- subplots[[7]] + 
  labs(
    y = expression(paste("Gemmae len. (", mu, "m)", sep = "")),
    x = expression(paste("DLI (", mol~m^-2~day^-1, ")", sep = ""))
    )
subplots[[8]] <- subplots[[8]] + labs(x = expression("Min. Rel. Humidity (%)"))
subplots[[9]] <- subplots[[9]] + labs(x = expression("Mean Temp. (°C)"))

#' Set amount of gap to close on left and top
left_close <- -15
top_close <- -10

#' Close gaps on left only: 2,3
subplots[2:3] <- subplots[2:3] %>%
  map(~ . + theme(plot.margin = margin(0,0,0,left_close)) 
  )

#' Close gaps on left side and top: 5,6,8,9
subplots[c(5,6,8,9)] <- subplots[c(5,6,8,9)] %>%
  map(~ . + theme(plot.margin = margin(top_close,0,0,left_close)) 
  )

#' Close gaps on top only: 4,7
subplots[c(4,7)] <- subplots[c(4,7)] %>%
  map(~ . + theme(plot.margin = margin(top_close,0,0,0)) 
  )

#' Add legend for middle bottom plot, combine, and write out.
subplots[[8]] <- subplots[[8]] + 
  theme(legend.position="bottom") + 
  labs(color = "Month", shape = "Year")

subplots[[1]] + subplots[[2]] + subplots[[3]] + 
subplots[[4]] + subplots[[5]] + subplots[[6]] + 
subplots[[7]] + subplots[[8]] + subplots[[9]] + 
plot_layout(ncol = 3)

ggplot2::ggsave(
  filename = "results/fig6_morph_climate.pdf", 
  height = 8, width = 7)

# ----
# Render this script as a report
# rmarkdown::render("code/morph_clim_analysis.R", output_file = glue::glue("morph_clim_{Sys.Date()}.html"), output_dir = "results/", knit_root_dir = here::here(), clean = TRUE)
