#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Load and clean data
#' 
#' This script processes the raw data files in `data_raw/` and outputs
#' the cleaned data as `.csv` files to `data/`.
#' 
#' ### Setup
setwd(here::here())

source("code/setup.R")

#' ### Data sources
#' 
#' The raw data are in `.xlsx` files. Some of the data are pre-processed
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

write_csv(okutama_microclimate, "data/okutama_microclimate.csv")

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

write_csv(daily_microclimate, "data/daily_microclimate.csv")

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

write_csv(combined_morph, "data/combined_morph.csv")

#' Render this script as a report (keep the below code commented-out)
#  rmarkdown::render(
#   "code/01_clean_data.R",
#   output_file = glue::glue("01_clean_data_{Sys.Date()}.html"),
#   output_dir = "results/",
#   knit_root_dir = here::here(),
#   clean = TRUE)