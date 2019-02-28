#' Clean microclimate data.
#' 
#' Add 12 hours if time is PM ("午後"). 
#' Time units are in seconds when adding.
#'
#' @param raw_microclimate_data Tibble; Raw microclimate data read in from xlsx file.
#' @return Tibble
clean_microclimate <- function(raw_microclimate_data) {
  raw_microclimate_data %>%
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
  
}

#' Calculate daily microclimate values from clean data
#' including multiple observerations per day.
#' 
#' For each day, calculates mean temperature (temp_mean), 
#' minimum relative humidity (rh_mean), and total PAR (
#' photosynthetically active radiation; par_total). Also adds
#' columns for season and site name.
#'
#' @param site_1_data Tibble of microclimate data from one site.
#' @param site_1_name String; name of first site.
#' @param site_2_data Tibble of microclimate data from another site.
#' @param site_2_name String; name of second site.
#'
#' @return Tibble
calculate_daily_microclimate <- function (site_1_data, site_1_name, site_2_data, site_2_name) {
  bind_rows(
    mutate(okutama_microclimate, site = site_1_name),
    mutate(takao_microclimate, site = site_2_name)
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
}


#' Clean raw cover data and calculate total cover.
#'
#' @param cover_raw Raw cover data. Raw cover data read in from xlsx file.
#' @return Tibble.
clean_cover_data <- function(cover_raw) {
  cover_raw %>%
  rename(date = "X__1", q_1 = `No. 1`, q_2 = `No. 2`, q_3 = `No. 3`, q_4 = `No. 4`) %>%
  mutate(date = lubridate::date(date)) %>%
  rowwise %>%
  mutate(total_cover = sum(q_1, q_2, q_3, q_4)) %>%
  mutate(
    month = month(date),
    year = year(date),
    month_year = paste(year, month, sep = "-"))
}

#' Clean gemmae data. 
#' 
#' "month" column originally includes the year only for March 
#' the first time, then January.
#' 
#' Split this up and manually add year.
#'
#' @param gemmae_count_raw Raw gemmae count data read in from xlsx file.
#' @param gemmae_length_raw Raw gemmae length data read in from xlsx file.

clean_gemmae_data <- function (gemmae_count_raw, gemmae_length_raw) {

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

  full_join(gemmae_count, gemmae_length) %>%
  mutate(month_year = paste(year, month, sep = "-"))

}

# Clean size data of asexual gametophytes (no gemmae)

# The id column actually includes dates in parentheses in some cells 
# but others. Presumably the same date applies down the column until 
# the next date. Separate these into their own column.
#'
#' @param size_asexual_raw Raw data for gemmae size of asexual individuals read
#' in from excel file.
#'
#' @return Tibble
clean_asexual_size <- function (size_asexual_raw) {
  
size_asexual <- 
  size_asexual_raw %>%
  select(id = `番号`, length = `長さ(mm)`, width = `幅(mm)`) %>%
  mutate(id = str_split(id, "\\)") %>% map_chr(last))

size_asexual_dates <- size_asexual_raw %>% pull(`番号`)
size_asexual_dates[!str_detect(size_asexual_dates, "\\)")] <- NA
size_asexual_dates <-
  size_asexual_dates %>% 
  str_split("\\)") %>% 
  map_chr(first) %>%
  str_remove_all("\\(") %>%
  str_replace("\\/", "-") %>%
  rollForward

mutate(size_asexual, date = size_asexual_dates, sex = "asexual")

}

#' Combine cover and gemmae size varibles into a single tibble.
#' 
#' Since these variables were measured once per month, treat day as 1st of
#' each month for joining with monthly microclimate variables later.
#'
#' @param cover_data Clean gametophyte cover data.
#' @param gemmae_data Clean gemmae count and size data.
#' 
#' @return Tibble
combine_morph_data = function (cover_data, gemmae_data) {
  full_join(
    cover_data,
    gemmae_data
  ) %>%
  mutate(day = 01,
         date = paste(year, month, day, sep = "-") %>% as.Date)
  
}

#' Make "paired" dataset
#' 
#' Subset the data to days only including means from both sites so they can
#' be compared.
#'
#' @param daily_microclimate Tibble including daily microclimate
#' variables for two sites (days may differ between sites)
#' 
#' @return Tibble; tibble with only days that are in common between
#' the two sites
make_paired_dataset <- function (daily_microclimate) {
  
# First get vector of days present in both sites, then use this to filter.
both_days <-
  daily_microclimate %>%
  select(date, site, rh_min) %>%
  spread(site, rh_min) %>%
  drop_na() %>%
  pull(date)

daily_microclimate %>%
  filter(date %in% both_days)

}


#' Nest microclimate data for mapping
#' 
#' Make a tibble of nested dataframes by season. We will loop across these
#' and calculate mean and sd of each selected variable, and do a paired
#' t-test between sites. 
#' @param paired_microclimate tibble with microclimate including
#' only days that are in common between the two sites.
#' @param selected_vars Variables to compare (mean temp, min rel. hum.,
#' total PAR).
#' @return tibble
#' 
nest_microclimate <- function (selected_vars, paired_microclimate) {
  # First make a cross-product dataframe including all seasons and
  # selected variables.
  var_grid <-
    list(
      var = selected_vars,
      season = c("winter", "spring", "summer", "fall")
    ) %>% cross_df()
  
  # Use `var_grid` to build a tibble of nested dataframes by season.
  paired_microclimate %>%
    select(date, site, season, selected_vars) %>%
    group_by(season) %>%
    nest %>%
    right_join(var_grid)
  
}

#' Compare microclimate between sites using a t-test
#'
#' @param paired_microclimate Clean, paired microclimate data for two sites.
#' @param nested_paired_microclimate Nested version of microclimate data.
#' @param selected_vars Variables to compare (mean temp, min rel. hum.,
#' total PAR).
#' 
#' @return Tibble; results of t-test comparing between the two sites.
compare_microclimate <- function (paired_microclimate, nested_paired_microclimate, selected_vars) {
  
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
  
  nested_paired_microclimate %>%
    mutate(
      results = map2(var, data, run_t_test, p_only = FALSE)
    ) %>%
    unnest(results) %>%
    rename(pval = p.value) %>%
    select(-data, -method, -alternative) %>%
    left_join(means_by_site_wide) %>%
    left_join(sd_by_site_wide)
}
