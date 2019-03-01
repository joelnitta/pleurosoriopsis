#' Fill NAs with last non-NA value in vector
#'
#' Copied from Claus Wilke's answer on SO:
#' https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
#'
#' @param x A vector with NAs
#'
#' @return Vector with NAs filled in with last non-NA value
rollForward <- function(x){
  curr <- 0
  for (i in 1:length(x)){
    if (is.na(x[i])){
      x[i] <- curr
    }
    else{
      curr <- x[i]
    }
  }
  return(x)
}



#' Clean times formatted with Japanese AM/PM characters.
#' 
#' @param data Tibble with column named 'date_time' which is
#' a character formatted like "09/22/12 12:07:03.0" for AM times or 
#' "09/24/13 03:52:44 午後" for PM times.
#' 
#' @return Tibble. "date_time" column is now a dttm, and a "date" column
#' also added.
clean_japanese_ampm <- function (data) {
  mutate(data,
         # Add 12 hours if time is PM ("午後")
         pm = str_detect(date_time, "午後"),
         date_time = mdy_hms(date_time, tz=Sys.timezone()),
         date_time = case_when (
           # Time units are in seconds when adding
           pm == TRUE ~ date_time + 60*60*12,
           TRUE ~ date_time
         ),
         date = date(date_time)
  ) %>%
    select(-pm)
}

#' Run a paired t-test comparing a microclimatic variable between
#' two sites
#'
#' @param var String; Name of the variable
#' @param paired_data Dataframe including measured variable at two sites
#' @param site1 String; Name of first site
#' @param site2 String; Name of second site
#' @param p_only Logical; should the p-value only be returned? If false, the
#' complete test results are returned as a dataframe.
#'
#' @return Numeric; p-value for null hypothesis that the difference between
#' the two means is zero.
#'
#' @examples
#' run_t_test("rh_min", paired_microclimate, "okutama", "uratakao")
run_t_test <- function (var, paired_data, site1 = "okutama", site2 = "uratakao", p_only = TRUE, ...) {
  
  var <- rlang::sym(var)
  
  test_data <-
    paired_data %>%
    select(date, !!var, site) %>%
    spread(site, !!var) %>%
    drop_na
  
  result <- 
  t.test(test_data[[site1]], test_data[[site2]], mu = 0, paired = TRUE) %>%
    tidy()
  
  if (isTRUE(p_only)) {
    result <-
    result %>%
    mutate(p.value = round(p.value, 3)) %>%
    pull(p.value)
  }
  
  return(result)
  
}

#' Make ridge plot showing distribution of microclimatic for two sites
#' and show p-value if they are significantly different.
#'
#' @param var String; Name of the variable
#' @param paired_data Dataframe including measured variable at two sites
#' @param site1 String; Name of first site
#' @param site2 String; Name of second site
#'
#' @return GGplot object
#'
#' @examples
#' plot_ridges("rh_min", "site", paired_microclimate, x_min = 0, x_max = 100)

plot_ridges <- function (x_var, y_var, data, x_min = NULL, x_max = NULL) {
  
  # p.value <- run_t_test(var, paired_data, site1, site2)
  
  # x_var <- rlang::sym(x_var)
  
  plot <- ggplot(data, aes_string(x = x_var, y = y_var, fill = y_var)) +
    geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
    coord_cartesian(clip = "off") + # to prevent lines from getting clipped near edge of plot
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank()
    )
  
  if(!is.null(x_min) & !is.null(x_max)) {
    plot <- plot +
      xlim(c(x_min, x_max))
  }
  
  plot
  
}

#' Function to run lm taking strings as variable names. 
#' 
#' Modified from
#' http://stat545.com/block025_lm-poly.html#use-our-lm-wrapper-with-broom
#'
#' @param x_var String, name of x variable (must be name of column in data)
#' @param y_var String, name of y variable (must be name of column in data)
#' @param data Dataframe
#' @param ... Not used by this function but needed so this can be used in pmap
#' with a dataframe including other non-argument columns.
make_lm <- function(x_var, y_var, data, ...) {
  x_var <- rlang::parse_expr(x_var)
  y_var <- rlang::parse_expr(y_var)
  lm_formula <- substitute(y_var ~ x_var)
  eval(lm(lm_formula, data = data))
}

#' Make a quick scatter plot with a linear regression
#' 
#' See these posts on how to get the formula in the upper RHC
#' 
#' https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
#' https://github.com/tidyverse/ggplot2/issues/1244
#'
#' @param x_var Name of x variable
#' @param y_var Name of y data
#' @param x_error Name of variable to use for error bars on x-axis
#' @param y_error Name of variable to use for error bars on y-axis
#' @param plot_data Data frame for plotting including x_var and y_var
#' @param model_data Data frame of results of linear model fit. Regression
#' line will be added only for models with p value < 0.05. 
#' @param no_legend Logical; should the legend be surpressed?
#'
#' @return Composite plot
#' @examples
#' quick_plot_with_stats(
#' "temp", "length", 
#' "Temp (C)", "Length (um)", 
#' combined_monthly_morph, model_results,
#' no_legend = FALSE)

quick_plot_with_stats <- function (x_var, y_var, x_error, y_error, plot_data, model_data, no_legend = TRUE, cols) {
  
  x_filter <- x_var
  y_filter <- y_var
  
  filtered_model_data <- filter(
    model_data, 
    x_var == x_filter, 
    y_var == y_filter
  )
  p.value <- pull(filtered_model_data, p.value) %>%
    signif(2)
  r.squared <- pull(filtered_model_data, r.squared) %>%
    signif(2)
  
  x_var <- rlang::parse_expr(x_var)
  y_var <- rlang::parse_expr(y_var)
  x_error <- ifelse(is.na(x_error), 0, rlang::parse_expr(x_error)) 
  y_error <- ifelse(is.na(y_error), 0, rlang::parse_expr(y_error)) 
  
  plot <- ggplot(plot_data, aes(x = !!x_var, y = !!y_var)) +
    # color by month, which is an ordered factor 1-12
    geom_errorbar(
      aes(ymin = !!y_var - !!y_error, 
          ymax = !!y_var + !!y_error), 
      width = 0,
      color = "grey",
      alpha = 0.5) + 
    geom_errorbarh(
      aes(xmin = !!x_var - !!x_error,
          xmax = !!x_var + !!x_error),
      height = 0,
      color = "grey",
      alpha = 0.5) + 
    geom_point(aes(color = month, shape = as.factor(year)), size = 2.25) +
    # start at a cool color for jan (teal-blue)
    scale_colour_hue(h.start = 180)
  
  if(isTRUE(no_legend)) {
    plot <- plot + theme(legend.position="none")
  }
  
  if(p.value < 0.05) {
    
    eq <- substitute(italic(r)^2~"="~r2*","~~italic(p)~"="~pval, 
                     list(r2 = r.squared,
                          pval = p.value)) %>% 
      as.expression %>%
      as.character
    
    plot <- plot +
      geom_line(
        data = model_data %>% 
          filter(x_var == x_filter, y_var == y_filter) %>% 
          pull(fits) %>%
          first,
        aes(y = `.fitted`),
        alpha = 0.8
      ) +
      annotate("text", Inf, Inf, label = eq, hjust = 1, vjust = 1, parse = TRUE)
  }
  
  plot
  
}

#' Shade years on a plot where the x-axis is a date
#'
#' @param plot ggplot2 plot object
#' @param rect_data dataframe including year_start and year_end columns,
#' where the rectangle should start and end
#'
#' @return ggplot2 object
shade_years <- function (plot, rect_data) {
plot +
geom_rect(
  data = rect_data, 
  mapping = aes(xmin = year_start, xmax = year_end, ymin = -Inf, ymax = +Inf),
  fill="grey", alpha=0.3, color = NA,
  inherit.aes = FALSE
)
}

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

#' ### Combine growth and microclimate data
#' 
#' Growth traits were measured once per month. To match these to microclimate,
#' calculate monthly mean and sd of the three selected microclimate variables.
#'
#' @param okutama_microclimate Tibble of clean microclimate data from okutama
#' @param combined_morph Tibble including combined morphogical variables
#' @param selected_vars Microclimate variables to include in the model
#'  (mean temp, min rel. hum., total PAR). 
#' @return tibble
combine_okutama_data <- function (okutama_microclimate, combined_morph, selected_vars) {
  
  # First calculate montly microclimate
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
  
  # Some of the growth data were measured before the
  # dataloggers were set up to measure mircroclimate.
  # Filter to only years including microclimate.
  year_microclimate_start <- okutama_microclimate %>% pull(date) %>% year %>% min(na.rm = TRUE) %>% as.numeric
  year_microclimate_end <- okutama_microclimate %>% pull(date) %>% year %>% max(na.rm = TRUE) %>% as.numeric
  
  # Join growth data with monthly climate data
  left_join(combined_morph, okutama_monthly) %>%
    ungroup %>% 
    mutate(month = as.factor(month)) %>%
    filter(between(year, year_microclimate_start, year_microclimate_end))
  
}

#' Make dataframe of arguments to feed to the `make_lm()` function. This includes all
#' combination of dependent (y) and independent (x) variables, and the
#' axis names to use for plotting.
#' @param selected_vars Microclimate variables to include in the model
#'  (mean temp, min rel. hum., total PAR).
#'  @return tibble
make_var_grid <- function (selected_vars) {
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
}

#' Fit models comparing microclimate and morphology
#'
#' Overall strategy is to loop through variables and make a model for each combination.
#' Note that since `var_grid` includes additional columns that aren't used
#' by the model function (`y_lab` and `x_lab`), need to include `...` in 
#' the `make_lm()` function arguments.
#'
#' First, make dataframe of arguments to feed to the `make_lm()` function. This includes all
#' combination of dependent (y) and independent (x) variables, and the
#' axis names to use for plotting.
#' 
#' For cover, since the values for each plot is the number of filled squares, 
#' use the total area of all them instead of taking the mean. This 
#' makes more sense since each plot started with a different
#' number of filled squares, so it is not clear how to compare them.
#' 
#' @param selected_vars Microclimate variables to include in the model
#'  (mean temp, min rel. hum., total PAR).
#' @param combined_monthly_morph Tibble including microclimate by month
#' and gametophyte growth data by month for Okutama site.
#' @return tibble; output of linear model.
make_climate_morph_lm <- function (selected_vars, combined_monthly_morph) {
  
  # Make grid of variables for mapping
  var_grid <- make_var_grid(selected_vars)
  
  #' Make tibble of data sets for looping. "data" column includes tibbles with
  #' x var, y var, month, and year for each set of variables.
  model_data_sets <- pmap(
    var_grid, ~select(combined_monthly_morph, .x, .y, month, year) %>% drop_na)
  
  model_data <-
    var_grid %>%
    mutate(data = model_data_sets)
  
  #' Loop through the datasets and fit models to each. Tidy the output into
  #' a single tibble with r.squared, p.value, and tibble of fits for each model.
  model_data %>%
    pmap(make_lm) %>%
    map_df(glance) %>%
    bind_cols(select(model_data, x_var, y_var)) %>%
    mutate(fits = 
             model_data %>%
             pmap(make_lm) %>%
             map(augment)
    )
  
}
