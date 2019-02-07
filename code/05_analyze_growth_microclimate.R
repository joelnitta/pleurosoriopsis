#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Analyze relationship between growth and microclimate (Fig. 6 and Table 2)
#'
#' This script analyzes the relationship between growth 
#' of gametophytes of *Pleurosoriopsis makinoi* and
#' microclimate at Okutama, Japan.
#' 
#' Specifically, it makes a linear model for each combination 
#' of microclimate variables (daily min RH, daily total PAR, 
#' daily mean temp) as the independent variable and 
#' growth traits (cover, number and length of gemmae) 
#' as dependent variable.
#'   
#' ### Setup
setwd(here::here())

source("code/setup.R")

#' ### Load pre-processed data
okutama_microclimate <- read_csv("data/okutama_microclimate.csv")
combined_morph <- read_csv("data/combined_morph.csv")

#' ### Combine growth and microclimate data
#' 
#' Growth traits were measured once per month. To match these to microclimate,
#' calculate monthly mean and sd of the three selected microclimate variables.
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
combined_monthly_morph <- left_join(combined_morph, okutama_monthly) %>%
  ungroup %>% 
  mutate(month = as.factor(month)) %>%
  filter(between(year, year_microclimate_start, year_microclimate_end))

combined_monthly_morph

#' #### Fit models
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
cols_numeric <- c("r.squared", "adj.r.squared", "sigma", "statistic", "logLik", "AIC", "BIC")

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

#' Render this script as a report (keep the below code commented-out)
# rmarkdown::render(
#   "code/05_analyze_growth_microclimate.R",
#   output_file = glue::glue("05_analyzer_growth_microclimate_{Sys.Date()}.html"),
#   output_dir = "results/",
#   knit_root_dir = here::here(),
#   clean = TRUE)