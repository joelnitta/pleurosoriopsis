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

#' In order to join the data, need to use mean daily values since
#' the times are slightly different.
daily_microclimate <-
  bind_rows(
    mutate(okutama_microclimate, site = "okutama"),
    mutate(takao_microclimate, site = "uratakao")
  ) %>%
  group_by(date, site) %>%
  summarize_at(
    c("par", "temp", "rh"),
    mean,
    na.rm = TRUE
  )

daily_microclimate

#' Quick visualization
daily_microclimate %>%
  gather(var, value, -date, -site) %>%
  ggplot(aes(x = date, y = value, color = site)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~var, scales = "free")

#' ## Cover data
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

#' ## Gemmae
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

#' ## Relationship between morphology and mean monthly climate 
#' 
#' Run in a loop and plot each combination of climate vars as indep var
#' and morphology as dep var.
#' 
#' Need to calculate mean monthly climate values,
#' since morphology was measured once per month.
okutama_monthly <-
  okutama_microclimate %>%
  mutate(
    month = month(date_time),
    year = year(date_time),
    month_year = paste(year, month, sep = "-")) %>%
  group_by(month_year) %>%
  summarize_at(
    c("par", "temp", "rh"),
    mean,
    na.rm = TRUE
  )

okutama_monthly

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
#'
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

#' Loop through variables and make a model for each combination.
#' Note that since var_grid includes additional columns that aren't used
#' by the model function (y_lab and x_lab),

#' Make dataframe of arguments to feed to the lm function. Includes all
#' combination of dependent (y) and independent (x) variables, and the
#' axis names to use for plotting.
var_grid <-
  list(
    x_var = c("rh", "par", "temp"),
    y_var = c("length", "number", "total_cover")
  ) %>% cross_df() %>%
  mutate(
    y_lab = case_when(
      y_var == "length" ~ "Gemmae length (μm)", 
      y_var == "number" ~ "Gemmae number", 
      y_var == "total_cover" ~ "Total cover (sq. cm)" 
    ),
    x_lab = case_when(
      x_var == "rh" ~ "Rel. Humidity (%)", 
      x_var == "par" ~ "PAR (mmol per sq. cm)", 
      x_var == "temp" ~ "Temp. (°C)" 
    )
  )

var_grid

#' Make tibble of data sets. "data" column includes tibbles with
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
#'

#' Make a quick scatter plot with a linear regression
#' 
#' See these posts on how to get the formula in the upper RHC
#' 
#' https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
#' https://github.com/tidyverse/ggplot2/issues/1244
#'
#' @param x_var Name of x variable
#' @param y_var Name of y data
#' @param x_lab Label for x variable
#' @param y_lab Label for y variable
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

quick_plot_with_stats <- function (x_var, y_var, x_lab, y_lab, plot_data, model_data, no_legend = TRUE, cols) {

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
  
 plot <- ggplot(plot_data, aes(x = !!x_var, y = !!y_var)) +
   # color by month, which is an ordered factor 1-12
   geom_point(aes(color = month)) +
   labs(x = x_lab,
        y = y_lab) +
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
       aes(y = `.fitted`)
     ) +
     annotate("text", Inf, Inf, label = eq, hjust = 1, vjust = 1, parse = TRUE)
 }
 
 plot
 
}

#' Loop through all combination of dep and indep variables and their models,
#' and make plots for each.
subplots <- 
  pmap(var_grid, 
       quick_plot_with_stats, 
       plot_data = combined_monthly_morph, 
       model_data = model_results)

#' Extract a legend to add to the combined plot.
legend <- quick_plot_with_stats(
  "rh", "length", 
  "rh", "length", 
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

plot_grid(main_plot, legend_plot, ncol = 1, rel_heights = c(1, .2))

# Save plot
ggsave("results/Pleurosoriopsis_combined_plot.pdf", width = 7, height = 9)

#'
# Render this script as a report
# rmarkdown::render("code/morph_clim_analysis.R", output_file = glue::glue("morph_clim_{Sys.Date()}.html"), output_dir = "reports/", knit_root_dir = here::here(), clean = TRUE)
