#+ include=FALSE
# Suppress chatty tidyverse messages when spinning
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' ## Compare microclimate between sites (Fig. 5 and Table 1)
#'
#' This script compares in microclimate 
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

#' Format Table 1 and output to RTF using `gt` package.

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

#' Render this script as a report (keep the below code commented-out)
# rmarkdown::render(
#   "code/04_compare_microclimate.R",
#   output_file = glue::glue("04_compare_microclimate_{Sys.Date()}.html"),
#   output_dir = "results/",
#   knit_root_dir = here::here(),
#   clean = TRUE)