#' Run a paired t-test comparing a microclimatic variable between
#' two sites
#'
#' @param var String; Name of the variable
#' @param paired_data Dataframe including measured variable at two sites
#' @param site1 String; Name of first site
#' @param site2 String; Name of second site
#'
#' @return Numeric; p-value for null hypothesis that the difference between
#' the two means is zero.
#'
#' @examples
#' run_t_test("rh_min", paired_microclimate, "okutama", "uratakao")
run_t_test <- function (var, paired_data, site1 = "okutama", site2 = "uratakao") {
  
  var <- rlang::sym(var)
  
  test_data <-
    paired_data %>%
    select(date, !!var, site) %>%
    spread(site, !!var) %>%
    drop_na
  
  t.test(test_data[[site1]], test_data[[site2]], mu = 0, paired = TRUE) %>%
    tidy() %>%
    mutate(p.value = round(p.value, 3)) %>%
    pull(p.value)
  
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
    geom_density_ridges() +
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