#' Run all analyses.

setwd(here::here())

scripts <- list.files("code", pattern = "0")

scripts <- sort(scripts)

purrr::walk(scripts, 
  ~ rmarkdown::render(
  input = glue::glue("code/{.}"),
  output_file = glue::glue("{.}_{Sys.Date()}.html"),
  output_dir = "results/",
  knit_root_dir = here::here(),
  clean = TRUE)
)