#' Workflow Plans
#'
#' ## Data sources
#' 
#' The raw data are in `.xlsx` files. Some of the data are pre-processed
#' (mean and sd of count and length of gemmae, 30 min averages of PPFD). 
#' 
#' The files include some additional analsyes and other data that won't 
#' be used here.

plan <- drake_plan (
  
  # Load and clean data ----
  
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
  #' For Okutama, the raw data for temperature, rel. hum,
  #' and light (PPFD) are in different columns
  #' in the `xlsx` file, so read in each variable separately.
  
  okutama_temp_raw = read_excel(
    file_in("data_raw/datalogger_raw.xlsm"),
    sheet = 1,
    range = "A2:B28683",
    col_names = c("date_time", "temp")),
  
  okutama_rh_raw = read_excel(
    file_in("data_raw/datalogger_raw.xlsm"),
    sheet = 1,
    range = "C2:D28683",
    col_names = c("date_time", "rh")),
  
  okutama_par_raw = read_excel(
    file_in("data_raw/datalogger_raw.xlsm"),
    sheet = 1,
    range = "F2:G28204",
    col_names = c("date_time", "par")),
  
  #' Combine raw okutama microclimate variables into a single tibble.
  okutama_microclimate_raw =
    full_join(okutama_temp_raw, okutama_rh_raw) %>%
    full_join(okutama_par_raw),
  
  #' Uratakao raw data have a single column for time of the three microclimate
  #' variables, so these can be read in all at once.
  takao_microclimate_raw = read_excel(
    file_in("data_raw/datalogger_raw.xlsm"),
    sheet = 2,
    range = "D2:G21459",
    col_names = c("date_time", "par", "temp", "rh")),
  
  #' ### Cover
  #'
  #' Cover data (area of gametophytes in sq. cm) was 
  #' measured once per month for 
  #' four 10 x 10 cm plots at the Okutama site. 
  cover_raw = read_excel(
    file_in("data_raw/pleurosoriopsis_data_figs.xlsx"),
    sheet = 1, 
    skip = 1,
    col_names = c("date", "q_1", "q_2", "q_3", "q_4")
    ),
  
  #' ### Gemmae
  #'
  #' Gemmae count and length were measured in 10 individuals per
  #' monthly census. The data provided are mean values with standard deviation
  #' already cacluated.
  
  gemmae_count_raw = read_xlsx(
    file_in("data_raw/pleurosoriopsis_data_figs.xlsx"),
    sheet = 3,
    range = "A2:C62"),
  
  gemmae_length_raw = read_xlsx(
    file_in("data_raw/pleurosoriopsis_data_figs.xlsx"),
    sheet = 3,
    range = "E2:G62",
    col_types = c("text", "numeric", "numeric")),
  
  #' ### Gametophyte size data
  #'
  #' Size data include length and width of invididuals, 
  #' split into different sheets based 
  #' on presence or absence of gametangia.
  #'
  #' Read in size data for individuals without gametangia
  size_asexual_raw = read_excel(
    file_in("data_raw/okutama_gameto_size.xlsx"),
    sheet = 1,
    range = "B4:D94"),
  
  #' Read in size data for individuals with gametangia.
  #' (No dates here)
  size_with_archegonia = read_excel(
    file_in("data_raw/okutama_gameto_size.xlsx"),
    sheet = 2,
    range = "B4:D36") %>%
    rename(id = `番号`, length = `長さ（㎜）`, width = `　幅（㎜）`) %>%
    mutate(sex = "female"),
  
  size_with_antheridia = read_excel(
    file_in("data_raw/okutama_gameto_size.xlsx"),
    sheet = 2,
    range = "O5:Q9")  %>%
    rename(id = `番号`, length = `長さ（㎜）　`, width = `幅（㎜）`) %>%
    mutate(sex = "male"),
  
  size_sexual = bind_rows(
    size_with_archegonia,
    size_with_antheridia
  ),
  
  #' ### Clean data
  #' 
  #' Select microclimate variables to use in downstream analyses
  selected_vars = c("rh_min", "par_total", "temp_mean"),
  
  #' Clean microclimate data
  okutama_microclimate = clean_microclimate(
    okutama_microclimate_raw),
  
  takao_microclimate = clean_microclimate(
    takao_microclimate_raw),
  
  #' Clean gametophyte cover data
  cover = clean_cover_data(cover_raw),
  
  #' Clean gemmae count and length data
  gemmae_data = clean_gemmae_data(
    gemmae_count_raw = gemmae_count_raw,
    gemmae_length_raw = gemmae_length_raw
  ),
  
  #' Clean asexual size data
  size_asexual_clean = clean_asexual_size(
    size_asexual_raw
  ),
  
  #' Combine asexual and sexual size data
  gameto_size = bind_rows(size_asexual_clean, size_sexual),
  
  #' Combine morphology into single tibble
  combined_morph = combine_morph_data (
    cover_data = cover,
    gemmae_data = gemmae_data
  ),
  
  #' Combine morphology and Okutama climate into single tibble
  combined_monthly_morph = combine_okutama_data (
    okutama_microclimate = okutama_microclimate, 
    combined_morph = combined_morph,
    selected_vars = selected_vars),
  
# Calculate daily summary variables ----
#' 
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

  # Combine clean microclimata data from Okutama and Uratakao
  # into a single tibble with rows of daily values.
  daily_microclimate = calculate_daily_microclimate (
    site_1_data = okutama_microclimate,
    site_1_name = "okutama",
    site_2_data = takao_microclimate,
    site_2_name = "uratakao"
  ),
  
  # Subset to only days that are shared between
  # sites for paired t-test.
  paired_microclimate = make_paired_dataset(
    daily_microclimate
  ),
  
  # Make nested data frame for looping.
  nested_paired_microclimate = nest_microclimate(
    paired_microclimate,
    selected_vars = selected_vars
  ),

# Analysis ----
  
  t_test_results = compare_microclimate(
    paired_microclimate = paired_microclimate,
    nested_paired_microclimate = nested_paired_microclimate,
    selected_vars = selected_vars
    ),
  
  linear_model_results = make_climate_morph_lm(
    selected_vars = selected_vars,
    combined_monthly_morph = combined_monthly_morph
  ),
  
  # Write out MS
  ms = rmarkdown::render(
    knitr_in("ms.Rmd"),
    output_format = "html_document",
    output_file = file_out("ms.html"),
    quiet = TRUE) 
  
)

#### TO DO: start from 05_analyze_growth_microclimate
