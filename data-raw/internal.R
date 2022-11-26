library(magrittr)

parse_and_extract_ranges <- function(data, column, lsuffix = "_l", rsuffix = "_r") {
  colname <- rlang::as_name(enquo(column))
  data %>%
    dplyr::mutate(
      {{ column }} := {{ column }} %>%
        stringr::str_replace(r"(^>(\d+|\d*\.\d+)$)", r"([\1,+Inf])") %>%
        stringr::str_replace(r"(^<(\d+|\d*\.\d+)$)", r"([-Inf,\1])") %>%
        stringr::str_replace(r"(^(\d+|\d*\.\d+)-(\d+|\d*\.\d+)$)", r"([\1,\2])") %>%
        stringr::str_replace(r"(^(\d+|\d*\.\d+)$)", r"([\1,\1])")
    ) %>%
    tidyr::extract({{ column }},
      c(paste0(colname, lsuffix), paste0(colname, rsuffix)),
      r"(\[(\d+|\d*\.\d+|-Inf),(\d+|\d*\.\d+|\+?Inf)\])",
      convert = TRUE
    )
}

nn_tables_age <-
  readxl::read_excel("data-raw/neuronorma.xlsx", sheet = "Age") %>%
  tidyr::fill(Age) %>%
  dplyr::mutate(SS = as.integer(SS)) %>%
  parse_and_extract_ranges(Age) %>%
  parse_and_extract_ranges(TMTa) %>%
  parse_and_extract_ranges(TMTb) %>%
  parse_and_extract_ranges(ROCF_Acc) %>%
  parse_and_extract_ranges(ROCF_DR_Acc)

nc_tables_age <-
  readxl::read_excel("data-raw/normacog.xlsx", sheet = "Age") %>%
  tidyr::fill(Age) %>%
  dplyr::mutate(SS = as.integer(SS)) %>%
  parse_and_extract_ranges(Age) %>%
  parse_and_extract_ranges(HVLT_A1) %>%
  parse_and_extract_ranges(HVLT_TR) %>%
  parse_and_extract_ranges(HVLT_A4) %>%
  parse_and_extract_ranges(HVLT_DI)

nn_tables_education <-
  readxl::read_excel(
    "data-raw/neuronorma.xlsx",
    sheet = "Education",
    skip = 1, .name_repair = "minimal"
  ) %>%
  setNames(c(
    "Education", "TMTa_gt50", "TMTb_lt50", "TMTb_gt50",
    "ROCF_Acc_lt50", "ROCF_Acc_gt50", "ROCF_DR_Acc_gt50"
  )) %>%
  dplyr::mutate(across(everything(), as.integer))

nc_tables_education <-
  readxl::read_excel("data-raw/normacog.xlsx", sheet = "Education") %>%
  dplyr::mutate(across(everything(), as.integer))

nn_tables_TMTa_lt50 <-
  readxl::read_excel(
    "data-raw/neuronorma.xlsx",
    sheet = "TMTa <50 Education", skip = 1
  ) %>%
  tidyr::pivot_longer(
    cols = "18":"49", names_to = "Age",
    names_transform = as.integer, values_to = "TMTa_lt50"
  ) %>%
  dplyr::mutate(across(everything(), as.integer))

nn_tables_ROCF_DR_Acc_lt50 <-
  readxl::read_excel(
    "data-raw/neuronorma.xlsx",
    sheet = "ROCF DR Acc <50 Education", skip = 1
  ) %>%
  tidyr::pivot_longer(
    cols = "18":"49", names_to = "Age",
    names_transform = as.integer, values_to = "ROCF_DR_Acc_lt50"
  ) %>%
  dplyr::mutate(across(everything(), as.integer))

usethis::use_data(
  nn_tables_age,
  nn_tables_education,
  nn_tables_TMTa_lt50,
  nn_tables_ROCF_DR_Acc_lt50,
  nc_tables_age,
  nc_tables_education,
  internal = TRUE, overwrite = TRUE
)
