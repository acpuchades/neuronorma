library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(tidyr)

nn_tables_age <-
  read_excel("data-raw/tables.xlsx", sheet = "Age") %>%
  fill(Age) %>%
  mutate(
    across(SS, as.integer),
    across(-SS, str_replace, r"(^>(\d+|\d*\.\d+)$)", r"([\1,+Inf])"),
    across(-SS, str_replace, r"(^<(\d+|\d*\.\d+)$)", r"([-Inf,\1])"),
    across(-SS, str_replace, r"(^(\d+|\d*\.\d+)-(\d+|\d*\.\d+)$)", r"([\1,\2])"),
    across(-SS, str_replace, r"(^(\d+|\d*\.\d+)$)", r"([\1,\1])")
  ) %>%
  tidyr::extract(Age, c("Age_l", "Age_r"), r"(\[(\d+|\d*\.\d+|-Inf),(\d+|\d*\.\d+|\+?Inf)\])", convert = TRUE) %>%
  tidyr::extract(TMTa, c("TMTa_l", "TMTa_r"), r"(\[(\d+|\d*\.\d+|-Inf),(\d+|\d*\.\d+|\+?Inf)\])", convert = TRUE) %>%
  tidyr::extract(TMTb, c("TMTb_l", "TMTb_r"), r"(\[(\d+|\d*\.\d+|-Inf),(\d+|\d*\.\d+|\+?Inf)\])", convert = TRUE) %>%
  tidyr::extract(`ROCF Acc`, c("ROCF_Acc_l", "ROCF_Acc_r"), r"(\[(\d+|\d*\.\d+|-Inf),(\d+|\d*\.\d+|\+?Inf)\])", convert = TRUE) %>%
  tidyr::extract(`ROCF DR Acc`, c("ROCF_DR_Acc_l", "ROCF_DR_Acc_r"), r"(\[(\d+|\d*\.\d+|-Inf),(\d+|\d*\.\d+|\+?Inf)\])", convert = TRUE)

nn_tables_education <-
  read_excel("data-raw/tables.xlsx", sheet = "Education", skip = 1, .name_repair = "minimal") %>%
  setNames(c(
    "Education", "TMTa_gt50", "TMTb_lt50", "TMTb_gt50",
    "ROCF_Acc_lt50", "ROCF_Acc_gt50", "ROCF_DR_Acc_gt50"
  )) %>%
  mutate(across(everything(), as.integer))

nn_tables_TMTa_lt50 <-
  read_excel("data-raw/tables.xlsx", sheet = "TMTa <50 Education", skip = 1) %>%
  pivot_longer(
    cols = "18":"49", names_to = "Age",
    names_transform = as.integer, values_to = "TMTa_lt50"
  ) %>%
  mutate(across(everything(), as.integer))

nn_tables_ROCF_DR_Acc_lt50 <-
  read_excel("data-raw/tables.xlsx", sheet = "ROCF DR Acc <50 Education", skip = 1) %>%
  pivot_longer(
    cols = "18":"49", names_to = "Age",
    names_transform = as.integer, values_to = "ROCF_DR_Acc_lt50"
  ) %>%
  mutate(across(everything(), as.integer))

usethis::use_data(
  nn_tables_age,
  nn_tables_education,
  nn_tables_TMTa_lt50,
  nn_tables_ROCF_DR_Acc_lt50,
  internal = TRUE, overwrite = TRUE
)
