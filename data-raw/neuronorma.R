library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(tidyr)

TMT_age <-
  read_excel("data-raw/neuronorma.xlsx", sheet = "TMT Age",
             col_types = c("text", "numeric", "text", "text")) %>%
  fill(Age) %>%
  mutate(across(c(Age, TMTa:TMTb), str_replace, r"(^>(\d+)$)", r"([\1,+Inf])")) %>%
  mutate(across(c(Age, TMTa:TMTb), str_replace, r"(^<(\d+)$)", r"([-Inf,\1])")) %>%
  mutate(across(c(Age, TMTa:TMTb), str_replace, r"(^(\d+)-(\d+)$)", r"([\1,\2])")) %>%
  mutate(across(c(Age, TMTa:TMTb), str_replace, r"(^(\d+)$)", r"([\1,\1])")) %>%
  tidyr::extract(Age, c("Age_l", "Age_r"), r"(\[(\d+|-Inf),(\d+|\+?Inf)\])", convert = TRUE) %>%
  tidyr::extract(TMTa, c("TMTa_l", "TMTa_r"), r"(\[(\d+|-Inf),(\d+|\+?Inf)\])", convert = TRUE) %>%
  tidyr::extract(TMTb, c("TMTb_l", "TMTb_r"), r"(\[(\d+|-Inf),(\d+|\+?Inf)\])", convert = TRUE)

TMTa_education_lt50 <-
  suppressMessages(read_excel("data-raw/neuronorma.xlsx",
                              sheet = "TMTa <50 Education",
                              skip = 1)) %>%
  rename(Education = 1) %>%
  pivot_longer(cols = "18":"49", names_to = "Age",
               names_transform = as.integer, values_to = "CF")

TMTa_education_gt50 <-
  suppressMessages(read_excel("data-raw/neuronorma.xlsx",
                              sheet = "TMTa >50 Education",
                              skip = 1)) %>%
  rename(NSSa = 1) %>%
  pivot_longer(cols = "0":"20", names_to = "Education",
                      names_transform = as.integer, values_to = "NSSae")

TMTb_education_lt50 <-
  suppressMessages(read_excel("data-raw/neuronorma.xlsx",
                              sheet = "TMTb <50 Education",
                              skip = 1)) %>%
  rename(NSSa = 1) %>%
  pivot_longer(cols = "8":"20", names_to = "Education",
               names_transform = as.integer, values_to = "NSSae")

TMTb_education_gt50 <-
  suppressMessages(read_excel("data-raw/neuronorma.xlsx",
                              sheet = "TMTb >50 Education",
                              skip = 1)) %>%
  rename(NSSa = 1) %>%
  pivot_longer(cols = "0":"20", names_to = "Education",
               names_transform = as.integer, values_to = "NSSae")

usethis::use_data(TMT_age, TMTa_education_lt50, TMTa_education_gt50,
                  TMTb_education_gt50, TMTb_education_lt50,
                  internal = TRUE, overwrite = TRUE)
