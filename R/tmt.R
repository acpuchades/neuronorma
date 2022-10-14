library(magrittr)

#' Adjust TMTa scores by age and education
#'
#' @param raw Raw TMTa scores.
#' @param age Age in years.
#' @param studies Number of study years.
#' @return Normalized TMTa scores.
#' @export
adjust_TMTa <- function(raw, age, studies) {
  raw <- as.integer(raw)
  age <- as.integer(age)
  studies <- as.integer(studies)
  age_adjusted = adjust_TMTa_by_age(raw, age)
  adjust_TMTa_by_studies(age_adjusted, age, studies)
}

adjust_TMTa_by_age <- function(raw, age) {
  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(TMT_age, by = character()) %>%
    dplyr::filter(age >= Age_l & age <= Age_r,
                  raw >= TMTa_l & raw <= TMTa_r) %>%
    dplyr::left_join(data, ., by = "id") %$% SS

  age_adjusted
}

adjust_TMTa_by_studies <- function(nss_a, age, studies) {
  data <- tibble::tibble(nss_a, age, studies) %>%
    dplyr::mutate(id = dplyr::row_number())

  ae_adjusted_lt50 <- data %>%
    dplyr::filter(age < 50) %>%
    dplyr::left_join(TMTa_studies_lt50, by = c("age" = "Age",
                                               "studies" = "Education")) %>%
    dplyr::mutate(NSSae = nss_a + CF)

  ae_adjusted_gt50 <- data %>%
    dplyr::filter(age >= 50) %>%
    dplyr::left_join(TMTa_studies_gt50, by = c("nss_a" = "NSSa",
                                               "studies" = "Education"))

  dplyr::bind_rows(ae_adjusted_lt50, ae_adjusted_gt50) %>%
    dplyr::left_join(data, ., by = "id") %$% NSSae
}

#' Adjust TMTb scores by age and education
#'
#' @param raw Raw TMTb scores.
#' @param age Age in years.
#' @param studies Number of study years.
#' @return Normalized TMTb scores.
#' @export
adjust_TMTb <- function(raw, age, studies) {
  raw <- as.integer(raw)
  age <- as.integer(age)
  studies <- as.integer(studies)
  age_adjusted = adjust_TMTb_by_age(raw, age)
  adjust_TMTb_by_studies(age_adjusted, age, studies)
}

adjust_TMTb_by_age <- function(raw, age) {
  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(TMT_age, by = character()) %>%
    dplyr::filter(age >= Age_l & age <= Age_r,
                  raw >= TMTb_l & raw <= TMTb_r) %>%
    dplyr::left_join(data, ., by = "id") %$% SS

  age_adjusted
}

adjust_TMTb_by_studies <- function(nss_a, age, studies) {
  data <- tibble::tibble(nss_a, age, studies) %>%
    dplyr::mutate(id = dplyr::row_number())

  ae_adjusted_gt50 <- data %>%
    dplyr::filter(age >= 50) %>%
    dplyr::left_join(TMTb_studies_gt50, by = c("nss_a" = "NSSa", "studies" = "Education"))

  ae_adjusted_lt50 <- data %>%
    dplyr::filter(age < 50) %>%
    dplyr::left_join(TMTb_studies_lt50, by = c("nss_a" = "NSSa", "studies" = "Education"))

  dplyr::bind_rows(ae_adjusted_lt50, ae_adjusted_gt50) %>%
    dplyr::left_join(data, ., by = "id") %$% NSSae
}
