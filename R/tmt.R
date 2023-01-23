#' Adjust TMTa scores by age and education
#'
#' @param raw Raw TMTa scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized TMTa scores.
#' @export
adjust_TMTa <- function(raw, age, education) {
  raw <- as.integer(raw)
  age <- as.integer(age)
  education <- as.integer(education)
  age_adjusted <- normalize_TMTa_by_age(raw, age)
  adjust_TMTa_by_education(age_adjusted, age, education)
}

normalize_TMTa_by_age <- function(raw, age) {
  . <- Age_l <- Age_r <- TMTa_l <- TMTa_r <- SS <- NULL

  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(nn_tables_age, by = character()) %>%
    dplyr::filter(
      age >= Age_l & age <= Age_r,
      raw >= TMTa_l & raw <= TMTa_r
    ) %>%
    dplyr::left_join(data, ., by = "id") %>%
    dplyr::pull(SS)

  age_adjusted
}

adjust_TMTa_by_education <- function(nss_a, age, education) {
  . <- NSSae <- TMTa_lt50 <- TMTa_gt50 <- Education_l <- Education_r <- NULL

  data <- tibble::tibble(nss_a, age, education) %>%
    dplyr::mutate(id = dplyr::row_number())

  ae_adjusted_lt50 <- data %>%
    dplyr::filter(age >= 18, age < 50) %>%
    dplyr::left_join(nn_tables_TMTa_lt50, by = c("age" = "Age")) %>%
    dplyr::filter(education >= Education_l & education <= Education_r) %>%
    dplyr::mutate(NSSae = nss_a + TMTa_lt50)

  ae_adjusted_gt50 <- data %>%
    dplyr::filter(age >= 50) %>%
    dplyr::left_join(nn_tables_education, by = character()) %>%
    dplyr::filter(education >= Education_l & education <= Education_r) %>%
    dplyr::mutate(NSSae = nss_a + TMTa_gt50)

  dplyr::bind_rows(ae_adjusted_lt50, ae_adjusted_gt50) %>%
    dplyr::left_join(data, ., by = "id") %>%
    dplyr::pull(NSSae)
}

#' Adjust TMTb scores by age and education
#'
#' @param raw Raw TMTb scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized TMTb scores.
#' @export
adjust_TMTb <- function(raw, age, education) {
  raw <- as.integer(raw)
  age <- as.integer(age)
  education <- as.integer(education)
  age_adjusted <- normalize_TMTb_by_age(raw, age)
  adjust_TMTb_by_education(age_adjusted, age, education)
}

normalize_TMTb_by_age <- function(raw, age) {
  . <- Age_l <- Age_r <- TMTb_l <- TMTb_r <- SS <- NULL

  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(nn_tables_age, by = character()) %>%
    dplyr::filter(
      age >= Age_l & age <= Age_r,
      raw >= TMTb_l & raw <= TMTb_r
    ) %>%
    dplyr::left_join(data, ., by = "id") %>%
    dplyr::pull(SS)

  age_adjusted
}

adjust_TMTb_by_education <- function(nss_a, age, education) {
  id <- NSSae <- Education_l <- Education_r <- NULL

  ae_adjusted <- tibble::tibble(nss_a, age, education) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::left_join(nn_tables_education, by = character()) %>%
    dplyr::filter(education >= Education_l & education <= Education_r) %>%
    dplyr::mutate(NSSae = dplyr::case_when(
      age >= 18 & age < 50 ~ nss_a + TMTb_lt50,
      age >= 50 ~ nss_a + TMTb_gt50,
      TRUE ~ NA_real_
    ))

  ae_adjusted %>%
    dplyr::arrange(id) %>%
    dplyr::pull(NSSae)
}
