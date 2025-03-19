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
  . <- id <- Age_l <- Age_r <- TMTa_l <- TMTa_r <- SS <- NULL

  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(
      nn_tables_age,
      by = dplyr::join_by(
        age >= Age_l, age <= Age_r,
        raw >= TMTa_l, raw <= TMTa_r
      ),
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    dplyr::arrange(id) %>%
    dplyr::pull(SS)

  age_adjusted
}

adjust_TMTa_by_education <- function(nss_a, age, education) {
  . <- Age <- NSSae <- TMTa_lt50 <- TMTa_gt50 <- Education_l <- Education_r <- NULL

  data <- tibble::tibble(nss_a, age, education) %>%
    dplyr::mutate(id = dplyr::row_number())

  ae_adjusted_lt50 <- data %>%
    dplyr::filter(age >= 18, age < 50) %>%
    dplyr::left_join(
      nn_tables_TMTa_lt50,
      by = dplyr::join_by(
        age == Age, education >= Education_l, education <= Education_r
      ),
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    dplyr::mutate(NSSae = nss_a + TMTa_lt50)

  ae_adjusted_gt50 <- data %>%
    dplyr::filter(age >= 50) %>%
    dplyr::left_join(
      nn_tables_education,
      by = dplyr::join_by(education >= Education_l, education <= Education_r),
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    dplyr::mutate(NSSae = nss_a + TMTa_gt50)


  data %>%
    dplyr::left_join(dplyr::bind_rows(ae_adjusted_lt50, ae_adjusted_gt50), by = "id") %>%
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
  . <- id <- Age_l <- Age_r <- TMTb_l <- TMTb_r <- SS <- NULL

  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(
      nn_tables_age,
      by = dplyr::join_by(
        age >= Age_l, age <= Age_r,
        raw >= TMTb_l, raw <= TMTb_r
      ),
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    dplyr::arrange(id) %>%
    dplyr::pull(SS)

  age_adjusted
}

adjust_TMTb_by_education <- function(nss_a, age, education) {
  data <- tibble::tibble(nss_a, age, education) %>%
    dplyr::mutate(id = dplyr::row_number())

  ae_adjusted <- data %>%
    dplyr::left_join(
      nn_tables_education,
      by = dplyr::join_by(
        education >= Education_l,
        education <= Education_r
      ),
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    dplyr::mutate(NSSae = dplyr::case_when(
      age >= 18 & age < 50 ~ nss_a + TMTb_lt50,
      age >= 50 ~ nss_a + TMTb_gt50,
      TRUE ~ NA_real_
    ))

  data %>%
    dplyr::left_join(ae_adjusted, by = "id") %>%
    dplyr::pull(NSSae)
}
