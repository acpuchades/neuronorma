#' Adjust ROCF accuracy scores by age and education
#'
#' @param raw Raw ROCF accuracy scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized ROCF accuracy scores.
#' @export
adjust_ROCF_Acc <- function(raw, age, education) {
  raw <- as.numeric(raw)
  age <- as.integer(age)
  education <- as.integer(education)
  age_adjusted <- normalize_ROCF_Acc_by_age(raw, age)
  adjust_ROCF_Acc_by_education(age_adjusted, age, education)
}

normalize_ROCF_Acc_by_age <- function(raw, age) {
  . <- Age_l <- Age_r <- ROCF_Acc_l <- ROCF_Acc_r <- SS <- NULL

  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(nn_tables_age, by = character()) %>%
    dplyr::filter(
      age >= Age_l & age <= Age_r,
      raw >= ROCF_Acc_l & raw <= ROCF_Acc_r
    ) %>%
    dplyr::left_join(data, ., by = "id") %>%
    dplyr::pull(SS)

  age_adjusted
}

adjust_ROCF_Acc_by_education <- function(nss_a, age, education) {
  . <- id <- NSSae <- ROCF_Acc_lt50 <- ROCF_Acc_gt50 <- NULL
  Education_l <- Education_r <- NULL

  ae_adjusted <- tibble::tibble(nss_a, age, education) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::left_join(nn_tables_education, by = character()) %>%
    dplyr::filter(education >= Education_l & education <= Education_r) %>%
    dplyr::mutate(NSSae = dplyr::case_when(
      age > 18 & age < 50 ~ nss_a + ROCF_Acc_lt50,
      age >= 50 ~ nss_a + ROCF_Acc_gt50,
      TRUE ~ NA_real_
    ))

  ae_adjusted %>%
    dplyr::arrange(id) %>%
    dplyr::pull(NSSae)
}

#' Adjust ROCF DR accuracy scores by age and education
#'
#' @param raw Raw ROCF DR accuracy scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized ROCF DR accuracy scores.
#' @export
adjust_ROCF_DR_Acc <- function(raw, age, education) {
  raw <- as.numeric(raw)
  age <- as.integer(age)
  education <- as.integer(education)
  age_adjusted <- normalize_ROCF_DR_Acc_by_age(raw, age)
  adjust_ROCF_DR_Acc_by_education(age_adjusted, age, education)
}

normalize_ROCF_DR_Acc_by_age <- function(raw, age) {
  . <- Age_l <- Age_r <- ROCF_DR_Acc_l <- ROCF_DR_Acc_r <- SS <- NULL

  data <- tibble::tibble(raw, age) %>%
    dplyr::mutate(id = dplyr::row_number())

  age_adjusted <- data %>%
    dplyr::left_join(nn_tables_age, by = character()) %>%
    dplyr::filter(
      age >= Age_l & age <= Age_r,
      raw >= ROCF_DR_Acc_l & raw <= ROCF_DR_Acc_r
    ) %>%
    dplyr::left_join(data, ., by = "id") %>%
    dplyr::pull(SS)

  age_adjusted
}

adjust_ROCF_DR_Acc_by_education <- function(nss_a, age, education) {
  . <- NSSae <- ROCF_DR_Acc_lt50 <- ROCF_DR_Acc_lt50 <- ROCF_DR_Acc_gt50 <- NULL
  Education_l <- Education_r <- NULL

  data <- tibble::tibble(nss_a, age, education) %>%
    dplyr::mutate(id = dplyr::row_number())

  ae_adjusted_lt50 <- data %>%
    dplyr::filter(age >= 18, age < 50) %>%
    dplyr::left_join(nn_tables_ROCF_DR_Acc_lt50, by = c("age" = "Age")) %>%
    dplyr::filter(education >= Education_l & education <= Education_r) %>%
    dplyr::mutate(NSSae = nss_a + ROCF_DR_Acc_lt50)

  ae_adjusted_gt50 <- data %>%
    dplyr::filter(age >= 50) %>%
    dplyr::left_join(nn_tables_education, by = character()) %>%
    dplyr::filter(education >= Education_l & education <= Education_r) %>%
    dplyr::mutate(NSSae = nss_a + ROCF_DR_Acc_gt50)

  dplyr::bind_rows(ae_adjusted_lt50, ae_adjusted_gt50) %>%
    dplyr::left_join(data, ., by = "id") %>%
    dplyr::pull(NSSae)
}
