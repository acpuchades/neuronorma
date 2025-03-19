#' Adjust HVLT_A1 scores by age and education
#'
#' @param raw Raw HVLT_A1 scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized HVLT_A1 scores.
#' @export
adjust_HVLT_A1 <- function(raw, age, education) {
    raw <- as.integer(raw)
    age <- as.integer(age)
    education <- as.integer(education)
    age_adjusted <- normalize_HVLT_A1_by_age(raw, age)
    adjust_HVLT_A1_by_education(age_adjusted, education)
}

normalize_HVLT_A1_by_age <- function(raw, age) {
    . <- id <- Age_l <- Age_r <- HVLT_A1_l <- HVLT_A1_r <- SS <- NULL

    data <- tibble::tibble(raw, age) %>%
        dplyr::mutate(id = dplyr::row_number())

    age_adjusted <- data %>%
        dplyr::left_join(
            nc_tables_age,
            by = dplyr::join_by(
                age >= Age_l, age <= Age_r,
                raw >= HVLT_A1_l, raw <= HVLT_A1_r
            ),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(SS)

    age_adjusted
}

adjust_HVLT_A1_by_education <- function(nss_a, education) {
    . <- id <- NSSae <- HVLT_A1 <- Education_l <- Education_r <- NULL

    tibble::tibble(nss_a, education) %>%
        dplyr::left_join(
            nc_tables_education,
            by = dplyr::join_by(education >= Education_l, education <= Education_r),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::mutate(NSSae = nss_a + HVLT_A1) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(NSSae)
}

#' Adjust HVLT_TR scores by age and education
#'
#' @param raw Raw HVLT_TR scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized HVLT_TR scores.
#' @export
adjust_HVLT_TR <- function(raw, age, education) {
    raw <- as.integer(raw)
    age <- as.integer(age)
    education <- as.integer(education)
    age_adjusted <- normalize_HVLT_TR_by_age(raw, age)
    adjust_HVLT_TR_by_education(age_adjusted, education)
}

normalize_HVLT_TR_by_age <- function(raw, age) {
    . <- id <- Age_l <- Age_r <- HVLT_TR_l <- HVLT_TR_r <- SS <- NULL

    data <- tibble::tibble(raw, age) %>%
        dplyr::mutate(id = dplyr::row_number())

    age_adjusted <- data %>%
        dplyr::left_join(nc_tables_age,
            by = dplyr::join_by(
                age >= Age_l, age <= Age_r,
                raw >= HVLT_TR_l, raw <= HVLT_TR_r
            ),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(SS)

    age_adjusted
}

adjust_HVLT_TR_by_education <- function(nss_a, education) {
    . <- id <- NSSae <- HVLT_TR <- Education_l <- Education_r <- NULL

    tibble::tibble(nss_a, education) %>%
        dplyr::left_join(nc_tables_education,
            by = dplyr::join_by(education >= Education_l, education <= Education_r),
            relationship = "many-to-one"
        ) %>%
        dplyr::mutate(NSSae = nss_a + HVLT_TR) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(NSSae)
}

#' Adjust HVLT_A4 scores by age and education
#'
#' @param raw Raw HVLT_A4 scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized HVLT_A4 scores.
#' @export
adjust_HVLT_A4 <- function(raw, age, education) {
    raw <- as.integer(raw)
    age <- as.integer(age)
    education <- as.integer(education)
    age_adjusted <- normalize_HVLT_A4_by_age(raw, age)
    adjust_HVLT_A4_by_education(age_adjusted, education)
}

normalize_HVLT_A4_by_age <- function(raw, age) {
    . <- id <- Age_l <- Age_r <- HVLT_A4_l <- HVLT_A4_r <- SS <- NULL

    data <- tibble::tibble(raw, age) %>%
        dplyr::mutate(id = dplyr::row_number())

    age_adjusted <- data %>%
        dplyr::left_join(
            nc_tables_age,
            by = dplyr::join_by(
                age >= Age_l, age <= Age_r,
                raw >= HVLT_A4_l, raw <= HVLT_A4_r
            ),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(SS)

    age_adjusted
}

adjust_HVLT_A4_by_education <- function(nss_a, education) {
    . <- id <- NSSae <- HVLT_A4 <- Education_l <- Education_r <- NULL

    tibble::tibble(nss_a, education) %>%
        dplyr::left_join(
            nc_tables_education,
            by = dplyr::join_by(
                education >= Education_l,
                education <= Education_r
            ),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::mutate(NSSae = nss_a + HVLT_A4) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(NSSae)
}

#' Adjust HVLT_DI scores by age and education
#'
#' @param raw Raw HVLT_DI scores.
#' @param age Age in years.
#' @param education Number of education years.
#' @return Normalized HVLT_DI scores.
#' @export
adjust_HVLT_DI <- function(raw, age, education) {
    raw <- as.integer(raw)
    age <- as.integer(age)
    education <- as.integer(education)
    age_adjusted <- normalize_HVLT_DI_by_age(raw, age)
    adjust_HVLT_DI_by_education(age_adjusted, education)
}

normalize_HVLT_DI_by_age <- function(raw, age) {
    . <- id <- Age_l <- Age_r <- HVLT_DI_l <- HVLT_DI_r <- SS <- NULL

    data <- tibble::tibble(raw, age) %>%
        dplyr::mutate(id = dplyr::row_number())

    age_adjusted <- data %>%
        dplyr::left_join(
            nc_tables_age,
            by = dplyr::join_by(
                age >= Age_l, age <= Age_r,
                raw >= HVLT_DI_l, raw <= HVLT_DI_r
            ),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(SS)

    age_adjusted
}

adjust_HVLT_DI_by_education <- function(nss_a, education) {
    . <- id <- NSSae <- HVLT_DI <- Education_l <- Education_r <- NULL

    tibble::tibble(nss_a, education) %>%
        dplyr::left_join(
            nc_tables_education,
            by = dplyr::join_by(
                education >= Education_l,
                education <= Education_r
            ),
            relationship = "many-to-one",
            na_matches = "never"
        ) %>%
        dplyr::mutate(NSSae = nss_a + HVLT_DI) %>%
        dplyr::arrange(id) %>%
        dplyr::pull(NSSae)
}
