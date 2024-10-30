#' Standardize Processing Times to Months
#'
#' Converts processing times from weeks to months for consistent analysis
#'
#' @param data Data frame containing processing times data
#' @return Data frame with times converted to months
#' @export
standardize_to_months <- function(data) {
    dplyr::mutate(
        data,
        range_upper = dplyr::case_when(
            range_upper_unit == "Weeks" ~ range_upper * 0.230137,
            TRUE ~ range_upper
        ),
        range_upper_unit = dplyr::case_when(
            range_upper_unit == "Weeks" ~ "Months",
            TRUE ~ range_upper_unit
        ),
        range_lower = dplyr::case_when(
            range_lower_unit == "Weeks" ~ range_lower * 0.230137,
            TRUE ~ range_lower
        ),
        range_lower_unit = dplyr::case_when(
            range_lower_unit == "Weeks" ~ "Months",
            TRUE ~ range_lower_unit
        )
    )
}
