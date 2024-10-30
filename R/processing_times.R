#' USCIS Processing Times
#'
#' Retrieves current processing times for USCIS forms
#'
#' @param form_offices Data frame from uscis_form_offices()
#' @param cookie_file Path to cookie file
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{form_name}{Factor. The name/identifier of the USCIS form}
#'     \item{form_subtype}{Factor. The subtype of the form being processed}
#'     \item{office_code}{Factor. The USCIS office location code}
#'     \item{publication_date}{Character. Date when the processing time information was published}
#'     \item{form_note_en}{Character or NA. Additional form notes in English}
#'     \item{form_note_es}{Character or NA. Additional form notes in Spanish}
#'     \item{range_upper}{Numeric. Upper bound of the processing time range}
#'     \item{range_upper_unit}{Factor. Time unit for the upper bound (e.g., "Months", "Days")}
#'     \item{range_lower}{Numeric. Lower bound of the processing time range}
#'     \item{range_lower_unit}{Factor. Time unit for the lower bound (e.g., "Months", "Days")}
#'     \item{service_request_date}{Character. Date when service request can be submitted if
#'       outside normal processing time}
#'     \item{subtype_info_en}{Character. Additional information about the form subtype in English}
#'     \item{subtype_info_es}{Character. Additional information about the form subtype in Spanish}
#'   }
#' @note All date fields are returned in character format and may need to be converted to Date objects.
#'   Factor columns are created to optimize memory usage and ensure consistent categories across the dataset.
#' @examples
#' \dontrun{
#' # Initialize USCIS session
#' uscis_init_session()
#'
#' # Retrieve USCIS forms
#' forms <- uscis_forms()
#'
#' # Retrieve form types based on form names
#' form_types <- uscis_form_types(forms)
#'
#' # Retrieve processing offices for each form type
#' offices <- uscis_form_offices(form_types)
#'
#' # Retrieve processing times for each form and office
#' processing_times <- uscis_processing_times(offices)
#' head(processing_times)
#' }
#' @export
uscis_processing_times <- function(form_offices, cookie_file = "uscis.cookies") {

    form_name = form_subtype = range_upper_unit = range_lower_unit = office_code = NULL

    time_process_data <- purrr::pmap_dfr(
        list(
            form_name = form_offices$form_name,
            office_code = form_offices$office_code,
            form_type = form_offices$form_type
        ),
        function(form_name, office_code, form_type) {
            endpoint <- paste0("processingtime/", form_name, "/", office_code, "/", form_type)

            message("Retrieving: ", endpoint)

            result <- uscis_query(endpoint, cookie_file)

            tibble::tibble(
                form_name = result$data$processing_time$form_name,
                form_subtype = form_type,
                office_code = result$data$processing_time$office_code,
                publication_date = result$data$processing_time$subtypes$publication_date,
                form_note_en = result$data$processing_time$form_note_en %||% NA,
                form_note_es = result$data$processing_time$form_note_es %||% NA,
                range_upper = result$data$processing_time$range$value[1],
                range_upper_unit = result$data$processing_time$range$unit[1],
                range_lower = result$data$processing_time$range$value[2],
                range_lower_unit = result$data$processing_time$range$unit[2],
                service_request_date = result$data$processing_time$subtypes$service_request_date,
                subtype_info_en = result$data$processing_time$subtypes$subtype_info_en,
                subtype_info_es = result$data$processing_time$subtypes$subtype_info_es
            )
        }
    )

    dplyr::mutate(
        time_process_data,
        form_name = factor(form_name),
        form_subtype = factor(form_subtype),
        range_upper_unit = factor(range_upper_unit),
        range_lower_unit = factor(range_lower_unit),
        office_code = factor(office_code)
    )
}
