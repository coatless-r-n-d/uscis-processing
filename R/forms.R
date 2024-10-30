#' USCIS Forms
#'
#' Retrieves list of available USCIS forms
#'
#' @param cookie_file Path to cookie file
#'
#' @return A data frame with the following columns:
#' \describe{
#'     \item{form_name}{Character. The name/identifier of the USCIS form}
#'     \item{form_description_en}{Character. Description of the USCIS form in English}
#'     \item{form_description_es}{Character. Description of the USCIS form in Spanish}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' init_uscis_session()
#' forms <- uscis_forms()
#' head(forms)
#' }
uscis_forms <- function(cookie_file = "uscis.cookies") {
    form_data <- uscis_query("forms", cookie_file)
    form_data$data$forms$forms
}

#' USCIS Form Types
#'
#' Retrieves available subtypes for each USCIS form
#'
#' @param forms Data frame of forms from `uscis_forms()`
#' @param cookie_file Path to cookie file
#' @return
#' Data frame of form types and subtypes with the following columns:
#' \describe{
#'     \item{form_name}{Character. The name/identifier of the USCIS form}
#'     \item{form_key}{Character. Unique identifier key for the form}
#'     \item{form_type}{Character. Type classification of the form}
#'     \item{form_type_description_en}{Character. Detailed description of the form type in English}
#'     \item{form_type_description_es}{Character. Detailed description of the form type in Spanish}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' init_uscis_session()
#' forms <- uscis_forms()
#' form_types <- uscis_form_types(forms)
#' head(form_types)
#' }
uscis_form_types <- function(forms, cookie_file = "uscis.cookies") {
    purrr::map_dfr(
        forms$form_name,
        function(x) {
            result <- uscis_query(paste0("formtypes/", x), cookie_file)
            data.frame(
                form_name = result$data$form_types$form_name,
                result$data$form_types$subtypes
            )
        }
    )
}

#' USCIS Form Offices
#'
#' Retrieves processing offices for each form type
#'
#' @param form_types Data frame from `uscis_form_types()`
#' @param cookie_file Path to cookie file
#' @return
#' Data frame of forms and their processing offices with the following columns:
#' \describe{
#'     \item{form_name}{Character. The name/identifier of the USCIS form}
#'     \item{form_type}{Character. Type classification of the form}
#'     \item{office_code}{Character. The USCIS office location code}
#'     \item{office_description}{Character. Description of the USCIS Office Code in English}
#' }
#' @export
#' @examples
#' \dontrun{
#' init_uscis_session()
#' forms <- uscis_forms()
#' form_types <- uscis_form_types(forms)
#' form_offices <- uscis_form_offices(form_types)
#' head(form_offices)
#' }
uscis_form_offices <- function(form_types, cookie_file = "uscis.cookies") {
    purrr::map2_dfr(
        form_types$form_name,
        form_types$form_key,
        function(x, y) {
            result <- uscis_query(paste0("formoffices/", x, "/", y), cookie_file)
            data.frame(
                form_name = result$data$form_offices$form_name,
                form_type = result$data$form_offices$form_type,
                result$data$form_offices$offices
            )
        }
    )
}
