## Libraries ----

library(httr2)
library(jsonlite)
library(dplyr)

## Setup ----

# Define constants
API_BASE_URL = "https://egov.uscis.gov/processing-times/api/"

# Null coalescing operator
`%||%` = function (x, y)
{
  if (is.null(x))
    y
  else x
}

## Main query interface ----

# Step 1: Initial request to get cookies
initial_headers <- c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:130.0) Gecko/20100101 Firefox/130.0",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/png,image/svg+xml,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.5",
    "Accept-Encoding" = "gzip, deflate, br, zstd",
    "DNT" = "1",
    "Sec-GPC" = "1",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none",
    "Sec-Fetch-User" = "?1",
    "Priority" = "u=0, i",
    "Pragma" = "no-cache",
    "Cache-Control" = "no-cache"
)

cookie_file <- "uscis.cookies"

# Make initial request to get cookies
initial_req <- request("https://egov.uscis.gov/processing-times/") |>
    req_headers(!!!initial_headers) |>
    req_cookie_preserve(cookie_file) |>
    req_perform()

# Define function to make API requests
uscis_query = function(endpoint, headers = NULL, config = NULL) {
    api_headers <- c(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:130.0) Gecko/20100101 Firefox/130.0",
        "Accept" = "application/json, text/plain, */*",
        "Accept-Language" = "en-US,en;q=0.5",
        "Accept-Encoding" = "gzip, deflate, br, zstd",
        "DNT" = "1",
        "Sec-GPC" = "1",
        "Connection" = "keep-alive",
        "Referer" = "https://egov.uscis.gov/processing-times/",
        "Sec-Fetch-Dest" = "empty",
        "Sec-Fetch-Mode" = "cors",
        "Sec-Fetch-Site" = "same-origin",
        "Pragma" = "no-cache",
        "Cache-Control" = "no-cache",
        "TE" = "trailers"
    )

    response <- request(
        paste0(API_BASE_URL, endpoint)) |>
        req_headers(!!!api_headers) |>
        req_cookie_preserve(cookie_file) |>
        req_perform()

    # Check for errors
    if (resp_status(response) != 200) {
        stop("API request failed with status code ", resp_status(response))
    }

    # Parse response
    resp_body_json(response, simplifyVector = TRUE)
}

## Download form IDs ----

uscis_forms = function() {
  form_data = uscis_query("forms")

  form_data$data$forms$forms
}

forms = uscis_forms()

write.csv(
  forms,
  file = paste0("data-raw/forms-",Sys.Date(),".csv"),
  row.names = FALSE
  )

## Forms Types ----
uscis_form_types = function(forms) {

  form_type_data = purrr::map_dfr(
    forms$form_name, function(x) {
      result = uscis_query(paste0("formtypes/", x))

      data.frame(form_name = result$data$form_types$form_name,
                 result$data$form_types$subtypes
      )
    }
  )

  form_type_data
}

form_types = uscis_form_types(forms)


write.csv(
  form_types,
  file = paste0("data-raw/form-types-",Sys.Date(),".csv"),
  row.names = FALSE
  )

## Forms processed by Office ----
uscis_form_offices = function(form_types) {

  form_office_data = purrr::map2_dfr(
    form_types$form_name, form_types$form_key, function(x, y) {
      result = uscis_query(paste0("formoffices/", x, "/", y))

      data.frame(
        form_name = result$data$form_offices$form_name,
        form_type = result$data$form_offices$form_type,
        result$data$form_offices$offices
      )
    }
  )

  form_office_data
}

form_offices = uscis_form_offices(form_types)

write.csv(
  form_offices,
  file = paste0("data-raw/form-offices-",Sys.Date(),".csv"),
  row.names = FALSE
  )

## Processing Times ----
uscis_processing_times = function(form_offices) {

  time_process_data = purrr::pmap_dfr(
    list(
      form_name = form_offices$form_name,
      office_code = form_offices$office_code,
      form_type = form_offices$form_type
    ),
    function(form_name, office_code, form_type) {
      endpoint = paste0("processingtime/", form_name, "/", office_code, "/", form_type)

      message("Retrieving: ", endpoint)

      result = uscis_query(endpoint)

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

  time_process_data %>%
    mutate(
      form_name = factor(form_name),
      form_subtype = factor(form_subtype),
      range_upper_unit = factor(range_upper_unit),
      range_lower_unit = factor(range_lower_unit),
      office_code = factor(office_code),
    )
}


processing_times = uscis_processing_times(form_offices)

write.csv(
  processing_times,
  file = paste0("data-raw/processing-times-",Sys.Date(),".csv"),
  row.names = FALSE
  )

