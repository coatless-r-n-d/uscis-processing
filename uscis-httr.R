## Libraries ---- 

library(httr)
library(jsonlite)

## Setup ---- 

# Define constants
API_BASE_URL = "https://egov.uscis.gov/processing-times/api/"
CA_FILE = "egov-uscis-gov.pem"

`%||%` = function (x, y) 
{
  if (is.null(x)) 
    y
  else x
}

## Main query interface ---- 

# Define function to make API requests
uscis_query = function(endpoint, headers = NULL, config = NULL) {
  # Set default headers and config
  if (is.null(headers)) {
    uscis_headers = add_headers(
      "Referer" = "https://egov.uscis.gov/processing-times/"
    )
  }
  
  if (is.null(config)) {
    uscis_config = config(
      cainfo = CA_FILE
    )
  }
  
  # Make API request
  response = GET(
    paste0(API_BASE_URL, endpoint),
    headers,
    config
  )
  
  # Check for errors
  if (status_code(response) != 200) {
    stop("API request failed with status code ", status_code(response))
  }
  
  # Parse response
  fromJSON(content(response, as = "text"), flatten = TRUE)
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
  
