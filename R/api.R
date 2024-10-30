#' Initialize USCIS API Session
#'
#' Makes initial request to establish session cookies
#'
#' @param cookie_file Path to save cookies. Default is "uscis.cookies" in the working directory.
#' @return
#' Boolean if session was successfully initialized
#' @export
init_uscis_session <- function(cookie_file = "uscis.cookies") {
    initial_headers <- c(
        "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:131.0) Gecko/20100101 Firefox/131.0",
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

    response <- httr2::request("https://egov.uscis.gov/processing-times/") |>
        httr2::req_headers(!!!initial_headers) |>
        httr2::req_cookie_preserve(cookie_file) |>
        httr2::req_perform()

    httr2::resp_status(response) == 200
}

#' Make USCIS API Request
#'
#' Internal function to make authenticated requests to USCIS API
#'
#' @param endpoint API endpoint to query
#' @param cookie_file Path to cookie file. Default is "uscis.cookies" in the working directory.
#'
#' @return
#' Parsed JSON response
#' @keywords internal
uscis_query <- function(endpoint, cookie_file = "uscis.cookies") {
    api_headers <- c(
        "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:131.0) Gecko/20100101 Firefox/131.0",
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

    response <- httr2::request(paste0(API_BASE_URL, endpoint)) |>
        httr2::req_headers(!!!api_headers) |>
        httr2::req_cookie_preserve(cookie_file) |>
        httr2::req_perform()

    if (httr2::resp_status(response) != 200) {
        stop("API request failed with status code ", httr2::resp_status(response))
    }

    httr2::resp_body_json(response, simplifyVector = TRUE)
}
