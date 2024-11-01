
<!-- README.md is generated from README.Rmd. Please edit that file -->

# USCIS Immigration Form Processing Times

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-r-n-d/uscis-processing/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-r-n-d/uscis-processing/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> \[!IMPORTANT\]
>
> This project is not affiliated with any government agency and should
> not be considered official by any means.

The repository contains a study in programmatically obtaining
[processing time data](https://egov.uscis.gov/processing-times/) for
various immigration forms from the [U.S. Citizenship and Immigration
Services (USCIS)](https://www.uscis.gov/) using the [R programming
language](https://www.r-project.org/). Historical median processing
information for the fiscal years is shared by USCIS at
<https://egov.uscis.gov/processing-times/historic-pt>

Information is obtained by making the same standard HTTPS requests the
[processing time form](https://egov.uscis.gov/processing-times/) makes
to communicate with the USCIS Processing Time API found at:

    https://egov.uscis.gov/processing-times/api/

You can see an example of the workflow for obtaining the I-485
processing time data for Chicago, IL in the following GIF:

<figure>
<img src="https://i.imgur.com/K9OalZF.gif"
alt="Web API Responses from USCIS Processing Times" />
<figcaption aria-hidden="true">Web API Responses from USCIS Processing
Times</figcaption>
</figure>

Source: <https://i.imgur.com/K9OalZF.gif>

## Installation

The `uscis` package is a GitHub-only package. You can install the
package from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("coatless-r-n-d/uscis-processing")
```

## Usage

The package provides a set of functions to interact with the USCIS
Processing Time API. The following is a brief overview of the functions
available:

- `uscis_init_session()`: Initializes a USCIS session.
- `uscis_forms()`: Retrieves available USCIS forms.
- `uscis_form_types()`: Retrieves available subtypes for each USCIS
  form.
- `uscis_form_offices()`: Retrieves processing offices for each form
  type.
- `uscis_processing_times()`: Retrieves current processing times for
  USCIS forms.

``` r
# Load the library
library(uscis)

# Initialize USCIS session
uscis_init_session()

# Retrieve USCIS forms
forms <- uscis_forms()

# Retrieve form types based on form names
form_types <- uscis_form_types(forms)

# Retrieve processing offices for each form type
offices <- uscis_form_offices(form_types)

# Retrieve processing times for each form and office
processing_times <- uscis_processing_times(offices)
```

The API is designed to mirror the steps taken by the USCIS Processing
Time website. The functions are also designed to be used in a pipeline
fashion to retrieve the desired information.

## Acknowledgements

### USCIS

I would like to take a moment to express my gratitude and appreciation
to the USCIS team for their hard work and dedication in reviewing and
processing immigration forms. Their tireless efforts help individuals
and families from around the world to realize their dreams and build new
lives in the United States. Thank you for your service to the community
and for making a positive impact on so many lives.

### Prior work

Retrieved processing time information from the USCIS has been
extensively studied, but there has been a lack of focus on utilizing the
R programming language for this purpose. For non-exhaustive examples of
prior projects, please see:

- [Ariel Rodriguez Romero’s exploration of API
  endpoints](https://github.com/arielsvn/arielsvn.github.io/issues/3)
- [Data for Democracy’s `immigration-connect`
  (Python)](https://github.com/Data4Democracy/immigration-connect/tree/master/uscis-processing-time)
- [Hemanth Makkapati’s `uscis-tracker`
  (Go)](https://github.com/hmakkapati/uscis-tracker)
- [Gregor Martynus’s `uscis-service-center-processing-times`
  (Node)](https://github.com/gr2m/uscis-service-center-processing-times)
- [Andrew Duberstein’s `uscis-wait-times`
  (Python)](https://github.com/ajduberstein/uscis-wait-times)
- [J. Zebedee’s `uscis` (SQL)](https://github.com/jzebedee/uscis)

## License

AGPL (\>= 2)
