#' Determine the Weekday
#'
#' This function takes a date as input and returns the corresponding weekday.
#' The function supports flexible date formats, trims input text,
#' and allows localized weekday names.
#'
#' @param date A date in various formats (character, Date, or POSIXt).
#' @param language The language for weekday names. Default is "en".
#' Supported options: "en" (English), "fr" (French), "es" (Spanish).
#'
#' @return A character string representing the weekday.
#' @importFrom lubridate ymd dmy mdy
#' @importFrom stringr str_trim
#' @importFrom cli cli_alert_danger
#' @export
#'
#' @examples
#' determine_weekday("2025-03-16")
#' determine_weekday("16/03/2025", language = "fr")
#' determine_weekday(Sys.Date())
determine_weekday <- function(date, language = "en") {

  # Load necessary libraries
  library(lubridate)
  library(stringr)
  library(cli)

  # Clean and trim input
  date <- str_trim(as.character(date))

  # Attempt to parse multiple date formats
  parsed_date <- tryCatch({
    if (!inherits(date, "Date")) {
      ymd(date) %||% dmy(date) %||% mdy(date)
    } else {
      date
    }
  }, error = function(e) {
    cli_alert_danger("Invalid date format. Please provide a valid date string or Date object.")
    return(NA)
  })

  # Handle invalid date values
  if (is.na(parsed_date)) {
    cli_alert_danger("Date parsing failed. Ensure format like 'YYYY-MM-DD'.")
    return(NA)
  }

  # Return the weekday with language support
  weekdays_list <- list(
    en = weekdays,
    fr = function(x) weekdays(x, abbreviate = FALSE, locale = "fr_FR"),
    es = function(x) weekdays(x, abbreviate = FALSE, locale = "es_ES")
  )

  if (!language %in% names(weekdays_list)) {
    cli_alert_danger("Unsupported language. Using default: English (en).")
    language <- "en"
  }

  return(weekdays_list[[language]](parsed_date))
}
