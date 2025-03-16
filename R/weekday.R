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
#' @importFrom lubridate parse_date_time wday
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

  # Clean and trim input
  date <- str_trim(as.character(date))

  # Attempt to parse multiple date formats
  parsed_date <- tryCatch({
    parse_date_time(date, orders = c("ymd", "dmy", "mdy"))
  }, error = function(e) {
    warning("Date parsing failed. Ensure format like 'YYYY-MM-DD'.")
    return(NA)
  })

  # Handle invalid date values
  if (is.na(parsed_date)) {
    warning("Date parsing failed. Ensure format like 'YYYY-MM-DD'.")
    return(NA)
  }

  # Return the weekday with language support
  weekdays_list <- list(
    en = function(x) wday(x, label = TRUE, abbr = FALSE, locale = "English"),
    fr = function(x) wday(x, label = TRUE, abbr = FALSE, locale = "French"),
    es = function(x) wday(x, label = TRUE, abbr = FALSE, locale = "Spanish")
  )

  if (!language %in% names(weekdays_list)) {
    warning("Unsupported language. Using default: English (en).")
    language <- "en"
  }

  return(as.character(weekdays_list[[language]](parsed_date)))
}
