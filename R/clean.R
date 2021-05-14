#' Checks that indicator is in data
#'
#' @param data data.frame
#' @param indicator character identifying a column to be checked
#'
#' @return
#' @export
#'
check_indicator <- function(data, indicator) {
  if (!indicator %in% names(data)) {
    warning(
      call. = F,
      paste0(
        indicator,
        " column was not found in source. Please check the column names."
      )
    )
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' Check that  list of indicators provided are in data
#'
#' @param data data.frame
#' @param indicators_to_check vector of indicators to be checked
#'
#' @return
#' @export
#'
check_source <- function(data,  indicators_to_check) {
  has_warnings <-
    lapply(indicators_to_check, check_indicator, data = data) %>%
    unlist()
  return(any(has_warnings == TRUE))
}

#' Clean dataset for missing data
#'
#' This function is a wrapper around \code{\link{rec_missing}} for
#' dataframes.
#'
#' @param data data.frame to be cleaned by replacing missing common missing
#'   values with proper NA.
#' @param missings character list of possible missing data. By default includes:
#'   'N/A','n/a',' ','(vide)','(empty)','d/m','','NA',and 'na'
#'
#' @seealso \code{\link{rec_missing}}
#' @family clean functions
#'
#' @return
#' @export
#'
#' @examples
#' df <- bfa_smart_2019_admin1
#' cleaned_df <- clean_dataset(df)
clean_dataset <- function(data, missings = NULL) {
  if (!is.data.frame(data)) {
    "data is a not a dataframe. It cannot be cleaned"
  }
  if (is.null(missings)) {
    map_dfc(data, rec_missing) %>%
      mutate(across(where(is.list), unlist))
  } else{
    map_dfc(data, rec_missing, missings) %>%
      mutate(across(where(is.list), unlist))
  }

  map_dfc(data, rec_missing) %>%
    mutate(across(where(is.list), unlist))
}

#' Recode missing values to NA
#'
#' @param x vector to be recoded.
#' @param missings character list of possible missing data. By default includes:
#'     'N/A','n/a',' ','(vide)','(empty)','d/m','','NA',and 'na'
#' @return recoded vector
#' @export
#' @family clean functions
#'
#' @examples
#' x <- c(1,2,NA, "N/A", "", "NA")
#' sum(is.na(x))
#' x_rec <- rec_missing(x)
#' sum(is.na(x_rec))
rec_missing <-
  function(x,
           missings = c('N/A', 'n/a', ' ', '(vide)', '(empty)', 'd/m', '', 'NA', 'na')) {
    x[x %in% missings] <- NA
    return(x)
  }

#' Normalise string
#'
#' Normalise provided string by :
#' 1. removing extra spaces ([stringr::str_squish()]) and [trimws()]) 2. passing
#' the string to lower case ([tolower()]) 3. removing all accents
#' ([stringi::stri_trans_general()]), with id = "Latin-ASCII") 4. removing other
#' special characters ("-',.()/ ") and replacing with '_'
#'
#' @param string character string to be normalised. Accepts vectors too.
#'
#' @return normalise string
#' @family clean functions
#'
#' @export
#'
#' @examples
#' normalise_string("àaF   kgfk")
normalise_string <- function(string) {
  no_ws <- stringr::str_squish(string)
  lower <- trimws(tolower(no_ws))
  no_accent <- stringi::stri_trans_general(lower, "Latin-ASCII")
  remove_other <-
    stringr::str_replace_all(no_accent, "[-',.()/ ]", "_")

  return(remove_other)
}
