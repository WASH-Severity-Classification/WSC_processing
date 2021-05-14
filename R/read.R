#' Read data from multiple formats
#'
#' This function takes information on how to read the data and tries different
#' options.
#' If parameter ss is empty or not compatible to a google sheet format
#' (URL starts with "https://docs.google.com/spreadsheets"), the function will
#' try to guess the extension of file by looking for all files in working
#' directory and find files that contain the data_source_name parameter and ends
#' with one of the supported file format:
#' \itemize{
#'  \item{"csv"}{.csv}
#'  \item{"xls"}{.xls}
#'  \item{"xlsx"}{.xlsx}
#'  \item{"sav"}{.sav}
#' }
#' If more than one file is found that could be matching, the function does not
#' read any file to avoid issues by reading the wrong files. For instance, if
#' you try to use the function as follow:
#' \code{read_df(ss = "", sheet = NA, data_source_name = "data")} the function
#' will look for any file where 'data' can be found in the name and read it.
#' This is obvisouly a quite hasardous entreprise, so data_source_name should be
#' as specific as possible.
#'
#' @param ss character URL to the spreadsheet to be read. If ss does not match the
#'     googlesheet URL pattern
#'     (URL starts with "https://docs.google.com/spreadsheets"), the function
#'     will try to read the file by finding all files with the data_source_name
#'     parameters with the supported extensions (.csv, .xls, .xlsx, and .sav).
#' @param sheet character string with name of the sheet to be read
#' @param data_source_name character string containing the name of the data source to
#'     be analysed according to the analysis plan.
#' @param ...
#'
#' @return dataframe containing the read data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_data <- read_df(ss = "https://docs.google.com/spreadsheets/d/1D13q4DP2p-ObvW6_b0rQ1O16dHbJGYCDoAYBDS6jDq4/edit?usp=sharing",
#'     sheet = "cleaned_data",
#'     data_source_name = "smart_rapid_idp_2020")
#'
#' }
read_df <- function(ss, sheet, data_source_name, ...) {
  if (grepl("https://docs.google.com/spreadsheets", ss)) {
    result <- googlesheets4::read_sheet(ss,
                                        sheet,
                                        .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
      mutate(across(where(is.list), unlist))
  } else{
    readable_files <-
      fs::dir_ls(".", recurse = TRUE, glob = "*.csv$|*.xls$|*.xlsx$|.sav$")
    readable_files_source_name <-
      readable_files[grepl(data_source_name, readable_files)]
    if (length(readable_files_source_name) > 1) {
      warning(
        paste0(
          data_source_name,
          " has more than one file that is readable (csv, sav, xls, or xlsx) and contains the name of the data source.
                           Please make sure to specify an URL to the google sheet contain"
        )
      )
      result <- NULL
    } else {
      if (length(readable_files_source_name) == 0) {
        warning(
          paste0(
            data_source_name,
            " - ",
            sheet,
            " could not be loaded. ",
            "No worksheet or file containing the source name could be found in the working directory."
          )
        )
        return(NULL)
      } else {
        ss <- readable_files_source_name
      }
    }

    if (fs::file_exists(ss)) {
      if (sum(grepl("*.csv$", readable_files_source_name)) != 0) {
        result <- readr::read_csv(readable_files_source_name)
      } else if(sum(grepl("*.sav$", readable_files_source_name)) != 0) {
        rename_select_mulitple_spss <- function(data, var){
          label_choice_dot <- gsub("\\/", "\\.", attributes(data[[var]])$label)
          if(nchar(label_choice_dot) >= 64){
            label_choice_dot <- var
          }
          index <- grep(paste0(var,"$"), names(data))
          names(data)[[index]] <- label_choice_dot
        }

        result <- haven::read_sav(readable_files_source_name, user_na = TRUE) %>%
          haven::as_factor()

        result <- map(result, rec_missing) %>%
          bind_cols() %>%
          as_tibble()

        names(result)[grepl("\\.", names(result))] <- map_chr(names(result)[grepl("\\.", names(result))], ~rename_select_mulitple_spss(result, .x))
        return(result)
      }else if (sum(grepl("*.xls$|*.xlsx$", readable_files_source_name)) !=
                0) {
        result <- readxl::read_excel(readable_files_source_name, sheet = sheet)
      } else{
        warning(
          paste0(
            ss,
            " could not be read. only .csv, .sav, .xls, and .xlsx are supported at the moment"
          )
        )
        return(NULL)
      }
    } else{
      warning(paste0(
        ss,
        " could not be found in the working directory and sub-folders."
      ))
      return(NULL)
    }

  }
}
