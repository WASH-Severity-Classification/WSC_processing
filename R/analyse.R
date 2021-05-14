#' Analyse data according to specified analysis plan
#'
#' This function takes an analysis plan following the \code{\link{context_AP}}
#' structure, a specified administrative level of analysis ("admin1","admin2",
#' or "admin3"), and a country iso3 code to conduct the analysis on.
#' Additional parameters are also recommended, especially \code{pop_df}, which
#' is used to weight data when aggregated to a higher administrative unit. If it
#' is not provided, the script will try to find a dataset on
#' \href{https://data.humdata.org}{HDX} which is an hasardous procedure.
#'
#' This function differs from [analyse_DAP()] by combining all the data sources
#' provided in analysis plan, cleaning administrative names over all data
#' sources, imputing data available at a lower or higher administrative unit
#' than the one chosen and **breaking ties** based on the data source reliability if
#' there is more than one data source for an indicator.
#'
#' The imputation of the data from one administrative unit to another level is
#' done by imputing the value value from a higher administrative unit to a lower
#' one, and by imputing a weighted by population value from a lower one to a
#' higher one. For instance if our \code{admin_analysis} is "admin2", all data
#' sources available at admin1 only will be imputed as is (all admin2 receive
#' the same value as their respective admin1), using
#' \code{assign_result_high2low()}. All data sources available at
#' admin3 will be imputed using a weighted mean based on the population figures
#' provided by \code{pop_df} to the admin2 using \code{assign_result_low2high()}.
#'
#' @param context_AP data.frame with context specific analysis plan (AP) that
#'   links the indicators in the WSC AP to the datasets used in the context
#'   analysis. See an example
#'   \href{https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing}{here}
#'   or in \code{\link{context_AP}}.
#' @param admin_analysis character string indicating at which administrative
#'   unit the analysis should be aggregated. Should follow the synthax: admin +
#'   admin_unit_number (e.g. admin1, admin2 , etc.)
#' @param country_iso3 character string with the ISO3 country code for the
#'   country analysed. See
#'   {https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{here} for more details.
#' @param pop_df data.frame containing population figures for each admin unit
#'   being analysed. This data frame is used to weight the datasets when
#'   aggregating from one administrative unit to another. The data frame must
#'   contain at a minimum: \describe{ \item{admin1}{contains names of
#'   administrative unit 1: regions, provinces, etc.} \item{admin2}{contains
#'   names of administrative unit 2: districts, departments, etc.}
#'   \item{total_pop}{contains the population figures associated with each
#'   administrative unit} } If additional administrative units are required, the
#'   name of the column should follow this structure: admin+number (e.g.
#'   admin3). If left to the default (NULL), the package will try to download a
#'   dataframe from \href{https://data.humdata.org}{HDX} Common Operational Datasets
#'   (COD). This is a quite hazardous approach as HDX COD's are not formatted in
#'   a consistent way.
#' @param google_api_key a string with a google API key. This is used to recode
#'   administrative units. See vignette("googleway-vignette") for more details.
#' @param pcodes_df dataframe containing matching key between pcodes and admin
#'   names. For more details see
#'  \href{https://github.com/elliottmess/pcodesOCHA}{here}.
#' @param ... parameters to be passed to other functions, must be following the
#'   format of a named list (e.g. WSC_AP = this_df).
#'
#' @family analysis functions
#'
#' @return data.frame containing the analysed data
#' @export
#'
#' @examples
#' \dontrun{
#' result <- analyse_country(context_AP, admin_analysis = "admin2", country_iso3 = "BFA")
#' }
analyse_country <- function(context_AP,
                            admin_analysis,
                            country_iso3,
                            pop_df = NULL,
                            google_api_key = NULL, pcodes_df = NULL, ...){
  params <- list(...)

  if(!is.null(pop_df) & sum(!c("admin1", "admin2", "total_pop") %in% names(pop_df))>0){
    stop("Missing necessary columns in pop_df.")
  }
  if(!is.null(admin_analysis)){
    if(!admin_analysis %in% c("admin1", "admin2", "admin3")){
      stop("admin_level must either NULL or one of 'admin1', 'admin2', or 'admin3'")
    }
  }

  if(is.null(pcodes_df)){
    pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
  }

  result <- analyse_DAP(context_AP = context_AP, country_iso3 = country_iso3,
                        google_api_key = google_api_key, pcodes_df = pcodes_df)

  if(nrow(result)==1 & sum(rowSums(is.na(result)) == ncol(result))){
    return(NULL)
  }

  final_result <- result %>%
    mutate(lowest_admin_n = case_when(lowest_admin == admin3 ~ 3,
                                         lowest_admin == admin2 ~ 2,
                                         lowest_admin == admin1 ~ 1))

  if(is.null(pcodes_df)){
    pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
  }

  admin_present_result <-  names(final_result)[grep("admin[0-3]$", names(final_result))]
  admin_present_result_n <- as.numeric(str_extract(admin_present_result, "[0-9]"))
  admin_analysis_n <- as.numeric(str_extract(admin_analysis, "[0-9]"))

  if(is.null(pop_df)){
    pop_df <- get_pop_df(country_iso3 = country_iso3,
                         admin_level = NULL,
                         google_api_key = google_api_key,
                         pcodes_df = pcodes_df
                         )
  }

  final_result_need_agg_high <- final_result %>%
   filter(lowest_admin_n < admin_analysis_n) %>%
   split(.$source)

  final_result_final <- final_result %>%
    filter(lowest_admin_n == admin_analysis_n)

  if(length(final_result_need_agg_high)>0){
    high2low <- map2_dfr(final_result_need_agg_high,names(final_result_need_agg_high), ~assign_result_high2low(
      high_df = .x, high_df_name = .y, low_admin = admin_analysis, pop_df = pop_df,
      context_AP = context_AP
    )) %>%
      mutate(value = as.character(value))
    if(nrow(final_result_final)>0){
    final_result_final <- bind_rows(final_result_final, high2low)
    }else{
      final_result_final <- high2low
    }
  }

  final_result_need_agg_low <- final_result %>%
    filter(lowest_admin_n > admin_analysis_n) %>%
    split(.$source)

  if(length(final_result_need_agg_low)>0){
    low2high <- map2_dfr(final_result_need_agg_low,names(final_result_need_agg_low), ~assign_result_low2high(
      low_df = .x, low_df_name = .y, high_admin = admin_analysis, pop_df = pop_df,
      context_AP = context_AP
    )) %>%
      mutate(value = as.character(value))
    if(nrow(final_result_final)>0){
      final_result_final <- bind_rows(final_result_final, low2high)
    }else{
      final_result_final <- low2high
    }
  }

  final_result_final <- final_result_final[rowSums(is.na(final_result_final)) != ncol(final_result_final),]

  double_indic <- final_result_final %>%
    group_by(!!sym(admin_analysis), context, indicator, choice, source) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)

  if(nrow(double_indic) > 0){
    warning("Some indicators are duplicated: some have more than one value per
            unique combination of admin unit, context, choice, and source.
            Please verify that this is normal. This will pose problems in the
            worksheets ")
    print(double_indic)
  }

  label_indicator <- WSC_AP %>%
    select(Indicator,Indicateur, indicator_code)

  final_result_final_labelled <- final_result_final %>%
    left_join(label_indicator, by = c("indicator"="indicator_code")) %>%
    mutate(indicator_choice = paste0(Indicator, "_", choice),
           indicateur_choice = paste0(Indicateur, "_", choice),
           indicator_code_choice = paste0(indicator,choice))
  return(final_result_final_labelled)
}

#' Analyse data according to the contextual data analysis plan (DAP)
#'
#'  This function takes an analysis plan following the \code{\link{context_AP}}
#' structure, and a country iso3 code to conduct the analysis on it. The
#' function essentially walks through all the data sources listed in
#' \code{context_AP}, read dataset associated to it, sheet by sheet (for
#' googlesheets and excel files) or file by file (for csv or sav files). It then
#' cleans the data and find pcodes to have an uniform output that can be put in
#' a pipeline (such as [analyse_country()]).
#'
#' The function outputs a data.frame.
#'
#' The individual variables in the dataset are analysed with a weighted mean of
#' if it is a numerical  variable, or the weighted mean frequency of its factors
#' if it is a  character/factor variable.
#'
#' Additional parameters are also recommended, especially \code{google_api_key},
#' which allows to match the administrative units with google places API. For
#' this, the \code{googleway} package is used. See
#' \code{vignette("googleway-vignette", package = "googleway")} for more details
#'
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param country_iso3 character string with the ISO3 country code for the
#'     country analysed. See [here](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) for more details.
#' @param google_api_key a string with a google API key. See
#'     \code{vignette("googleway-vignette", package = "googleway")} for more details.
#' @param pcodes_df dataframe containing matching key between pcodes and admin
#'     names. For more details see [here](https://github.com/elliottmess/pcodesOCHA).
#' @param ... parameters to be passed to other functions, must be following the
#'     format of a named list (e.g. WSC_AP = this_df).
#'
#' @return data.frame containing the analysed data
#' @export
#' @family analysis functions
#'
#' @examples
#' \dontrun{
#' context_AP <- WSCprocessing::context_AP
#'
#' result <- analyse_DAP(context_AP, country_iso3 = "BFA")
#'
#' }
analyse_DAP <- function(context_AP, country_iso3,google_api_key = NULL, pcodes_df = NULL, ...){

  params <- list(...)

  if (is.null(params$WSC_AP)){
    WSC_AP <- WSCprocessing::WSC_AP
    params$WSC_AP <- WSCprocessing::WSC_AP
  }

  generic_context_AP <- WSCprocessing::context_AP

  if(sum(!names(context_AP) %in% names(generic_context_AP)) > 0){
    stop("The column names in your analysis plan (AP) do not match the template. Please check ?WSCprocessing::context_AP for more information")
  }

  data_sources <- unique(context_AP$data_source_name)[!is.na(unique(context_AP$data_source_name))]

  source_analysed <- map(data_sources, analyse_source_all_sheets, context_AP = context_AP, WSC_AP = WSC_AP,
                         params)

  source_analysed_combined <- map_dfr(source_analysed, function(x){
    if(sum(grepl("Core variables are missing.$", x[[1]])) == 0 ){
      return(x)
    }
  })

  if(nrow(source_analysed_combined)==1 & sum(rowSums(is.na(source_analysed_combined)) == ncol(source_analysed_combined))>0 ){
    warning(paste0("No data could be read"))
    return(source_analysed_combined)
  }
  admin_cols <- c(admin0 = NA_character_, admin1 = NA_character_,
                  admin2 = NA_character_, admin3 = NA_character_)
  admin_lvls <- paste0("admin", 0:3)

  source_analysed_relocated <- source_analysed_combined %>%
    tibble::add_column(!!!admin_cols[setdiff(names(admin_cols), names(source_analysed_combined))]) %>%
    mutate(across(where(is.factor), as.character)) %>%
    relocate(!!admin_lvls) %>%
    mutate(lowest_admin = case_when(!is.na(admin3) ~ admin3,
                                    !is.na(admin2) ~ admin2,
                                    !is.na(admin1) ~ admin1),
           ref_name = paste(admin3, admin2, admin1)
    )

  source_analysed_relocated$ref_name <- trimws(gsub("NA |NA$", "", source_analysed_relocated$ref_name))
  source_analysed_relocated <- source_analysed_relocated[source_analysed_relocated$ref_name != "",]

  if(!is.null(google_api_key)){
    admin_present <- unique(source_analysed_relocated$ref_name)
  }else{
    admin_present <- unique(source_analysed_relocated$lowest_admin)
  }

  source_analysed_relocated <- select(source_analysed_relocated,
                                      -any_of(paste0("admin",rep(0:3,1)))) %>%
    mutate(ref_name = case_when(str_count(ref_name) > 1 ~ word(ref_name,1),
                                TRUE ~ ref_name))

  if(is.null(pcodes_df)){
    pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
  }

  pcode_cols <- c(admin_cols, admin0pcode = NA_character_,
                  admin1pcode = NA_character_,
                  admin2pcode = NA_character_,
                  admin3pcode = NA_character_)

  admin_pcoded <- find_pcodes_admin(admin_present,
                                    country_iso3 = country_iso3,
                                    admin_level = NULL,
                                    google_api_key = google_api_key,
                                    pcodes_df = pcodes_df) %>%
    mutate(lowest_pcode = case_when(grepl( "^c\\(\"", lowest_pcode) ~ NA_character_,
                                    TRUE ~ lowest_pcode)) %>%
    tibble::add_column(!!!pcode_cols[setdiff(names(pcode_cols), names(.data))]) %>%
    select(admin1, admin1pcode, admin2,admin2pcode, admin3,admin3pcode, ref_name)

  if(!"choice" %in% names(source_analysed_relocated)){
    source_analysed_relocated$choice <- NA
  }


  source_analysed_final <- admin_pcoded %>%
    left_join(source_analysed_relocated, by = c("ref_name" = "ref_name")) %>%
    select(paste0("admin", rep(1:3,1)), indicator, choice, value,  context,
           source,
           paste0("admin", rep(1:3,1),"pcode"), lowest_admin) %>%
    filter(!is.na(indicator))

  return(source_analysed_final)
}

#' Analyse all data sheets in a data source
#'
#' Function to analyse data source according to the analysis plan provided. This
#' function is similar to [analyse_DAP()] or [analyse_data()], but walks over
#' all sheets individually in order to analyse each individually.
#'
#' @param source_name character string containing the name of the data source to
#'     be analysed according to the analysis plan.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be
#'     found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#' @param ... parameters to be passed to other functions, must be following the
#'     format of a named list (e.g. WSC_AP = this_df).
#'
#' @return data.frame containing the analysed data
#' @export
#'
#' @family analysis functions
#'
#' @examples
#' \dontrun{
#' analysed_source <- analyse_source_all_sheets("REACH-MSNA-2020", WSCprocessing::context_AP)
#' }

analyse_source_all_sheets <-
  function(source_name, context_AP, WSC_AP = WSCprocessing::WSC_AP, ...) {

    params <- list(...)

    sheets_name <- unique(context_AP$data_sheet_name[context_AP$data_source_name == source_name])
    sheets_name <- sheets_name[!is.na(sheets_name)]

    if(length(sheets_name) == 0){
      sheets_name <- NA
      analysis_param <- context_AP %>%
        filter(data_source_name == source_name)
    }

    analysis_param <- map_dfr(sheets_name,
                          get_dataAP, source_name = source_name, context_AP = context_AP)

    check_analysis_param <- analysis_param %>%
      dplyr::group_by(data_sheet_name,data_worksheet_url) %>%
      dplyr::summarise(n = dplyr::n(),.groups = "drop")

    if(sum(check_analysis_param$n > 1)>0){
      result_test <- dplyr::filter(check_analysis_param, n >1)
      stop(paste0(source_name, " - ", result_test$data_sheet_name,"  has one combination of worksheet/sheet where there is multiple combinaison of admin_level, data_type, or context.
                  Please make sure that there is one type of data from one type of administrative units in your context_AP."))
    }

    sheets_list <- analysis_param %>%
      dplyr::rename(ss = data_worksheet_url, sheet = data_sheet_name) %>%
      dplyr::select(ss, sheet, data_source_name) %>%
      purrr::pmap(read_df)

    if (length(sheets_list) == 0 | (sum(map(sheets_list, is.null) %>% unlist()) == length(sheets_list))) {
      warning(
        paste0(
          source_name,
          " : Impossible to read the dataset, please check the information in context_AP"
        )
      )
      result <- tibble(admin1= NA, indicator = NA, choice = NA, value = NA, context = NA)
      return(result)
    }

    analysis_param_list <- split(analysis_param, seq(nrow(analysis_param)))

    scored_data <- pmap_dfr(list(data_AP = analysis_param_list, data = sheets_list),
                            ~with(list(...),analyse_data(data, data_AP, WSC_AP = WSC_AP, params)))

    return(scored_data)
  }

#' Analyse a specific dataframe according to the analysis plan (AP)
#'
#' This function analyse one data source according to the analysis plan provided
#' in data_AP, which is short for data analysis plan. \code{data_AP} is a
#' filtered and standardised version of context_AP for a specific dataset.
#'
#' This function differs from [analyse_source_all_sheets()] and [analyse_DAP()]
#' as it takes a unique data source rather than walking over multiple datasets.
#'
#' #' The individual variables in the dataset are analysed with a weighted mean of
#' if it is a numerical  variable, or the weighted mean frequency of its factors
#' if it is a  character/factor variable.
#'
#' @param data data.frame containing the data to be analysed.
#' @param data_AP data.frame with data specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used.
#'    Most of those parameters are coming from context_AP and are automatically
#'    passed from analyse_source_all_sheets.
#'    The dataframe must contain:
#'    \describe{
#'       \item{data_sheet_name}{name of the sheet where the data is stored}
#'       \item{data_worksheet_url}{URL of the worksheet to be used.}
#'       \item{admin_level}{administrative level unit at which the data will be
#'           aggregated}
#'       \item{data_type}{type of data: either hh or area}
#'       \item{context}{context identifying the time or place where the analysis
#'           is conducted.}
#'       \item{indicator_code_source}{list of indicators to be analysed as they
#'           appear in the original dataset (data)}
#'       \item{indicator_code}{list of indicators to be analysed as they
#'           appear in the WSC analytical framework.}
#'    }
#'
#' @param ... parameters to be passed to other functions, must be following the
#'     format of a named list (e.g. WSC_AP = this_df).
#'
#' @family analysis functions
#'
#' @export
#'
#' @return a dataframe with the analysed data.
#' @examples
#' \dontrun{
#'
#' this_dataAP <- get_dataAP("SMART-2019", "cleaned_data_admin1", context_AP)
#'
#' df <- bfa_smart_2019_admin1
#' results <- analyse_data(df, this_dataAP)
#' }

analyse_data <- function(data, data_AP, ...){

  params <- list(...)

  if(is.list(data)) data <- as.data.frame(data)

  names_param <- c("data_sheet_name", "data_worksheet_url", "admin_level",
                   "data_type", "context","indicator_code_source",
                   "indicator_code", "choices_name", "score_recoding", "question_type")


  data_cleaned <- data %>%
    clean_dataset()

  data_renamed <- suppressWarnings(recode_source(data_cleaned, data_AP = data_AP))
  admin_level <- as.character(unlist(data_AP$admin_level))
  indicator_code <- as.character(unlist(data_AP$indicator_code))

  indicators_to_check <- c(admin_level, indicator_code)

  indicators_to_check_clean <- indicators_to_check[!is.na(indicators_to_check)]
  indicators_to_check_clean <- indicators_to_check_clean[!indicators_to_check_clean %in% c("admin1", "admin2", "admin3", "weights", "sampling_id", "cluster_id")]

  missing_core_elements <- check_source(data = data_renamed,
                                        indicators_to_check = indicators_to_check_clean)

  admin_cols <- names(data_renamed)[grepl("^admin[0-9]$", names(data_renamed))]

  if(length(admin_cols) == 0){
    stop(paste0(data_AP$data_source_name, " no column identifying admin units. Must follow the patern admin and admin number (e.g. admin1, admin2, etc.)."))
  }
  admin_df <- select(data_renamed, !!admin_cols) %>% distinct()


  if(missing_core_elements == FALSE){
    if (sum(
      names(data_renamed) %in% c(
        as.character(unlist(data_AP$admin_level)),
        "indicator",
        "choice",
        "value",
        "context"
      )
    ) == ncol(data_renamed)) {
      return(data_renamed)
    } else{

      reduced_data <- data_renamed %>%
        select(any_of(c(indicators_to_check_clean, admin_cols)))

      agg_data <- aggregate_admin(data = reduced_data,
        data_AP = data_AP) %>%
        dplyr::mutate(value = as.character(value))

      lowest_admin_agg_data <- max(as.numeric(str_extract(
        names(agg_data)[grepl("admin[0-9]", names(agg_data))],"[0-3]")))
      lowest_admin_agg_data_name <- paste0("admin", lowest_admin_agg_data)
      lowest_admin_list_agg <- setNames(lowest_admin_agg_data_name, lowest_admin_agg_data_name)
      agg_data_admin <- agg_data %>%
        left_join(admin_df, by = c(lowest_admin_list_agg))

      scores <- score_source(
          data = data_renamed,
          data_AP = data_AP,
          WSC_AP = WSC_AP
        )

      lowest_admin_scores <- max(as.numeric(str_extract(
        names(scores)[grepl("admin[0-9]", names(scores))],"[0-3]")))
      lowest_admin_scores_name <- paste0("admin", lowest_admin_scores)
      lowest_admin_list <- setNames(lowest_admin_scores_name, lowest_admin_scores_name)
      scores_admin <- scores %>%
        left_join(admin_df, by = c(lowest_admin_list))

      full_AP <- data_AP%>%
        tidyr::unnest(cols = where(is.list)) %>%
        dplyr::left_join(WSC_AP, by = "indicator_code") %>%
        dplyr::mutate(
          indicator_code_source = normalise_string(indicator_code_source)
        ) %>%
        distinct()

      if(sum(full_AP$wash_scoring, na.rm = T) > 0 & sum(full_AP$data_type == "hh") >0){


      WIS_scores <- agg_score(data_renamed,
                              data_AP = data_AP,
                              WSC_AP = WSC_AP,
                              .WIS_water = WIS_water,
                              .WIS_sanitation = WIS_sanitation,
                              .WIS_final = WIS_final) %>%
        dplyr::mutate(value = as.character(value))


      lowest_admin_WIS <- max(as.numeric(str_extract(
        names(WIS_scores)[grepl("admin[0-9]", names(WIS_scores))],"[0-3]")))
      lowest_admin_WIS_name <- paste0("admin", lowest_admin_WIS)
      lowest_admin_list_WIS <- setNames(lowest_admin_WIS_name, lowest_admin_WIS_name)
      WIS_admin <- WIS_scores %>%
        left_join(admin_df, by = c(lowest_admin_list_WIS))
      scores_admin <- bind_rows(scores_admin, WIS_admin)
      }

      data_scored <- dplyr::bind_rows(agg_data_admin, scores_admin) %>%
        ungroup()
    }

    data_scored <-
      data_scored[rowSums(is.na(data_scored)) != ncol(data_scored),] %>%
      dplyr::distinct()

    data_scored <-
      data_scored[, names(data_scored) %in% admin_cols | colSums(is.na(data_scored)) < nrow(data_scored)]

    final_cols <- c("indicator", "choice", "value", "context")

    data_scored <-
      tibble::add_column(data_scored,!!!final_cols[setdiff(names(final_cols), names(data_scored))])

    data_scored$source <- paste0(data_AP$data_source_name, "-",data_AP$data_sheet_name)



    return(data_scored)
  }else{
    warning(paste0(unique(data_AP$data_source_name)), " could not be analysed. Core variables are missing.")
  }
}

#' Analyse a given variable
#'
#' Analyse (summarise/aggregate) a variable contained in a
#' \code{\link[srvyr]{as_survey_design}} object by getting the weighted mean of
#' the data if it is a numerical  variable, or the weighted mean frequency of
#' its factors if it is a  character/factor variable.
#'
#' @param var_analyse character string identifying the column with the variable
#'    to analyse.
#' @param design_data \code{\link[srvyr]{as_survey_design}} object containing the data,
#'    weights, cluster IDs, and other elements required for complexe surveys
#'    analysis.
#' @param agg_level character string identifying the column to be used to
#'    aggregate the data.
#' @param source character string containing the name of the data source
#'    analysed.
#' @return dataframe containing the analysed data. The returned table follows
#'    this structure:
#'    \describe{
#'       \item{agg_level}{contains the administrative unit at which the data
#'       has been aggregated. The name of the column will be the value of the
#'       agg_level argument provided.}
#'       \item{indicator}{Identifier of the indicator (variable) being analysed.
#'       The name of the column will be the value of the var_analyse argument
#'       provided.}
#'       \item{choice}{contains the value }
#'     }
#' @family analysis functions
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' this_dataAP <- get_dataAP("REACH-MSNA-2020", NA, context_AP)
#'
#' df <- bfa_msna_2020
#'
#' this_design <- srvyr::as_survey(df ,
#' ids = sampling_id,
#' weights = weights_sampling)
#'
#' var_analyse <- "dispo_lave_main"
#'
#' result <- analyse_var(this_design, var_analyse,
#' agg_level = this_dataAP$admin_level)
#' }
analyse_var <- function(design_data, var_analyse, agg_level,...) {

  if(!"tbl_svy" %in% class(design_data)){
    stop("design_data must be a srvyr::as_survey_design object")
  }
  if(!is.character(var_analyse)){
    stop("var_analyse must be a character vector.")
  }
  if(!is.character(agg_level)){
    stop("agg_level must be a character vector.")
  }

  if (class(design_data$variables[[var_analyse]]) %in% c("factor", "character")) {
    design_data$variables[[var_analyse]] <-
      factor(design_data$variables[[var_analyse]])
    addVars_agg_table <- design_data %>%
      dplyr::filter(!is.na(!!dplyr::sym(var_analyse))) %>%
      dplyr::group_by(!!dplyr::sym(agg_level), !!dplyr::sym(var_analyse)) %>%
      dplyr::summarise(value = srvyr::survey_mean(na.rm = TRUE)) %>%
      dplyr::mutate(indicator = var_analyse) %>%
      dplyr::rename(choice = as.character(var_analyse)) %>%
      dplyr::select(!!agg_level, indicator, choice, value)
  } else{
    addVars_agg_table <- design_data %>%
      dplyr::filter(!is.na(!!dplyr::sym(var_analyse))) %>%
      dplyr::group_by(!!dplyr::sym(agg_level)) %>%
      dplyr::summarise(!!dplyr::sym(var_analyse) := srvyr::survey_mean(!!dplyr::sym(var_analyse), na.rm = TRUE)) %>%
      dplyr::mutate(
        indicator = var_analyse,
        choice = NA,
        value = !!dplyr::sym(var_analyse)
      ) %>%
      dplyr::select(!!agg_level, indicator, choice, value)
  }
  return(addVars_agg_table)
}

#' Get the analysis plan for a specific dataset.
#'
#' This is essentially a formatting and filtering function.
#'
#' @param source_name character string containing the name of the data source to
#'     be analysed according to the analysis plan.
#' @param data_sheet_name character string of the sheet where the data is stored
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#'
#' @return dataframe containing the analysis plan for the specified dataset.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' this_dataAP <- get_dataAP("SMART-2019", "cleaned_data_admin1", context_AP)
#' }
get_dataAP <- function(source_name, data_sheet_name, context_AP){

  if(is.na(data_sheet_name)){
    data_source <- context_AP %>%
      dplyr::filter(data_source_name == !!source_name) %>%
      dplyr::distinct()
  }else{
    data_source <- context_AP %>%
      dplyr::filter(data_source_name == !!source_name, data_sheet_name == !!data_sheet_name) %>%
      dplyr::distinct()
  }

  analysis_param <- data_source %>%
    dplyr::select(data_source_name, data_sheet_name,data_worksheet_url,
                  admin_level,data_type, context, indicator_code_source,
                  indicator_code, choices_name, score_recoding,
                  question_type) %>%
    dplyr::group_by(data_source_name, data_sheet_name, data_worksheet_url,
                    admin_level,data_type, context) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), list),
                     .groups = "drop")

  return(analysis_param)

}

