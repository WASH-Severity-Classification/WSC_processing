#' Analyse data for a specific country analysis plan at a specificied administative unit
#'
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param admin_analysis character string indicating at which administrative
#'     unit the analysis should be aggregated.
#'     Should follow the synthax: admin + admin_unit_number (e.g. admin1, admin2
#'     , etc.)
#' @param country_iso3 character string with the ISO3 country code for the
#'     country analysed. See [here](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) for more details.
#' @param pop_df dataframe containing population figures for each admin unit
#'     being analysed. This dataframe is used to weight the datasets when
#'     aggregating from one administrative unit to another.
#'     The dataframe must contain at a minimum:
#'     \describe{
#'       \item{admin1}{contains names of administrative unit 1: regions,
#'       provinces, etc.}
#'       \item{admin2}{contains names of administrative unit 2: districts,
#'       departments, etc.}
#'       \item{total_pop}{contains the population figures associated with each
#'       administrative unit}
#'     }
#'     If additional administrative units are required, the name of the column
#'     should follow this structure: admin+number (e.g. admin3).
#'     If left to the default (NULL), the package will try to
#'     download a dataframe from [HDX](https://data.humdata.org) Common
#'     Operational Datasets (COD). This is a quite hazardous approach as HDX
#'     COD's are not formatted in a consistent way.
#' @param google_api_key a string with a google API key. See vignette("googleway-vignette") for more details.
#' @param pcodes_df dataframe containing matching key between pcodes and admin
#'     names. For more details see [here](https://github.com/elliottmess/pcodesOCHA).
#' @param ... parameters to be passed to other functions, must be following the
#'     format of a named list (e.g. WSC_AP = this_df).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' result <- analyse_country(context_AP, "admin2", "BFA", google_api_key = NULL)
#' }
analyse_country <- function(context_AP, admin_analysis, country_iso3,
                            pop_df = NULL,
                            google_api_key = NULL, pcodes_df = NULL, ...){
  params <- list(...)

  if (is.null(params$WSC_AP)){
    WSC_AP <- WSCprocessing::WSC_AP
    params$WSC_AP <- WSCprocessing::WSC_AP
  }

  if(sum(!c("admin1", "admin2", "total_pop") %in% names(pop_df))>0){
    stop("Missing necessary columns in pop_df.")
  }

  if(is.null(pcodes_df)){
    pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
  }

  result <- analyse_DAP(context_AP = context_AP, country_iso3 = country_iso3,
                        google_api_key = google_api_key, pcodes_df = pcodes_df,
                        params)

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
    pop_df_rhdx <- search_datasets(paste(country_iso3, "subnational population statistics")) %>%
      pluck(1)
    pop_resources <- pop_df_rhdx$resources
    pop_resources_names <- map(pop_resources, ~c(.x$data$name)) %>%
      unlist()
    pop_resources_readable <- pop_resources_names[grep(".xlsx$|.xls$|.csv$", pop_resources_names)]
    pop_resources_good <- pop_resources_readable[grep("pop|POP", pop_resources_readable)]
    admin_units <- pop_resources_good[grep("adm[Z-a]*?[0-9]|ADM[Z-a]*?[0-9]", pop_resources_good)]
    min_admin <- max(as.numeric(str_extract(str_extract(admin_units, "adm[Z-a]*?[0-9]|ADM[Z-a]*?[0-9]"), "[0-9]")))
    min_admin_pop_res <- pop_resources_names[grep(paste0("adm[Z-a]*?",min_admin, "|ADM[Z-a]*?",min_admin), pop_resources_names)]

    pop_df <- pop_df_rhdx %>%
      get_resource(grep(min_admin_pop_res, pop_resources_names)) %>%
      read_resource() %>%
      rename(total_pop = contains("total"))

    admin_cols_pcodes <- names(pop_df)[grep("^(adm|ADM)([A-Z]{0,})([a-z]{0,})[0-9].?", names(pop_df))]
    admin_cols <- admin_cols_pcodes[!grepl("PCODES{0,1}$|pcode.{0,1}$", admin_cols_pcodes)]
    admin_pcodes <- admin_cols_pcodes[grepl("PCODES{0,1}$|pcode.{0,1}$", admin_cols_pcodes)]

    admin_cols_n <- as.numeric(str_extract(str_extract(admin_cols, "adm[Z-a]*?[0-9]|ADM[Z-a]*?[0-9]"), "[0-9]"))
    admin_cols_f <- setNames(admin_cols, paste0("admin", admin_cols_n))
    admin_pcodes_f <- set_names(admin_pcodes,paste0("admin", admin_cols_n, "_pcode") )

    pop_df <- rename(pop_df, admin_cols_f, admin_pcodes_f) %>%
      select(paste0("admin", max(admin_cols_n)), contains("pop")) %>%
      mutate(!!sym(paste0("admin", max(admin_cols_n))):= normalise_string(!!sym(paste0("admin", max(admin_cols_n)))))

    pop_df_lowest_adm_rename <-  find_pcodes_admin(pop_df[[paste0("admin", max(admin_cols_n))]],
                                                   country_iso3 = country_iso3,
                                                   google_api_key = google_api_key,
                                                   pcodes_df = pcodes_df)

    pop_df <- left_join(pop_df, pop_df_lowest_adm_rename,
                        by = paste0("admin", max(admin_cols_n)))
  }

  final_result_need_agg_high <- final_result %>%
   filter(lowest_admin_n < admin_analysis_n) %>%
   split(.$source)

  final_result_final <- final_result %>%
    filter(lowest_admin_n == admin_analysis_n)

  if(length(final_result_need_agg_high)>0){
    high2low <- map2(final_result_need_agg_high,names(final_result_need_agg_high), ~assign_result_high2low(
      high_df = .x, high_df_name = .y, low_admin = admin_analysis, pop_df = pop_df,
      context_AP = context_AP
    ))
    final_result_final <- bind_rows(final_result_final, high2low)
  }

  final_result_need_agg_low <- final_result %>%
    filter(lowest_admin_n > admin_analysis_n) %>%
    split(.$source)

  if(length(final_result_need_agg_low)>0){
    low2high <- map2(final_result_need_agg_low,names(final_result_need_agg_low), ~assign_result_low2high(
      low_df = .x, low_df_name = .y, high_admin = admin_analysis, pop_df = pop_df,
      context_AP = context_AP
    ))
    final_result_final <- bind_rows(final_result_final, low2high)
  }

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
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param country_iso3 character string with the ISO3 country code for the
#'     country analysed. See [here](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) for more details.
#' @param google_api_key a string with a google API key. See vignette("googleway-vignette") for more details.
#' @param pcodes_df dataframe containing matching key between pcodes and admin
#'     names. For more details see [here](https://github.com/elliottmess/pcodesOCHA).
#' @param ... parameters to be passed to other functions, must be following the
#'     format of a named list (e.g. WSC_AP = this_df).
#'
#' @return data.frame containing the analysed data
#' @export
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

  source_analysed <- map(data_sources, analyse_source_all_sheets, context_AP = context_AP, WSC_AP = WSC_AP)

  source_analysed_combined <- map_dfr(source_analysed, function(x){
    if(sum(grepl("Core variables are missing.$", x[[1]])) == 0 ){
      return(x)
    }
  })

  if(nrow(source_analysed_combined)==1 & rowSums(is.na(source_analysed_combined)) == ncol(source_analysed_combined)){
    warning(paste0("No data could be read in ", source_name))
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
                                      -any_of(paste0("admin",rep(0:3,1))))

  if(is.null(pcodes_df)){
    pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
  }

  pcode_cols <- c(admin_cols, admin0pcode = NA_character_,
                  admin1pcode = NA_character_,
                  admin2pcode = NA_character_,
                  admin3pcode = NA_character_)

  admin_pcoded <- find_pcodes_admin(admin_present,
                                    country_iso3 = country_iso3,
                                    google_api_key = google_api_key,
                                    pcodes_df = pcodes_df) %>%
    mutate(lowest_pcode = case_when(grepl( "^c\\(\"", lowest_pcode) ~ NA_character_,
                                    TRUE ~ lowest_pcode)) %>%
    tibble::add_column(!!!pcode_cols[setdiff(names(pcode_cols), names(.data))]) %>%
    select(admin1, admin1pcode, admin2,admin2pcode, admin3,admin3pcode, ref_name)

  source_analysed_final <- admin_pcoded %>%
    left_join(source_analysed_relocated, by = c("ref_name" = "ref_name")) %>%
    select(paste0("admin", rep(1:3,1)), indicator, choice, value,  context,
           source,
           paste0("admin", rep(1:3,1),"pcode"), lowest_admin) %>%
    filter(!is.na(indicator))

  return(source_analysed_final)
}

#' Analyse all data sheets in a data source
#'name = unlist(data_AP$indicator_code_source)
#' Function to analyse data source according to the analysis plan provided.
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
#' @examples
#' \dontrun{
#' analysed_source <- analyse_source_all_sheets("smart_rapid_idp_2020", WSCprocessing::context_AP)
#' }

analyse_source_all_sheets <-
  function(source_name, context_AP, WSC_AP = WSCprocessing::WSC_AP, ...) {

    params <- list(...)

    data_source <- context_AP %>%
      dplyr::filter(data_source_name == !!source_name) %>%
      dplyr::distinct()

    analysis_param <- data_source %>%
      dplyr::select(data_source_name, data_sheet_name,data_worksheet_url,
                    admin_level,data_type, context, indicator_code_source,
                    indicator_code, choices_name, score_recoding,
                    question_type) %>%
      dplyr::group_by(data_source_name, data_sheet_name, data_worksheet_url,
                      admin_level,data_type, context) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list),
                       .groups = "drop")

    check_analysis_param <- analysis_param %>%
      dplyr::group_by(data_sheet_name,data_worksheet_url) %>%
      dplyr::summarise(n = dplyr::n(),.groups = "drop")

    if(sum(check_analysis_param$n > 1)>0){
      result_test <- dplyr::filter(check_analysis_param, n >1)
      stop(paste0(source_name, " - ", result_test$data_sheet_name,"  has one combination of worksheet/sheet where there is multiple combinaison of admin_level, data_type, or context.
                  Please make sure that there is one type of data from one type of administrative units in your context_AP."))
    }

    sheets <- analysis_param %>%
      dplyr::rename(ss = data_worksheet_url, sheet = data_sheet_name) %>%
      dplyr::select(ss, sheet, data_source_name) %>%
      purrr::pmap(read_df)

    if (length(sheets) == 0 | (sum(map(sheets, is.null) %>% unlist()) == length(sheets))) {
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

    sheets_list <- sheets

    scored_data <- pmap_dfr(list(data_AP = analysis_param_list, data = sheets_list),
                            ~with(list(...),analyse_data(data, data_AP, WSC_AP = WSC_AP, ...)))

    return(scored_data)
  }

#' Analyse a specific dataframe according to the analysis plan (AP)
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
#' @export
#'
#' @return a dataframe with the analysed data.
#' @examples
#' \dontrun{
#' analyse_data()
#' }

analyse_data <- function(data, data_AP, ...){

  params <- list(...)

  if(is.list(data)) data <- as.data.frame(data)

  names_param <- c("data_sheet_name", "data_worksheet_url", "admin_level",
                   "data_type", "context","indicator_code_source",
                   "indicator_code", "choices_name", "score_recoding", "question_type")

  data_renamed <- suppressWarnings(recode_source(data, data_AP = data_AP))
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

      agg_data <- aggregate_admin(
        data = data_renamed,
        data_AP = data_AP) %>%
        dplyr::mutate(value = as.character(value))

      lowest_admin_agg_data <- max(as.numeric(str_extract(
        names(agg_data)[grepl("admin[0-9]", names(agg_data))],"[0-3]")))
      lowest_admin_agg_data_name <- paste0("admin", lowest_admin_agg_data)
      lowest_admin_list_agg <- setNames(lowest_admin_agg_data_name, lowest_admin_agg_data_name)
      agg_data_admin <- agg_data %>%
        left_join(admin_df, by = c(lowest_admin_list_agg))

      scores <-
        score_data_AP(
          data = data_renamed,
          data_AP = data_AP,
          WSC_AP = params$WSC_AP
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
                              WSC_AP = params$WSC_AP) %>%
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
