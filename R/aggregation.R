#' Aggregate scores at specified aggregation level
#'
#' @param data data.frame containing the data to be analysed with the function
#' @param data_name character string identifying the name of the data frame used
#'      in \code{data}. Should be equivalent to the \code{data_source_name}
#'      called in \code{context_AP}.
#' @param context Character string identifying the context to be used in the
#'     function call. This is to be used if multiple context (geographical or
#'     temporal) are being analysed. For instance, if data is used for Burkina
#'     Faso in 2020 and 2019, this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that
#'     links the indicators in the WSC AP to the datasets used in the context
#'     analysis.
#'     See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be
#'     found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#' @param WIS_water data.frame with the scoring reference matrix for the Water
#'     component of the WASH Insecurity Score (WIS)
#' @param WIS_sanitation data.frame with the scoring reference matrix for the
#'     Sanitation component of the WIS
#' @param WIS_final data.frame with the scoring reference matrix for the final
#'     score of the WIS
#' @param agg_level character string specifying which column should be used to
#'     aggregate the data. This is is typically an administrative unit (e.g.
#'     province, region, departement, admin2, etc.)
#'
#' @return data.frame containing the aggregated data according to context_AP and
#'    WSC_AP
#' @export
#'
#' @import magrittr
#' @import dplyr
#' @importFrom stringr str_detect
#' @import srvyr
#'
#' @examples
#' \dontrun{
#' agg_score(context = "bfa_2020", context_AP = WSCprocessing::context_AP,
#'           data_name = "bfa_msna_2020",
#'           WSC_AP = WSCprocessing::WSC_AP,
#'           data = WSCprocessing::bfa_msna_2020, agg_level = "admin2")
#'}

agg_score <-
  function(.data,
           data_AP,
           WIS_water = WSCprocessing::WIS_water,
           WIS_sanitation = WSCprocessing::WIS_sanitation,
           WIS_final = WSCprocessing::WIS_final,
           ...) {

    if(is.null(WSC_AP)){
      if(is.null(params$WSC_AP)){
        WSC_AP <- WSCprocessing::WSC_AP
      }else{
        WSC_AP <- params$WSC_AP
      }
    }


    full_AP <- data_AP%>%
      tidyr::unnest(cols = where(is.list)) %>%
      dplyr::left_join(WSC_AP, by = "indicator_code") %>%
      dplyr::mutate(
        indicator_code_source = normalise_string(indicator_code_source)
      ) %>%
      distinct()

    data_scoring <-
      score_WIS(
        .data = .data,
        data_AP = data_AP,
        WSC_AP = WSC_AP,
        WIS_water = WIS_water,
        WIS_sanitation = WIS_sanitation,
        WIS_final = WIS_final
      ) %>%
      as_tibble() %>%
      dplyr::mutate(
        water_score = factor(water_score),
        sanit_score = factor(sanit_score),
        score_final = factor(score_finale),
        key_score = dplyr::if_else(
          stringr::str_detect(key_score, "NA"),
          NA_character_,
          key_score
        ),
        key_water = dplyr::if_else(
          stringr::str_detect(key_water, "NA"),
          NA_character_,
          key_water
        ),
        key_sanit = dplyr::if_else(
          stringr::str_detect(key_sanit, "^NA"),
          NA_character_,
          key_sanit
        )
      )

    weights_ap <-
      full_AP$indicator_code[full_AP$indicator_code == "weights"]

    if (length(weights_ap) == 0) {
      data_scoring$weights <- 1
      weights <- "weights"
    } else if (length(weights_ap) != 0) {
      weights <- weights_ap
    }

    cluster_id <-
      full_AP$indicator_code[full_AP$indicator_code %in% c("cluster_id")]

    if (length(cluster_id) == 0) {
      cluster_id <- NULL
    }

    agg_level <- data_AP$admin_level
    if(!agg_level%in% names(data_scoring) & agg_level %in% names(.data)){
      data_scoring[[agg_level]] <- .data[[agg_level]]
    }


    ### Formating data_scoring
    design_data_scoring <-data_scoring %>%
      dplyr::mutate(!!sym(weights) := as.numeric(!!sym(weights))) %>%
      srvyr::as_survey_design(ids = !!cluster_id,
                              weights = !!weights) %>%
      dplyr::mutate(water_score = as.factor(water_score),
                    sanit_score = as.factor(sanit_score))

    var_to_analyse <- c(
      "water_score",
      "key_water",
      "sanit_score",
      "key_sanit",
      "score_final",
      "key_score"
    )

    score_agg_table <-
      data.frame(indicator = NA,
                 choice = NA,
                 value = NA)

    for (i in 1:length(var_to_analyse)) {
      if (class(design_data_scoring$variables[[var_to_analyse[i]]]) %in% c("factor", "character")) {
        design_data_scoring$variables[[var_to_analyse[i]]] <-
          factor(design_data_scoring$variables[[var_to_analyse[i]]])
        score_agg_table <- design_data_scoring %>%
          dplyr::filter(!is.na(!!dplyr::sym(var_to_analyse[i]))) %>%
          dplyr::group_by(!!dplyr::sym(agg_level),
                          !!dplyr::sym(var_to_analyse[i])) %>%
          dplyr::summarise(value = srvyr::survey_mean(na.rm = TRUE)) %>%
          dplyr::mutate(indicator = var_to_analyse[i]) %>%
          dplyr::rename(choice = as.character(var_to_analyse[i])) %>%
          dplyr::select(!!agg_level, indicator, choice, value) %>%
          dplyr::bind_rows(score_agg_table)
      } else{
        score_agg_table <- design_data_scoring %>%
          dplyr::filter(!is.na(!!dplyr::sym(var_to_analyse[i]))) %>%
          dplyr::group_by(!!dplyr::sym(agg_level)) %>%
          dplyr::summarise(
            !!dplyr::sym(var_to_analyse[i]) := srvyr::survey_mean(!!dplyr::sym(var_to_analyse[i]), na.rm = TRUE)
          ) %>%
          dplyr::mutate(
            indicator = var_to_analyse[i],
            choice = NA,
            value = !!dplyr::sym(var_to_analyse[i])
          ) %>%
          dplyr::select(!!agg_level, indicator, choice, value) %>%
          dplyr::bind_rows(score_agg_table)

      }
    }

    score_agg_table[[agg_level]] <- normalise_string(score_agg_table[[agg_level]])

    score_agg_table$context <- unique(data_AP$context)

    return(score_agg_table)
  }


#' Aggregate variables at the specified administrative unit
#'
#' @param data data.frame containing the data to be aggregated
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
#'     format of a named list (e.g. WSC_AP = this_df)
#'
#' @return data.frame containing the aggregated data to the data_AP
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr separate
#' @import srvyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' agg_admin2 <- aggregate_admin()
#' }

aggregate_admin <-
  function(data,
           data_AP,
           ...) {

    weights_ap <-
      data_AP$indicator_code_source[data_AP$indicator_code == "weights"]

    if (length(weights_ap) == 0) {
      data$weights <- 1
      weights <- "weights"
    } else if (length(weights_ap) != 0) {
      weights <- weights_ap
    }

    sampling_id <-
      data_AP$indicator_code_source[data_AP$indicator_code %in% c("sampling_id", "cluster_id")]

    if (length(sampling_id) == 0) {
      sampling_id <- NULL
    }

    agg_level <- data_AP$admin_level
    context <- data_AP$context

    var_to_analyse <- data_AP %>%
      unnest(cols = c(indicator_code, indicator_code_source)) %>%
      select(indicator_code) %>%
      distinct()
    var_to_analyse <- unique(var_to_analyse$indicator_code)
    var_to_analyse <-
      var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3", "weights", "sampling_id")]

    reduced_data <- data %>%
      dplyr::select(weights, !!sampling_id,data_AP$admin_level, starts_with(var_to_analyse))

    if (nrow(reduced_data) == 1) {
      addVars_agg_table <- reduced_data %>%
        dplyr::select(-weights,-sampling_id) %>%
        dplyr::group_by(!!sym(agg_level)) %>%
        tidyr::pivot_longer(-!!agg_level, names_to = "indicator", values_to = "value") %>%
        dplyr::mutate(choice = NA,
               context = context) %>%
        dplyr::select(!!agg_level, indicator, choice, value, context)

      return(addVars_agg_table)
    }

    design_data <-
      srvyr::as_survey(reduced_data ,
                ids = !!sampling_id,
                weights = !!weights)

    addVars_agg_table <-
      data.frame(
        admin2 = NA,
        indicator = NA,
        choice = NA,
        value = NA
      )

    select_multiple_in_data <-
      butteR::auto_detect_select_multiple(design_data$variables)
    select_multiples_in_var_to_analyse <-
      var_to_analyse[which(var_to_analyse %in% select_multiple_in_data)]

    if (length(select_multiples_in_var_to_analyse) > 0) {
      select_multiples_in_data_with_dot <-
        paste0(select_multiple_in_data, ".")
      select_multiples_in_given_list_with_dot <-
        paste0(select_multiples_in_var_to_analyse, ".")
      vars_selection_helper <-
        paste0("^(",
               paste(select_multiples_in_given_list_with_dot, collapse = "|"),
               ")")
      # vars_selection_helper <- paste0("^(", paste(select_multiples_in_data_with_dot, collapse="|"), ")")
      select_multiple_logical_names <-
        dplyr::select(design_data$variables, dplyr::matches(vars_selection_helper)) %>%
        dplyr::select(-dplyr::ends_with("_other")) %>% colnames()
      var_to_analyse_no_concatenated_select_multiple <-
        var_to_analyse [which(var_to_analyse %in% select_multiple_in_data == FALSE)]
      var_to_analyse <-
        c(
          var_to_analyse_no_concatenated_select_multiple,
          select_multiple_logical_names
        )
    }
    if (length(select_multiples_in_var_to_analyse) == 0) {
      var_to_analyse <- var_to_analyse
    }

    analyse_var <- function(var_analyse) {
      if (class(design_data$variables[[var_analyse]]) %in% c("factor", "character")) {
        design_data$variables[[var_analyse]] <-
          factor(design_data$variables[[var_analyse]])
        addVars_agg_table <- design_data %>%
          dplyr::filter(!is.na(!!dplyr::sym(var_analyse))) %>%
          dplyr::group_by(!!dplyr::sym(agg_level), !!dplyr::sym(var_analyse)) %>%
          dplyr::summarise(value = srvyr::survey_mean(na.rm = TRUE)) %>%
          dplyr::mutate(indicator = var_analyse) %>%
          dplyr::rename(choice = as.character(var_analyse)) %>%
          dplyr::select(!!agg_level, indicator, choice, value) %>%
          dplyr::bind_rows(addVars_agg_table)
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
          dplyr::select(!!agg_level, indicator, choice, value) %>%
          dplyr::bind_rows(addVars_agg_table)
      }
      return(addVars_agg_table)
    }

    analysed_var <- lapply(var_to_analyse, analyse_var) %>%
      dplyr::bind_rows()

    analysed_var <-
      analysed_var[rowSums(is.na(analysed_var)) != ncol(analysed_var),]

    addVars_agg_table <- suppressWarnings(
      analysed_var %>%
        tidyr::separate(
          indicator,
          into = c("indicator", "choice2"),
          sep = "\\."
        ) %>%
        dplyr::mutate(
          context = context,
          choice = case_when(
            !is.na(choice2) ~ as.character(choice2),
            TRUE ~ as.character(choice)
          ),!!dplyr::sym(agg_level) := normalise_string(!!dplyr::sym(agg_level))
        ) %>%
        dplyr::select(!!agg_level, indicator, choice, value) %>%
        dplyr::mutate(context = context) %>%
        dplyr::filter(!is.na(indicator))
    )

    return(addVars_agg_table)
  }
