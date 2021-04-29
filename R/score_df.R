#' Score individual variables
#'
#' @param var character string identifying a variable present in \code{survey_hh_data}
#' @param survey_hh_data a srvyr::as_survey object containing necessary data and
#'     survey information according to srvyr::as_survey definition.
#' @param agg_level character string specifying which column should be used to
#'    aggregate the data. This is is typically an administrative unit (e.g. province,
#'    region, departement, admin2, etc.)
#'
#' @return a data.frame containing the calculated scores
#'
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @importFrom stringr str_detect
#' @import srvyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  score_var(var = "rsci_score", survey_hh_data = hh_data_scored, "admin1")
#' }
#'
score_var <- function(var, survey_hh_data, agg_level) {
  survey_hh_data$variables[[var]] <-
    factor(survey_hh_data$variables[[var]])

  survey_hh_data_calc <- survey_hh_data %>%
    dplyr::filter(!is.na(!!var)) %>%
    dplyr::group_by(!!dplyr::sym(agg_level),!!dplyr::sym(var)) %>%
    dplyr::summarise(value = srvyr::survey_mean(na.rm = TRUE)) %>%
    dplyr::mutate(indicator = !!var) %>%
    dplyr::rename(choice = as.character(var)) %>%
    dplyr::select(!!agg_level, indicator, choice, value)

  return(survey_hh_data_calc)
}


#' Score individual variables
#'
#' @param var character string identifying a variable present in \code{survey_hh_data}
#' @param survey_hh_data a srvyr::as_survey object containing necessary data and
#'     survey information according to srvyr::as_survey definition.
#' @param agg_level character string specifying which column should be used to
#'    aggregate the data. This is is typically an administrative unit (e.g. province,
#'    region, departement, admin2, etc.)
#'
#' @return a data.frame containing the calculated scores
#'
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @importFrom stringr str_detect
#' @import srvyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  score_var(var = "rsci_score", survey_hh_data = hh_data_scored, "admin1")
#' }
#'
score_var <- function(var, survey_hh_data, agg_level) {
  survey_hh_data$variables[[var]] <-
    factor(survey_hh_data$variables[[var]])

  survey_hh_data_calc <- survey_hh_data %>%
    dplyr::filter(!is.na(!!var)) %>%
    dplyr::group_by(!!dplyr::sym(agg_level),!!dplyr::sym(var)) %>%
    dplyr::summarise(value = srvyr::survey_mean(na.rm = TRUE)) %>%
    dplyr::mutate(indicator = !!var) %>%
    dplyr::rename(choice = as.character(var)) %>%
    dplyr::select(!!agg_level, indicator, choice, value)

  return(survey_hh_data_calc)
}


#' Score dataset according to the Analysis Plan (AP) phases.
#'
#' This function scores datasets according to the phases documented in the AP under
#' the columns None/Minimal to Catastrophic.
#'
#' @param data data.frame containing the data to be scored
#' @param data_name character string identifying the name of the data frame used in \code{data}. Should be equivalent to the \code{data_source_name} called in \code{context_AP}.
#' @param data_sheet_name character string with the name of the \code{sheet_name}
#' @param data_type character string with the type of data source in \code{data}. Must be "area" or "hh".
#' @param agg_level character string specifying which column should be used to
#'    aggregate the data. This is is typically an administrative unit (e.g. province,
#'    region, departement, admin2, etc.)
#' @param context character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSCprocessing::context_AP}.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#'
#' @return a data.frame containing the phase for each administrative level taken
#'    into consideration
#'
#' @importFrom magrittr `%>%`
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
#' area_df <- score_df_AP(data = WSCprocessing::bfa_smart_2019_admin1,
#'          data_name = "smart_2019",
#'          data_sheet_name = "cleaned_data_admin1", data_type = "area",
#'          agg_level = "admin1", context = "bfa_2020",
#'          context_AP = WSCprocessing::context_AP,
#'          WSC_AP = WSCprocessing::WSC_AP)
#'
#' hh_df <- score_df_AP(data = WSCprocessing::bfa_msna_2020,
#'          data_type = "hh", data_name = "bfa_msna_2020",
#'          data_sheet_name = "BFA_MSNA_2020_dataset_cleanedWeighted_ADM1",
#'          agg_level = "admin1", context = "bfa_2020",
#'          context_AP = WSCprocessing::context_AP,
#'          WSC_AP = WSCprocessing::WSC_AP)
#'}

score_data_AP <-
  function(data,
           data_AP,
           WSC_AP = WSCprocessing::WSC_AP,
           ...) {
    if (is.null(data)) {
      stop("data must be supplied")
    }
    if (is.null(data_AP$data_type) || !data_AP$data_type %in% c("area", "hh")) {
      stop("data_type must be either 'area' or 'hh'")
    }
    if(is.null(WSC_AP)){
      if(is.null(params$WSC_AP)){
       WSC_AP <- WSCprocessing::WSC_AP
      }else{
      WSC_AP <- params$WSC_AP
      }
    }
    context <- data_AP$context
    agg_level <- data_AP$admin_level

    full_AP <- data_AP %>%
      tidyr::unnest(cols = c(indicator_code_source, indicator_code, choices_name,
                             score_recoding, question_type)) %>%
      dplyr::mutate(
        indicator_code_source = normalise_string(indicator_code_source)
        )%>%
      dplyr::filter(context == !!context) %>%
      dplyr::left_join(WSC_AP, by = "indicator_code") %>%
      dplyr::rename(
        minimal = "None/ minimal",
        stress = "Stressed",
        crisis = "Crisis",
        critical = "Critical",
        catastrophic = "Catastrophic"
      ) %>%
      dplyr::mutate(dplyr::across(c(minimal, stress, crisis, critical, catastrophic), function(col) {
        stringr::str_replace_all(as.character(col), '\\"', "'")
      }))

    ap_scaled <- full_AP %>%
      dplyr::filter(globally_scaled == TRUE & wash_scoring == FALSE)

    scores_AP <- ap_scaled %>%
      dplyr::select(indicator_code,
                    minimal,
                    stress,
                    crisis,
                    critical,
                    catastrophic) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(across(c(minimal, stress, crisis, critical, catastrophic), function(col) {
        stringr::str_replace_all(col, as.character(indicator_code), "value")
      })) %>%
      dplyr::mutate(dplyr::across(c(minimal, stress, crisis, critical, catastrophic), function(col) {
        dplyr::case_when(col == "NULL" ~ NA_character_,
                         col == "FALSE" ~ NA_character_,
                         TRUE ~ col)
      }))

    var_names <- full_AP %>%
      dplyr::select(
        data_sheet_name,
        indicator_code,
        indicator_code_source,
        context
      )

    # names(data) <- normalise_string(names(data))
    #
    # names(data) <-
    #   rename_vec(names(data),
    #       var_names$indicator_code_source,
    #       var_names$indicator_code)

    if (sum(names(data) %in% ap_scaled$indicator_code) == 0) {
      warning(
        paste0(
          ap_scaled$indicator_code,
          " does not have a scale to provide the phase of the indicator."
        )
      )

      result <- tibble(!!sym(agg_level):= NA, indicator = NA, choice = NA, value = NA, context = NA)

      return(result)
    } else{
      if (data_AP$data_type == "area") {
        area_data <- data
        area_indic <-
          ap_scaled$indicator_code[ap_scaled$level == "area"]
        area_data_AP <- area_data %>%
          dplyr::select(dplyr::any_of(area_indic),!!agg_level) %>%
          dplyr::group_by(!!dplyr::sym(agg_level)) %>%
          tidyr::pivot_longer(-!!dplyr::sym(agg_level),
                              names_to = "indicator",
                              values_to = "value")

        data_scored <- area_data_AP %>%
          dplyr::left_join(scores_AP, by = c("indicator" = "indicator_code")) %>%
          dplyr::filter(indicator %in% !!area_indic) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            score_final = dplyr::case_when(
              eval(parse(text = critical)) &
                eval(parse(text = catastrophic)) ~ "4-5",
              eval(parse(text = critical)) &
                eval(parse(text = crisis)) ~ "3-4",
              eval(parse(text = stress)) &
                eval(parse(text = crisis)) ~ "2-3",
              eval(parse(text = stress)) &
                eval(parse(text = minimal)) ~ "1-2",
              eval(parse(text = catastrophic)) ~ "5",
              eval(parse(text = critical)) ~ "4",
              eval(parse(text = crisis)) ~ "3",
              eval(parse(text = stress)) ~ "2",
              eval(parse(text = minimal)) ~ "1",
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::mutate(choice = "Phase",
                        value = score_final,
                        context = !!context) %>%
          dplyr::select(!!agg_level, indicator, choice, value, context)

      }

      if (data_AP$data_type == "hh") {
        hh_data <- data

        weights_ap <-
          full_AP$indicator_code[full_AP$indicator_code == "weights"]

        if (length(weights_ap) == 0) {
          hh_data$weights <- 1
          weights <- "weights"
        } else if (length(weights_ap) != 0) {
          weights <- weights_ap
        }

        cluster_id <-
          full_AP$indicator_code[full_AP$indicator_code %in% c("cluster_id")]

        if (length(cluster_id) == 0) {
          cluster_id <- NULL
        }

        var_to_analyse <-
          unlist(unique(data_AP$indicator_code[!is.na(data_AP$indicator_code)])) %>%
          unique()
        var_to_analyse <-
          var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3", "weights", "sampling_id", "cluster_id")]

        var_to_analyse_df <- hh_data %>%
          select(!!var_to_analyse)
          # map_dfc(var_to_analyse, ~recode_variable(data = hh_data, variable = ., data_AP = data_AP))

        supporting_vars_df <- hh_data %>%
          select(all_of(c(agg_level, cluster_id, weights)))

        hh_data <- bind_cols(supporting_vars_df, var_to_analyse_df)

        hh_indic <-
          unique(ap_scaled$indicator_code[ap_scaled$level == "hh"])

        hh_data_AP <- hh_data %>%
          dplyr::select(dplyr::any_of(hh_indic),
                        !!agg_level,
                        !!cluster_id,
                        !!weights) %>%
          dplyr::mutate(row_id = dplyr::row_number(),
                        across(all_of(hh_indic), as.character ))%>%
          dplyr::group_by(!!dplyr::sym(agg_level), row_id) %>%
          tidyr::pivot_longer(
            -c(
              !!dplyr::sym(agg_level),
              row_id,
              !!cluster_id,
              !!weights
            ),
            names_to = "indicator",
            values_to = "value"
          )

        hh_data_scored <- hh_data_AP %>%
          dplyr::left_join(scores_AP, by = c("indicator" = "indicator_code")) %>%
          dplyr::distinct() %>%
          dplyr::filter(indicator %in% !!hh_indic) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            score_final = dplyr::case_when(
              eval(parse(text = critical)) &
                eval(parse(text = catastrophic)) ~ "4-5",
              eval(parse(text = critical)) &
                eval(parse(text = crisis)) ~ "3-4",
              eval(parse(text = stress)) &
                eval(parse(text = crisis)) ~ "2-3",
              eval(parse(text = stress)) &
                eval(parse(text = minimal)) ~ "1-2",
              eval(parse(text = catastrophic)) ~ "5",
              eval(parse(text = critical)) ~ "4",
              eval(parse(text = crisis)) ~ "3",
              eval(parse(text = stress)) ~ "2",
              eval(parse(text = minimal)) ~ "1",
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::mutate(choice = "Phase",
                        value = score_final,
                        context = !!context) %>%
          dplyr::select(-c(minimal, stress, crisis, critical, catastrophic, value)) %>%
          dplyr::group_by(row_id) %>%
          tidyr::pivot_wider(names_from = indicator, values_from = score_final) %>%
          srvyr::as_survey(ids = !!cluster_id, weights = !!weights)

        addVars_agg_table <-
          dplyr::tibble(
            "{agg_level}" := NA,
            indicator = NA,
            choice = NA,
            value = NA
          )

        hh_indic_avail <-
          hh_indic[hh_indic %in% names(hh_data_scored$variables)]

        addVars_agg_table <-
          lapply(hh_indic_avail,
                 score_var,
                 survey_hh_data = hh_data_scored,
                 agg_level = agg_level) %>%
          dplyr::bind_rows()


        addVars_agg_table <- suppressWarnings(
          addVars_agg_table %>%
            tidyr::separate(
              indicator,
              into = c("indicator", "choice2"),
              sep = "\\."
            ) %>%
            dplyr::mutate(
              context = !!context,
              choice = dplyr::case_when(
                !is.na(choice2) ~ as.character(choice2),
                TRUE ~ as.character(choice)
              )
            ) %>%
            dplyr::select(!!agg_level, indicator, choice, value) %>%
            dplyr::mutate(context = !!context) %>%
            dplyr::filter(!is.na(indicator))
        )

        data_scored <-
          lapply(unique(addVars_agg_table$indicator), function(x) {
            twenty_rule(
              data = addVars_agg_table,
              col_score = "indicator",
              col_label = "choice",
              name_final_score = x,
              col_agg = agg_level,
              col_value = "value"
            )
          }) %>% do.call(rbind, .) %>%
          dplyr::mutate(
            choice = "Phase",
            value = score_final,!!sym(agg_level) := normalise_string(!!sym(agg_level))
          ) %>%
          dplyr::select(!!agg_level, indicator, choice, value, context) %>%
          dplyr::distinct()

      }

      data_scored <-
        data_scored[rowSums(is.na(data_scored)) != ncol(data_scored),]

      data_scored <- data_scored %>%
        dplyr::mutate(!!sym(agg_level) := normalise_string(!!sym(agg_level))) %>%
        dplyr::distinct()
      return(data_scored)
    }
  }
