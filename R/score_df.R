#' Score individual variables
#'
#' Takes an individual variable and scores it against the scales outlined in the
#' \code{WSC_AP} in columns "None/ minimal",	"Stressed", "Crisis",	"Critical",
#' and "Catastrophic".
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
#' @family score functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  score_variable(var = "rsci_score", survey_hh_data = hh_data_scored,
#'  agg_level = "admin1")
#' }
#'
score_variable <- function(var, survey_hh_data, agg_level) {
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
#' This function scores datasets according to the phases documented in the
#' \code{WSC_AP}  under the columns None/Minimal to Catastrophic.
#'
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
#' @inherit analyse_data
#' @family score functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'}

score_source <-
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

    if (sum(names(data) %in% ap_scaled$indicator_code) == 0) {

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

        if(length(hh_indic) == 0){
          stop(paste0(data_AP$data_source_name, " ", data_AP$data_sheet_name,
                      ": There is no indicator that can be analysed at household-level.\n
                      Are you sure you context_AP states the right type of data level?")
          )
        }

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
                 score_variable,
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
