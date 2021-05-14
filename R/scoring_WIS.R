#' Score WASH Insecurity Score (WIS)
#'
#' This function takes a dataset containing data to calculate the WASH
#' Insecurity Score (WIS) and calculates it according to the procedures outlined
#' in the WSC [Implementation handbook](https://docs.google.com/document/d/1ikSd_3KMOyhJ8pTr5BLXlLZ92y6h5ZpjEeyPxFilxN8/edit#).
#'
#' The WASH insecurity score (WIS) rests on the recoding of variable so that
#' they match the values provided in columns \code{sufficiency_of_water} and
#' \code{water_source_dist} in the [WIS_water](https://docs.google.com/spreadsheets/d/1UCr-G9gD6YZmiOHDoP95qiMkEqi9jMG3lfzzv7WCFnM/edit#gid=0)
#' matrix, and the columns \code{type_of_sanitation_facility},
#' \code{sanitation_facility_sharing}, and \code{access_to_soap} in the
#' [WIS_santiation](https://docs.google.com/spreadsheets/d/1UCr-G9gD6YZmiOHDoP95qiMkEqi9jMG3lfzzv7WCFnM/edit#gid=953151630)
#' matrix. The intersection of the two matrices is then calculated with
#' [WIS_final](https://docs.google.com/spreadsheets/d/1UCr-G9gD6YZmiOHDoP95qiMkEqi9jMG3lfzzv7WCFnM/edit#gid=120011495)
#'
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be
#'    found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#' @param WIS_water data.frame with the scoring reference matrix for the Water
#'    component of the WASH Insecurity Score (WIS)
#' @param WIS_sanitation data.frame with the scoring reference matrix for the
#'    Sanitation component of the WIS
#' @param WIS_final data.frame with the scoring reference matrix for the final
#'    score of the WIS
#'
#' @return a dataframe containing the results of the scoring for each household
#'    in the dataset. NAs are introduced if variables cannot be found or score
#'    calculated.
#' @export
#' @inherit analyse_data
#' @family score functions
#'
#' @return a data.frame containing the calculated scores
#'
#' @examples
#' \dontrun{
#'score_WIS(data = WSCprocessing::bfa_msna_2020, context_AP =
#'WSCprocessing::context_AP, context = "bfa_2020", WSC_AP =
#'WSCprocessing::WSC_AP, WIS_water = WSCprocessing::WIS_water, WIS_sanitation =
#'WSCprocessing::WIS_sanitation, WIS_final = WSCprocessing::WIS_final) }

score_WIS <-
  function(.data,
           data_AP,
           WSC_AP = WSCprocessing::WSC_AP,
           WIS_water = WSCprocessing::WIS_water,
           WIS_sanitation = WSCprocessing::WIS_sanitation,
           WIS_final = WSCprocessing::WIS_final) {
    if (is.null(WSC_AP)) {
      if (is.null(params$WSC_AP)) {
        WSC_AP <- WSCprocessing::WSC_AP
      } else{
        WSC_AP <- params$WSC_AP
      }
    }

    full_AP <- data_AP %>%
      tidyr::unnest(cols = where(is.list)) %>%
      dplyr::left_join(WSC_AP, by = "indicator_code") %>%
      dplyr::mutate(indicator_code_source = normalise_string(indicator_code_source)) %>%
      distinct()

    WIS_AP <- full_AP %>%
      dplyr::filter(
        wash_scoring == TRUE |
          indicator_code %in% c("weights", "cluster_id", "admin1", "admin2", "admin3")
      )

    data <- .data %>%
      lapply(rec_missing) %>%
      dplyr::bind_cols() %>%
      as.data.frame()

    # harmonize uuid columns
    names(data) <-
      recode_var(names(data),
                 "c('x_uuid','X_uuid','_uuid','@_uuid')='uuid'")

    # select the column needed
    datascore <-
      data[, names(data) %in% c("uuid", unique(WIS_AP$"indicator_code"))]

    if (sum(grepl("select_multiple", WIS_AP$question_type[WIS_AP$indicator_code == "sufficiency_of_water"])) > 0) {
      datascore[["sufficiency_of_water"]] <-
        datascore[["sufficiency_of_water"]] %>%
        stringr::str_split(" ") %>%
        lapply(function(y) {
          if (all(is.na(y))) {
            var <- NA_character_
          } else if (any(y == "no_drinking")) {
            var <- "no_drinking"
          } else if (any(y == "drinking") &
                     any(y == "cooking") &
                     any(y == "pers_hyg") &
                     any(y == "other_dom")) {
            var <- "drinking_cooking_pers_hyg_other_dom"
          } else if (any(y == "drinking") &
                     any(y == "cooking") &
                     any(y == "pers_hyg") &
                     !any(y == "other_dom")) {
            var <- "drinking_cooking_pers_hyg"
          } else if (any(y == "drinking") &
                     (any(y == "cooking") |
                      any(y == "pers_hyg"))) {
            var <- "drinking_cooking_OR_pers_hyg"
          } else if (any(y == "drinking") &
                     !(any(y == "cooking") | any(y == "pers_hyg"))) {
            var <- "drinking"
          } else{
            var <- "NA"
          }
          return(var)
        }) %>% unlist
    }

    # concatenate water source and distance
    datascore$water_source_dist <- ifelse(
      datascore$water_source == "improved",
      paste0(
        datascore$water_source,
        "_",
        datascore$distance_to_water_source
      ),
      datascore$water_source
    )

    # create a key for a lookup
    datascore$key_water <-
      paste0(datascore$sufficiency_of_water,
             "-/-",
             datascore$water_source_dist)
    datascore$key_sanit <-
      paste0(
        datascore$type_of_sanitation_facility,
        "-/-",
        datascore$sanitation_facility_sharing,
        "-/-",
        datascore$access_to_soap
      )
    # # recode water scores from the excel datascore table
    WIS_water_recoding <- select(WIS_water, key_water, score_water)

    datascore$water_score <- ifelse(is.na(datascore$key_water),
                                    NA_integer_,
                                    WIS_water$score_water[match(datascore$key_water, WIS_water$key_water)])

    # recode sanit scores from the excel datascore table
    datascore <- datascore %>%
      mutate(key_sanit = case_when(
        str_detect(key_sanit, "-/-NA$") ~ NA_character_,
        TRUE ~ key_sanit
      ))

    datascore$sanit_score <- ifelse(is.na(datascore$key_sanit),
                                    NA_integer_,
                                    WIS_sanitation$score_sanit[match(datascore$key_sanit, WIS_sanitation$key_sanit)])



    # recode final scores from the excel datascore table
    datascore$key_score <-
      paste0(datascore$water_score, "-/-", datascore$sanit_score)
    datascore$score <- ifelse(is.na(datascore$key_score),
                              NA_integer_,
                              WIS_final$score[match(datascore$key_score, WIS_final$key_score)])

    datascore$score_finale <- as.numeric(datascore$score)

    return(datascore)
  }

#' @title Function to determine severity in a specific area based on the 20 percent rule.
#'
#' @details The 20 percent rule states that "an area is classified as the most severe phase that
#'     affects at least 20 percent of the population", based on the percentage of population
#'     in each of the five severity phases.
#'
#'     The 20 percent rule is based on the \href{https://en.wikipedia.org/wiki/Pareto_principle}{Pareto principale}
#'
#' @param data data.frame containing the data to be analysed.
#' @param col_score character string specifying the name the column with scores
#' @param col_label character string specifying the name the column with the
#'     labels to be applied to the choices
#' @param name_final_score character string specifying the name score variable to
#'     be filtered from col_score. The values of col_score for name_final_score
#'     must be convertible to integers between 1 and 5.
#' @param col_agg character string specifying the name the column
#' @param col_value character string specifying which column should be used to
#'     aggregate the data. This is is typically an administrative unit (e.g. province,
#'     region, departement, admin2, etc.)
#' @family score functions
#'
#' @return a dataframe with the results of the 20 percent rule.
#'
#' @importFrom magrittr `%>%`
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' WSI_score_admin2 <- agg_score(data = WSCprocessing::bfa_msna_2020, context =
#' "bfa_2020", context_AP = WSCprocessing::context_AP, data_name =
#' "bfa_msna_2020", WSC_AP = WSCprocessing::WSC_AP, agg_level = "admin2",
#' WIS_water = WSCprocessing::WIS_water, WIS_sanitation =
#' WSCprocessing::WIS_sanitation, WIS_final = WSCprocessing::WIS_final)
#'
#' twenty_rule(data = WSI_score_admin2, col_score = "indicator",
#'             col_label = "choice", name_final_score = "score_final",
#'             col_agg = "admin2", col_value = "value")
#'}

twenty_rule <-
  function(data,
           col_score,
           col_label,
           name_final_score,
           col_agg,
           col_value) {
    scores_col <-
      c("score_1" , "score_2", "score_3", "score_4", "score_5")

    df <- data %>%
      dplyr::filter(!!dplyr::sym(col_score) == !!name_final_score) %>%
      dplyr::group_by(!!dplyr::sym(col_agg)) %>%
      tidyr::pivot_wider(
        names_from = !!dplyr::sym(col_label),
        values_from = !!dplyr::sym(col_value),
        names_prefix = "score_",
        values_fn = sum
      )

    missing_scores <- setdiff(scores_col, names(df))
    df[missing_scores] <- NA_integer_

    df %>%
      dplyr::mutate(score_final = as.factor(
        dplyr::case_when(
          score_5 >= 0.2 ~ "5",
          sum(score_5, score_4, na.rm = T) >= 0.2 ~ "4",
          sum(score_5, score_4, score_3, na.rm = T) >= 0.2 ~ "3",
          sum(score_5, score_4, score_3, score_2, na.rm = T) >= 0.2 ~ "2",
          sum(score_5, score_4, score_3, score_2, score_1, na.rm = T) >= 0.2 ~ "1",
          TRUE ~ NA_character_
        )
      )) %>%
      dplyr::relocate(c(score_1, score_2, score_3, score_4, score_5, score_final),
                      .after = last_col())
  }
