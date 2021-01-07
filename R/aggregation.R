#' Aggregate scores at specified aggregation level
#'
#' @param data data.frame containing the data to be analysed with the function
#' @param context Character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSC::context_AP```.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSC::WSC_AP```)
#' @param WIS_water data.frame with the scoring reference matrix for the Water
#'    component of the WASH Insecurity Score (WIS)
#' @param WIS_sanitation data.frame with the scoring reference matrix for the
#'    Sanitation component of the WIS
#' @param WIS_final data.frame with the scoring reference matrix for the final
#'    score of the WIS
#' @param agg_level character string specifying which column should be used to
#'    aggregate the data. This is is typically an administrative unit (e.g. province,
#'    region, departement, admin2, etc.)
#'
#' @return data.frame containing the aggregated data according to context_AP and WSC_AP
#' @export
#'
#' @examples
#' agg_score(context = "bfa_2020", context_AP = WSC::context_AP,
#'           WSC_AP = WSC::WSC_AP, data = WSC::bfa_msna_2020)
agg_score <- function(data, context, context_AP, WSC_AP, agg_level = "admin2", WIS_water = WSC::WIS_water,
                      WIS_sanitation = WSC::WIS_sanitation, WIS_final = WSC::WIS_final){

  full_AP <- context_AP%>%
    dplyr::filter(context_AP$context == context)%>%
    dplyr::left_join(WSC_AP, by = "indicator_code")

  data_scoring <- WSC::score_WIS(data = data,context = context, context_AP = context_AP, WSC_AP = WSC_AP,
                                 WIS_water = WIS_water, WIS_sanitation = WIS_sanitation, WIS_final = WIS_final)%>%
    dplyr::mutate(water_score = factor(water_score),
                  sanit_score = factor(sanit_score),
                  score_final = factor(score_final),
                  key_score = dplyr::if_else(stringr::str_detect(key_score, "NA"), NA_character_, key_score),
                  key_water = dplyr::if_else(stringr::str_detect(key_water, "NA"), NA_character_, key_water),
                  key_sanit = dplyr::if_else(stringr::str_detect(key_sanit, "^NA"), NA_character_, key_sanit)
    )

  cluster_id <- full_AP$indicator_code[full_AP$indicator_code=="cluster_id"]
  weights <- full_AP$indicator_code[full_AP$indicator_code=="weights"]
  data_scoring[,weights] <- as.numeric(data_scoring[,weights])

  ### Formating data_scoring
  design_data_scoring <- srvyr::as_survey_design(data_scoring,ids=cluster_id, weights = !!weights)%>%
    dplyr::mutate(water_score = as.factor(water_score),
                  sanit_score = as.factor(sanit_score))

  var_to_analyse <- c("water_score", "key_water",
                      "sanit_score", "key_sanit",
                      "score_final", "key_score")

  score_agg_table <- data.frame(indicator = NA, choice = NA, value = NA)

  for(i in 1:length(var_to_analyse)){
    if(class(design_data_scoring$variables[[var_to_analyse[i]]]) %in% c("factor", "character")){
      design_data_scoring$variables[[var_to_analyse[i]]] <- factor(design_data_scoring$variables[[var_to_analyse[i]]])
      score_agg_table <- design_data_scoring%>%
        dplyr::filter(!is.na(!!dplyr::sym(var_to_analyse[i])))%>%
        dplyr::group_by(!!dplyr::sym(agg_level),!!dplyr::sym(var_to_analyse[i]))%>%
        dplyr::summarise(value= srvyr::survey_mean(na.rm = TRUE))%>%
        dplyr::mutate(indicator = var_to_analyse[i])%>%
        dplyr::rename(choice = as.character(var_to_analyse[i]))%>%
        dplyr::select(indicator, choice, value)%>%
        dplyr::bind_rows(score_agg_table)
    }else{
      score_agg_table <- design_data_scoring%>%
        dplyr::filter(!is.na(!!dplyr::sym(var_to_analyse[i])))%>%
        dplyr::group_by(!!dplyr::sym(agg_level))%>%
        dplyr::summarise(!!dplyr::sym(var_to_analyse[i]):= srvyr::survey_mean(!!dplyr::sym(var_to_analyse[i]), na.rm = TRUE))%>%
        dplyr::mutate(indicator = var_to_analyse[i],
                      choice = NA, value = !!dplyr::sym(var_to_analyse[i]))%>%
        dplyr::select(indicator, choice, value)%>%
        dplyr::bind_rows(score_agg_table)

    }
  }

  score_agg_table$context <- context

  return(score_agg_table)
}

#' Assign higher administrative unit data to a lower one
#'
#' As data is not always available at the required level, it might be needed to
#' assign/input data from a higher administrative unit to a lower one. For instance,
#' if a country has three administrative unit: region, province, and communes, this
#' function can help to bring data available only at the region-level to a
#' province-level data set.
#'
#'
#' @param HiAdmin_df data.frame with the higher administrative unit data
#' @param HiAdmin_df_name character string with the name of the \code{HiAdmin_df} as is in the \code{data_source_name} column of \code{context_AP}.
#' @param HiAdmin_name character string with the column name of the higher administrative
#'     unit. Must be identical to the relevant column name in \code{LoAdmin_df} to
#'     allow cross-reference.
#' @param context character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSC::context_AP}.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSC::WSC_AP```)
#' @param LoAdmin_df data.frame with the lower administrative unit data. Can be just a list of lower administrative levels.
#' @param LoAdmin_name character string with the column name of the higher administrative
#'     unit. Must be identical to the relevant column name in \code{LoAdmin_df} to
#'     allow cross-reference.
#'
#' @return a data.frame with the aggregated data at a lower administrative level
#' @export
#'
#' @examples
#' assign_hiAdmin_loAdmin(HiAdmin_df = WSC::bfa_smart_2019_admin1, HiAdmin_name = "admin1",
#'                        HiAdmin_df_name = "smart_2019_admin1",
#'                        context = "bfa_2020", context_AP = WSC::context_AP,
#'                        WSC_AP = WSC::WSC_AP, LoAdmin_df = WSC::bfa_msna_2020, LoAdmin_name = "admin2")
#'
assign_hiAdmin_loAdmin <- function(HiAdmin_df, HiAdmin_df_name, HiAdmin_name, context, context_AP, WSC_AP, LoAdmin_df, LoAdmin_name){

  full_AP <- context_AP%>%
    dplyr::filter(context_AP$context == !!context)%>%
    dplyr::left_join(WSC_AP, by = "indicator_code")%>%
    dplyr::filter(data_source_name == !!HiAdmin_df_name)

  cluster_id <- HiAdmin_name
  HiAdmin_df$cluster_id <- HiAdmin_df[,HiAdmin_name]

  HiAdmin_df$weights <- 1

  names(HiAdmin_df)<- recode_var(names(HiAdmin_df),"c('x_uuid','X_uuid','_uuid')='uuid'")

  from <- full_AP$indicator
  to <- full_AP$indicator_code

  var_to_analyse <- unique(full_AP$indicator_code)[!is.na(unique(full_AP$indicator_code))]
  var_to_analyse <- var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3","weights", "cluster_id")]

  names(HiAdmin_df)<-r3c(names(HiAdmin_df),from,to)


  addVars_agg_table <- HiAdmin_df%>%
    dplyr::select(any_of(var_to_analyse), !!HiAdmin_name)

  addVars_agg_table_loadm <- LoAdmin_df%>%
    dplyr::left_join(addVars_agg_table, by = HiAdmin_name)%>%
    tidyr::pivot_longer(any_of(var_to_analyse), names_to = "indicator", values_to = "value")%>%
    dplyr::mutate(
      context = !!context,
      choice = NA
    )%>%
    dplyr::select(!!!LoAdmin_name, indicator, choice, value)%>%
    dplyr::mutate(context = !!context)%>%
    dplyr::filter(!is.na(indicator))%>%
    dplyr::distinct()


  return(addVars_agg_table_loadm)

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
#' @export
#'
#' @examples
#' \dontrun{
#'  score_var(var = "rsci_score", survey_hh_data = hh_data_scored, "admin1")
#' }
#'
score_var <- function(var, survey_hh_data, agg_level){
  survey_hh_data$variables[[var]] <- factor(survey_hh_data$variables[[var]])

  survey_hh_data_calc <- survey_hh_data%>%
    dplyr::filter(!is.na(!!var)) %>%
    dplyr::group_by(!!dplyr::sym(agg_level), !!dplyr::sym(var)) %>%
    dplyr::summarise(value= srvyr::survey_mean(na.rm = TRUE))%>%
    dplyr::mutate(indicator = !!var)%>%
    dplyr::rename(choice = as.character(var))%>%
    dplyr::select(!!agg_level, indicator, choice, value)

  return(survey_hh_data_calc)
}


#' Score dataset according to the Analysis Plan (AP) phases.
#'
#' This function scores datasets according to the phases documented in the AP under
#' the columns None/Minimal to Catastrophic.
#'
#' @param data data.frame containing the data to be scored
#' @param data_name character string with the name of the \code{data} as is in the \code{data_source_name} column of \code{context_AP}.
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
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSC::context_AP}.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSC::WSC_AP```)
#'
#' @return a data.frame containing the phase for each administrative level taken into consideration
#' @export
#'
#' @examples
#' area_df <- score_df_AP(data = WSC::bfa_smart_2019_admin1, data_name = "smart_2019_admin1",
#'          data_type = "area",
#'          agg_level = "admin1", context = "bfa_2020", context_AP = WSC::context_AP,
#'          WSC_AP = WSC::WSC_AP)
#'
#' hh_df <- score_df_AP(data = WSC::bfa_msna_2020, data_name = "msna_2020",
#'          data_type = "hh",
#'          agg_level = "admin1", context = "bfa_2020", context_AP = WSC::context_AP,
#'          WSC_AP = WSC::WSC_AP)
#'

score_df_AP <- function(data = NULL, data_name = NULL, data_type = NULL, agg_level, context, context_AP, WSC_AP){

  if(is.null(data)){
    stop("data must be supplied")
  }
  if(is.null(data_type) || !data_type %in% c("area", "hh")){
    stop("data_type must be either 'area' or 'hh'")
  }

  full_AP <- context_AP%>%
    dplyr::filter(context_AP$context == !!context)%>%
    dplyr::left_join(WSC_AP, by = "indicator_code")%>%
    dplyr::rename(minimal = "None/ minimal", stress = "Stressed", crisis = "Crisis", critical = "Critical", catastrophic = "Catastrophic")%>%
    dplyr::mutate(dplyr::across(c(minimal,stress, crisis, critical, catastrophic), function(col){ stringr::str_replace_all(as.character(col), '\\"', "'")}))

  ap_scaled <- full_AP%>%
    dplyr::filter(globally_scaled == TRUE & wash_scoring == FALSE)

  scores_AP <- ap_scaled%>%
    dplyr::select(indicator_code, minimal, stress, crisis, critical, catastrophic)%>%
    dplyr::rowwise()%>%
    dplyr::mutate(across(c(minimal,stress, crisis, critical, catastrophic), function(col){ stringr::str_replace_all(col, as.character(indicator_code), "value")}))%>%
    dplyr::mutate(dplyr::across(c(minimal,stress, crisis, critical, catastrophic), function(col){dplyr::case_when(
      col == "NULL" ~ NA_character_,
      col == "FALSE" ~ NA_character_,
      TRUE ~ col
    )}))

  if(data_type == "area"){

    area_data <- data
    area_indic <- ap_scaled$indicator_code[ap_scaled$level == "area"]
    area_data_AP <- area_data%>%
      dplyr::select(dplyr::any_of(area_indic), !!agg_level)%>%
      dplyr::group_by(!!dplyr::sym(agg_level))%>%
      tidyr::pivot_longer(-!!dplyr::sym(agg_level), names_to = "indicator", values_to = "value")

    area_data_scored <- area_data_AP%>%
      dplyr::left_join(scores_AP, by = c("indicator" = "indicator_code"))%>%
      filter(indicator %in% !!area_indic)%>%
      dplyr::rowwise()%>%
      dplyr::mutate(
        score_final = dplyr::case_when(
          eval(parse(text = critical)) & eval(parse(text = catastrophic)) ~ "4-5",
          eval(parse(text = critical)) & eval(parse(text = crisis)) ~ "3-4",
          eval(parse(text = stress)) & eval(parse(text = crisis)) ~ "2-3",
          eval(parse(text = stress)) & eval(parse(text = minimal)) ~ "1-2",
          eval(parse(text = catastrophic)) ~ "5",
          eval(parse(text = critical)) ~ "4",
          eval(parse(text = crisis)) ~ "3",
          eval(parse(text = stress)) ~ "2",
          eval(parse(text = minimal)) ~ "1",
          TRUE ~ NA_character_
        )
      )%>%
      dplyr::mutate(choice = "Phase", value = score_final, context = "bfa_2020")%>%
      dplyr::select(!!agg_level, indicator, choice, value, context)

    return(area_data_scored)

  }

  if(data_type == "hh"){

    hh_data <- data
    hh_data_name <- data_name

    hh_cluster_id <- full_AP$indicator[full_AP$indicator_code=="cluster_id" & full_AP$data_source_name == hh_data_name]
    hh_weights <- full_AP$indicator[full_AP$indicator_code=="weights" & full_AP$data_source_name == hh_data_name]


    hh_indic <- unique(ap_scaled$indicator_code[ap_scaled$level == "hh"])
    hh_data_AP <- hh_data%>%
      dplyr::select(dplyr::any_of(hh_indic), !!agg_level, !!hh_cluster_id, !!hh_weights)%>%
      dplyr::mutate(row_id = dplyr::row_number())%>%
      group_by(!!dplyr::sym(agg_level), row_id) %>%
      tidyr::pivot_longer(-c(!!dplyr::sym(agg_level), row_id, !!hh_cluster_id, !!hh_weights),
                          names_to = "indicator", values_to = "value")

    from <- full_AP$indicator
    to <- full_AP$indicator_code
    names(hh_data_AP)<-r3c(names(hh_data_AP),from,to)


    hh_data_scored <- hh_data_AP%>%
      dplyr::left_join(scores_AP, by = c("indicator" = "indicator_code"))%>%
      dplyr::filter(indicator %in% !!hh_indic) %>%
      dplyr::rowwise()%>%
      dplyr::mutate(
        score_final = dplyr::case_when(
          eval(parse(text = critical)) & eval(parse(text = catastrophic)) ~ "4-5",
          eval(parse(text = critical)) & eval(parse(text = crisis)) ~ "3-4",
          eval(parse(text = stress)) & eval(parse(text = crisis)) ~ "2-3",
          eval(parse(text = stress)) & eval(parse(text = minimal)) ~ "1-2",
          eval(parse(text = catastrophic)) ~ "5",
          eval(parse(text = critical)) ~ "4",
          eval(parse(text = crisis)) ~ "3",
          eval(parse(text = stress)) ~ "2",
          eval(parse(text = minimal)) ~ "1",
          TRUE ~ NA_character_
        )
      )%>%
      dplyr::mutate(choice = "Phase", value = score_final, context = "bfa_2020")%>%
      dplyr::select(-c(minimal,stress, crisis, critical, catastrophic,value))%>%
      dplyr::group_by(row_id)%>%
      tidyr::pivot_wider(names_from = indicator, values_from = score_final)%>%
      srvyr::as_survey(ids = cluster_id, weights = weights)

    addVars_agg_table <- dplyr::tibble("{agg_level}" := NA, indicator = NA, choice = NA, value = NA)

    hh_indic_avail <- hh_indic[hh_indic %in% names(hh_data_scored$variables)]

    addVars_agg_table <- lapply(hh_indic_avail, score_var, survey_hh_data = hh_data_scored, agg_level = agg_level)%>%
      dplyr::bind_rows()


    addVars_agg_table <- suppressWarnings(addVars_agg_table%>%
      tidyr::separate(indicator, into = c("indicator", "choice2"), sep = "\\.")%>%
      dplyr::mutate(context = context,
             choice = dplyr::case_when(!is.na(choice2)~ as.character(choice2),
                                TRUE ~ as.character(choice))
      )%>%
      dplyr::select(!!agg_level, indicator, choice, value)%>%
      dplyr::mutate(context = context)%>%
      dplyr::filter(!is.na(indicator))
    )

    addVars_agg_table_phases <- lapply(unique(addVars_agg_table$indicator), function(x){
      WSC::twenty_rule(data = addVars_agg_table, col_score = "indicator", col_label = "choice",
                  name_final_score = x, col_agg = agg_level, col_value = "value")
    })%>%do.call(rbind,.)%>%
      mutate(choice= "Phase", value = score_final)%>%
      select(!!agg_level, indicator, choice, value, context)

    return(addVars_agg_table)
  }
}

