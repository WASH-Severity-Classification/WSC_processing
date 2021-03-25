#' Aggregate scores at specified aggregation level
#'
#' @param data data.frame containing the data to be analysed with the function
#' @param data_name character string identifying the name of the data frame used in \code{data}. Should be equivalent to the \code{data_source_name} called in \code{context_AP}.
#' @param context Character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
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
#' agg_score(context = "bfa_2020", context_AP = WSCprocessing::context_AP,
#'           data_name = "bfa_msna_2020",
#'           WSC_AP = WSCprocessing::WSC_AP, data = WSCprocessing::bfa_msna_2020, agg_level = "admin2")
agg_score <- function(data, context, context_AP, WSC_AP, agg_level = "admin2",
                      data_name,
                      WIS_water = WSCprocessing::WIS_water,
                      WIS_sanitation = WSCprocessing::WIS_sanitation,
                      WIS_final = WSCprocessing::WIS_final){

  agg_AP <- context_AP %>%
    dplyr::filter(context == !!context & data_source_name == !!data_name) %>%
    dplyr::mutate(
      indicator_code_source = normalise_string(indicator_code_source)
    )


  full_AP <- agg_AP%>%
    dplyr::left_join(WSC_AP, by = "indicator_code")

  data_scoring <- score_WIS(data = data, context = context, context_AP = agg_AP, WSC_AP = WSC_AP,
                                 WIS_water = WIS_water, WIS_sanitation = WIS_sanitation, WIS_final = WIS_final)%>%
    as_tibble() %>%
    dplyr::mutate(water_score = factor(water_score),
                  sanit_score = factor(sanit_score),
                  score_final = factor(score_final),
                  key_score = dplyr::if_else(stringr::str_detect(key_score, "NA"), NA_character_, key_score),
                  key_water = dplyr::if_else(stringr::str_detect(key_water, "NA"), NA_character_, key_water),
                  key_sanit = dplyr::if_else(stringr::str_detect(key_sanit, "^NA"), NA_character_, key_sanit)
    )

  weights_ap <- full_AP$indicator_code_source[full_AP$indicator_code=="weights"]

  if(length(weights_ap) == 0){
    data_scoring$weights <- 1
    weights <- "weights"
  }else if(length(weights_ap) != 0){
    weights <- weights_ap
  }

  cluster_id <- full_AP$indicator_code_source[full_AP$indicator_code %in% c("sampling_id", "cluster_id")]

  if(length(cluster_id) == 0){
    cluster_id <- NULL
  }

  ### Formating data_scoring
  design_data_scoring <- srvyr::as_survey_design(data_scoring, ids= !!cluster_id, weights = !!weights)%>%
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
        dplyr::select(!!agg_level, indicator, choice, value)%>%
        dplyr::bind_rows(score_agg_table)
    }else{
      score_agg_table <- design_data_scoring%>%
        dplyr::filter(!is.na(!!dplyr::sym(var_to_analyse[i])))%>%
        dplyr::group_by(!!dplyr::sym(agg_level))%>%
        dplyr::summarise(!!dplyr::sym(var_to_analyse[i]):= srvyr::survey_mean(!!dplyr::sym(var_to_analyse[i]), na.rm = TRUE))%>%
        dplyr::mutate(indicator = var_to_analyse[i],
                      choice = NA, value = !!dplyr::sym(var_to_analyse[i]))%>%
        dplyr::select(!!agg_level, indicator, choice, value)%>%
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
#' @param HiAdmin_df_sheet_name character string with the name of the \code{sheet_name}
#' @param HiAdmin_name character string with the column name of the higher administrative
#'     unit. Must be identical to the relevant column name in \code{LoAdmin_df} to
#'     allow cross-reference.
#' @param context character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSCprocessing::context_AP}.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#' @param LoAdmin_df data.frame with the lower administrative unit data. Can be just a list of lower administrative levels.
#' @param LoAdmin_name character string with the column name of the higher administrative
#'     unit. Must be identical to the relevant column name in \code{LoAdmin_df} to
#'     allow cross-reference.
#'
#' @return a data.frame with the aggregated data at a lower administrative level
#' @export
#'
#' @examples
#' assign_hiAdmin2loAdmin(HiAdmin_df = WSCprocessing::bfa_smart_2019_admin1, HiAdmin_name = "admin1",
#'                        HiAdmin_df_name = "smart_2019", HiAdmin_df_sheet_name = "cleaned_data_admin1",
#'                        context = "bfa_2020", context_AP = WSCprocessing::context_AP,
#'                        WSC_AP = WSCprocessing::WSC_AP, LoAdmin_df = WSCprocessing::bfa_msna_2020, LoAdmin_name = "admin2")
#'

assign_hiAdmin2loAdmin <- function(HiAdmin_df, HiAdmin_df_name,HiAdmin_df_sheet_name, HiAdmin_name, context, context_AP, WSC_AP, LoAdmin_df, LoAdmin_name){

  full_AP <- context_AP%>%
    dplyr::mutate(
      unique_data_source_name = dplyr::case_when(is.na(data_sheet_name) == FALSE ~ paste(data_source_name,data_sheet_name, sep = "_"), TRUE ~ data_source_name),
      indicator_code_source = normalise_string(indicator_code_source)
    )%>%
    dplyr::filter(context == !!context)%>%
    dplyr::left_join(WSC_AP, by = "indicator_code")%>%
    dplyr::filter(unique_data_source_name == paste(!!HiAdmin_df_name, !!HiAdmin_df_sheet_name, sep = "_"))

  cluster_id <- HiAdmin_name
  HiAdmin_df$cluster_id <- HiAdmin_df[,HiAdmin_name]

  HiAdmin_df$weights <- 1

  names(HiAdmin_df)<- recode_var(names(HiAdmin_df),"c('x_uuid','X_uuid','_uuid')='uuid'")

  from <- full_AP$indicator_code_source
  to <- full_AP$indicator_code

  var_to_analyse <- unique(full_AP$indicator_code)[!is.na(unique(full_AP$indicator_code))]
  var_to_analyse <- var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3","weights", "cluster_id")]

  names(HiAdmin_df)<-r3c(names(HiAdmin_df),from,to)

  HiAdmin_df$diarrhea_rate <- c(1:nrow(HiAdmin_df))

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
#' @export
#'
#' @examples
#'
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
#'

score_df_AP <- function(data = NULL, data_name = NULL, data_sheet_name = NULL, data_type = NULL, agg_level, context, context_AP, WSC_AP){

  if(is.null(data)){
    stop("data must be supplied")
  }
  if(is.null(data_type) || !data_type %in% c("area", "hh")){
    stop("data_type must be either 'area' or 'hh'")
  }

  data_name_unique <- dplyr::case_when(is.na(data_sheet_name) == FALSE ~ paste(data_name, data_sheet_name, sep = "_"), TRUE ~ data_name)

  full_AP <- context_AP%>%
    dplyr::mutate(
      indicator_code_source = normalise_string(indicator_code_source),
      unique_data_source_name = dplyr::case_when(is.na(data_sheet_name) == FALSE ~ paste(data_source_name,data_sheet_name, sep = "_"), TRUE ~ data_source_name))%>%
    dplyr::filter(context == !!context)%>%
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

  var_names <- full_AP %>%
    select(data_source_name, data_sheet_name, indicator_code, indicator_code_source, context) %>%
      filter(data_source_name == !!data_name, data_sheet_name == !!data_sheet_name, context == !!context)

  names(data) <- normalise_string(names(data))

  names(data) <- r3c(names(data), var_names$indicator_code_source, var_names$indicator_code)

  if(data_type == "area"){

    area_data <- data
    area_indic <- ap_scaled$indicator_code[ap_scaled$level == "area"]
    area_data_AP <- area_data%>%
      dplyr::select(dplyr::any_of(area_indic), !!agg_level)%>%
      dplyr::group_by(!!dplyr::sym(agg_level))%>%
      tidyr::pivot_longer(-!!dplyr::sym(agg_level), names_to = "indicator", values_to = "value")

    data_scored <- area_data_AP%>%
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

  }

  if(data_type == "hh"){

    hh_data <- data
    hh_data_name <- data_name_unique

    hh_cluster_id <- full_AP$indicator_code_source[full_AP$indicator_code=="cluster_id" & full_AP$unique_data_source_name == hh_data_name]
    hh_weights <- full_AP$indicator_code_source[full_AP$indicator_code=="weights" & full_AP$unique_data_source_name == hh_data_name]


    hh_indic <- unique(ap_scaled$indicator_code[ap_scaled$level == "hh"])
    hh_data_AP <- hh_data%>%
      dplyr::select(dplyr::any_of(hh_indic), !!agg_level, !!hh_cluster_id, !!hh_weights)%>%
      dplyr::mutate(row_id = dplyr::row_number())%>%
      group_by(!!dplyr::sym(agg_level), row_id) %>%
      tidyr::pivot_longer(-c(!!dplyr::sym(agg_level), row_id, !!hh_cluster_id, !!hh_weights),
                          names_to = "indicator", values_to = "value")

    from <- full_AP$indicator_code_source
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
      dplyr::mutate(context = !!context,
             choice = dplyr::case_when(!is.na(choice2)~ as.character(choice2),
                                TRUE ~ as.character(choice))
      )%>%
      dplyr::select(!!agg_level, indicator, choice, value)%>%
      dplyr::mutate(context = !!context)%>%
      dplyr::filter(!is.na(indicator))
    )

    data_scored <- lapply(unique(addVars_agg_table$indicator), function(x){
      WSCprocessing::twenty_rule(data = addVars_agg_table, col_score = "indicator", col_label = "choice",
                  name_final_score = x, col_agg = agg_level, col_value = "value")
    })%>%do.call(rbind,.)%>%
      mutate(choice= "Phase", value = score_final,
             !!sym(agg_level) := normalise_string(!!sym(agg_level)))%>%
      select(!!agg_level, indicator, choice, value, context) %>%
      distinct()

  }

  data_scored <- data_scored[rowSums(is.na(data_scored)) != ncol(data_scored), ]

  data_scored <- data_scored %>%
    mutate(
      !!sym(agg_level) := normalise_string(!!sym(agg_level))
    ) %>%
    distinct()

}

#' Aggregate variables at the specified administrative unit
#'
#' @param data data.frame containing the data to be aggregated
#' @param context character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSCprocessing::context_AP}.
#' @param agg_level character string specifying which column should be used to
#'    aggregate the data. This is is typically an administrative unit (e.g. province,
#'    region, departement, admin2, etc.)
#' @param data_name character string identifying the name of the data frame used
#'     in \code{data}. Should be equivalent to the \code{data_source_name} called in \code{context_AP}.
#' @param data_sheet_name character string with the name of the \code{sheet_name}
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#' @param weights string containing the column name where weights are stored.
#'
#' @return data.frame containing the aggregated data according to context_AP and WSC_AP
#' @export
#'
#' @examples
#'
#' agg_admin2 <- admin_agg(data = WSCprocessing::bfa_msna_2020, context ="bfa_2020",
#' context_AP = WSCprocessing::context_AP,  agg_level = "admin2", data_name = "bfa_msna_2020",
#' weights = "weights_sampling",data_sheet_name = "BFA_MSNA_2020_dataset_cleanedWeighted_ADM1",
#' WSC_AP = WSCprocessing::WSC_AP)
#'

admin_agg <- function(data, context, context_AP, agg_level = NULL, data_name,
                      WSC_AP , weights = NULL,data_sheet_name = NULL){

  if(is.null(data_sheet_name)){
    full_AP <- context_AP%>%
      filter(context == !!context)%>%
      left_join(WSC_AP, by = "indicator_code")%>%
      filter(data_source_name == data_name, is.na(data_sheet_name),
             !is.na(indicator_code_source))
  }else{
    full_AP <- context_AP%>%
      filter(context == !!context)%>%
      left_join(WSC_AP, by = "indicator_code")%>%
      filter(data_source_name == data_name, data_sheet_name == !!data_sheet_name,
             !is.na(indicator_code_source))
  }

  names(data)<-car::recode(names(data),"c('x_uuid','X_uuid','_uuid')='uuid'")

  weights_ap <- full_AP$indicator_code_source[full_AP$indicator_code=="weights"]

  if(length(weights_ap) == 0){
    data$weights <- 1
    weights <- "weights"
  }else if(length(weights_ap) != 0){
    weights <- weights_ap
  }

  sampling_id <- full_AP$indicator_code_source[full_AP$indicator_code %in% c("sampling_id", "cluster_id")]

  if(length(sampling_id) == 0){
    sampling_id <- NULL
  }

  var_to_analyse <- unique(full_AP$indicator_code[!is.na(full_AP$indicator_code)])
  var_to_analyse <- var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3","weights", "sampling_id")]

  var_to_analyse_df <- lapply(var_to_analyse, recode_variable, data = data,
                              context_AP = context_AP, data_source_name= data_name) %>%
    bind_cols()

  from <- full_AP$indicator_code_source
  to <- full_AP$indicator_code

  reduced_data <- data%>%
    select(weights, sampling_id, !!agg_level) %>%
    bind_cols(var_to_analyse_df)

  names(data)<-r3c(names(data),from,to)

  if(nrow(reduced_data) == 1){
    addVars_agg_table <- reduced_data %>%
      mutate(!!sym(agg_level) := !!agg_level,
             indicator = !!var_to_analyse,
             choice = NA,
             value = !!sym(var_to_analyse),
             context = context) %>%
      select(!!agg_level, indicator, choice, value, context)

    return(addVars_agg_table)
  }

  design_data <- as_survey(reduced_data ,ids= !!sampling_id, weights = !!weights)

  addVars_agg_table <- data.frame(admin2=NA, indicator = NA, choice = NA, value = NA)

  select_multiple_in_data<-auto_detect_select_multiple(design_data$variables)
  select_multiples_in_var_to_analyse<-var_to_analyse[which(var_to_analyse%in%select_multiple_in_data)]

  if(length(select_multiples_in_var_to_analyse)>0){
    select_multiples_in_data_with_dot<-paste0(select_multiple_in_data,".")
    select_multiples_in_given_list_with_dot<-paste0(select_multiples_in_var_to_analyse, ".")
    vars_selection_helper <- paste0("^(", paste(select_multiples_in_given_list_with_dot, collapse="|"), ")")
    # vars_selection_helper <- paste0("^(", paste(select_multiples_in_data_with_dot, collapse="|"), ")")
    select_multiple_logical_names<-select(design_data$variables, matches(vars_selection_helper)) %>%
      select(-ends_with("_other")) %>% colnames()
    var_to_analyse_no_concatenated_select_multiple<-var_to_analyse [which(var_to_analyse%in%select_multiple_in_data==FALSE)]
    var_to_analyse<-c(var_to_analyse_no_concatenated_select_multiple,select_multiple_logical_names)
  }
  if(length(select_multiples_in_var_to_analyse)==0){
    var_to_analyse<-var_to_analyse
  }

  analyse_var <- function(var_analyse){
    if(class(design_data$variables[[var_analyse]]) %in% c("factor", "character")){
      design_data$variables[[var_analyse]] <- factor(design_data$variables[[var_analyse]])
      addVars_agg_table <- design_data%>%
        filter(!is.na(!!sym(var_analyse)))%>%
        group_by(!!sym(agg_level),!!sym(var_analyse))%>%
        summarise(value= survey_mean(na.rm = TRUE))%>%
        mutate(indicator = var_analyse)%>%
        rename(choice = as.character(var_analyse))%>%
        select(!!agg_level, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
    }else{
      addVars_agg_table <- design_data%>%
        filter(!is.na(!!sym(var_analyse)))%>%
        group_by(!!sym(agg_level))%>%
        summarise(!!sym(var_analyse):= survey_mean(!!sym(var_analyse), na.rm = TRUE))%>%
        mutate(indicator = var_analyse,
               choice = NA, value = !!sym(var_analyse))%>%
        select(!!agg_level, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
    }
    return(addVars_agg_table)
  }

  analysed_var <- lapply(var_to_analyse, analyse_var) %>%
    bind_rows()

  analysed_var <- analysed_var[rowSums(is.na(analysed_var)) != ncol(analysed_var), ]

  addVars_agg_table <- suppressWarnings(analysed_var%>%
    separate(indicator, into = c("indicator", "choice2"), sep = "\\.")%>%
    mutate(context = context,
           choice = case_when(!is.na(choice2)~ as.character(choice2),
                              TRUE ~ as.character(choice)),
           !!sym(agg_level) := normalise_string(!!sym(agg_level))
    )%>%
    select(!!agg_level, indicator, choice, value)%>%
    mutate(context = context)%>%
    filter(!is.na(indicator)))

  return(addVars_agg_table)

}

#' Recode variable according to the analysis plan
#'
#' @param data data.frame containing the data to be recoded
#' @param var string containing the name of the variable to be recoded. Must be present in \code{context_AP} in column \code{indicator_code}
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'    the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSCprocessing::context_AP}.
#'
#' @return data.frame with the variable recoded
#' @export
#'
#' @examples
#'
#' var_recoded <- recode_variable(data = WSCprocessing::bfa_msna_2020,
#' variable = "critical_handwashing_times", context_AP = WSCprocessing::context_AP,
#' data_source_name = "bfa_msna_2020")
#'

recode_variable <- function(data, variable, context_AP,data_source_name){
  context_AP_var <- context_AP[context_AP$indicator_code == variable & context_AP$data_source_name == data_source_name, ]
  var_source <- unique(context_AP_var$indicator_code_source)
  from <- context_AP_var$choices_name
  to <- context_AP_var$score_recoding
  if(is.na(from)){
    from <- var_source
  }
  if(sum(is.na(to)) == length(to)){
    to <- from
  }
  sel_mult <- grep("select_multiple", context_AP_var$question_type)
  # if select multiple for water source recode each option, and take the worse
  if (length(sel_mult) > 0) {
    y <- data %>%
      select(!!var_source)

    fill_choices <- function(df, col, choices_col){
      df %>%
        mutate(!!sym(col) := if_else(str_detect(!!sym(choices_col), !!col), TRUE, FALSE))%>%
        select(!!col)
    }

    y_filled <- lapply(from, function(x){fill_choices(df = y, col = x, choices_col = var_source)}) %>% bind_cols()
    y_filled <- y_filled[,!is.na(to)]
    from_noNA <- from[!is.na(to)]
    to_noNA <- to[!is.na(to)]

    names(y_filled) <- r3c(names(y_filled), from_noNA, to_noNA)

    y_filled[,variable] <- ""

    for(i in 1:nrow(y_filled)){
      y_filled[i,variable] <- paste(names(y_filled[,which(y_filled[i,] == TRUE)]), collapse = " ")
    }

    final_y <- relocate(y_filled, !!variable)

  }else{
    y<- data[[var_source]]
    if(class(y) == "numeric"){
      final_y <- as.data.frame(y)
      names(final_y)<-variable
    }else{
      y <- y%>% r3c(.,from,to) %>% as.data.frame()
      names(y)<-variable
      final_y <- y %>%
        mutate(across(where(is.character), ~case_when(
          is.na(.x) ~ NA_character_,
          !.x %in% to ~ "other_choice",
          TRUE ~ .x))
          )
    }
  }
  return(final_y)
}
