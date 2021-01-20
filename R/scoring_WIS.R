#' Score WASH Insecurity Score (WIS)
#' @param data data.frame containing the data to be analysed with the function.
#'     If data is.null, the data sheet associated in the context_AP will be downloaded.
#' @param context_AP data.frame with context specific analysis plan (AP) that links
#'     the indicators in the WSC AP to the datasets used in the context analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in ```WSCprocessing::context_AP```.
#' @param context Character string identifying the context to be used in the function call.
#'    This is to be used if multiple context (geographical or temporal) are being
#'    analysed. For instance, if data is used for Burkina Faso in 2020 and 2019,
#'    this column can help distinguish the indicators.
#' @param WSC_AP data.frame with the general WSC analysis plan (AP) than can be
#'    found \href{https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit?usp=sharing}{here} or as an object in the package (```WSCprocessing::WSC_AP```)
#' @param WIS_water data.frame with the scoring reference matrix for the Water
#'    component of the WASH Insecurity Score (WIS)
#' @param WIS_sanitation data.frame with the scoring reference matrix for the
#'    Sanitation component of the WIS
#' @param WIS_final data.frame with the scoring reference matrix for the final
#'    score of the WIS
#' @param data data.frame containing the data to be analysed with the function
#'
#' @return a dataframe containing the results of the scoring for each household
#'    in the dataset. NAs are introduced if variables cannot be found or score
#'    calculated.
#' @export
#'
#' @examples
#' score_WIS(data = WSCprocessing::bfa_msna_2020, context_AP = WSCprocessing::context_AP, context = "bfa_2020",
#'          WSC_AP = WSCprocessing::WSC_AP, WIS_water = WSCprocessing::WIS_water, WIS_sanitation = WSCprocessing::WIS_sanitation,
#'          WIS_final = WSCprocessing::WIS_final)
#' @md
score_WIS<-function(data, context_AP, context = NULL, WSC_AP = WSC_AP, WIS_water = WIS_water, WIS_sanitation = WIS_sanitation, WIS_final = WIS_final){

  full_AP <- context_AP%>%
    dplyr::left_join(WSC_AP, by = "indicator_code")

  WIS_AP <- full_AP%>%
    dplyr::filter(wash_scoring == TRUE | indicator_code %in% c("weights", "cluster_id", "admin1", "admin2", "admin3"))


  if(is.null(data)){
    data_url <- unique(WIS_AP$data_worksheet_url)
    data_sheet <- unique(WIS_AP$data_sheet_name)
    if(length(data_url)>1){
      stop("Please organise all data used to score the WIS (see WSCprocessing::WSC_AP$wash_scoring) in one googlesheet. At the moment you have more than URL pointing at your data.")
    }
    data <- googlesheets4::read_sheet(data_url, sheet =  data_sheet)
  }

  data<-data %>%
    lapply(rec_missing) %>%
    dplyr::bind_cols() %>%
    as.data.frame()

  # harmonize uuid columns
  names(data)<-recode_var(names(data),"c('x_uuid','X_uuid','_uuid')='uuid'")

  # load the recoding sheet
  recoding<- WIS_AP %>% as.data.frame
  recoding<-recoding[recoding$context==context,]
  # value to recode from
  from<-recoding$"indicator_code_source"
  # value to recode to
  to<-recoding$"indicator_code"

  # select the column needed
  datascore<-data[,names(data)%in%c("uuid",unique(recoding$"indicator_code_source"))]
  # rename with indicator
  names(datascore)<-r3c(names(datascore),from,to)

  # recode all the variables
  scoring<-lapply(names(datascore),
                  function(x,recoding,data){
                    index<-recoding$indicator_code==x
                    recoding<-recoding[index,]
                    from<-recoding$choices_name
                    to<-recoding$score_recoding
                    sel_mult<-grep("select_multiple",recoding$question_type)
                    # if select multiple for water source recode each option, and take the worse
                    if(length(sel_mult)>0){
                      y<-data[[x]] %>% stringr::str_split(" ") %>% lapply(r3c,from,to) %>%
                        lapply(function(y){if(all(is.na(y))){var<- NA_character_
                        }else if(any(y=="no_drinking")){var<-"no_drinking"
                        }else if(any(y=="drinking") & any(y=="cooking") & any(y=="pers_hyg") & any(y=="other_dom")){var<-"drinking_cooking_pers_hyg_other_dom"
                        }else if(any(y=="drinking") & any(y=="cooking") & any(y=="pers_hyg") & !any(y=="other_dom")){var<-"drinking_cooking_pers_hyg"
                        }else if(any(y=="drinking") & (any(y=="cooking") | any(y=="pers_hyg"))){var <- "drinking_cooking_OR_pers_hyg"
                        }else if(any(y=="drinking") & !(any(y=="cooking") | any(y=="pers_hyg"))){var <- "drinking"
                        }else{var<-"NA"}
                          return(var)
                        }) %>% unlist %>% c()%>% as.data.frame()
                    }else{
                      y<- data[[x]] %>% r3c(.,from,to) %>% as.data.frame()
                    }
                    names(y)<-x
                    return(y)
                  },recoding=recoding,data=datascore) %>% do.call(cbind,.)

  # concatenate water source and distance
  scoring$water_source_dist<-ifelse(
    scoring$water_source=="improved",
    paste0(scoring$water_source,"_",scoring$distance_to_water_source),
    scoring$water_source)

  # create a key for a lookup
  scoring$key_water<-paste0(scoring$sufficiency_of_water,"-/-",scoring$water_source_dist)
  scoring$key_sanit<-paste0(scoring$type_of_sanitation_facility,"-/-",scoring$sanitation_facility_sharing,"-/-",scoring$access_to_soap)

  # recode water scores from the excel scoring table
  scoring$water_score<- suppressWarnings(r3c(scoring$key_water,WIS_water$key_water,WIS_water$score_water)%>%
    as.numeric())

  # recode sanit scores from the excel scoring table
  scoring$sanit_score<- suppressWarnings(r3c(scoring$key_sanit,WIS_sanitation$key_sanit,WIS_sanitation$score_sanit)%>%
    as.numeric())

  # recode final scores from the excel scoring table
  scoring$key_score<-paste0(scoring$water_score,"-/-",scoring$sanit_score)
  scoring$score<- suppressWarnings(r3c(scoring$key_score,WIS_final$key_score,WIS_final$score))
  scoring$score_final<- suppressWarnings(scoring$score%>%
    as.numeric())

  return(scoring)
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
#'
#' @return a dataframe with the results of the 20 percent rule.
#' @export
#'
#' @examples
#' WSI_score_admin2 <- agg_score(data = WSCprocessing::bfa_msna_2020, context = "bfa_2020",
#'                               context_AP = WSCprocessing::context_AP,
#'                               WSC_AP = WSCprocessing::WSC_AP, agg_level = "admin2",
#'                               WIS_water = WSCprocessing::WIS_water, WIS_sanitation = WSCprocessing::WIS_sanitation,
#'                               WIS_final = WSCprocessing::WIS_final)
#'
#' twenty_rule(data = WSI_score_admin2, col_score = "indicator",
#'             col_label = "choice", name_final_score = "score_final",
#'             col_agg = "admin2", col_value = "value")

twenty_rule <- function(data, col_score, col_label, name_final_score, col_agg, col_value){

  scores_col <- c("score_1" , "score_2", "score_3", "score_4", "score_5")

  df <- data%>%
    dplyr::filter(!!dplyr::sym(col_score) == !!name_final_score)%>%
    dplyr::group_by(!!dplyr::sym(col_agg))%>%
    tidyr::pivot_wider(names_from = !!dplyr::sym(col_label), values_from = !!dplyr::sym(col_value),
                       names_prefix = "score_", values_fn = sum)

  missing_scores <- setdiff(scores_col, names(df))
  df[missing_scores] <- NA_integer_

  df%>%
    dplyr::mutate(
      score_final = as.factor(dplyr::case_when(
        score_5 >= 0.2 ~ "5",
        sum(score_5, score_4, na.rm = T) >= 0.2 ~ "4",
        sum(score_5, score_4,score_3, na.rm = T) >= 0.2 ~ "3",
        sum(score_5, score_4,score_3,score_2, na.rm = T) >= 0.2 ~ "2",
        sum(score_5, score_4,score_3,score_2,score_1, na.rm = T) >= 0.2 ~ "1",
        TRUE ~ NA_character_
    )))%>%
    dplyr::relocate(c(score_1, score_2, score_3, score_4, score_5,score_final), .after=last_col())
}


