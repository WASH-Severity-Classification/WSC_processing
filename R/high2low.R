#' Assign higher administrative unit data to a lower one
#'
#' As data is not always available at the required level, it might be needed to
#' assign/input data from a higher administrative unit to a lower one. For
#' instance,if a country has three administrative unit: region, province, and
#' communes, this function can help to bring data available only at the
#' region-level to a province-level data set.
#'
#'
#' @param high_df data.frame with the higher administrative unit data to be
#'     assigned.
#' @param high_df_name character string with the name of the
#'     \code{high_df}. Must follow the format:
#'          \code{data_source_name} - \code{data_sheet_name} columns of
#'     \code{context_AP}.
#' @param low_admin character string with the name of the
#'     \code{low_df}. Must follow the format:
#'          \code{data_source_name} - \code{data_sheet_name} columns of
#'     \code{context_AP}.
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
#' @param context_AP data.frame with context specific analysis plan (AP) that
#'    links the indicators in the WSC AP to the datasets used in the context
#'    analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSCprocessing::context_AP}.
#'
#' @return a data.frame with the aggregated data at a lower administrative level
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
#' assign_high2low(high_df = WSCprocessing::bfa_smart_2019_admin1,
#'      high_df_name = "smart_2019",
#'     context_AP = WSCprocessing::context_AP, WSC_AP = WSCprocessing::WSC_AP,
#'     LoAdmin_name = "admin2")
#'}

assign_result_high2low <-
  function(high_df,
           high_df_name,
           low_admin,
           pop_df,
           context_AP) {

    full_AP <- context_AP %>%
      dplyr::mutate(
        unique_data_source_name = paste(data_source_name,
                                        data_sheet_name, sep = "-")
      ) %>%
      dplyr::filter(
        unique_data_source_name == !!high_df_name
      )

    high_admin <- unique(full_AP$admin_level)
    high_admin_n <- as.numeric(str_extract(high_admin, "[0-9]"))

    low_admin_n <- as.numeric(str_extract(low_admin, "[0-9]"))

    # data_pop_df <- pop_df %>%
    #   select(paste0("admin", rep(0:high_admin_n)),
    #          any_of(paste0("admin", rep(0:high_admin_n), "_pcode")),
    #          total_pop)
    #
    # weigths_df <- data_pop_df %>%
    #   group_by(!!sym(paste0("admin", low_admin_n-1))) %>%
    #   mutate(weights = total_pop/sum(total_pop)) %>%
    #   summarise(weights = sum(weights))


    highest_pop_level <- min(as.numeric(str_extract(names(data_pop_df), "[0-9]")), na.rm = TRUE)
    highest_agg_level <- min(as.numeric(str_extract(names(high_df), "[0-9]")), na.rm = T)
    highest_both <- max(highest_pop_level, highest_agg_level)

    low_df <- pop_df %>%
      select(paste0("admin", rep(highest_pop_level:low_admin_n))) %>%
      left_join(high_df, by = c(paste0("admin", rep(highest_both:high_admin_n))))

    return(low_df)

  }

#' Assign lower administrative unit data to a higher one
#'
#' As data is not always available at the required level, it might be needed to
#' assign/input data from a lower administrative unit to a higher one. For
#' instance, if a country has three administrative unit: region, province, and
#' communes, this function can help to bring data available only at the
#' communes-level to a province-level data set.
#'
#' @param low_df data.frame with the lower administrative unit data. Can be
#'     just a list of lower administrative levels.
#' @param low_df_name character string with the name of the
#'     \code{low_df}. Must follow the format:
#'          \code{data_source_name} - \code{data_sheet_name} columns of
#'     \code{context_AP}.
#' @param high_admin character string with the name of the
#'     \code{pop_df}. Must follow the format:
#'          \code{data_source_name} - \code{data_sheet_name} columns of
#'     \code{context_AP}.
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
#' @param context_AP data.frame with context specific analysis plan (AP) that
#'    links the indicators in the WSC AP to the datasets used in the context
#'    analysis.
#'    See an example [here](https://docs.google.com/spreadsheets/d/1Pv1BBf32faE6J5tryubhVOsQJfGXaDb2t23KWGab52U/edit?usp=sharing) or in \code{WSCprocessing::context_AP}.
#'
#' @return a data.frame with the aggregated data at a lower administrative level
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
#' assign_high2low(high_df = WSCprocessing::bfa_smart_2019_admin1,
#'      high_df_name = "smart_2019",
#'     context_AP = WSCprocessing::context_AP, WSC_AP = WSCprocessing::WSC_AP,
#'     LoAdmin_name = "admin2")
#'}

assign_result_low2high <-
  function(low_df,
           low_df_name,
           high_admin,
           pop_df,
           context_AP) {

    full_AP <- context_AP %>%
      dplyr::mutate(
        unique_data_source_name = paste(data_source_name,
                                        data_sheet_name, sep = "-")
      ) %>%
      dplyr::filter(
        unique_data_source_name == !!low_df_name
      )

    low_admin <- unique(full_AP$admin_level)
    low_admin_n <- as.numeric(str_extract(low_admin, "[0-9]"))

    high_admin_n <- as.numeric(str_extract(high_admin, "[0-9]"))

    data_pop_df <- pop_df %>%
      select(paste0("admin", rep(0:low_admin_n)),
             any_of(paste0("admin", rep(0:low_admin_n), "_pcode")),
             total_pop) %>%
        group_by(!!sym(paste0("admin", low_admin_n-1))) %>%
        mutate(weights = total_pop/sum(total_pop)) %>%
      select(-total_pop)

    low_df_weighted <- low_df %>%
      left_join(data_pop_df, by = c(paste0("admin", rep(highest_both:low_admin_n))))

    highest_pop_level <- min(as.numeric(str_extract(names(data_pop_df), "[0-9]")), na.rm = TRUE)
    highest_agg_level <- min(as.numeric(str_extract(names(low_df), "[0-9]")), na.rm = T)
    highest_both <- max(highest_pop_level, highest_agg_level)

    source <- unique(full_AP$unique_data_source_name)
    context <- unique(full_AP$context)

    high_df <- low_df_weighted %>%
      group_by(!!sym(paste0("admin", rep(highest_agg_level:high_admin_n))), indicator, choice) %>%
      summarise(value = weighted.mean(as.numeric(value), w = weights),
                source = !!source, context = !!context, .groups = "drop")

    return(high_df)
  }

