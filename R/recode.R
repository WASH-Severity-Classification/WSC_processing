#' Recode variable according to the analysis plan
#'
#' @param data data.frame containing the data to be recoded
#' @param variable string containing the name of the variable to be recoded. Must be present in \code{context_AP} in column \code{indicator_code}
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
#' @return data.frame with the variable recoded
#' @export
#'
#' @examples
#' \dontrun{
#' var_recoded <- recode_variable(data = WSCprocessing::bfa_msna_2020,
#' variable = "critical_handwashing_times")
#' }

recode_variable <-
  function(data,
           variable,
           data_AP) {
    context_AP_var <- data_AP %>%
      tidyr::unnest(cols = where(is.list)) %>%
      dplyr::filter(indicator_code == variable)

    context_AP_var <- context_AP_var[rowSums(is.na(context_AP_var)) != ncol(context_AP_var),]

    sel_mult <- grep("select_multiple", unlist(context_AP_var$question_type))

    var_source <- unique(context_AP_var$indicator_code_source)
    names(data) <- rename_vec(names(data), from = unlist(context_AP_var$indicator_code_source),
                              to = unlist(context_AP_var$indicator_code))

    if(sum(variable %in% names(data)) == 0 & length(sel_mult) > 0 ){
      data[[variable]] <- NA
    }

    if(length(grep(paste0("^",var_source,"\\."), names(data))) >0 & length(sel_mult) > 0){
      names(data) <- gsub(paste0("^",var_source,"\\."), paste0(variable,"\\."), names(data))
    }

    if(sum(is.na(var_source)) > 0){
      if(variable %in% names(data)){
        var_source <- variable
      }else{
        warning(paste0(variable, " did not find a match in the AP. Make sure that indicator_code_source is filled."))
        return(NULL)
      }
    }
    # if(sum(duplicated(unlist(context_AP_var$indicator_code))) > 0){
    #   context_AP_var$indicator_code <- paste(context_AP_var$indicator_code,context_AP_var$choices_name, sep = ".")
    # }

    from <- unlist(context_AP_var$choices_name)
    to <- unlist(context_AP_var$score_recoding)
    if (sum(is.na(from))==length(from)) {
      from <- var_source
    }
    if (sum(is.na(to)) == length(to)) {
      to <- from
    }
    # if select multiple for water source recode each option, and take the worse
    if (length(sel_mult) > 0) {
      y <- data %>%
        dplyr::select(starts_with(!!variable))

      fill_choices <- function(df, col, choices_col) {
        df %>%
          dplyr::mutate(!!dplyr::sym(col) := dplyr::if_else(stringr::str_detect(!!dplyr::sym(choices_col),!!col), TRUE, FALSE)) %>%
          dplyr::select(!!col)
      }
      fill_col <- function(df, col, q_col){
        result <- df %>%
          dplyr::mutate(!!dplyr::sym(paste0(q_col, "_", col)) := case_when(
            !!dplyr::sym(paste0(q_col, ".", col)) %in% c(TRUE, 1) ~ paste0(!!col),
            !!dplyr::sym(paste0(q_col, ".", col)) %in% c(FALSE, 0) ~ paste0(""),
            TRUE ~ NA_character_
          )) %>%
          select(!!paste0(q_col, "_", col))
      }

      if(sum(paste0(variable, ".", from) %in% names(data)) == length(from)){
        old_cols <- paste0(variable, ".", from)
        new_cols <- paste0(variable, ".", to)
        names(y) <- rename_vec(names(y), from = old_cols, to = new_cols)
        duplicated_cols <- names(y)[duplicated(new_cols)]
        to_no_dupl <- to[!duplicated(to)]

        concate_duplicated_cols <- function(df, duplicated_col){
          result <- df %>%
            set_names(make.unique(names(.))) %>%
            select(matches(duplicated_col)) %>%
            mutate(sum = rowSums(select(.,everything()), na.rm = T),
                   !!sym(duplicated_col):= case_when(
                     sum == 0 ~ FALSE,
                     sum > 0 ~ TRUE,
                     TRUE ~ na_lgl
                   )) %>%
            select(!!duplicated_col)
        }

        if(length(duplicated_cols)!=0){
          y_filled <- y %>%
            select(-!!sym(duplicated_cols))%>%
            bind_cols(map_dfc(duplicated_cols, ~concate_duplicated_cols(df = y, duplicated_col = .x)))
        }else{
          y_filled <- y
        }
        final_y <- y_filled %>%
          bind_cols(map_dfc(to_no_dupl, ~fill_col(df = y_filled, col = .x, q_col = variable)))%>%
          unite(!!sym(variable), starts_with(paste0(variable,"_")), sep = " ") %>%
          mutate(!!sym(variable):= trimws(!!sym(variable))) %>%
          relocate(!!variable)
      }else{
        if(sum(from == to) ==1){
          from_fill <- str_remove_all(names(y), "^.*\\.")
          from_fill <- from_fill[from_fill!= variable]
          y_filled <- lapply(from_fill, function(x) {
           y %>%
              mutate(!!sym(paste0(variable,".",x)) := case_when(
                !!sym(paste0(variable,".",x))>= 1 ~ TRUE,
                !!sym(paste0(variable,".",x)) == 0 ~ FALSE,
                TRUE ~ NA
              )) %>%
              dplyr::mutate(!!dplyr::sym(paste0(variable,"_",x)) := dplyr::if_else(!!sym(paste0(variable,".",x)),
                                                                     x,
                                                                     "")) %>%
              dplyr::select(!!dplyr::sym(paste0(variable,"_",x)))
          }) %>% bind_cols() %>%
            unite(!!sym(variable), starts_with(paste0(variable,"_")), sep = " ") %>%
            mutate(!!sym(variable):= str_squish(str_remove_all(!!sym(variable), "NA"))) %>%
            relocate(!!variable)
          y_filled <- y %>%
            select(-!!variable) %>%
            bind_cols(y_filled) %>%
            relocate(!!sym(variable))
        }else{
          y_filled <-
            lapply(from, function(x) {
              fill_choices(df = y,
                           col = x,
                           choices_col = variable)
            }) %>% bind_cols()

          y_filled <- y_filled[, !is.na(to)]
          from_noNA <- from[!is.na(to)]
          to_noNA <- to[!is.na(to)]

          names(y_filled) <- rename_vec(names(y_filled), from_noNA, to_noNA)

          y_filled[, variable] <- ""
          for (i in 1:nrow(y_filled)) {
            y_filled[i, variable] <-
              paste(names(y_filled[, which(y_filled[i, ] == TRUE)]), collapse = " ")
          }
          names(y_filled) <- if_else(names(y_filled) !=variable, paste0(variable, ".", names(y_filled)), names(y_filled))
        }
        final_y <- relocate(y_filled,!!variable)
      }

    } else{
      y <- data[[variable]]
      if (class(y) == "numeric") {
        final_y <- as.data.frame(y)
        names(final_y) <- variable
      } else{
        if((from == to) && from == var_source){
          final_y <- as.data.frame(y)
          names(final_y) <- variable
        }else{
          y <- y %>% rename_vec(., from, to) %>% as.data.frame()
          names(y) <- variable
          final_y <- y %>%
            dplyr::mutate(across(
              where(is.character),
              ~ case_when(
                is.na(.x) ~ NA_character_,!.x %in% to ~ "other_choice",
                TRUE ~ .x
              )
            ))
        }
      }
    }
    return(final_y)
  }

recode_source <- function(data, data_AP,...){

  from <- unlist(data_AP$indicator_code_source)
  to <- unlist(data_AP$indicator_code)
  names(data)<- rename_vec(names(data), from = from, to = to)

  var_to_recode <- data_AP %>%
    unnest(cols = where(is.list)) %>%
    select(indicator_code) %>%
    distinct()

  if(nrow(var_to_recode) == 0){
    return(data)
  }else{

    var_to_recode <- as.character(var_to_recode$indicator_code)

    var_to_recode <-
      var_to_recode[!var_to_recode %in% c("admin1", "admin2", "admin3", "weights", "sampling_id", "cluster_id")]

    recoded_vars <- map_dfc(var_to_recode, ~recode_variable(data = data,
                                                            variable = .,
                                                            data_AP = data_AP))

    data_recoded <- data %>%
      select(!starts_with(all_of(!!var_to_recode))) %>%
      bind_cols(recoded_vars)

    return(data_recoded)
  }
}
