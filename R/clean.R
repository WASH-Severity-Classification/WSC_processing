check_indicator <- function(data, indicator){
  if(!indicator %in% names(data)){
    warning(call. = F, paste0(indicator," column was not found in source. Please check the column names."))
    return(TRUE)
  }else{
    return(FALSE)
  }
}

check_source <- function(data,  indicators_to_check){
  has_warnings <- lapply(indicators_to_check, check_indicator, data = data) %>%
    unlist()
  return(any(has_warnings == TRUE))
}

