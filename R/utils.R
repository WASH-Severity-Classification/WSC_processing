# recode missing values
rec_missing<-function(x,missings=c('N/A','n/a',999,888,' ','(vide)','d/m','','NA','na')) {
  x[x %in% missings] <- NA
  return(x)
}

rename_vec<-function(vec,from,to){
  from <-  as.character(from)
  vec <-  as.character(vec)
  to <-  as.character(to)

  if(length(from)==length(to)){
    for (i in 1:length(from)){
      cond<-which(vec%in%from[i])
      if(length(cond)>0){
        if(sum(vec[cond] == from[i]) == length(vec[cond])|sum(is.na(vec[cond]))==length(vec[cond])){
          vec[cond]<-to[i]
        }
      if(length(grep(paste0(from[i], "\\."), vec)) > 0){
        vec[grep(paste0(from[i], "\\."), vec)] <- gsub(paste0(from[i], "\\."), paste0(to[i],"\\."), vec[grep(paste0(from[i], "\\."), vec)])
      }
      }
    }
    return(vec)
  } else {
    print("from and to must have the same length")
  }
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

############################################################################################
############################################################################################

# character operation
ch<-as.character
chr<-as.character

coerc<-function(x){as.numeric(chr(x))}

recode_var <- function (var, recodes, as.factor, as.numeric = TRUE, levels)
{
  # function from car package https://cran.r-project.org/web/packages/car/index.html

  lo <- -Inf
  hi <- Inf
  recodes <- gsub("\n|\t", " ", recodes)
  recode.list <- rev(strsplit(recodes, ";")[[1]])
  is.fac <- is.factor(var)
  if (missing(as.factor))
    as.factor <- is.fac
  if (is.fac)
    var <- as.character(var)
  result <- var
  for (term in recode.list) {
    if (0 < length(grep(":", term))) {
      range <- strsplit(strsplit(term, "=")[[1]][1], ":")
      low <- try(eval(parse(text = range[[1]][1])), silent = TRUE)
      if (inherits(low, "try-error")) {
        stop("\n  in recode term: ", term, "\n  message: ",
             low)
      }
      high <- try(eval(parse(text = range[[1]][2])), silent = TRUE)
      if (inherits(high, "try-error")) {
        stop("\n  in recode term: ", term, "\n  message: ",
             high)
      }
      target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])),
                    silent = TRUE)
      if (inherits(target, "try-error")) {
        stop("\n  in recode term: ", term, "\n  message: ",
             target)
      }
      result[(var >= low) & (var <= high)] <- target
    }
    else if (0 < length(grep("^else=", squeezeBlanks(term)))) {
      target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])),
                    silent = TRUE)
      if (inherits(target, "try-error")) {
        stop("\n  in recode term: ", term, "\n  message: ",
             target)
      }
      result[1:length(var)] <- target
    }
    else {
      set <- try(eval(parse(text = strsplit(term, "=")[[1]][1])),
                 silent = TRUE)
      if (inherits(set, "try-error")) {
        stop("\n  in recode term: ", term, "\n  message: ",
             set)
      }
      target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])),
                    silent = TRUE)
      if (inherits(target, "try-error")) {
        stop("\n  in recode term: ", term, "\n  message: ",
             target)
      }
      for (val in set) {
        if (is.na(val))
          result[is.na(var)] <- target
        else result[var == val] <- target
      }
    }
  }
  if (as.factor) {
    result <- if (!missing(levels))
      factor(result, levels = levels)
    else as.factor(result)
  }
  else if (as.numeric && (!is.numeric(result))) {
    result.valid <- stats::na.omit(result)
    opt <- options(warn = -1)
    result.valid <- as.numeric(result.valid)
    options(opt)
    if (!any(is.na(result.valid)))
      result <- as.numeric(result)
  }
  result
}

squeezeBlanks<- function (text){
  # function from car package https://cran.r-project.org/web/packages/car/index.html
  gsub(" *", "", text)
}

normalise_string <- function(string){
  no_ws <- stringr::str_squish(string)
  lower <- trimws(tolower(no_ws))
  no_accent <- stringi::stri_trans_general(lower,"Latin-ASCII")
  remove_other <- stringr::str_replace_all(no_accent, "[-',.()/ ]", "_")

  return(remove_other)
}


read_df <- function(ss, sheet, data_source_name, ...) {
  if (grepl("https://docs.google.com/spreadsheets", ss)) {
    result <- googlesheets4::read_sheet(ss,
                              sheet,
                              .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  } else{
    readable_files <-
      fs::dir_ls(".", recurse = TRUE, glob = "*.csv$|*.xls$|*.xlsx$|.sav$")
    readable_files_source_name <-
      readable_files[grepl(data_source_name, readable_files)]
    if (length(readable_files_source_name) > 1) {
      warning(
        paste0(
          data_source_name,
          " has more than one file that is readable (csv, sav, xls, or xlsx) and contains the name of the data source.
                           Please make sure to specify an URL to the google sheet contain"
        )
      )
      result <- NULL
    } else {
      if (length(readable_files_source_name) == 0) {
        warning(
          paste0(
            data_source_name,
            " - ",
            sheet,
            " could not be loaded. ",
            "No worksheet or file containing the source name could be found in the working directory."
          )
        )
        return(NULL)
      } else {
        ss <- readable_files_source_name
      }
    }

    if (fs::file_exists(ss)) {
      if (sum(grepl("*.csv$", readable_files_source_name)) != 0) {
        result <- readr::read_csv(readable_files_source_name)
      } else if(sum(grepl("*.sav$", readable_files_source_name)) != 0) {
        rename_select_mulitple_spss <- function(data, var){
          label_choice_dot <- gsub("\\/", "\\.", attributes(data[[var]])$label)
          if(nchar(label_choice_dot) >= 64){
            label_choice_dot <- var
          }
          index <- grep(paste0(var,"$"), names(data))
          names(data)[[index]] <- label_choice_dot
        }

        result <- haven::read_sav(readable_files_source_name, user_na = TRUE) %>%
          haven::as_factor()
        result <- lapply(result, rec_missing) %>%
          bind_cols() %>%
          as_tibble()

        names(result)[grepl("\\.", names(result))] <- map_chr(names(result)[grepl("\\.", names(result))], ~rename_select_mulitple_spss(result, .x))
        return(result)
      }else if (sum(grepl("*.xls$|*.xlsx$", readable_files_source_name)) !=
                 0) {
        result <- readxl::read_excel(readable_files_source_name, sheet = sheet)
      } else{
        warning(
          paste0(
            ss,
            " could not be read. only .csv, .sav, .xls, and .xlsx are supported at the moment"
          )
        )
        return(NULL)
      }
    } else{
      warning(paste0(
        ss,
        " could not be found in the working directory and sub-folders."
      ))
      return(NULL)
    }

  }
}

