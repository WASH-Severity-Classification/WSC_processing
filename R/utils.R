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
