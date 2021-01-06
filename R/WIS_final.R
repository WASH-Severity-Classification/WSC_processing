#' Scoring reference matrix for WASH Insecurity Score (WIS) final score
#'
#' A dataset containing the scoring reference matrix for WASH Insecurity Score (WIS) final score
#' For more information on the WIS, please see the \href{placeholder_link}{full calculation model}.
#'
#' @format A data frame with 21 rows and 4 variables:
#' \describe{
#'   \item{water_score}{Score value as calculated through the \link{WIS_water} reference matrix}
#'   \item{sanit_score}{Score value as calculated through the \link{WIS_sanitation} reference matrix}
#'   \item{key_score}{Key to uniquely identify combinaison of water_score and sanit_score}
#'   \item{score}{Final WIS score}
#' }
#' @source \url{https://docs.google.com/spreadsheets/d/1UCr-G9gD6YZmiOHDoP95qiMkEqi9jMG3lfzzv7WCFnM/edit?usp=sharing}
"WIS_final"
