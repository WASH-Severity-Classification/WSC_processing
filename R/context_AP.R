#' WASH Severity Classification (WSC) contextual Analysis Plan (AP) example
#'
#' A dataset containing an example of contextualised AP
#'
#' @format A data frame with 228 rows and 15 variables:
#' \describe{
#'   \item{Niveau du cadre analytique}{character Niveau/Dimension du cadre analytique comme vu dans le \href{https://docs.google.com/document/d/1ikSd_3KMOyhJ8pTr5BLXlLZ92y6h5ZpjEeyPxFilxN8/edit}{cadre analytique CSW}}
#'   \item{Analytical framework level}{character Analytical framework level/dimension as in the \href{https://docs.google.com/document/d/1ikSd_3KMOyhJ8pTr5BLXlLZ92y6h5ZpjEeyPxFilxN8/edit}{WSC analytical framework}}
#'   \item{Sous-niveau du cadre analytique}{character Sous-niveau/sous-dimension du cadre analytique comme vu dans le \href{https://docs.google.com/document/d/1ikSd_3KMOyhJ8pTr5BLXlLZ92y6h5ZpjEeyPxFilxN8/edit}{cadre analytique CSW}. Analytical framework sub-level/sub-dimension as in the \href{https://docs.google.com/document/d/1ikSd_3KMOyhJ8pTr5BLXlLZ92y6h5ZpjEeyPxFilxN8/edit}{WSC analytical framework}}
#'   \item{Analytical framework sub-level}{character Analytical framework sub-level/sub-dimension as in the \href{https://docs.google.com/document/d/1ikSd_3KMOyhJ8pTr5BLXlLZ92y6h5ZpjEeyPxFilxN8/edit}{WSC analytical framework}}
#'   \item{Indicateur}{character Description complète de l'indicateur. Complete description of the indicator.}
#'   \item{Indicator}{character Complete description of the indicator.}
#'   \item{indicator_code}{character Indicator code, as in the WSC analysis plan}
#'   \item{context}{character Context to which the specific indicator applies. This is to be used if multiple context (geographical or temporal) are being analysed. For instance, if data is used for Burkina Faso in 2020 and 2019, this column can help distinguish the indicators.}
#'   \item{data_source_name}{character Name of the data source}
#'   \item{data_worksheet_url}{character URL (link) to where the indicator data source is stored and accessible as a spreadsheet}
#'   \item{data_sheet_name}{character sheet name where the indicator data source is stored and accessible }
#'   \item{indicator_code_source}{character Indicator code as in the analysed dataset (e.g. the column name)}
#'   \item{admin_level}{Most granular level at which the data is available}
#'   \item{data_type}{Type of data: either household (coded hh) or area (coded area).}
#'   \item{question_type}{character Type of questions with choice list linked to it. It should follow the same structure as in \href{https://xlsform.org/en/}{XLSforms}}
#'   \item{question_label}{character Label of question as in the form (select_one, select_multiple, integer).}
#'   \item{choices_list}{character Name of choices list, if relevant. This is particularly helpful to link the indicator to an ODK/kobo form with XLSForm. See \href{https://xlsform.org/en/}{here} for more details on XLSforms. }
#'   \item{choices_name}{character Code for the choice that will be used to classify/score the question}
#'   \item{choices_label}{character Name for the choice that will be used to classify/score the question}
#'   \item{score_recoding}{character Score/new value to attributed to the choice for the indicator}

#' }
#' @source \url{https://docs.google.com/spreadsheets/d/1nBzXeqxVJzS5g8nbEGCIPL8fwTYyu3KYWpFgwfJQ1so/edit?usp=sharing}
"context_AP"
