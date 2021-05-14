
#' Get population data frame from HDX
#'
#' @param country_iso3 character string with the ISO3 country code for the
#'   country analysed. See
#'   {https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{here} for more details.
#' @param admin_level character string identifying a specific administrative
#'     level where the pcode should be looked for. Three values are possible:
#'     "admin1", "admin2", and "admin3". For instance, if you are looking to
#'     pcode the region of Bamako in Mali, you should specify "admin1", as there
#'     is also an administrative unit 2 with the same name. By default the
#'     lowest administrative unit found will be returned
#' @param google_api_key a string with a google API key. This is used to recode
#'   administrative units. See vignette("googleway-vignette") for more details.
#' @param pcodes_df dataframe containing matching key between pcodes and admin
#'   names. For more details see
#'  \href{https://github.com/elliottmess/pcodesOCHA}{here}.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pop_df_example <- get_pop_df(country_iso3 = "BFA")
#' }
get_pop_df <- function(country_iso3, admin_level = NULL,google_api_key = NULL, pcodes_df = NULL){
  if(is.null(pcodes_df)){
    pcodes_df <-  country_pcodes_iso3(country_iso3 = country_iso3)
  }

  pop_df_rhdx <- search_datasets(paste(country_iso3, "subnational population statistics")) %>%
    pluck(1)
  pop_resources <- pop_df_rhdx$resources
  pop_resources_names <- map(pop_resources, ~c(.x$data$name)) %>%
    unlist()
  pop_resources_readable <- pop_resources_names[grep(".xlsx$|.xls$|.csv$", pop_resources_names)]
  pop_resources_good <- pop_resources_readable[grep("pop|POP", pop_resources_readable)]
  admin_units <- pop_resources_good[grep("adm[Z-a]*?[0-9]|ADM[Z-a]*?[0-9]", pop_resources_good)]
  min_admin <- max(as.numeric(str_extract(str_extract(admin_units, "adm[Z-a]*?[0-9]|ADM[Z-a]*?[0-9]"), "[0-9]")))
  min_admin_pop_res <- pop_resources_names[grep(paste0("adm[Z-a]*?",min_admin, "|ADM[Z-a]*?",min_admin), pop_resources_names)]

  pop_df <- pop_df_rhdx %>%
    get_resource(grep(min_admin_pop_res, pop_resources_names)) %>%
    read_resource() %>%
    rename(total_pop = contains("total"))

  admin_cols_pcodes <- names(pop_df)[grep("^(adm|ADM)([A-Z]{0,})([a-z]{0,})[0-9].?", names(pop_df))]
  admin_cols <- admin_cols_pcodes[!grepl("PCODE.{0,1}$|pcode.{0,1}$", admin_cols_pcodes)]
  language_cols <- str_remove_all(str_extract(admin_cols, "_[A-Z]{2}$"), "_")
  if(length(unique(language_cols))>1){
    ux <- unique(language_cols)
    language <- ux[which.max(tabulate(match(language_cols, ux)))]
    admin_cols <- admin_cols[grepl(paste0(language,"$"), admin_cols )]
  }
  admin_pcodes <- admin_cols_pcodes[grepl("PCODE.{0,1}$|pcode.{0,1}$", admin_cols_pcodes)]
  admin_pcodes <- admin_pcodes[!duplicated(admin_pcodes)]

  admin_cols_n <- as.numeric(str_extract(str_extract(admin_cols, "adm[Z-a]*?[0-9]|ADM[Z-a]*?[0-9]"), "[0-9]"))
  admin_cols_n <- admin_cols_n[!duplicated(admin_cols_n)]
  admin_cols_f <- setNames(admin_cols, paste0("admin", admin_cols_n))
  admin_pcodes_f <- set_names(admin_pcodes,paste0("admin", admin_cols_n, "_pcode") )

  pop_df <- rename(pop_df, all_of(admin_cols_f), all_of(admin_pcodes_f)) %>%
    select(paste0("admin", max(admin_cols_n)), contains("pop")) %>%
    mutate(!!sym(paste0("admin", max(admin_cols_n))):= normalise_string(!!sym(paste0("admin", max(admin_cols_n)))))

  pop_df_lowest_adm_rename <-  find_pcodes_admin(pop_df[[paste0("admin", max(admin_cols_n))]],
                                                 country_iso3 = country_iso3,
                                                 admin_level = NULL,
                                                 google_api_key = google_api_key,
                                                 pcodes_df = pcodes_df)

  pop_df <- left_join(pop_df, pop_df_lowest_adm_rename,
                      by = paste0("admin", max(admin_cols_n)))

  return(pop_df)

}
