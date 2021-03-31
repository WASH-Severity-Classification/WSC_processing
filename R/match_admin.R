geocode_one_admin <- function(admin,
                          country_iso3 = NULL,
                          google_api_key,
                          language = NULL) {
  if (is.null(google_api_key)) {
    stop("A valid private key for Google API 'Places' must be provided.")
  }
  if(!is.null(country_iso3)){
    cntry_name <-
      countrycode::countrycode(country_iso3, origin = 'iso3c', destination = "country.name")
  }

  admin_loc <- paste(admin, cntry_name)
  admin_levels <- paste0("admin", 0:3)
  admin_levels_df <- bind_rows(setNames(rep("", length(admin_levels)), admin_levels))[0, ]

  geocode <-
    google_geocode(admin_loc, key = google_api_key, language = language)$results

  if(length(geocode) == 0){
    warning(paste0(admin, " was not found by google. Check the inputs. Providing a country code will improve the results."))
    return(admin_levels_df)
  }

  geocode_admin_lvl <- geocode %>% select(address_components) %>%
    unnest(cols = c(address_components))
  geocode_admin_lvl_unnested <-
    suppressMessages(unnest_wider(geocode_admin_lvl, types)) %>%
    select(-...2) %>%
    rename(admin_lvl = ...1)

  geocode_admin_lvl_unnested_renamed <- geocode_admin_lvl_unnested %>%
    mutate(
      admin_lvl = case_when(
        admin_lvl == "locality" ~ "locality",
        admin_lvl == "administrative_area_level_3" ~ "admin3",
        admin_lvl == "administrative_area_level_2" ~ "admin2",
        admin_lvl == "administrative_area_level_1" ~ "admin1",
        admin_lvl == "country" ~ "admin0",
        TRUE ~ NA_character_)) %>%
    filter(!is.na(admin_lvl))

  adm_lvls_not_in <- admin_levels[!admin_levels %in% geocode_admin_lvl_unnested_renamed$admin_lvl]

  if(length(adm_lvls_not_in) != 0){
    geocode_admin_lvl_unnested_renamed<- add_row(geocode_admin_lvl_unnested_renamed, admin_lvl = adm_lvls_not_in) %>%
      bind_rows() %>%
      distinct()
  }

  if(length(geocode_admin_lvl_unnested_renamed$admin_lvl[geocode_admin_lvl_unnested_renamed$admin_lvl %in% admin_levels])> length(admin_levels)){
    warning(paste0(admin, " could not find an unique match. Check the inputs, including the country code."))
    return(admin_levels_df)

  }else{

  final_geocoded_admins<- geocode_admin_lvl_unnested_renamed %>%
    as.data.frame() %>%
    pivot_wider(-long_name, names_from = admin_lvl, values_from = c(short_name)) %>%
    relocate(!!admin_levels) %>%
    mutate(name_origin = admin,
           across(any_of(admin_levels),
                  ~ trimws(str_remove_all(.x,"Region|Department|Commune|Municipality|Province|Governorate|Urban Community"))))
  }

  return(final_geocoded_admins)
}

geocode_admin <- function(admin,
                          country_iso3 = NULL,
                          google_api_key,
                          language = NULL){
  if(length(admin) == 1){
    result <- geocode_one_admin(admin, country_iso3, google_api_key = google_api_key, language = language)
  }else{
    result <- map(admin, geocode_one_admin, country_iso3 = country_iso3,
                  google_api_key = google_api_key, language = language) %>%
      bind_rows()
  }
  return(result)
}

find_pcodes_one_admin <- function(admin,
                              country_iso3 = NULL,
                              google_api_key,
                              pcodes_df = NULL,
                              language = NULL){

  if(!is.null(country_iso3) & is.null(pcodes_df)){
    pcodes <- country_pcodes_iso3(country_iso3 = country_iso3)
  }else if(!is.null(country_iso3)){
    pcodes <- pcodes_df
  }

  pcodes_admin_nbs <- sort(as.numeric(stringr::str_extract(names(pcodes)[grepl("Pcode|pcode", names(pcodes))], "[0-9]")))

  regex_admin_names <- lapply(pcodes_admin_nbs, function(x){
    paste0("admin",x,"(N|n)ame(_[a-z]{2}$|$)")
  }) %>% unlist()

  actual_names <- lapply(regex_admin_names, function(x){
    names(pcodes)[grep(x, names(pcodes))]
  }) %>% unlist()

  normalised_pcodes <- pcodes %>%
    mutate(across(where(is.character), normalise_string)) %>%
    select(!!actual_names)
  names(normalised_pcodes) <- normalise_string(names(normalised_pcodes))

  normalised_admin <- normalise_string(admin)

  admin_in_df <- which(normalised_pcodes == normalised_admin, arr.ind = TRUE) %>%
    as_tibble()

  if(nrow(admin_in_df) ==0){
    geocodes <- geocode_admin(admin, country_iso3 = country_iso3, google_api_key = google_api_key, language = language)
    normalised_geocodes_admin <- geocodes %>%
      select(matches("admin[0-9]$")) %>%
      mutate(across(where(is.character), normalise_string))

    normalised_geocodes_admin <-
      normalised_geocodes_admin[colSums(is.na(normalised_geocodes_admin)) != nrow(normalised_geocodes_admin)]


    names(normalised_geocodes_admin) <- normalise_string(names(normalised_geocodes_admin))
    str_norm_geocodes <- as.character(normalised_geocodes_admin)
    adm_na_nb <- sort(as.numeric(stringr::str_extract(names(normalised_geocodes_admin[str_norm_geocodes == normalised_admin]), "[0-9]")))
    lowest_adm <- min(adm_na_nb)
    admin_geocoded <- paste0(names(geocodes[,paste0("admin", lowest_adm)]), "Name")

    filtered_pcodes <- pcodes %>%
      select(-contains(paste0(!!lowest_adm+1)), -OBJECTID) %>%
      filter(!!sym(admin_geocoded) == !!admin) %>%
      distinct()
  }else{
    lowest_admin_found_nb <- sort(as.numeric(stringr::str_extract(names(normalised_pcodes)[admin_in_df$col], "[0-9]")))

    filtered_pcodes <- pcodes %>%
      select(-contains(paste0(!!lowest_admin_found_nb+1)))
    filtered_pcodes <- filtered_pcodes[admin_in_df$row,] %>% distinct()

  }

  if(nrow(filtered_pcodes) == 0){
    warning(paste0(admin, " could not be found in OCHA's pcodes servers."))
  }else{
    return(filtered_pcodes)
  }
}

find_pcodes_admin <- function(admin,
                                  country_iso3 = NULL,
                                  google_api_key,
                                  pcodes_df = NULL,
                                  language = NULL){
  if(length(admin) == 1){
    result <- find_pcodes_one_admin(admin, country_iso3, google_api_key = google_api_key,pcodes_df = pcodes_df, language = language)
  }else{
    result <- map(admin, find_pcodes_one_admin, country_iso3 = country_iso3,
                  google_api_key = google_api_key, pcodes_df = pcodes_df,language = language) %>%
      bind_rows()
  }
  return(result)

}
