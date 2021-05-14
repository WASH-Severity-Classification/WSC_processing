source("packages.R")
source("health_data_plots.R")
source("health_data_fcts.R")
source("drive_manipulation_ssd.R")

WSC_AP <- read_sheet("https://docs.google.com/spreadsheets/d/1TKxD_DyBTTN6onxYiooqtcI_TVSwPfeE-t7ZHK1zzMU/edit#gid=0",
                     sheet = "analysis_plan")

ssd_analysis_plan <-
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/17YtEXopkCKyYjaQqdlZ-vPBiowVnPgZPPDoXi8JiO74/edit?usp=sharing",
    sheet = "ssd_analysis_plan"
  )

pop_df <- read_sheet("https://docs.google.com/spreadsheets/d/1-6fBRs0Mc3dzlRT-FgZatIhALT0CO4ltvRwmg9Tx2mg/edit#gid=1625707272")

FSNMS_26 <-
  haven::read_sav("data/FSNMS+_26_GPS_no_flatliners_FINAL_cleaned_WASH.sav") %>%
  as_factor() %>%
  mutate(
    fcs_thresholds = case_when(
      FCS <= 21 ~ "acceptable",
      FCS > 21 &
        FCS <= 35 ~ "borderline",
      FCS > 35 ~ "poor",
      TRUE ~ NA_character_
    ),
    hhs_score = HHSQ1 + HHSQ2 + HHSQ3,
    type_latrine = case_when(
      G06 %in% c("Family latrine", "Shared latrine (between neighbouring HHs)") ~ "improved_sanitation",
      G06 %in% c("Communal/institutional latrine (in marketplace, school, etc.)") ~ "unimproved_sanitation",
      G06 == "No" ~ "open_defec",
      TRUE ~ NA_character_
    ),
    shared_latrine = case_when(
      G06 %in% c(
        "Communal/institutional latrine (in marketplace, school, etc.)",
        "Shared latrine (between neighbouring HHs)"
      ) ~ "shared",
      G06 == "Family latrine" ~ "not_shared",
      TRUE ~ NA_character_
    )
  )


write_sav(FSNMS_26, "data/FSNMS_26.sav")

reach_ssd_aok_data <- read_csv("data/REACH_SSD_aok_03_2021.csv") %>%
  filter(month == "01/03/2021") %>%
  select(K.water_boreholes, K.repair_mech, K.water_safety, K.latrine_usage,
         K.latrine_no_usage,K.hand_washing, D.info_county) %>%
  rename(admin2 = D.info_county) %>%
  mutate(fctional_borehole = case_when(K.water_boreholes == "yes" ~ 1L,
                                       K.water_boreholes == "no" ~ 0L,
                                       TRUE ~ NA_integer_),
         no_access_water_sec = case_when(K.water_safety == "yes" ~ 1L,
                                         K.water_safety == "no" ~ 0L,
                                         TRUE ~ NA_integer_)
  ) %>%
  group_by(admin2) %>%
  summarise(fctional_borehole = mean(fctional_borehole, na.rm = T),
            no_access_water_sec = mean(no_access_water_sec, na.rm = T))

write_sheet(reach_ssd_aok_data,
            "https://docs.google.com/spreadsheets/d/1UlBD6DlGpKWX4SiFReWMwhdYRkGdG4EY3sgyIHtMeSM/edit#gid=0",
            sheet = "result")


# reach_ssd_aok <- read_sheet("https://docs.google.com/spreadsheets/d/1enkbcHOUJNmgEWtE39N989o38c31xuAhf2hx_iBMbw4/edit#gid=328970427",
#                             sheet = "Cleaned dataset")
#
# counties <- reach_ssd_aok$info_county
# ssd_perc <- reach_ssd_aok%>%
#   mutate(across(where(is.list), unlist)) %>%
#   mutate(across(!where(is.numeric), ~ str_replace_all(., "SL", NA_character_))) %>%
#   mutate(across(!c(info_county), as.numeric)) %>%
#   mutate(across(!c(ki_sex.f, ki_sex.m, ki_age, info_county), ~./100))
#
#  write_sheet(ssd_perc,
#   "https://docs.google.com/spreadsheets/d/1enkbcHOUJNmgEWtE39N989o38c31xuAhf2hx_iBMbw4/edit?usp=sharing",
#                             sheet = "Cleaned dataset-perc")

# label_names <- labelled::var_label(FSNMS_26, unlist = TRUE) %>%
#   as.data.frame() %>%
#   rownames_to_column(var = "var")


acled_ssd <- read_csv("outputs/ACLED/SSD/acled_data_SSDevents_last30days_2021-05-03.csv")

write_sheet(acled_ssd, "https://docs.google.com/spreadsheets/d/150KYbveqmi4qtTTW94OzkJMKswVFqX_97iqHyyu4ud4/edit#gid=0",
            sheet = "ACLED_data_03052021")


admin_analysis <- "admin2"

WIS_water_alt3 <- read_sheet("https://docs.google.com/spreadsheets/d/1wvcf52mGetkNrGAJ0IdefePnFlaK4KO6NJ4Va4Kn-Ng/edit#gid=1791456062",
                             sheet = "WIS_water_alt3")

result_analysis_IPC <- analyse_country(
  context_AP = filter(ssd_analysis_plan, data_source_name == "IPC"),
  admin_analysis = admin_analysis,
  country_iso3 =  "SSD",
  google_api_key = "AIzaSyAfchnrcGcaq4J-k_Y59NWRFqBvBUY5-JY",
  pop_df = pop_df,
  WIS_water = WIS_water_alt3,
  WSC_AP = WSC_AP
)


result_analysis <- analyse_country(
  context_AP = ssd_analysis_plan,
  admin_analysis = admin_analysis,
  country_iso3 =  "SSD",
  google_api_key = "AIzaSyAfchnrcGcaq4J-k_Y59NWRFqBvBUY5-JY",
  pop_df = pop_df,
  WIS_water = WIS_water_alt3,
  WSC_AP = WSC_AP
)


write_csv(result_analysis, "outputs/result_analysis_datasets.csv")

epi_week_df <- read_csv("data/epi_summary_weekly.csv") %>%
  rename(master_county = master_county.x)

admin_df <- pop_df %>%
  select(admin1, admin2)
admin_list <- as.character(WSCprocessing:::normalise_string(admin_df[["admin2"]]))

start_date <- today()
analysis_period <- "6months"

if(analysis_period == "6months"){
  date_end <- start_date - months(6)
}else   if(analysis_period == "1month"){
  date_end <- start_date - months(1)
}else if(analysis_period == "1year"){
  date_end <- start_date - months(12)
}

epi_week_df_cases <- epi_week_df %>%
  filter(date >= date_end & date <= start_date) %>%
  group_by(master_county,var) %>%
  summarise(value = as.character(sum(value))) %>%
  mutate(var = paste0(var,"_cases_last6months")) %>%
  rename(admin2 = master_county, indicator = var)

epi_data_plots <- epi_graphs_analysis(epi_week_df, admin_list, date = "date",
                                             var = "var", analysis_period = "6months",
                                             start_date = ymd("2021-05-01"),
                                             write = TRUE)


# admin_list_sub_under <- admin_list[grep("_", admin_list)]
#
# admin_list_sub_under <- gsub("kajo_keji", "kajo-keji", admin_list_sub_under)
#
# epi_data_plots_unders <- epi_graphs_analysis(epi_week_df, admin_list_sub_under, date = "date",
#                                       var = "var", analysis_period = "6months",
#                                       start_date = ymd("2021-05-01"),
#                                       write = TRUE)

# epi_data_plots <- bind_rows(epi_data_plots, epi_data_plots_unders) %>%

write_csv(epi_data_plots, "data/epi_data_plots.csv")

epi_data_plots <- epi_data_plots %>%
  distinct()

label_indicator <- WSC_AP %>%
  select(Indicator,Indicateur, indicator_code)

prop_week_cases_above_thresholds_df <- epi_data_plots %>%
  filter(severity_level %in% c("3", "4","5")) %>%
  group_by(admin, var) %>%
  summarise(value = sum(perc_weeks, na.rm = T),
            indicator = paste0("prop_week_",var,"_above_thresholds"),
            choice = NA_character_,
            .groups = "drop"
  ) %>%
  rename(admin2 = admin) %>%
  select(-var) %>%
  distinct()

prop_week_cases_above_thresholds_df_0 <- epi_data_plots %>%
  filter(!severity_level %in% c("3", "4","5")) %>%
  group_by(admin, var) %>%
  summarise(value = 0,
            indicator = paste0("prop_week_",var,"_above_thresholds"),
            choice = NA_character_,
            .groups = "drop"
  ) %>%
  rename(admin2 = admin) %>%
  select(-var) %>%
  distinct()


prop_week_diarrhea_above_thresholds_df <- epi_data_plots %>%
  filter(var == "awd_total_cases", severity_level %in% c("3", "4","5")) %>%
  group_by(admin) %>%
  summarise(value = sum(perc_weeks, na.rm = T),
            indicator = "prop_week_diarrhea_above_thresholds",
            choice = NA_character_
            ) %>%
  rename(admin2 = admin)

prop_week_diarrhea_above_thresholds_df_0 <- epi_data_plots %>%
  filter(var == "awd_total_cases", !severity_level %in% c("3", "4","5")) %>%
  group_by(admin) %>%
  summarise(value = 0,
            indicator = "prop_week_diarrhea_above_thresholds",
            choice = NA_character_
  ) %>%
  rename(admin2 = admin)

prop_week_above_thresholds_df <- bind_rows(
  prop_week_diarrhea_above_thresholds_df,
  prop_week_diarrhea_above_thresholds_df_0,
  prop_week_cases_above_thresholds_df,
  prop_week_cases_above_thresholds_df_0
)


prop_week_diarrhea_above_thresholds_phase <- prop_week_above_thresholds_df %>%
  group_by(admin2) %>%
  summarise(indicator = indicator,
    value = case_when(
              value == 0 ~ "1",
              value >0 & value <=.05 ~ "2",
              value >.05 & value <= .25 ~ "3",
              value > .25 & value <= .5 ~ "4",
              value > 0.5 ~ "5",
              TRUE ~ NA_character_
            ),
            choice = "Phase"
  )

admin_list_nounder <- str_remove_all(admin_list, "_")

admin_list_with_no_under <- bind_cols(nrml = admin_list, no_under = admin_list_nounder)
admin_list_with_no_under$no_under <- str_replace(admin_list_with_no_under$no_under, "kajokeji", "kajo-keji")

epi_data_plots_summarised_with_labels <-prop_week_above_thresholds_df%>%
  mutate(value = as.character(value)) %>%
  bind_rows(prop_week_diarrhea_above_thresholds_phase ) %>%
  bind_rows(epi_week_df_cases) %>%
  left_join(admin_list_with_no_under, by = c("admin2" = "no_under")) %>%
  mutate(admin2 = nrml) %>%
  select(-nrml) %>%
  mutate(context = "ssd_2021", source = "IDSR 2021") %>%
  left_join(label_indicator, by = c("indicator"="indicator_code")) %>%
  mutate(indicator_choice = paste0(Indicator, "_", choice),
         indicateur_choice = paste0(Indicateur, "_", choice),
         indicator_code_choice = paste0(indicator,choice)) %>%
  distinct()

write_sheet(epi_data_plots_summarised_with_labels,
            "https://docs.google.com/spreadsheets/d/11J5tcUpZfXxCLZ-tvgMY0aFCQW1NAcqbksFJZ1S8dnw/edit#gid=2071216252",
            sheet = "prop_week_diarrhea_above_thresholds")
result_analysis_with_epi <- bind_rows(result_analysis, epi_data_plots_summarised_with_labels)


more_one_source <- result_analysis_with_epi %>%
  select(!!sym(admin_analysis), indicator, source) %>%
  distinct() %>%
  group_by(!!sym(admin_analysis), indicator) %>%
  summarise(duplicated_source = n()) %>%
  filter(duplicated_source > 1)

result_analysis_no_dupl <- result_analysis_with_epi %>%
  left_join(more_one_source, by =c("admin2", "indicator")) %>%
  mutate(year_source_dupl = case_when(duplicated_source > 1 ~ as.numeric(str_remove(context,"ssd_")))) %>%
  group_by(!!sym(admin_analysis), indicator) %>%
  filter(year_source_dupl == max(year_source_dupl) | is.na(year_source_dupl)) %>%
  distinct() %>%
  mutate(choice = case_when(indicator %in% c("score_final", "water_score", "key_score")~ str_replace_all(choice, "5", "4+"),
                            TRUE ~ choice),
         source = case_when(str_detect(source, "_NA$") ~ str_replace(source, "_NA$",""),
                            TRUE ~ source),
         source = case_when(str_detect(source, "[0-9]{4}") ~ source,
                            TRUE ~ paste0(source, "-", str_extract(context, "[0-9]{4}$")))) %>%
  mutate(indicator_choice = paste0(Indicator, "_", choice),
         indicateur_choice = paste0(Indicateur, "_", choice),
         indicator_code_choice = paste0(indicator,choice)) %>%
  distinct()


all_admin_list <- pop_df$admin2
admin_touched <- c("Juba", "Kajo-keji", "Bor South", "Pibor", "Awerial","Cueibet",
                   "Aweil Centre", "Aweil East", "Guit", "Rubkona", "Twic", "Tonj South",
                   "Jur River","Ezo")

pop_df_untouched <- pop_df %>% filter(!admin2 %in% admin_touched)

data_ulang <- result_analysis_no_dupl %>%
  filter(admin2 == "ulang")

write_sheet(data_ulang, "https://docs.google.com/spreadsheets/d/1P5d8Fpj-INDV_JSmn2ocUcvlkQ3rXz06opfMzJ2F3E8/edit#gid=948277148",
            sheet = "data")


update_data_drive("https://drive.google.com/drive/u/1/folders/1ZH8p-XZwS1TIoKZugil9MvzfgigX1itj",
                  admin_analysis,
                  result_analysis_no_dupl,
                  pop_df_untouched,
                  last_modif_day = today()
                  )

wis_initial <- result_analysis_no_dupl %>%
  filter(indicator == "score_final") %>%
  mutate(value = as.numeric(value))

wis_phases <- wis_initial %>%
  mutate(choice = case_when(choice == "4+" ~ "5",
                            TRUE ~ choice)) %>%
  select(indicator, choice, admin2, value) %>%
  twenty_rule(col_score = "indicator", col_label = "choice", name_final_score = "score_final",
            col_agg = "admin2", col_value = "value") %>%
  select(admin2, score_final) %>%
  distinct() %>%
  mutate(choice = "Phase") %>%
  rename(value = score_final)

wis_initial_phased <- bind_rows(mutate(wis_initial, value = as.character(value)), wis_phases) %>%
  mutate(value = case_when(value == 5 ~ "4+",
                              TRUE ~ value)) %>%
  ungroup() %>%
  select(admin2, choice, value) %>%
  group_by(admin2) %>%
  pivot_wider(names_from = choice, values_from = value) %>%
  rename(final_phase_WIS = Phase, P1 = `1`,P2 = `2`,P3 = `3`,P4 = `4`,`P4+` = `4+`) %>%
  relocate(admin2, P1, P2, P3, P4, `P4+`, final_phase_WIS)

write_sheet(wis_initial_phased, "https://docs.google.com/spreadsheets/d/1FaiSbfAwga_GzOYhMnRU81tfVCvySzxvtx39rS0USog/edit#gid=0",
            sheet = "data")

# create_impl_fldr_full_str_ssd("https://drive.google.com/drive/u/1/folders/1ZH8p-XZwS1TIoKZugil9MvzfgigX1itj",
#                           admin_analysis = admin_analysis,
#                           result = result_analysis_no_dupl, admin_df = pop_df,
#                           analysis_period = "6months",
#                           start_date = ymd("2021-05-01"))

