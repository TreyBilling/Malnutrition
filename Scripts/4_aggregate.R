library(tidyverse)
library(sf)
theme_set(theme_bw())



# Data --------------------------------------------------------------------
load(here::here("Data", "dhs_working.Rdata"))

# Remove values that are missing country locations
df_m <- filter(df_m, !is.na(NAME_0))

# "hv270"
# "hv271", # wealth fac score
# "hc61", # mother edu
# "hc63", # preceding birth interval
# "hc64", # birth order
# "hv204", # time to water

df_m <- 
  df_m %>% 
  mutate(wealth = case_when(hv270 == "poorest" | hv270 == "lowest" ~ 1,
                            hv270 == "poorer" | hv270 == "second" ~ 2,
                            hv270 == "middle" ~ 3,
                            hv270 == "richer" | hv270 == "fourth" ~ 4,
                            hv270 == "richest" | hv270 == "highest" ~ 5),
         prebirthinterval = case_when(hc63 == "missing" ~ NA_real_),
         timetowater = case_when(hv204 == "on premises" ~ 0,
                                 hv204 == "don't know" ~ NA_real_,
                                 hv204 == "missing" ~ NA_real_,
                                 hv204 == "more than 12 hours" ~ 720,
                                 hv204 == "995+" ~ 995))

# Confirm that weighted.mean() is appropriate for dhs weights 
df_agg <-
  df_m %>% 
  group_by(NAME_0, NAME_1, SurveyId) %>% 
  summarise(wasted_prev = weighted.mean(whz2, hv005 / 1000000, na.rm = T),
            stunted_prev = weighted.mean(haz2,  hv005 / 1000000, na.rm = T),
            under_prev = weighted.mean(waz2, hv005 / 1000000, na.rm = T),
            
            first_month = min(ymd),
            last_month = max(ymd),
            mean_month = mean(ymd),
            median_month = median(ymd),
            
            n = n())




# Add MICS and SMART for Nigeria and Kenya ------------------------------------

# MICS
df_mics <- haven::read_dta(here::here("Data", "SMART_DHS_MICS_adm1_month.dta")) %>% 
  filter(country == "Kenya" | country == "Nigeria") %>% 
  filter(MICS == 1) %>% 
  arrange(country, adm1, year, month) %>% 
  mutate(SurveyId = case_when(year == 2007 ~ paste0(year, "MICS"), 
                              year == 2009 ~ paste0(year, "MICS"),
                              year == 2011 ~ paste0(year, "MICS"),
                              year == 2013 | year == 2014 ~ paste0(2013, "MICS")),
         
         ymd = lubridate::ymd(paste0(year, "-", month, "-01"))
         ) %>% 
  group_by(country, adm1, SurveyId) %>% 
  summarise(wasted_prev = weighted.mean(whz2, respondents, na.rm = T),
            stunted_prev = weighted.mean(haz2,  respondents, na.rm = T),
            under_prev = weighted.mean(waz2, respondents, na.rm = T),
            
            first_month = min(ymd),
            last_month = max(ymd),
            mean_month = mean(ymd),
            median_month = median(ymd),
            n = sum(respondents))

# SMART
df_smart <- haven::read_dta(here::here("Data", "SMART_DHS_MICS_adm1_month.dta")) %>% 
  filter(country == "Kenya" | country == "Nigeria") %>% 
  filter(SMART == 1) %>% 
  arrange(country, adm1, year, month) %>% 
  mutate(SurveyId =  paste0(year, "SMART"),
         ymd = lubridate::ymd(paste0(year, "-", month, "-01"))
  ) %>% 
  group_by(country, adm1, SurveyId) %>% 
  summarise(wasted_prev = weighted.mean(whz2, respondents, na.rm = T),
            stunted_prev = weighted.mean(haz2,  respondents, na.rm = T),
            under_prev = weighted.mean(waz2, respondents, na.rm = T),
            
            first_month = min(ymd),
            last_month = max(ymd),
            mean_month = mean(ymd),
            median_month = median(ymd),
            n = sum(respondents))


df_alts <- bind_rows(df_mics, df_smart) %>% 
  rename(NAME_0 = country,
         NAME_1 = adm1)

df_full <- bind_rows(df_agg, df_alts) %>% 
  arrange(NAME_1, SurveyId)


# Climate -----------------------------------------------------------------
load(here::here("Data", "df_climate_out.Rdata"))
df_climate_out <- df_climate_out %>% 
  select(-ends_with("month"))

df_aggc <- 
  df_full %>% 
  left_join(df_climate_out, by = c("NAME_0", "NAME_1",
                                   "SurveyId"))


# Conflict ----------------------------------------------------------------
load(here::here("Data", "sf_melt_final.Rdata"))

df_conframe <-
  df_m %>% 
  group_by(cc, NAME_0, NAME_1, month, year, SurveyId, ymd) %>% 
  summarise(n = n())

# Create ADM1-surveymonth frame
df_aggcon <- 
  df_conframe %>% 
  left_join(sf_melt_final, by = c("NAME_0", "NAME_1", "ymd"))

# Need to take the weighted average for each adm1-survey wave of each indicator
tempvars <- 
  df_aggcon %>% 
  group_by(cc, NAME_0, NAME_1, SurveyId) %>% 
  mutate(
    
    # grab various month tags
    first_month = min(ymd),
    last_month = max(ymd),
    mean_month = mean(ymd),
    median_month = median(ymd)
  ) 


# Yes, I should've used summarise_at() but I'm pot committed now
df_dhs_conflict <- 
  tempvars %>% 
  group_by(cc, NAME_0, NAME_1, SurveyId) %>% 
  summarise(
    # grab months
    first_month = min(ymd),
    last_month = max(ymd),
    mean_month = mean(ymd),
    median_month = median(ymd),
    
    # violent events
    violent_events = weighted.mean(violent_events, n),
    violent_events_min = min(violent_events),
    violent_events_max = max(violent_events),
    
    violent_events_lag1 = weighted.mean(violent_events_lag1, n),
    violent_events_lag1_min = min(violent_events_lag1),
    violent_events_lag1_max = max(violent_events_lag1),
    
    violent_events_lag2 = weighted.mean(violent_events_lag2, n),
    violent_events_lag2_min = min(violent_events_lag2),
    violent_events_lag2_max = max(violent_events_lag2),
    
    violent_events_lag3 = weighted.mean(violent_events_lag3, n),
    violent_events_lag3_min = min(violent_events_lag3),
    violent_events_lag3_max = max(violent_events_lag3),
    
    violent_events_lag4 = weighted.mean(violent_events_lag4, n),
    violent_events_lag4_min = min(violent_events_lag4),
    violent_events_lag4_max = max(violent_events_lag4),
    
    violent_events_lag5 = weighted.mean(violent_events_lag5, n),
    violent_events_lag5_min = min(violent_events_lag5),
    violent_events_lag5_max = max(violent_events_lag5),
    
    violent_events_lag6 = weighted.mean(violent_events_lag6, n),
    violent_events_lag6_min = min(violent_events_lag6),
    violent_events_lag6_max = max(violent_events_lag6),
    
    violent_events_lag7 = weighted.mean(violent_events_lag7, n),
    violent_events_lag7_min = min(violent_events_lag7),
    violent_events_lag7_max = max(violent_events_lag7),
    
    violent_events_lag8 = weighted.mean(violent_events_lag8, n),
    violent_events_lag8_min = min(violent_events_lag8),
    violent_events_lag8_max = max(violent_events_lag8),
    
    violent_events_lag9 = weighted.mean(violent_events_lag9, n),
    violent_events_lag9_min = min(violent_events_lag9),
    violent_events_lag9_max = max(violent_events_lag9),
    
    violent_events_lag10 = weighted.mean(violent_events_lag10, n),
    violent_events_lag10_min = min(violent_events_lag10),
    violent_events_lag10_max = max(violent_events_lag10),
    
    violent_events_lag11 = weighted.mean(violent_events_lag11, n),
    violent_events_lag11_min = min(violent_events_lag11),
    violent_events_lag11_max = max(violent_events_lag11),
    
    violent_events_lag12 = weighted.mean(violent_events_lag12, n),
    violent_events_lag12_min = min(violent_events_lag12),
    violent_events_lag12_max = max(violent_events_lag12),
    
    violent_events_lag13 = weighted.mean(violent_events_lag13, n),
    violent_events_lag13_min = min(violent_events_lag13),
    violent_events_lag13_max = max(violent_events_lag13),
    
    violent_events_lag14 = weighted.mean(violent_events_lag14, n),
    violent_events_lag14_min = min(violent_events_lag14),
    violent_events_lag14_max = max(violent_events_lag14),
    
    violent_events_lag15 = weighted.mean(violent_events_lag15, n),
    violent_events_lag15_min = min(violent_events_lag15),
    violent_events_lag15_max = max(violent_events_lag15),
    
    violent_events_lag16 = weighted.mean(violent_events_lag16, n),
    violent_events_lag16_min = min(violent_events_lag16),
    violent_events_lag16_max = max(violent_events_lag16),
    
    violent_events_lag17 = weighted.mean(violent_events_lag17, n),
    violent_events_lag17_min = min(violent_events_lag17),
    violent_events_lag17_max = max(violent_events_lag17),
    
    violent_events_lag18 = weighted.mean(violent_events_lag18, n),
    violent_events_lag18_min = min(violent_events_lag18),
    violent_events_lag18_max = max(violent_events_lag18),
    
    violent_events_lag19 = weighted.mean(violent_events_lag19, n),
    violent_events_lag19_min = min(violent_events_lag19),
    violent_events_lag19_max = max(violent_events_lag19),
    
    violent_events_lag20 = weighted.mean(violent_events_lag20, n),
    violent_events_lag20_min = min(violent_events_lag20),
    violent_events_lag20_max = max(violent_events_lag20),
    
    violent_events_lag21 = weighted.mean(violent_events_lag21, n),
    violent_events_lag21_min = min(violent_events_lag21),
    violent_events_lag22_max = max(violent_events_lag21),
    
    violent_events_lag22 = weighted.mean(violent_events_lag22, n),
    violent_events_lag22_min = min(violent_events_lag22),
    violent_events_lag22_max = max(violent_events_lag22),
    
    violent_events_lag23 = weighted.mean(violent_events_lag23, n),
    violent_events_lag23_min = min(violent_events_lag23),
    violent_events_lag23_max = max(violent_events_lag23),
    
    violent_events_lag24 = weighted.mean(violent_events_lag24, n),
    violent_events_lag24_min = min(violent_events_lag24),
    violent_events_lag24_max = max(violent_events_lag24)
    
  )




# MICS and SMART
df_mics <- haven::read_dta(here::here("Data", "SMART_DHS_MICS_adm1_month.dta")) %>% 
  filter(country == "Kenya" | country == "Nigeria") %>% 
  filter(MICS == 1) %>% 
  arrange(country, adm1, year, month) %>% 
  mutate(SurveyId = case_when(year == 2007 ~ paste0(year, "MICS"), 
                              year == 2009 ~ paste0(year, "MICS"),
                              year == 2011 ~ paste0(year, "MICS"),
                              year == 2013 | year == 2014 ~ paste0(2013, "MICS")),
         
         ymd = lubridate::ymd(paste0(year, "-", month, "-01"))
  ) %>% 
  group_by(country, adm1, SurveyId, month, year, ymd) %>% 
  summarise(n = sum(respondents))

# SMART
df_smart <- haven::read_dta(here::here("Data", "SMART_DHS_MICS_adm1_month.dta")) %>% 
  filter(country == "Kenya" | country == "Nigeria") %>% 
  filter(SMART == 1) %>% 
  arrange(country, adm1, year, month, month, year) %>% 
  mutate(SurveyId =  paste0(year, "SMART"),
         ymd = lubridate::ymd(paste0(year, "-", month, "-01"))
  ) %>% 
  group_by(country, adm1, SurveyId, month, year, ymd) %>% 
  summarise(n = sum(respondents))


df_alts <- bind_rows(df_mics, df_smart) %>% 
  rename(NAME_0 = country,
         NAME_1 = adm1)

df_alts_conflict <- 
  df_alts %>% 
  left_join(sf_melt_final, by = c("NAME_0", "NAME_1", "ymd")) %>% 
  group_by(NAME_0, NAME_1, SurveyId) %>% 
  summarise(
    # grab months
    first_month = min(ymd),
    last_month = max(ymd),
    mean_month = mean(ymd),
    median_month = median(ymd),
    
    # violent events
    violent_events = weighted.mean(violent_events, n),
    violent_events_min = min(violent_events),
    violent_events_max = max(violent_events),
    
    violent_events_lag1 = weighted.mean(violent_events_lag1, n),
    violent_events_lag1_min = min(violent_events_lag1),
    violent_events_lag1_max = max(violent_events_lag1),
    
    violent_events_lag2 = weighted.mean(violent_events_lag2, n),
    violent_events_lag2_min = min(violent_events_lag2),
    violent_events_lag2_max = max(violent_events_lag2),
    
    violent_events_lag3 = weighted.mean(violent_events_lag3, n),
    violent_events_lag3_min = min(violent_events_lag3),
    violent_events_lag3_max = max(violent_events_lag3),
    
    violent_events_lag4 = weighted.mean(violent_events_lag4, n),
    violent_events_lag4_min = min(violent_events_lag4),
    violent_events_lag4_max = max(violent_events_lag4),
    
    violent_events_lag5 = weighted.mean(violent_events_lag5, n),
    violent_events_lag5_min = min(violent_events_lag5),
    violent_events_lag5_max = max(violent_events_lag5),
    
    violent_events_lag6 = weighted.mean(violent_events_lag6, n),
    violent_events_lag6_min = min(violent_events_lag6),
    violent_events_lag6_max = max(violent_events_lag6),
    
    violent_events_lag7 = weighted.mean(violent_events_lag7, n),
    violent_events_lag7_min = min(violent_events_lag7),
    violent_events_lag7_max = max(violent_events_lag7),
    
    violent_events_lag8 = weighted.mean(violent_events_lag8, n),
    violent_events_lag8_min = min(violent_events_lag8),
    violent_events_lag8_max = max(violent_events_lag8),
    
    violent_events_lag9 = weighted.mean(violent_events_lag9, n),
    violent_events_lag9_min = min(violent_events_lag9),
    violent_events_lag9_max = max(violent_events_lag9),
    
    violent_events_lag10 = weighted.mean(violent_events_lag10, n),
    violent_events_lag10_min = min(violent_events_lag10),
    violent_events_lag10_max = max(violent_events_lag10),
    
    violent_events_lag11 = weighted.mean(violent_events_lag11, n),
    violent_events_lag11_min = min(violent_events_lag11),
    violent_events_lag11_max = max(violent_events_lag11),
    
    violent_events_lag12 = weighted.mean(violent_events_lag12, n),
    violent_events_lag12_min = min(violent_events_lag12),
    violent_events_lag12_max = max(violent_events_lag12),
    
    violent_events_lag13 = weighted.mean(violent_events_lag13, n),
    violent_events_lag13_min = min(violent_events_lag13),
    violent_events_lag13_max = max(violent_events_lag13),
    
    violent_events_lag14 = weighted.mean(violent_events_lag14, n),
    violent_events_lag14_min = min(violent_events_lag14),
    violent_events_lag14_max = max(violent_events_lag14),
    
    violent_events_lag15 = weighted.mean(violent_events_lag15, n),
    violent_events_lag15_min = min(violent_events_lag15),
    violent_events_lag15_max = max(violent_events_lag15),
    
    violent_events_lag16 = weighted.mean(violent_events_lag16, n),
    violent_events_lag16_min = min(violent_events_lag16),
    violent_events_lag16_max = max(violent_events_lag16),
    
    violent_events_lag17 = weighted.mean(violent_events_lag17, n),
    violent_events_lag17_min = min(violent_events_lag17),
    violent_events_lag17_max = max(violent_events_lag17),
    
    violent_events_lag18 = weighted.mean(violent_events_lag18, n),
    violent_events_lag18_min = min(violent_events_lag18),
    violent_events_lag18_max = max(violent_events_lag18),
    
    violent_events_lag19 = weighted.mean(violent_events_lag19, n),
    violent_events_lag19_min = min(violent_events_lag19),
    violent_events_lag19_max = max(violent_events_lag19),
    
    violent_events_lag20 = weighted.mean(violent_events_lag20, n),
    violent_events_lag20_min = min(violent_events_lag20),
    violent_events_lag20_max = max(violent_events_lag20),
    
    violent_events_lag21 = weighted.mean(violent_events_lag21, n),
    violent_events_lag21_min = min(violent_events_lag21),
    violent_events_lag22_max = max(violent_events_lag21),
    
    violent_events_lag22 = weighted.mean(violent_events_lag22, n),
    violent_events_lag22_min = min(violent_events_lag22),
    violent_events_lag22_max = max(violent_events_lag22),
    
    violent_events_lag23 = weighted.mean(violent_events_lag23, n),
    violent_events_lag23_min = min(violent_events_lag23),
    violent_events_lag23_max = max(violent_events_lag23),
    
    violent_events_lag24 = weighted.mean(violent_events_lag24, n),
    violent_events_lag24_min = min(violent_events_lag24),
    violent_events_lag24_max = max(violent_events_lag24)
    
    
  )




# Export data -------------------------------------------------------------

df <-
  df_dhs_conflict %>% 
  bind_rows(df_alts_conflict) %>% 
  ungroup() %>% 
  select(-cc) %>% 
  full_join(df_aggc) 

save(file = here::here("Data", "df.Rdata"), df)



