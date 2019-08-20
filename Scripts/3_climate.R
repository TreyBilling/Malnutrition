library(lubridate)
library(tidyverse)
library(haven)
library(gridExtra)

# Load and clean climate data ---------------------------------------------

# Region names
mb_regions <- read_csv(here::here("mb_regions.csv")) %>% 
  # Create columns for country and region -- they are not all adm1s
  separate(ID_LINK1, c("country", "region"), "_") %>% 
  filter(UniqueID != 0)

# NDVI
# July 2002 to June 2017
mb_ndvi_mean <- read_csv(here::here("NDVI", "Gallup_MODIS_mean.csv"),
                         col_names = T)

mb_ndvi_mean <- mb_ndvi_mean %>% select(-1, -2) 

colnames(mb_ndvi_mean) <- 
  seq(lubridate::ymd('2002-07-01'), 
      lubridate::ymd('2015-06-01'), 
      by = 'months')

mb_ndvi_mean <- mb_ndvi_mean %>% mutate(UniqueID = as.numeric(row.names(.))) %>% 
  full_join(., mb_regions, by = "UniqueID") %>% 
  gather(ymd, ndvi_mean, -c(UniqueID, country, region, ADMIN)) %>% 
  mutate(ndvi_mean = ifelse(ndvi_mean == Inf, NA, ndvi_mean))


# CHIRPS
# January 1981 to August 2017
mb_chirps <- read_csv(here::here("CHIRPS", "Gallup_Chirps2.0_mean_9.17.csv"),
                      col_names = F)

# January 2017 is missing, so just cut out 2017 to start
mb_chirps <- mb_chirps[1:432]

colnames(mb_chirps) <- seq(ymd('1981-01-01'),
                           ymd('2016-12-01'), 
                           by = 'months')
mb_chirps <- mb_chirps %>% mutate(UniqueID = as.numeric(row.names(.))) %>% 
  full_join(., mb_regions, by = "UniqueID") %>% 
  gather(ymd, chirps, -c(UniqueID, country, region, ADMIN)) %>% 
  mutate(chirps = ifelse(chirps == "NaN", NA, chirps))



# CHIRTS
# January 1983 to March 2017
mb_chirts <- read_csv(here::here("CHIRTS", "Gallup_Chirts1.0_mean_4.19.csv"),
                      col_names = F)

colnames(mb_chirts) <- seq(ymd('1983-01-01'),
                           ymd('2016-12-01'), 
                           by = 'months') 

mb_chirts <- mb_chirts %>% mutate(UniqueID = as.numeric(row.names(.))) %>% 
  full_join(., mb_regions, by = "UniqueID") %>% 
  gather(ymd, chirts, -c(UniqueID, country, region, ADMIN)) %>% 
  mutate(chirts = ifelse(chirts == "NaN", NA, chirts))





# Match regions -----------------------------------------------------------

# Fn to find the the mismatches between Molly's and my adm1 names

name_finder <- function(country, mb_data, my_data) {
  
  mb_names <- mb_data$region[mb_data$country == country]
  my_names <- my_data$adm1[my_data$country == country] %>% unique()
  list((mb_names[-which(mb_names %in% my_names)]), 
       my_names[-which(my_names %in% mb_names)])
  
}



# Nigeria
### Anambra not in MB data. Not sure the cause of this miss. Others ok ###
nigeria_misses <- name_finder(country = "Nigeria",
                              mb_data = mb_regions, 
                              my_data = df_ndvi)
nigeria_misses
# Changes -- FCT, Abuja = "Federal Capital Territory"


# Kenya
### All misses line up logically
kenya_misses <- name_finder(country = "Kenya", 
                            mb_data = mb_regions, 
                            my_data = df_ndvi)
kenya_misses
# Changes -- "Central Kisii" == "Kisii", "Marakwet" == "Elgeyo-Marakwet", 
#           "Meru North" == "Meru", "Muranga" == "Murang'a", 
#            "Nandi North" == "Nandi, "Tharaka" == "Tharak-Nithi" 



# Combine -----------------------------------------------------------------
# For clarity with merges, subset to our 3 countries for now

chirts_merge <- mb_chirts %>% 
  filter(country == "Nigeria" | country == "Uganda" | country == "Kenya")

chirps_merge <- mb_chirps %>% 
  filter(country == "Nigeria" | country == "Uganda" | country == "Kenya")

ndvi_merge <- mb_ndvi_mean %>% 
  filter(country == "Nigeria" | country == "Uganda" | country == "Kenya")

# Join and correct mismatched names
df_climate <- full_join(chirts_merge, 
                        chirps_merge, 
                        by = c("UniqueID",
                               "country",
                               "region",
                               "ADMIN",
                               "ymd")) %>% 
  full_join(ndvi_merge, by = c("UniqueID",
                               "country",
                               "region",
                               "ADMIN",
                               "ymd")) %>% 
  mutate(adm1 = region,
         adm1 = case_when(adm1 == "FCT, Abuja" ~ "Federal Capital Territory", # Line up Nigeria adm1 names
                          adm1 == "Central Kisii" ~ "Kisii",                  # Line up Kenya adm1 names
                          adm1 == "Marakwet" ~ "Elgeyo-Marakwet",
                          adm1 == "Meru North" ~ "Meru",
                          adm1 == "Muranga" ~ "Murang'a",
                          adm1 == "Nandi North" ~ "Nandi",
                          adm1 == "Tharaka" ~ "Tharaka-Nithi",
                          TRUE ~ adm1),
         ymd = lubridate::ymd(ymd)
  ) %>% 
  arrange(country, adm1, ymd) 





# Create lags of base indicators -----------------------------------------------
# Creating lags via nested lists is slow -- find better way in the future

df_climate <- 
  df_climate %>% 
  gather(select = (c("ndvi_mean"))) %>% 
  mutate(n_lag = list(c(0:14, 24, 36, 48, 60))) %>% # lags up to 5 years
  unnest() %>% 
  arrange(UniqueID, country, region, ADMIN, key, n_lag, ymd) %>% 
  group_by(UniqueID, country, region, ADMIN, key, n_lag) %>% 
  mutate(lag_value = lag(value, n_lag[1])) %>% 
  ungroup() %>% 
  mutate(var_name = ifelse(n_lag == 0, key, paste0("lag_", key, "_", n_lag))) %>% 
  select(-c(key, value, n_lag)) %>% 
  spread(var_name, lag_value) 

df_climate <- 
  df_climate %>% 
  gather(select = (c("chirts"))) %>% 
  mutate(n_lag = list(c(0:14, 24, 36, 48, 60))) %>% # lags up to 5 years
  unnest() %>% 
  arrange(UniqueID, country, region, ADMIN, key, n_lag, ymd) %>% 
  group_by(UniqueID, country, region, ADMIN, key, n_lag) %>% 
  mutate(lag_value = lag(value, n_lag[1])) %>% 
  ungroup() %>% 
  mutate(var_name = ifelse(n_lag == 0, key, paste0("lag_", key, "_", n_lag))) %>% 
  select(-c(key, value, n_lag)) %>% 
  spread(var_name, lag_value) 

df_climate <- 
  df_climate %>% 
  gather(select = (c("chirps"))) %>% 
  mutate(n_lag = list(c(0:14, 24, 36, 48, 60))) %>% # lags up to 5 years
  unnest() %>% 
  arrange(UniqueID, country, region, ADMIN, key, n_lag, ymd) %>% 
  group_by(UniqueID, country, region, ADMIN, key, n_lag) %>% 
  mutate(lag_value = lag(value, n_lag[1])) %>% 
  ungroup() %>% 
  mutate(var_name = ifelse(n_lag == 0, key, paste0("lag_", key, "_", n_lag))) %>% 
  select(-c(key, value, n_lag)) %>% 
  spread(var_name, lag_value) 



# Five year moving averages and deviations -------------------------------------

df_climate <- 
  df_climate %>% 
  mutate(chirts_5avg = (lag_chirts_12 + lag_chirts_24 + lag_chirts_36 +
                          lag_chirts_48 + lag_chirts_60) / 5,
         chirps_5avg = (lag_chirps_12 + lag_chirps_24 + lag_chirps_36 +
                          lag_chirps_48 + lag_chirps_60) / 5,
         ndvi_mean_5avg = (lag_ndvi_mean_12 + lag_ndvi_mean_24 + lag_ndvi_mean_36 +
                             lag_ndvi_mean_48 + lag_ndvi_mean_60) / 5,
         
         chirts_dev5 = chirts - chirts_5avg,
         chirps_dev5 = chirps - chirps_5avg,
         ndvi_dev5 = ndvi_mean - ndvi_mean_5avg
  )




# Growing season 3 month runners  ----------------------------------------------

# Need to highest average consecutive months in last year
# Hacky fix -- create 3 month means for each 3 consecutive month combo
# Use pmax() to choose the highest among all options
# To scope within last year, need to extend to 14 months or miss the appearance of month 12 twice

df_climate <- df_climate %>% 
  group_by(adm1) %>% 
  mutate(
    # ndvi
    ndvi123m = (lag_ndvi_mean_1 + lag_ndvi_mean_2 + lag_ndvi_mean_3) / 3,
    ndvi234m = (lag_ndvi_mean_2 + lag_ndvi_mean_3 + lag_ndvi_mean_4) / 3,
    ndvi345m = (lag_ndvi_mean_3 + lag_ndvi_mean_4 + lag_ndvi_mean_5) / 3,
    ndvi456m = (lag_ndvi_mean_4 + lag_ndvi_mean_5 + lag_ndvi_mean_6) / 3,
    ndvi567m = (lag_ndvi_mean_5 + lag_ndvi_mean_6 + lag_ndvi_mean_7) / 3,
    ndvi678m = (lag_ndvi_mean_6 + lag_ndvi_mean_7 + lag_ndvi_mean_8) / 3,
    ndvi789m = (lag_ndvi_mean_7 + lag_ndvi_mean_8 + lag_ndvi_mean_9) / 3,
    ndvi8910m = (lag_ndvi_mean_8 + lag_ndvi_mean_9 + lag_ndvi_mean_10) / 3,
    ndvi91011m = (lag_ndvi_mean_9 + lag_ndvi_mean_10 + lag_ndvi_mean_11) / 3,
    ndvi10112m = (lag_ndvi_mean_10 + lag_ndvi_mean_11 + lag_ndvi_mean_12) / 3,
    ndvi111213m = (lag_ndvi_mean_11 + lag_ndvi_mean_12 + lag_ndvi_mean_13) / 3,
    ndvi121314m = (lag_ndvi_mean_12 + lag_ndvi_mean_13 + lag_ndvi_mean_14) / 3,
    
    ndvi_3green12 = pmax(ndvi123m, ndvi234m, ndvi345m, ndvi456m, ndvi567m, 
                         ndvi678m, ndvi789m, ndvi8910m, ndvi91011m, ndvi10112m),
    
    ndvi_3green14 = pmax(ndvi123m, ndvi234m, ndvi345m, ndvi456m, ndvi567m, 
                         ndvi678m, ndvi789m, ndvi8910m, ndvi91011m, 
                         ndvi10112m, ndvi111213m, ndvi121314m),
    
    # chirts
    chirts123 = (lag_chirts_1 + lag_chirts_2 + lag_chirts_3) / 3,
    chirts234 = (lag_chirts_2 + lag_chirts_3 + lag_chirts_4) / 3,
    chirts345 = (lag_chirts_3 + lag_chirts_4 + lag_chirts_5) / 3,
    chirts456 = (lag_chirts_4 + lag_chirts_5 + lag_chirts_6) / 3,
    chirts567 = (lag_chirts_5 + lag_chirts_6 + lag_chirts_7) / 3,
    chirts678 = (lag_chirts_6 + lag_chirts_7 + lag_chirts_8) / 3,
    chirts789 = (lag_chirts_7 + lag_chirts_8 + lag_chirts_9) / 3,
    chirts8910 = (lag_chirts_8 + lag_chirts_9 + lag_chirts_10) / 3,
    chirts91011 = (lag_chirts_9 + lag_chirts_10 + lag_chirts_11) / 3,
    chirts10112 = (lag_chirts_10 + lag_chirts_11 + lag_chirts_12) / 3,
    chirts111213 = (lag_chirts_11 + lag_chirts_12 + lag_chirts_13) / 3,
    chirts121314 = (lag_chirts_12 + lag_chirts_13 + lag_chirts_14) / 3,
    chirts_3hot12 = pmax(chirts123, chirts234 ,chirts345, chirts456, chirts567, 
                         chirts678, chirts789, chirts8910, chirts91011, chirts10112),
    chirts_3hot14 = pmax(chirts123, chirts234 ,chirts345, chirts456, chirts567, 
                         chirts678, chirts789, chirts8910, chirts91011, chirts10112,
                         chirts111213, chirts121314),
    # chirps       
    
    chirps123 = (lag_chirps_1 + lag_chirps_2 + lag_chirps_3) / 3,
    chirps234 = (lag_chirps_2 + lag_chirps_3 + lag_chirps_4) / 3,
    chirps345 = (lag_chirps_3 + lag_chirps_4 + lag_chirps_5) / 3,
    chirps456 = (lag_chirps_4 + lag_chirps_5 + lag_chirps_6) / 3,
    chirps567 = (lag_chirps_5 + lag_chirps_6 + lag_chirps_7) / 3,
    chirps678 = (lag_chirps_6 + lag_chirps_7 + lag_chirps_8) / 3,
    chirps789 = (lag_chirps_7 + lag_chirps_8 + lag_chirps_9) / 3,
    chirps8910 = (lag_chirps_8 + lag_chirps_9 + lag_chirps_10) / 3,
    chirps91011 = (lag_chirps_9 + lag_chirps_10 + lag_chirps_11) / 3,
    chirps10112 = (lag_chirps_10 + lag_chirps_11 + lag_chirps_12) / 3,
    chirps111213 = (lag_chirps_11 + lag_chirps_12 + lag_chirps_13) / 3,
    chirps121314 = (lag_chirps_12 + lag_chirps_13 + lag_chirps_14) / 3,
    chirps_3hot12 = pmax(chirps123, chirps234 ,chirps345, chirps456, chirps567, 
                         chirps678, chirps789, chirps8910, chirps91011, chirps10112),
    chirps_3rainy14 = pmax(chirps123, chirps234 ,chirps345, chirps456, chirps567, 
                           chirps678, chirps789, chirps8910, chirps91011, chirps10112,
                           chirps111213, chirps121314)
    
  )




# Create growing season anamolies -----------------------------------------

# Lags for growing season indicators -- break into two chunks bc memory
df_climate <- df_climate %>% 
  gather(select = (c("chirts_3hot14"))) %>% 
  mutate(n_lag = list(c(0:12, 24, 36, 48, 60))) %>% 
  unnest() %>% 
  arrange(adm1, country, key, n_lag, ymd) %>% 
  group_by(adm1, country, key, n_lag) %>% 
  mutate(lag_value = lag(value, n_lag[1])) %>% 
  ungroup() %>% 
  mutate(var_name = ifelse(n_lag == 0, key, paste0("lag_", key, "_", n_lag))) %>% 
  select(-c(key, value, n_lag)) %>% 
  spread(var_name, lag_value)


df_climate <- df_climate %>% 
  gather(select = (c("chirps_3rainy14", "ndvi_3green14"))) %>% 
  mutate(n_lag = list(c(0:12, 24, 36, 48, 60))) %>% # lags up to 5 years
  unnest() %>% 
  arrange(adm1, country, key, n_lag, ymd) %>% 
  group_by(adm1, country, key, n_lag) %>% 
  mutate(lag_value = lag(value, n_lag[1])) %>% 
  ungroup() %>% 
  mutate(var_name = ifelse(n_lag == 0, key, paste0("lag_", key, "_", n_lag))) %>% 
  select(-c(key, value, n_lag)) %>% 
  spread(var_name, lag_value)


# 5 year moving average and deviations

df_climate <-
  df_climate %>% 
  mutate(chirts_3hot14_5avg = (lag_chirts_3hot14_12 + lag_chirts_3hot14_24 +
                                 lag_chirts_3hot14_36 + lag_chirts_3hot14_48 + 
                                 lag_chirts_3hot14_60) / 5,
         
         chirps_3rainy14_5avg = (lag_chirps_3rainy14_12 + lag_chirps_3rainy14_24 +
                                   lag_chirps_3rainy14_36 + lag_chirps_3rainy14_48 + 
                                   lag_chirps_3rainy14_60) / 5,
         
         ndvi_3green14_5avg = (lag_ndvi_3green14_12 + lag_ndvi_3green14_24 +
                                 lag_ndvi_3green14_36 + lag_ndvi_3green14_48 + 
                                 lag_ndvi_3green14_60) / 5,
         
         chirts_3hot14_dev5 = chirts_3hot14 - chirts_3hot14_5avg,
         chirps_3rainy14_dev5 = chirps_3rainy14 - chirps_3rainy14_5avg,
         ndvi_3green14_dev5 = ndvi_3green14 - ndvi_3green14_5avg)




# Time from peak indicator ------------------------------------------------

# tfp = time from peak
df_climate <- df_climate %>% 
  mutate(
    # chirts
    peak_chirts = pmax(lag_chirts_1, lag_chirts_2, lag_chirts_3,
                       lag_chirts_2, lag_chirts_3, lag_chirts_4,
                       lag_chirts_3, lag_chirts_4, lag_chirts_5,
                       lag_chirts_4, lag_chirts_5, lag_chirts_6,
                       lag_chirts_5, lag_chirts_6, lag_chirts_7,
                       lag_chirts_6, lag_chirts_7, lag_chirts_8,
                       lag_chirts_7, lag_chirts_8, lag_chirts_9,
                       lag_chirts_10, lag_chirts_11, lag_chirts_12,
                       lag_chirts_13, lag_chirts_14),
    tfp_chirts = case_when(peak_chirts == lag_chirts_1 ~ 1,
                           peak_chirts == lag_chirts_2 ~ 2,
                           peak_chirts == lag_chirts_3 ~ 3,
                           peak_chirts == lag_chirts_4 ~ 4,
                           peak_chirts == lag_chirts_5 ~ 5,
                           peak_chirts == lag_chirts_6 ~ 6,
                           peak_chirts == lag_chirts_7 ~ 7,
                           peak_chirts == lag_chirts_8 ~ 8,
                           peak_chirts == lag_chirts_9 ~ 9,
                           peak_chirts == lag_chirts_10 ~ 10,
                           peak_chirts == lag_chirts_11 ~ 11,
                           peak_chirts == lag_chirts_12 ~ 12,
                           peak_chirts == lag_chirts_13 ~ 13,
                           peak_chirts == lag_chirts_14 ~ 14),
    
    # chirps
    peak_chirps = pmax(lag_chirps_1, lag_chirps_2, lag_chirps_3,
                       lag_chirps_2, lag_chirps_3, lag_chirps_4,
                       lag_chirps_3, lag_chirps_4, lag_chirps_5,
                       lag_chirps_4, lag_chirps_5, lag_chirps_6,
                       lag_chirps_5, lag_chirps_6, lag_chirps_7,
                       lag_chirps_6, lag_chirps_7, lag_chirps_8,
                       lag_chirps_7, lag_chirps_8, lag_chirps_9,
                       lag_chirps_10, lag_chirps_11, lag_chirps_12,
                       lag_chirps_13, lag_chirps_14),
    tfp_chirps = case_when(peak_chirps == lag_chirps_1 ~ 1,
                           peak_chirps == lag_chirps_2 ~ 2,
                           peak_chirps == lag_chirps_3 ~ 3,
                           peak_chirps == lag_chirps_4 ~ 4,
                           peak_chirps == lag_chirps_5 ~ 5,
                           peak_chirps == lag_chirps_6 ~ 6,
                           peak_chirps == lag_chirps_7 ~ 7,
                           peak_chirps == lag_chirps_8 ~ 8,
                           peak_chirps == lag_chirps_9 ~ 9,
                           peak_chirps == lag_chirps_10 ~ 10,
                           peak_chirps == lag_chirps_11 ~ 11,
                           peak_chirps == lag_chirps_12 ~ 12,
                           peak_chirps == lag_chirps_13 ~ 13,
                           peak_chirps == lag_chirps_14 ~ 14),
    
    # ndvi
    peak_ndvi = pmax(lag_ndvi_mean_1, lag_ndvi_mean_2, lag_ndvi_mean_3,
                     lag_ndvi_mean_2, lag_ndvi_mean_3, lag_ndvi_mean_4,
                     lag_ndvi_mean_3, lag_ndvi_mean_4, lag_ndvi_mean_5,
                     lag_ndvi_mean_4, lag_ndvi_mean_5, lag_ndvi_mean_6,
                     lag_ndvi_mean_5, lag_ndvi_mean_6, lag_ndvi_mean_7,
                     lag_ndvi_mean_6, lag_ndvi_mean_7, lag_ndvi_mean_8,
                     lag_ndvi_mean_7, lag_ndvi_mean_8, lag_ndvi_mean_9,
                     lag_ndvi_mean_10, lag_ndvi_mean_11, lag_ndvi_mean_12,
                     lag_ndvi_mean_13, lag_ndvi_mean_14),
    tfp_ndvi = case_when(peak_ndvi == lag_ndvi_mean_1 ~ 1,
                         peak_ndvi == lag_ndvi_mean_2 ~ 2,
                         peak_ndvi == lag_ndvi_mean_3 ~ 3,
                         peak_ndvi == lag_ndvi_mean_4 ~ 4,
                         peak_ndvi == lag_ndvi_mean_5 ~ 5,
                         peak_ndvi == lag_ndvi_mean_6 ~ 6,
                         peak_ndvi == lag_ndvi_mean_7 ~ 7,
                         peak_ndvi == lag_ndvi_mean_8 ~ 8,
                         peak_ndvi == lag_ndvi_mean_9 ~ 9,
                         peak_ndvi == lag_ndvi_mean_10 ~ 10,
                         peak_ndvi == lag_ndvi_mean_11 ~ 11,
                         peak_ndvi == lag_ndvi_mean_12 ~ 12,
                         peak_ndvi == lag_ndvi_mean_13 ~ 13,
                         peak_ndvi == lag_ndvi_mean_14 ~ 14)
    
    
  )




# Match to ADM1-month from DHS --------------------------------------------
load(here::here("Data", "dhs_working.Rdata"))
#load(here::here("Data", "df_climate.Rdata"))

# Remove values that are missing country locations
df_m <- filter(df_m, !is.na(NAME_0))

df_agg <-
  df_m %>% 
  group_by(cc, NAME_0, NAME_1, month, year, SurveyId, ymd) %>% 
  summarise(n = n())


# Create ADM1-surveymonth frame
df_aggc <- 
  df_agg %>% 
  left_join(df_climate, by = c("NAME_0" = "country",
                               "NAME_1" = "adm1",
                               "ymd"))

# Need to take the weighted average for each adm1-survey wave of each indicator
tempvars <- 
  df_aggc %>% 
  group_by(cc, NAME_0, NAME_1, SurveyId) %>% 
  mutate(
    
    # grab various month tags
    first_month = min(ymd),
    last_month = max(ymd),
    mean_month = mean(ymd),
    median_month = median(ymd)
  ) 
  
  
df_dhs_climate <- 
  tempvars %>% 
  group_by(cc, NAME_0, NAME_1, SurveyId) %>% 
  summarise(
    # grab months
    first_month = min(ymd),
    last_month = max(ymd),
    mean_month = mean(ymd),
    median_month = median(ymd),
    
    # time from peak vars
    tfp_chirps_mean = weighted.mean(tfp_chirps, n),
    tfp_chirts_mean = weighted.mean(tfp_chirts, n),
    tfp_ndvi_mean = weighted.mean(tfp_ndvi, n),
    tfp_chirps_min = min(tfp_chirps),
    tfp_chirts_min = min(tfp_chirts),
    tfp_ndvi_min = min(tfp_ndvi),
    tfp_chirps_max = max(tfp_chirps),
    tfp_chirts_max = max(tfp_chirts),
    tfp_ndvi_max = max(tfp_ndvi),
    
    ### ndvi ###
    # 3 greenest
    lag_ndvi_3green14_1 = weighted.mean(lag_ndvi_3green14_1, n),
    lag_ndvi_3green14_2 = weighted.mean(lag_ndvi_3green14_2, n),
    lag_ndvi_3green14_3 = weighted.mean(lag_ndvi_3green14_3, n),
    lag_ndvi_3green14_4 = weighted.mean(lag_ndvi_3green14_4, n),
    lag_ndvi_3green14_5 = weighted.mean(lag_ndvi_3green14_5, n),
    lag_ndvi_3green14_6 = weighted.mean(lag_ndvi_3green14_6, n),
    lag_ndvi_3green14_7 = weighted.mean(lag_ndvi_3green14_7, n),
    
    ### chirps ###
    # 3 rainy
    lag_chirps_3rainy14_1 = weighted.mean(lag_chirps_3rainy14_1, n),
    lag_chirps_3rainy14_2 = weighted.mean(lag_chirps_3rainy14_2, n),
    lag_chirps_3rainy14_3 = weighted.mean(lag_chirps_3rainy14_3, n),
    lag_chirps_3rainy14_4 = weighted.mean(lag_chirps_3rainy14_4, n),
    lag_chirps_3rainy14_5 = weighted.mean(lag_chirps_3rainy14_5, n),
    lag_chirps_3rainy14_6 = weighted.mean(lag_chirps_3rainy14_6, n),
    lag_chirps_3rainy14_7 = weighted.mean(lag_chirps_3rainy14_7, n),
    
    ### chirts ###
    # 3 hot
    lag_chirts_3hot14_1 = weighted.mean(lag_chirts_3hot14_1, n),
    lag_chirts_3hot14_2 = weighted.mean(lag_chirts_3hot14_2, n),
    lag_chirts_3hot14_3 = weighted.mean(lag_chirts_3hot14_3, n),
    lag_chirts_3hot14_4 = weighted.mean(lag_chirts_3hot14_4, n),
    lag_chirts_3hot14_5 = weighted.mean(lag_chirts_3hot14_5, n),
    lag_chirts_3hot14_6 = weighted.mean(lag_chirts_3hot14_6, n),
    lag_chirts_3hot14_7 = weighted.mean(lag_chirts_3hot14_7, n)
    
    
    )





# SMART and MICS ----------------------------------------------------------
#load(here::here("Data", "df_climate.Rdata"))

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
  group_by(country, adm1, SurveyId, ymd) %>% 
  summarise(first_month = min(ymd),
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
  group_by(country, adm1, SurveyId, ymd) %>% 
  summarise(first_month = min(ymd),
            last_month = max(ymd),
            mean_month = mean(ymd),
            median_month = median(ymd),
            n = sum(respondents))


df_alts <- bind_rows(df_mics, df_smart) %>% 
  rename(NAME_0 = country,
         NAME_1 = adm1)

# Create ADM1-surveymonth frame
df_altsc <- 
  df_alts %>% 
  left_join(df_climate, by = c("NAME_0" = "country",
                               "NAME_1" = "adm1",
                               "ymd")) %>% 
  group_by(NAME_0, NAME_1, SurveyId) %>% 
  summarise(
    # grab months
    first_month = min(ymd),
    last_month = max(ymd),
    mean_month = mean(ymd),
    median_month = median(ymd),
    
    # time from peak vars
    tfp_chirps_mean = weighted.mean(tfp_chirps, n),
    tfp_chirts_mean = weighted.mean(tfp_chirts, n),
    tfp_ndvi_mean = weighted.mean(tfp_ndvi, n),
    tfp_chirps_min = min(tfp_chirps),
    tfp_chirts_min = min(tfp_chirts),
    tfp_ndvi_min = min(tfp_ndvi),
    tfp_chirps_max = max(tfp_chirps),
    tfp_chirts_max = max(tfp_chirts),
    tfp_ndvi_max = max(tfp_ndvi),
    
    ### ndvi ###
    # 3 greenest
    lag_ndvi_3green14_1 = weighted.mean(lag_ndvi_3green14_1, n),
    lag_ndvi_3green14_2 = weighted.mean(lag_ndvi_3green14_2, n),
    lag_ndvi_3green14_3 = weighted.mean(lag_ndvi_3green14_3, n),
    lag_ndvi_3green14_4 = weighted.mean(lag_ndvi_3green14_4, n),
    lag_ndvi_3green14_5 = weighted.mean(lag_ndvi_3green14_5, n),
    lag_ndvi_3green14_6 = weighted.mean(lag_ndvi_3green14_6, n),
    lag_ndvi_3green14_7 = weighted.mean(lag_ndvi_3green14_7, n),
    
    ### chirps ###
    # 3 rainy
    lag_chirps_3rainy14_1 = weighted.mean(lag_chirps_3rainy14_1, n),
    lag_chirps_3rainy14_2 = weighted.mean(lag_chirps_3rainy14_2, n),
    lag_chirps_3rainy14_3 = weighted.mean(lag_chirps_3rainy14_3, n),
    lag_chirps_3rainy14_4 = weighted.mean(lag_chirps_3rainy14_4, n),
    lag_chirps_3rainy14_5 = weighted.mean(lag_chirps_3rainy14_5, n),
    lag_chirps_3rainy14_6 = weighted.mean(lag_chirps_3rainy14_6, n),
    lag_chirps_3rainy14_7 = weighted.mean(lag_chirps_3rainy14_7, n),
    
    ### chirts ###
    # 3 hot
    lag_chirts_3hot14_1 = weighted.mean(lag_chirts_3hot14_1, n),
    lag_chirts_3hot14_2 = weighted.mean(lag_chirts_3hot14_2, n),
    lag_chirts_3hot14_3 = weighted.mean(lag_chirts_3hot14_3, n),
    lag_chirts_3hot14_4 = weighted.mean(lag_chirts_3hot14_4, n),
    lag_chirts_3hot14_5 = weighted.mean(lag_chirts_3hot14_5, n),
    lag_chirts_3hot14_6 = weighted.mean(lag_chirts_3hot14_6, n),
    lag_chirts_3hot14_7 = weighted.mean(lag_chirts_3hot14_7, n)
    
    
  )


# Bind surveys and save ---------------------------------------------------

df_climate_out <- 
  df_dhs_climate %>% 
  ungroup %>% 
  select(-cc) %>% 
  bind_rows(df_altsc)




save(df_climate_out, file = here::here("Data", "df_climate_out.Rdata"))



