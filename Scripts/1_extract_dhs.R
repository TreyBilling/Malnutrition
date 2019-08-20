library(tidyverse)
library(rdhs)
library(sf)
#library(here)
theme_set(theme_bw())


# Setup DHS extraction ----------------------------------------------------

# DHS account info
set_rdhs_config(email = "tbilling@umd.edu",
                project = "Regional Development, Foreign Aid, and Weak Institutions",
                config_path = "~/.rdhs.json",
                global = TRUE)
1

# To find dhs country naming conventions
(cnames <- dhs_countries(returnFields = c("CountryName", "DHS_CountryCode")))

# Want Nigeria, Kenya, and Uganda waves
wants <- c("NG", "KE", "UG")

survs <- dhs_surveys(countryIds = wants,
                     surveyType = "DHS")

datasets <- dhs_datasets(surveyIds = survs$SurveyId, 
                         fileFormat = "flat",
                         fileType = c("PR"),
                         surveyYear = c(2004:2015))

# Pulling only a handfull of variables
varnames <- c("hv000", # survey id
              "hc27", # sex
              "hv270", # wealth cat
              "hv271", # wealth fac score
              "hc61", # mother edu
              "hc63", # preceding birth interval
              "hc64", # birth order
              "hv204", # time to water
              "hv201", # water source
              "hv005", 
              "hc1", # child age in months
              "hc19", # survey year
              "hc18", # survey month
              "hc70",  # height for age z score
              "hc71", # weight for age z score
              "hc72") # weight for heigh z score


# Tell rdhs what variables and datasets to pull
questions <- search_variables(datasets$FileName, 
                              variables = varnames,
                              reformat = T)

# Extract the surveys from rdhs into list
extract <- extract_dhs(questions, add_geo = T)

# Shapefiles --------------------------------------------------------------

# Countries we want
countries <- c("Kenya", "Nigeria", "Uganda")

# Function to pull in GADM shapefiles from raster 
shpfun <- function(country) {
  raster::getData(name = "GADM", country = country, download = T, level = 1)
}

# Extract shapefiles and bind into one object
shps_afr <- map(countries, shpfun)
shp <- list(shps_afr, makeUniqueIDs = T) %>% 
  purrr::flatten() %>% 
  do.call(rbind, .) %>% 
  st_as_sf()

  


# Process DHS -------------------------------------------------------------

# Function to clean and define DHS variables
clean_dhs <- function(data) {
  
  data %>%
    filter(hc1 <= 59 & as.numeric(hc72) < 9995 & as.numeric(hc71) < 9995 & as.numeric(hc70) < 9995) %>% 
    mutate(hc72 = as.numeric(hc72) / 100, 
           hc71 = as.numeric(hc71) / 100,
           hc70 = as.numeric(hc70) / 100,
           whz2 = ifelse(hc72 < -2, 1, 0 ),
           haz2 = ifelse(hc70 < -2, 1, 0 ),
           waz2 = ifelse(hc71 < -2, 1, 0 ),
           month = hc18,
           year = hc19,
           ymd = lubridate::ymd(paste0(year, "-", month, "-01")))
}

crs <- st_crs(shp)

# Remove observations not within an ADM1 unit
sf_extract <- 
  extract %>% 
  # filter out missing coords
  map(function(z) filter(z, !is.na(LATNUM) & !is.na(LONGNUM))) %>%           
  # convert to sf object
  map(function(x) st_as_sf(x, crs = crs, coords = c("LONGNUM", "LATNUM")))



sf_extract_clean <- sf_extract %>% map(clean_dhs)

# Function to map DHS cluster locations to ADM1 locations
finder <- function(data) {
  
  # Simplifying to st_intersect by DHS cluster instad of observation (more important with many DHS waves)
  simpler <- function(data) {
    data %>% group_by(CLUSTER) %>% 
      summarise(n = n())}
  
  data %>% 
    map(simpler) %>% 
    map(~st_join(.x, shp, join = st_intersects))
}


temp <- finder(sf_extract_clean) %>% do.call(sf:::rbind.sf, .)

# Bind all waves into one frame
sf_extract_big <- sf_extract_clean %>% do.call(sf:::rbind.sf, .)

# Match up sf frame with frame without geometry features
sf_extract_big_nog <- sf_extract_big
st_geometry(sf_extract_big_nog) <- NULL
temp_nog <- temp
st_geometry(temp_nog) <- NULL
sf_extract_big_nog <- sf_extract_big_nog %>% 
  mutate(cc = substr(SurveyId, 1,2))
temp_nog <- temp_nog %>% mutate(cc = substr(GID_0, 1,2))

df_m <- sf_extract_big_nog %>% left_join(temp_nog, by = c("CLUSTER", "cc"))


# Save frame
save(df_m, file = here::here("Data", "dhs_working.Rdata"))

