library(here)
library(meltt)
library(sf)
library(tidyverse)

countries = c("Nigeria", "Kenya", "Uganda", "Mali",
              "Burundi", "Democratic Republic of the Congo",
              "Rwanda", "Chad", "Niger", "DR Congo (Zaire)", 
              "Democratic Republic of Congo")

# GED
GED <- read.csv(here("Data","ged171-csv", "ged171.csv"))

GED <- filter(GED, country %in% countries & year > 2002)

# ACLED
ACLED <- read.csv(here("Data",
                       "ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file",
                       "ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv"))
ACLED <- filter(ACLED, COUNTRY %in% countries & YEAR > 2002)


# SCAD
SCAD <- read.csv(here("Data", "SCAD_Africa_33", "SCAD2017Africa_Final.csv"))
SCAD <- filter(SCAD, countryname %in% countries & styr > 2002)

# Event taxonomy
event_tax <- read.csv(here("Data", "event_taxonomy.csv"))


### Setup variables ###

# Base categories
ACLED$event_tax <- as.character(ACLED$EVENT_TYPE)
ACLED$event_tax[ACLED$EVENT_TYPE=="Riots/Protests"&ACLED$FATALITIES>0] <- "Riots"
ACLED$event_tax[ACLED$EVENT_TYPE=="Riots/Protests"&ACLED$FATALITIES<1] <- "Protests"
ACLED$event_tax <- as.factor(ACLED$event_tax)

GED$event_tax <- as.factor(GED$type_of_violence)

SCAD$event_tax <- as.factor(SCAD$etype)

# Dates
ACLED$date <- as.Date(ACLED$EVENT_DATE, format = "%d/%m/%Y")
GED$date <- as.Date(GED$date_start)
# GTD$date <- paste(GTD$iday, "/", GTD$imonth, "/", GTD$iyear, sep="")
# GTD$date <- as.Date(GTD$date, format = "%d/%m/%Y")
SCAD$date <- as.Date(SCAD$startdate, format = "%d-%b-%y")

# End dates
GED$enddate <- as.Date(GED$date_end)
SCAD$enddate <- as.Date(SCAD$enddate, format = "%d-%b-%y")

# Coordinates 
ACLED$longitude <- ACLED$LONGITUDE
ACLED$latitude <- ACLED$LATITUDE

# Define taxonomy in a list
tax <- list(event_tax = event_tax)

# Attempt meltt
output <- meltt(ACLED, GED, SCAD,
                taxonomies = tax,
                spatwindow = 4,
                twindow = 2)

plot(output)

# Extract meltted data
df.meltt <- 
  meltt_data(output, 
             columns = c("date", "event_tax", "longitude", "latitude"))

df.meltt$year <- substr(df.meltt$date, 1, 4)
df.meltt$base.categories <- as.character(df.meltt$event_tax)
event_tax$base.categories <- as.character(event_tax$base.categories)
df.meltt <- full_join(df.meltt, event_tax, 
                      by = c("base.categories", "dataset" = "data.source"))





# To shapefile ----------------------------------------------------------------
get_countries <- function(level, countries) {
  
  temp0 <- vector("list", 0)
  for(i in countries) {
    print(i)
    temp0[[i]] <- raster::getData(name = "GADM", 
                                  country = i, 
                                  download = T, 
                                  level = level)
  }
  
  list(temp0, makeUniqueIDs = T) %>% 
    purrr::flatten() %>% 
    do.call(rbind, .) %>% 
    st_as_sf()
}

adm1 <- get_countries(level = 1, 
                      countries = c("Nigeria", "Kenya", "Uganda", "Mali",
                                    "Burundi", "Democratic Republic of the Congo",
                                    "Rwanda", "Niger")) %>% 
  filter(ENGTYPE_1 != "Water body") %>% 
  st_cast()

chad1 <- sf::read_sf(here::here("Spatial Data Repository",
                                "chad","ch14", "dhs", "shps",
                                "sdr_subnational_data_dhs_2014.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  st_cast(., "MULTIPOLYGON")

joined1 <- adm1 %>% select(NAME_0, NAME_1) %>% 
  rbind(chad1)

p <- df.meltt %>% group_by(date, latitude, longitude, Level_4_text) %>% 
  summarise(events = n()) %>% 
  ungroup() %>% 
  filter(Level_4_text == "Violent Event" & !is.na(latitude)) 
sp::coordinates(p) <- ~longitude+latitude
sp::proj4string(p) <- raster::crs(joined1)
sf_melt <- st_as_sf(p) %>% st_join(joined1, join = st_intersects)

sf_melt_agg <-
  sf_melt %>%
  mutate(month = lubridate::month(sf_melt$date),
         year = lubridate::year(sf_melt$date),
         ymd = lubridate::ymd(paste0(year, "-", month, "-01"))) %>%
  group_by(NAME_0, NAME_1, year, month, ymd) %>%
  summarise(violent_events = n())


# Write a function for this you psycho
ken_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                  lubridate::ymd("2017-01-01"), by = 1),
                        NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Kenya"],
                        NAME_0 = "Kenya") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

nigeria_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Nigeria"],
                         NAME_0 = "Nigeria") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

uga_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Uganda"],
                         NAME_0 = "Uganda") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

mal_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Mali"],
                         NAME_0 = "Mali") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

bur_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Burundi"],
                         NAME_0 = "Burundi") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

drc_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Democratic Republic of the Congo"],
                         NAME_0 = "Democratic Republic of the Congo") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

rwa_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Rwanda"],
                         NAME_0 = "Rwanda") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)


cha_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Chad"],
                         NAME_0 = "Chad") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)

nig_blank <- expand.grid(ymd = seq(lubridate::ymd("2002-01-01"), 
                                   lubridate::ymd("2017-01-01"), by = 1),
                         NAME_1 = joined1$NAME_1[joined1$NAME_0 == "Niger"],
                         NAME_0 = "Niger") %>% 
  mutate(day = lubridate::day(ymd)) %>% 
  filter(day == 1) %>% select(-day)


df_blank <- bind_rows(ken_blank, nigeria_blank, uga_blank, mal_blank, drc_blank, 
                      bur_blank, rwa_blank, cha_blank, nig_blank)
  

sf_melt_final <- df_blank %>% 
  full_join(sf_melt_agg, by = c("NAME_1","NAME_0", "ymd")) %>% 
  mutate(violent_events = ifelse(is.na(violent_events), 0, violent_events)) 

sf_melt_final$geometry <- NULL

### From Romain Francois https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/ ###
lags <- function(var, n){
  library(rlang)
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("lag_%s_%02d", quo_text(var), indices))
  
}



sf_melt_final <-
  sf_melt_final %>% 
  arrange(NAME_1, ymd) %>% 
  select(-year, -month) %>% 
  group_by(NAME_1, NAME_0) %>% 
  mutate(!!!lags(violent_events, 36))





save(sf_melt_final, file = here::here("Data", "sf_melt_final.Rdata"))



