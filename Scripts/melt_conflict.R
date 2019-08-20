library(here)
library(meltt)
library(tidyverse)

countries <- c("Nigeria", "Kenya", "Uganda")
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

GTD$event_tax <- as.factor(GTD$attacktype1)

SCAD$event_tax <- as.factor(SCAD$etype)

# Dates
ACLED$date <- as.Date(ACLED$EVENT_DATE, format = "%d/%m/%Y")
GED$date <- as.Date(GED$date_start)
GTD$date <- paste(GTD$iday, "/", GTD$imonth, "/", GTD$iyear, sep="")
GTD$date <- as.Date(GTD$date, format = "%d/%m/%Y")
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

# All countries in Africa + Yemen
afnames <- raster::ccodes() %>% 
  filter(NAME %in% countries) %>% 
  dplyr::select(NAME)
temp1 <- vector("list", 0)
for(i in afnames$NAME) {
  print(i)
  temp1[[i]] <- raster::getData(name = "GADM", 
                                country = i, 
                                download = T, 
                                level = 1)
}

# Join into sf frame
joined1 <- list(temp1, makeUniqueIDs = T) %>% 
  purrr::flatten() %>% 
  do.call(rbind, .) %>% 
  st_as_sf()


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

df_blank <- bind_rows(ken_blank, nigeria_blank, uga_blank)
  

sf_melt_final <- df_blank %>% 
  full_join(sf_melt_agg, by = c("NAME_1","NAME_0", "ymd")) %>% 
  mutate(violent_events = ifelse(is.na(violent_events), 0, violent_events))

sf_melt_final$geometry <- NULL

sf_melt_final <-
  sf_melt_final %>% 
  arrange(NAME_1, ymd) %>% 
  select(-year, -month) %>% 
  group_by(NAME_1, NAME_0) %>% 
  mutate(violent_events_lag1 = lag(violent_events, 1, order_by = NAME_1),
         violent_events_lag2 = lag(violent_events, 2, order_by = NAME_1),
         violent_events_lag3 = lag(violent_events, 3, order_by = NAME_1),
         violent_events_lag4 = lag(violent_events, 4, order_by = NAME_1),
         violent_events_lag5 = lag(violent_events, 5, order_by = NAME_1),
         violent_events_lag6 = lag(violent_events, 6, order_by = NAME_1),
         violent_events_lag7 = lag(violent_events, 7, order_by = NAME_1),
         violent_events_lag8 = lag(violent_events, 8, order_by = NAME_1),
         violent_events_lag9 = lag(violent_events, 9, order_by = NAME_1),
         violent_events_lag10 = lag(violent_events, 10, order_by = NAME_1),
         violent_events_lag11 = lag(violent_events, 11, order_by = NAME_1),
         violent_events_lag12 = lag(violent_events, 12, order_by = NAME_1),
         violent_events_lag13 = lag(violent_events, 13, order_by = NAME_1),
         violent_events_lag14 = lag(violent_events, 14, order_by = NAME_1),
         violent_events_lag15 = lag(violent_events, 15, order_by = NAME_1),
         violent_events_lag16 = lag(violent_events, 16, order_by = NAME_1),
         violent_events_lag17 = lag(violent_events, 17, order_by = NAME_1),
         violent_events_lag18 = lag(violent_events, 18, order_by = NAME_1),
         violent_events_lag19 = lag(violent_events, 19, order_by = NAME_1),
         violent_events_lag20 = lag(violent_events, 20, order_by = NAME_1),
         violent_events_lag21 = lag(violent_events, 21, order_by = NAME_1),
         violent_events_lag22 = lag(violent_events, 22, order_by = NAME_1),
         violent_events_lag23 = lag(violent_events, 23, order_by = NAME_1),
         violent_events_lag24 = lag(violent_events, 24, order_by = NAME_1))




save(sf_melt_final, file = here::here("Data", "sf_melt_final.Rdata"))



