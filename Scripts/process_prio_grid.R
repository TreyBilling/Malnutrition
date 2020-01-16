library(sf)
library(dplyr)



# Data --------------------------------------------------------------------

# Read in PRIO grid shapefile
prio_shp <- read_sf(here::here("Data", "PRIO", "priogrid_cell.shp"))

# PRIO grid yearly variables
prio_yearly <- readr::read_csv(here::here("Data", "PRIO", "yearly.csv"))

# PRIO grid static variables
prio_static <- readr::read_csv(here::here("Data", "PRIO", "static.csv"))



# Align PRIO grid to my ADM1s ---------------------------------------------

# Pull in GADM
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
                      countries = c("Nigeria", "Kenya", "Mali",
                                    "Burundi", 
                                    "Rwanda", "Niger")) %>% 
  filter(ENGTYPE_1 != "Water body") %>% 
  st_cast()

# DHS's Chad
chad1 <- sf::read_sf(here::here("Spatial Data Repository",
                                "chad","ch14", "dhs", "shps",
                                "sdr_subnational_data_dhs_2014.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  st_cast(., "MULTIPOLYGON")

# DHS's Uganda
uganda1 <- sf::read_sf(here::here("Spatial Data Repository",
                                  "uganda","uga16", "shps",
                                  "sdr_subnational_boundaries2.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  st_cast(., "MULTIPOLYGON")

# DHS's DRC
drc1 <- sf::read_sf(here::here("Spatial Data Repository",
                               "drc","drc_provinces", "shps",
                               "sdr_subnational_boundaries.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  mutate(NAME_0 = "Democratic Republic of the Congo") %>% 
  st_cast(., "MULTIPOLYGON")

# Bind into one sf frame
adm1 <- adm1 %>% select(NAME_0, NAME_1) %>% 
  rbind(chad1, uganda1, drc1)


# Join PRIO to adm1
adm1_prio <- st_join(adm1, prio_shp) # Every grid cell that intersects with an adm1



# Match to static and yearly data -----------------------------------------

# Joins
adm1_prio_tidy <- 
  adm1_prio %>% 
  # Join static
  left_join(., prio_static, by = c("gid", "xcoord", "ycoord", "col", "row")) %>% 
  # Join yearly
  left_join(., prio_yearly, by = c("gid")) %>% 
  select(year, everything()) %>% 
  arrange(NAME_0, NAME_1, year)

# Aggregate to adm1 level

adm1_prio_tidy_agg <-
  adm1_prio_tidy %>% 
  st_drop_geometry() %>% 
  select(-c("gid", "xcoord", "ycoord", "col", "row")) %>% 
  group_by(NAME_0, NAME_1, year) %>% 
  summarise_all(., ~mean(., na.rm = T)) %>% 
  mutate_all(~case_when(is.nan(.) ~ NA_real_, TRUE ~ .)) %>% 
  group_by(NAME_0, NAME_1) %>% 
  # fill population and nighlights data forward
  tidyr::fill(starts_with("pop"), starts_with("nlights")) 

# Fill values for 2015 and 2016 with 2014 values for all units
adm1_prio_tidy_agg15 <- 
  adm1_prio_tidy_agg %>% 
  filter(year == 2014) %>% 
  mutate(year = 2015)

adm1_prio_tidy_agg16 <- 
  adm1_prio_tidy_agg %>% 
  filter(year == 2014) %>% 
  mutate(year = 2016)

adm1_prio_tidy_agg <- bind_rows(adm1_prio_tidy_agg, 
                                adm1_prio_tidy_agg15, 
                                adm1_prio_tidy_agg16) %>% 
  # All variables lagged one year
  mutate(year = year + 1)

# Save frame
save(adm1_prio_tidy_agg, file = here::here("Data", "adm1_prio_tidy_agg.Rdata"))  
  

