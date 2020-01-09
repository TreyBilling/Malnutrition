library(raster)
library(sf)
library(velox)
library(tidyr)
library(dplyr)
library(ggplot2)


# Stack chirps rasters ----------------------------------------------------
# Rasters on usb drive
chirts_files <- list.files(path = "E:/CHIRTS",
                           pattern =".tiff$", 
                           full.names=TRUE)

chirts_stack <- stack(chirts_files)

# Replace NAs
NAvalue(chirts_stack) <- -9999



# Velox to efficiently extract to ADM1s -----------------------------------

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

adm1 <- adm1 %>% select(NAME_0, NAME_1) %>% 
  rbind(chad1)

sf::st_geometry_type(adm1$geometry)

# Convert to velox raster
chirts_vx <- velox(chirts_stack)

# Extract mean
chirts_extract <- chirts_vx$extract(adm1, fun = function(x) mean(x, na.rm = TRUE))

# Files are alpha order; since filenames have alpha months, not sorted correctly
# Hacky way to create ymd var ordered by alpha month, year

alphadates <- c(
  seq(lubridate::ymd("1995-04-01"),
      lubridate::ymd("2016-04-01"),
      by = "year"),
  seq(lubridate::ymd("1995-08-01"),
      lubridate::ymd("2016-08-01"),
      by = "year"),
  seq(lubridate::ymd("1995-12-01"),
      lubridate::ymd("2016-12-01"),
      by = "year"),
  seq(lubridate::ymd("1995-02-01"),
      lubridate::ymd("2016-02-01"),
      by = "year"),
  seq(lubridate::ymd("1995-01-01"),
      lubridate::ymd("2016-01-01"),
      by = "year"),
  seq(lubridate::ymd("1995-07-01"),
      lubridate::ymd("2016-07-01"),
      by = "year"),
  seq(lubridate::ymd("1995-06-01"),
      lubridate::ymd("2016-06-01"),
      by = "year"),
  seq(lubridate::ymd("1995-03-01"),
      lubridate::ymd("2016-03-01"),
      by = "year"),
  seq(lubridate::ymd("1995-05-01"),
      lubridate::ymd("2016-05-01"),
      by = "year"),
  seq(lubridate::ymd("1995-11-01"),
      lubridate::ymd("2016-11-01"),
      by = "year"),
  seq(lubridate::ymd("1995-10-01"),
      lubridate::ymd("2016-10-01"),
      by = "year"),
  seq(lubridate::ymd("1995-09-01"),
      lubridate::ymd("2016-09-01"),
      by = "year")
)


chirts_tidy <- 
  bind_cols(adm1, data.frame(chirts_extract)) %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "chirts_index",
               values_to = "chirts") %>% 
  bind_cols(., ymd = rep(alphadates, length(adm1$NAME_1)))

# Some quick checks on the curves
chirts_tidy %>% 
  ggplot() +
  geom_line(aes(x = ymd, y = chirts, group = NAME_1)) +
  facet_wrap(~NAME_0) +
  theme_bw()

# Checks out with alt sources of temp patterns
chirts_tidy %>% 
  group_by(NAME_1, NAME_0, lubridate::month(ymd)) %>% 
  filter(NAME_0 == "Chad") %>% 
  summarise(m = mean(chirts)) %>% 
  ggplot() +
  geom_point(aes(x = factor(`lubridate::month(ymd)`), y = m, group = NAME_1)) +
  geom_line(aes(x = `lubridate::month(ymd)`, y = m, group = NAME_1)) +
  facet_wrap(~NAME_1) +
  theme_bw()

save(chirts_tidy, file = here::here("chirts_tidy.Rdata"))
