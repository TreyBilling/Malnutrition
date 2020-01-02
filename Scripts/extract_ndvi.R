library(raster)
library(sf)
library(velox)
library(tidyr)
library(dplyr)
library(ggplot2)
library(furrr)

### The NDVI rasters are too large to raster::extract() or velox::extract() ###
### Causes memory overload, even when splitting into smaller chunks ###
### As a slow-ish alternative, this script maps through the entire process ###
### for each month's raster in parallel with furrr, which keeps the physical 
### memory use from maxing out and crashing. There's likely a better way... ###


# NDVI files --------------------------------------------------------------
ndvi_files <- list.files("E:/NDVI", pattern =".tif$", full.names = T)


# Map through each file, aggregating mean NDVI per adm1 unit --------------

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
                      countries = c("Nigeria", "Kenya", "Uganda", "Mali"))

# Reproject shapefile to match ndvi (sin)
ndvi = raster(ndvi_files[[1]])
adm1 <- adm1 %>% st_transform(., crs = crs(ndvi)) 
rm(ndvi)

# Function to velox and extract
fun <- function(x) {
  r <- raster(ndvi_files[x]) 
  v <- velox(r)
  ndvi_extract <- v$extract(adm1, fun = function(x) mean(x, na.rm = TRUE))
}

# Furrr multiprocess map
plan(multiprocess)
ndvi <- future_map_dfr(.x = c(ndvi_extract = 1:238), ~fun(.x))
save(ndvi, file = "E:/NDVI/Tibbles/ndvi.Rdata")

ndvi %>% head()


ndvi_tidy <- 
  bind_cols(adm1, data.frame(ndvi)) %>% 
  pivot_longer(cols = starts_with("ndvi"),
               names_to = "ndvi_index",
               values_to = "ndvi") %>% 
  bind_cols(., ymd = rep(seq(lubridate::ymd("2000-02-01"),
                             lubridate::ymd("2019-11-01"),
                             by = "month"), 
                         length(adm1$NAME_1)))


save(ndvi_tidy, file = here::here("ndvi_tidy.Rdata"))
