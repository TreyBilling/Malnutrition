library(raster)
library(heavyRain) # used to download and unzip chirps tifs
library(sf)
library(velox)
library(tidyr)
library(dplyr)
library(ggplot2)




# Download chirps ---------------------------------------------------------
getCHIRPS(region = "africa", 
          format = "tifs", 
          tres = "monthly", 
          begin = as.Date("1995-01-01"),
          dsn = "E:/CHIRPS", cores = 4)


chirps_files <- list.files("E:/CHIRPS", full.names = T)
extractCHIRPS(chirps_files, dsn = "E:/CHIRPS")


# Stack chirps rasters ----------------------------------------------------
# Rasters on usb drive
chirps_files <- list.files(path = ("E:/CHIRPS"),
                        pattern =".tif$", 
                        full.names = TRUE)
chirps_stack <- stack(chirps_files)

# Replace NAs
NAvalue(chirps_stack) <- -9999
plot(chirps_stack$chirps.v2.0.2000.01)


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
                      countries = c("Nigeria", "Kenya", "Uganda", "Mali"))

# Convert to velox raster
chirps_vx <- velox(chirps_stack)

# Extract mean
chirps_extract <- chirps_vx$extract(adm1, fun = function(x) mean(x, na.rm = TRUE))

# Should be a matrix 151 X 299 
dim(chirps_extract)

plot(density(chirps_extract[,1]))
plot(density(chirps_extract[,299]))

# Need to go from wide matrix to long df
# Can take extract, create column nams seq along the min to max month-year
# january 2000 to november 2019

chirps_tidy <- 
  bind_cols(adm1, data.frame(chirps_extract)) %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "chirps_index",
               values_to = "chirps") %>% 
  bind_cols(., ymd = rep(seq(lubridate::ymd("1995-01-01"),
                       lubridate::ymd("2019-11-01"),
                       by = "month"), 
                       length(adm1$NAME_1)))

save(chirps_tidy, file = here::here("chirps_tidy.Rdata"))

