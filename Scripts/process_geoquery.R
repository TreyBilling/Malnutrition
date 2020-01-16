library(tidyverse)


gq <- list.files(here::here("Data", "geoquery"), full.names = T) %>% 
  map(., ~read_csv(.))


map(gq,~"NAME_1" %in% colnames(.x))

gq[[3]]$NAME_1 = gq[[3]]$NAME 
gq[[3]]$NAME_0 = gq[[3]]$iso

gq_bind <- bind_rows(gq_nigeria, gq_kenya, gq_niger)

gq <-
  map(gq, ~.x %>% select(starts_with("viirs"), 
         starts_with("v4composites"), 
         starts_with("gpw"),
         starts_with("dist_to_groads.none.mean"),
         starts_with("access_50k"),
         "NAME_0", "NAME_1") %>% # select variable
  select(ends_with("mean"), "NAME_0", "NAME_1"))

gq_bind <- bind_rows(gq) %>% 
  mutate(NAME_0 = case_when(NAME_0 == "COD" ~ "Democratic Republic of the Congo", TRUE ~ NAME_0),
         myid = paste(NAME_0, NAME_1, sep = "-"))


# VIIRS -------------------------------------------------------------------

viirs <- gq_bind %>% 
  dplyr::select(starts_with("viirs"), "NAME_0", "NAME_1", "myid") %>% # select variable
  dplyr::select(ends_with("mean"), "NAME_0", "NAME_1", "myid") %>% # select means only
  reshape2::melt(id.vars = c("NAME_1", "NAME_0", "myid")) %>% # sorry for still using reshape2
  mutate(year = as.numeric(substr(variable, 44, 47))) %>% 
  rename(lights_viirs = value) %>% dplyr::select(-variable)



# DMPS lights -------------------------------------------------------------

dmps <- gq_bind %>% 
  dplyr::select(starts_with("v4composites"), "NAME_0", "NAME_1", "myid") %>% # select variable
  dplyr::select(ends_with("mean"), "NAME_0", "NAME_1", "myid") %>% # select means only
  reshape2::melt(id.vars = c("NAME_1", "NAME_0", "myid")) %>% # sorry for still using reshape2
  mutate(year = as.numeric(substr(variable, 32, 35))) %>% 
  rename(lights_v4 = value) %>% dplyr::select(-variable)



# GPW ---------------------------------------------------------------------

gpw <- gq_bind %>% 
  dplyr::select(starts_with("gpw_v4"), "NAME_0", "NAME_1", "myid") %>% # select variable
  dplyr::select(ends_with("mean"), "NAME_0", "NAME_1", "myid") %>% # select sum
  reshape2::melt(id.vars = c("NAME_1", "NAME_0", "myid")) %>% # sorry for still using reshape2
  mutate(year = as.numeric(substr(variable, 16, 19))) %>% 
  rename(gpw_mean = value) %>% dplyr::select(-variable)


# Travel and roads --------------------------------------------------------


gq_bind$access_50k.none.mean

access <- gq_bind %>% 
  dplyr::select(starts_with("access_50k"), "NAME_0", "NAME_1", "myid") %>% # select variable
  dplyr::select(ends_with("mean"), "NAME_0", "NAME_1", "myid") %>% 
  rename(access_50kmean = access_50k.none.mean) 


roads <- gq_bind %>% 
  dplyr::select(starts_with("dist_to_groads.none.mean"), "NAME_0", "NAME_1", "myid") %>% # select variable
  dplyr::select(ends_with("mean"), "NAME_0", "NAME_1", "myid") %>% 
  rename(roads = dist_to_groads.none.mean) 





# To frame ----------------------------------------------------------------

df_blank <- expand.grid(year = seq(2002, 2017, by = 1), myid = paste(gq_bind$NAME_0, gq_bind$NAME_1, sep = "-"))

df_covs <- df_blank %>% full_join(viirs, by = c("myid", "year")) %>% 
  full_join(dmps, by = c("myid", "year", "NAME_1", "NAME_0")) %>% 
  full_join(gpw, by = c("myid", "year", "NAME_1", "NAME_0")) %>% 
  # mutate(NAME_0 = ifelse(is.na(NAME_0.x), NAME_0.y, NAME_0.x)) %>% 
  # dplyr::select(-NAME_0.x, -NAME_0.y) %>% 
  group_by(NAME_0, NAME_1) %>% arrange(NAME_1, year) %>% 
  fill(gpw_mean)

df_covs <- df_covs %>% group_by(NAME_1, NAME_0) %>% arrange(NAME_1, year) %>% 
  mutate(gpw_lag1 = lag(gpw_mean, 1, order_by = NAME_1),
         gpw_lag2 = lag(gpw_mean, 2, order_by = NAME_1),
         lights_v4_lag1 = lag(lights_v4, 1, order_by = NAME_1),
         lights_v4_lag2 = lag(lights_v4, 2, order_by = NAME_1),
         lights_viirs_lag1 = lag(lights_viirs, 1, order_by = NAME_1),
         lights_viirs_lag2 = lag(lights_viirs, 2, order_by = NAME_1)) %>% filter(year > 2000)


df_covs <- df_covs %>% full_join(access, by = c("NAME_1", "NAME_0")) %>% 
  full_join(roads, by = c("NAME_1", "NAME_0"))





# Line up names with DHS adm1s --------------------------------------------

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

chad1 <- sf::read_sf(here::here("Spatial Data Repository",
                                "chad","ch14", "dhs", "shps",
                                "sdr_subnational_data_dhs_2014.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  st_cast(., "MULTIPOLYGON")

uganda1 <- sf::read_sf(here::here("Spatial Data Repository",
                                  "uganda","uga16", "shps",
                                  "sdr_subnational_boundaries2.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  st_cast(., "MULTIPOLYGON")

drc1 <- sf::read_sf(here::here("Spatial Data Repository",
                               "drc","drc_provinces", "shps",
                               "sdr_subnational_boundaries.shp")) %>% 
  select(CNTRYNAMEE, DHSREGEN) %>% 
  rename(NAME_1 = DHSREGEN, NAME_0 = CNTRYNAMEE) %>% 
  mutate(NAME_0 = "Democratic Republic of the Congo") %>% 
  st_cast(., "MULTIPOLYGON")

adm1 <- adm1 %>% select(NAME_0, NAME_1) %>% 
  rbind(chad1, uganda1, drc1)


### Burundi ###
gq[[1]]$NAME_1 
adm1$NAME_1[adm1$NAME_0 == "Burundi"]

# All match

### Chad ###

# gq = adm1
# Barh el Ghazel = Barh El Ghazel?
# Borkou = Borkou/Tibesti
# Tibesti = Borkou/Tibesti
# Chari-Baguirmi = Chari Baguirmi
# Ennedi Est = Ennedi Est/Ennedi Ouest
# Ennedi Ouest = Ennedi Est/Ennedi Ouest
# Mayo-Kebbi Est = Mayo Kebbi Est

gq[[2]] %>% select(NAME_1) -> t1
adm1[adm1$NAME_0 == "Chad",] %>% select(NAME_1) -> t2

df_covs %>% filter(NAME_0 == "Chad") %>% 
  mutate(new_name = case_when(NAME_1 == "Barh el Ghazel" ~ "Barh El Gazal",
                              NAME_1 == "Borkou" ~ "Borkou/Tibesti",
                              NAME_1 == "Tibesti" ~ "Borkou/Tibesti",
                              NAME_1 == "Chari-Baguirmi" ~ "Chari Baguirmi",
                              NAME_1 == "Ennedi Est" ~ "Ennedi Est/Ennedi Ouest",
                              NAME_1 == "Ennedi Ouest" ~ "Ennedi Est/Ennedi Ouest",
                              NAME_1 == "Mayo-Kebbi Est" ~ "Mayo Kebbi Est",
                              NAME_1 == "Mayo-Kebbi Ouest" ~ "Mayo Kebbi Ouest",
                              NAME_1 == "Moyen-Chari" ~ "Moyen Chari",
                              TRUE ~ NAME_1)) %>% 
  filter(year == 2001) %>% 
  select(NAME_1, new_name) %>% 
  rename(old_name = NAME_1) -> temp


temp %>% 
  full_join(., adm1[adm1$NAME_0 == "Chad",], by = c("new_name" = "NAME_1", "NAME_0")) %>% 
  left_join(., df_covs, by = c("old_name" = "NAME_1", "NAME_0")) %>% 
  select(NAME_1, NAME_0, everything()) %>%
  group_by(new_name, NAME_0, year) %>%
  summarise_all(., ~mean(., na.rm = T))







df_covs <- df_covs %>% ungroup() %>% 
  mutate(NAME_1 = ifelse(NAME_1 == "TillabÃ©ry", "Tillabéry", NAME_1))

df_covs$NAME_1 %>% unique

save(df_covs, file = here("Data", "covs_nnk.rdata"))  



