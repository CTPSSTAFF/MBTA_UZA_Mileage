library(sf)
library(readxl)
library(tidyverse)
library(mapview)
library(gtfstools)
library(stringr)
library(gt)
mapviewOptions(fgb = FALSE)

sf_from_zip <- function(URL) {
  # based on:
  # https://stackoverflow.com/questions/59740419/unzipping-and-reading-shape-file-in-r-without-rgdal-installed
  
  # Load local temp files
  temp_0 <- tempfile()
  temp_1 <- tempfile()
  
  # download the zip, put it in the first temp directory
  download.file(URL, temp_0)
  
  # Unzip and place in temp_1
  unzip(zipfile = temp_0, exdir = temp_1)
  
  # Read the shapefile.
  geom_sf <- sf::read_sf(temp_1)
  
  out <- geom_sf
}


UZA20_URL <- "https://www2.census.gov/geo/tiger/TIGER2020/UAC/tl_2020_us_uac20.zip"

UZA20 <- sf_from_zip(UZA20_URL)

NE <- c("MA", "CT", "RI", "VT", "NH", "ME")
NE_collapse <- paste0(NE,collapse = "|")


# Read data from population file:
# https://www.transit.dot.gov/ntd/2020-census-changes-uzapopulation
UZA20_pop <- read_xlsx("./data/base/UZA_CHANGES_1990-2020_2_2.xlsx", 
                       sheet = "UZA 2020 Master",
                       col_types = "text") |> 
  janitor::clean_names() |> 
  mutate(x2020_uace = str_pad(x2020_uace, side = "left", pad = "0", 5))



# There are only Urban Areas in 2020.
UZA20_NE <- UZA20 |>
  filter(str_detect(NAME20, NE_collapse)) |> 
  left_join(UZA20_pop |> select(x2020_uace, x2020_population), by = c("GEOID20" = "x2020_uace")) |> 
  mutate(UZA_type = if_else(!is.na(x2020_population), 
                            "Pop: GTE 50k", 
                            "Pop: LT 50k"))



# This is the last Summer 2022 GTFS schedule. 
# We chose this because it contains the CapeFlyer if we need it. 
gtfs_summ22 <- gtfstools::read_gtfs("https://cdn.mbtace.com/archive/20220812.zip")

# Convert shapes to MA system.
gtfs_summ22_shp <- convert_shapes_to_sf(gtfs_summ22) |> 
  st_transform(26986)

# Attach them to our route_ids so we can have hints about what each shape is.
gtfs_summ22_shp <-
  inner_join(gtfs_summ22_shp,
             gtfs_summ22$trips |> distinct(route_id, shape_id),
             by = "shape_id")


gtfs_summ22_shp_filt <- gtfs_summ22_shp |> 
  filter(str_detect(route_id, "CR-") | str_detect(route_id, "Cape"))

# gtfs_summ22_shp_filt |> mapview(zcol = "route_id", color = viridis::turbo)

# Attach them to our route_ids so we can have hints about what each shape is.
gtfs_summ22_stp <- gtfs_summ22 |> convert_stops_to_sf() 
gtfs_summ22_stp <- gtfs_summ22_stp |> filter(vehicle_type == 2) |> 
  distinct(stop_name, .keep_all = TRUE) |> 
  filter(stop_name != "Plymouth")

UZA20_NE_MANHME <- UZA20_NE |> 
  filter(str_detect(NAME20, pattern = "MA")) |>
  mutate(x2020_population = as.numeric(x2020_population)) |> 
  select(UACE20, GEOID20, x2020_population, NAME20, UZA_type)


map <- mapview(UZA20_NE_MANHME |> 
                 filter(UZA_type != "Pop: GTE 50k"),
        col.regions = "grey50", layer.name = "2020 Urbanized Area",
        label = "UACE20",
        homebutton = FALSE) +
mapview(UZA20_NE_MANHME |> 
          filter(UZA_type == "Pop: GTE 50k"),
        zcol = "UACE20",
        lwd = 2,
        legend = FALSE,
        label = "UACE20",
        layer.name = "2020 Urbanized Area",
        col.regions = viridis::turbo) +
mapview(gtfs_summ22_shp_filt, 
        lwd = 2,
        label = "route_id",
        color = "#80276C", layer.name = "Commuter Rail Line or Station") +
mapview(gtfs_summ22_stp, color = "white", 
        label = "stop_name",
        col.regions = "#80276C", cex = 4, 
        homebutton = FALSE,
        layer.name = "Commuter Rail Line or Station")

saveRDS(map, file = "./data/processed/outputmap.rds")

# Source: 
# MBTA Shapes Summer 2022 GTFS: https://cdn.mbtace.com/archive/20220812.zip
# UZA Shapes: https://www2.census.gov/geo/tiger/TIGER2020/UAC/tl_2020_us_uac20.zip
# UZA Population: https://www.transit.dot.gov/ntd/2020-census-changes-uzapopulation
