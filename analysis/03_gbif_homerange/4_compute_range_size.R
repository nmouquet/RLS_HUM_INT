rm(list = ls())


## Params ----
#DO with res=0.05 and 0.1 

res      <- 0.1
mc_cores <- parallel::detectCores()-1
proj     <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

behrmann <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

## Create World grid -----

world_grid <- raster::raster(res = res, xmn = -180, xmx = 180, ymn = -90,
                             ymx = 90,
                             crs = proj)

world_grid[] <- NA
world_grid <- raster::projectRaster(world_grid, crs = behrmann)

## List csv files ----

path_data <- here::here("results", "03_gbif","gbif")
filenames <- list.files(path_data, pattern = "\\.csv$", full.names = TRUE)


## Import csv ----

gbif_records <- do.call(rbind.data.frame, lapply(filenames, function(x) {
  readr::read_csv(x) |> 
    as.data.frame()
}))


## Get species names -----

species <- unique(gbif_records$"species") |> 
  sort()


## Number of occurrences cells ----

n_gbif_records <- unlist(pbmcapply::pbmclapply(species, function(sp) {
  
  subset(gbif_records, gbif_records$"species" == sp) |> 
    nrow()
}, mc.cores = mc_cores))

## Number of occurrences cells ----

n_cells <- unlist(pbmcapply::pbmclapply(species, function(sp) {
  
  gbif_subset <- subset(gbif_records, gbif_records$"species" == sp)

  xy <- sf::st_as_sf(gbif_subset, coords = c("decimalLongitude", "decimalLatitude"), crs = proj)
  xy_berg <- sf::st_transform(xy, crs = sf::st_crs(behrmann))

  
  raster::cellFromXY(world_grid, xy_berg) |> 
    unique() |> 
    length()
}, mc.cores = mc_cores))

species_range <- data.frame("gbif_valid_name" = species, 
                            "GBIF_NC_records" = n_gbif_records,
                            "GBIF_NC_n_cells" = n_cells)

colnames(species_range)[3] <- paste0("GBIF_NC_n_cells_",gsub("\\.","",as.character(res)))

species_list <- read.csv(here::here("results","03_gbif","species_list_w_gbif_id.csv"))

species_range <- merge(species_list,species_range,all.x=T)

#the NA are set to 1 because as the species were recorded in RLS even if they do not appear in GBIF they must at least have 
#one occurence 
  species_range$GBIF_NC_records[is.na(species_range$GBIF_NC_records)] <- 1
  species_range[,6][is.na(species_range[,6])] <- 1
  
  
## Export ----

write.csv(species_range, file = here::here("results","03_gbif", paste0("species_range_size_",gsub("\\.","",as.character(res)),".csv")),
          row.names = FALSE)


# plot(species_range[ , 3:2], pch = 19, col = "#00000088", 
#      xlim = c(0, max(species_range$"n_gbif_records")),
#      ylim = c(0, max(species_range$"n_gbif_records")))
# lines(c(0, 20000), c(0, 20000), lty = 2)
