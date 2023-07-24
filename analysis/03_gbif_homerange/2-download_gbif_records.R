#' Download and extract ZIP files from GBIF server


## Download ZIP files ----

dir.create(here::here("results", "03_gbif","gbif"), recursive = TRUE, showWarnings = FALSE)

requests_keys <- read.csv(here::here("results", "03_gbif", "gbif_requests_keys.csv"))


for (i in 1:nrow(requests_keys)) {
  
  cat(paste(i, "on", nrow(requests_keys), "\n"))
  
  rgbif::occ_download_get(key       = requests_keys[i, "download_key"],
                          path      = here::here("results", "03_gbif","gbif"),
                          overwrite = TRUE)
  
  unzip(zipfile = here::here("results", "03_gbif","gbif",
                             paste0(requests_keys[i, "download_key"], ".zip")),
        exdir = here::here("results", "03_gbif","gbif"), overwrite = TRUE)
  
  invisible(file.remove(here::here("results", "03_gbif","gbif", 
                                   paste0(requests_keys[i, "download_key"],
                                          ".zip"))))
}
