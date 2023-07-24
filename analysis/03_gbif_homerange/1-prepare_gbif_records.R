#' Send queries to GBIF server to prepare records

#' @details
#' N.B. you must store your GBIF credentials in the ~/.Renviron file.
#' Use the function `usethis::edit_r_environ()` to edit this file and add the 
#' following three entries:
#'   GBIF_USER='your_gbif_username'
#'   GBIF_PWD='your_gbif_password'
#'   GBIF_EMAIL='your_gbif_email'
#'   

rm(list = ls())

## Import species list ----

species_list <- read.csv(here::here("results","03_gbif","species_list_w_gbif_id.csv"))

length(unique(species_list$gbif_valid_key))
sum(species_list$fb_sci_name%in%species_list$gbif_valid_name)

## Prepare batches of species keys ----

batches <- list()

n <- 6  # Number of ZIP files (6 ZIP for 2415 request)
k <- 1

for (i in 1:nrow(species_list)) {
  
  if (k > n) k <- 1
  
  if (i <= n) {
  
    batches[[k]] <- as.character(species_list[i, "gbif_valid_key"])
    
  } else {
    
    batches[[k]] <- c(batches[[k]], as.character(species_list[i, "gbif_valid_key"])) 
  }
  
  k <- k + 1
}

## Send queries to GBIF to prepare ZIP files ----

requests_keys <- data.frame()
inforequest <- list()
k <- 1

for (i in 1:length(batches)) {
  
  cat("Requesting chunk", i, "from", length(batches), "\n")
  
  #i=1
  #k <- 1
  
  if (k <= 3) {
    
    ## Prepare ZIP files on GBIF servers ----
    
    inforequest[[k]] <- rgbif::occ_download(
      rgbif::pred_in("taxonKey", batches[[i]]),
      format = "SIMPLE_CSV",
      user   = Sys.getenv("GBIF_USER"),
      pwd    = Sys.getenv("GBIF_PWD"),
      email  = Sys.getenv("GBIF_EMAIL"),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE),
      rgbif::pred("occurrenceStatus", "PRESENT"),
      rgbif::pred_in("basisOfRecord", c("HUMAN_OBSERVATION", "OBSERVATION", 
                                        "OCCURRENCE","MACHINE_OBSERVATION")),
      rgbif::pred_gte("year", 1960),
      rgbif::pred_lte("year", 2022))
    
    tmp <- inforequest[[k]]
    
    tmp_save <- attributes(tmp)
    tmp_save <- data.frame("download_key"  = tmp[1],
                           "created"       = tmp_save$"created",
                           "download_link" = tmp_save$"downloadLink",
                           "doi"           = tmp_save$"doi",
                           "citation"      = tmp_save$"citation",
                           "format"        = tmp_save$"format",
                           "user"          = tmp_save$"user",
                           "email"         = tmp_save$"email")
    
    requests_keys <- rbind(requests_keys, tmp_save)
    k <- k + 1
  }
  
  if (k > 3) {
    
    ## Wait until ZIP files are done on GBIF servers ----
    
    rgbif::occ_download_wait(inforequest[[1]], status_ping = 300)
    rgbif::occ_download_wait(inforequest[[2]], status_ping = 300)
    rgbif::occ_download_wait(inforequest[[3]], status_ping = 300)
    
    k <- 1
    
    inforequest <- list()
  }
}


write.csv(requests_keys, here::here("results","03_gbif", "gbif_requests_keys.csv"), 
          row.names = FALSE)
