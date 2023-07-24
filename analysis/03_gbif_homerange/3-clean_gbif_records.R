#' Clean GBIF records


## Get list of GBIF raw csv files ----

csv_files <- list.files(here::here("results", "03_gbif","gbif"), pattern = "\\.csv$")


## Columns to keep ----

col_names <- c("species", "taxonKey", "decimalLongitude", "decimalLatitude", "year")


for (i in 1:length(csv_files)) {
  
  #i=1
  cat("Processing file", i, "\n")
  
  ## Read file ----
  
  tab <- readr::read_tsv(here::here("results", "03_gbif","gbif", csv_files[i]))
  tab <- as.data.frame(tab)
  
  ## Keep only SPECIES rank ----
  
  tab <- tab[tab$"taxonRank" == "SPECIES", ]
  
  
  ## Convert dates ----
  
  tab$"eventDate" <- as.character(as.Date(tab$"eventDate"))
  
  
  ## Remove duplicates ----
  
  keys <- paste(tab$"taxonKey", tab$"eventDate", tab$"decimalLongitude", 
                tab$"decimalLatitude", sep = "__")
  
  pos <- which(duplicated(keys))
  if (length(pos)) tab <- tab[-pos, ]
  
  
  ## Select columns and order rows ----
  
  tab <- tab[ , col_names]
  tab <- tab[with(tab, order(species, year)), ]
  rownames(tab) <- NULL
  
  
  ## Export results ----
  
  write.csv(tab, here::here("results", "03_gbif","gbif", csv_files[i]), row.names = FALSE)
}
