
rm(list = ls())

## Import species names ----

  #when  running for the first time 

    RLS_species <- read.csv2(here::here("results","01_build_species_list","RLS_species_init.csv"))
    sp_infos <- cbind.data.frame(fb_sci_name=RLS_species$fb_sci_name,gbif_valid_key=NA)
    sp_infos$fb_sci_name <- gsub("_"," ",sp_infos$fb_sci_name)
    str <- sum(!is.na(sp_infos$gbif_valid_key))+1

  #when starting from an incomplete sp_infos file 

    sp_infos <- read.csv2(file = here::here("results","03_gbif","species_list_w_gbif_id.csv")) # several columns
    str <- sum(!is.na(sp_infos$gbif_valid_key))+1
   
  #Start the scrap 
    #pb with Centropogon australis i=350
    #pb with Pyronotanthias lori i=1871
    
    for (i in 1872:nrow(sp_infos)) { #nrow(sp_infos)
  #i=1871
  usethis::ui_info(paste0("Processing species ", i, " / ", nrow(sp_infos)))
  
  sp <- sp_infos[i, "fb_sci_name"]
  
  ## Retrieve GBIF information ----
  
  gbif_data <- as.data.frame(rgbif::name_backbone(sp, rank = "species"))
  
  if (nrow(gbif_data) > 1) stop("Multiple match")
  
  
  ## Parse information ----
  
  if (gbif_data$"status" == "ACCEPTED") {
    
    gbif_data <- gbif_data[ , c("verbatim_name", "usageKey", "synonym")]
    
  } else {
    
    gbif_data <- gbif_data[ , c("species", "acceptedUsageKey", "synonym")]
  }
  
  colnames(gbif_data) <- c("gbif_valid_name", "gbif_valid_key", "gbif_synonym")
  
  ## Add information ----
  
  sp_infos[i, "gbif_valid_name"] <- gbif_data[1, "gbif_valid_name"]
  sp_infos[i, "gbif_valid_key"]  <- gbif_data[1, "gbif_valid_key"]
  sp_infos[i, "gbif_synonym"]    <- gbif_data[1, "gbif_synonym"]
  
  write.csv(sp_infos, here::here("results", "03_gbif","species_list_w_gbif_id.csv"), row.names = FALSE)
  
  Sys.sleep(sample(seq(0.1, 0.2, by = 0.01), 1))
}
    
#Check the NAs if any (there was an issue with Centropogon australis and Pyronotanthias lori
#the gbif_valid_keys were found directy on the GBIF web site and added to the file directly 
    
    sp_infos$fb_sci_name[is.na(sp_infos$gbif_valid_key)]
    sp_infos[is.na(sp_infos$gbif_valid_key),]
    
    sp_infos$gbif_valid_name[sp_infos$fb_sci_name%in%"Centropogon australis"]="Centropogon australis"
    sp_infos$gbif_synonym[sp_infos$fb_sci_name%in%"Centropogon australis"]=FALSE
    sp_infos$gbif_valid_key[sp_infos$fb_sci_name%in%"Centropogon australis"]=as.integer("2335165")
    
    sp_infos$gbif_valid_name[sp_infos$fb_sci_name%in%"Pyronotanthias lori"]="Pyronotanthias lori"
    sp_infos$gbif_synonym[sp_infos$fb_sci_name%in%"Pyronotanthias lori"]=FALSE
    sp_infos$gbif_valid_key[sp_infos$fb_sci_name%in%"Pyronotanthias lori"]=as.integer("11978557")
    
    write.csv(sp_infos, here::here("results", "03_gbif","species_list_w_gbif_id.csv"), row.names = FALSE)
    

