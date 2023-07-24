###################################################################################################
#'Scrap Fishbase Nas
#'  
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#' 
#' Produce 
#'    - fishbase_table.csv
#' 
#' @date 2021/04/02 
#' @lastrun 2023/04/01
##################################################################################################

rm(list = ls())

#scrap FISHBASE----
#note that the scrap is divided in chunks to simplify the scraping

  RLS_species <- read.csv2(here::here("results","01_build_species_list","RLS_species_init.csv"))
  sp_list <- gsub("_"," ",RLS_species$fb_sci_name)
  chunks <- split(sp_list, ceiling(seq_along(sp_list)/150))
  
  for (i in 1:length(chunks)) { #
    #i=1
    sp_list_chunk <- chunks[[i]]
    #sp_list_chunk <- "Amphiprion ocellaris"
    cat(paste("chunk= ", i,"\n"))
    
    #sp_list_chunk <- sp_list_chunk[1:5]
    
    ER <- FALSE
    data_fishbase_1 <- tryCatch({
      rfishbase::species(sp_list_chunk)},
      error = function(e){ER <- TRUE},
      warning = function(w){ER <- TRUE})
    data_fishbase_1 <- as.data.frame(data_fishbase_1)
    
    Sys.sleep(1)

    ER <- FALSE
    data_fishbase_2 <- tryCatch({
      rfishbase::stocks(sp_list_chunk)},
      error = function(e){ER <- TRUE},
      warning = function(w){ER <- TRUE})
    data_fishbase_2 <- as.data.frame(data_fishbase_2)    
    data_fishbase_2 <- data_fishbase_2[(data_fishbase_2$Level=="species in general") | (data_fishbase_2$Species %in% c("Diplodus argenteus","Stichaeus punctatus")),]

    data_fishbase <- merge(data_fishbase_1,data_fishbase_2,by="Species")
    save(data_fishbase,file=here::here("results","02_scrap","fishbase","chunks",paste0("data_fishbase_chunk_",i,".RData")))
    
    Sys.sleep(2)
    
  }

#----
  
#Assemble and save----
  
  ##get the chunk files
    
    files <- list.files(here::here("results","02_scrap","fishbase","chunks"))
    
    l_ch <- lapply(files,function(id_file){
      get(load(file=here::here("results","02_scrap","fishbase","chunks",id_file))) 
    })
    data_fishbase<- do.call(rbind,l_ch)
  
  ##get the variables of interest 
    
    ##count the number of NAs for each species 
    ## get all the colnames into fishbase_colnames.csv, add a row keep (T or F) to keep the colums for the NAs analysis 
    
      #fishbase_colnames <- cbind.data.frame(colnames=colnames(data_fishbase),keep=TRUE)
      #write.csv2(fishbase_colnames, here::here("results","02_scrap","fishbase","fishbase_colnames.csv"), row.names = FALSE)
      #add the FALSE by hand
      #fishbase_colnames <- read.csv2(here::here("results","02_scrap","fishbase","fishbase_colnames.csv"))
      #fishbase_colnames$colnames[fishbase_colnames$keep==TRUE]
  
      #nas <- data.frame(FB_nas=apply(apply(data_fishbase[,fishbase_colnames$colnames[fishbase_colnames$keep==TRUE]],1,is.na),2,sum),fb_sci_name=data_fishbase$Species)
  
      nas <- data.frame(FB_nas=apply(apply(data_fishbase,1,is.na),2,sum),fb_sci_name=data_fishbase$Species)
      
    ##get other intel for later analysis
      other_intel <- c("Importance","IUCN_Code","UsedasBait","UsedforAquaculture","PriceCateg","Aquarium","AquariumFishII")
      otherfb <- data.frame(fb_sci_name=data_fishbase$Species,data_fishbase[,other_intel])
      colnames(otherfb) <- c("fb_sci_name","FB_Importance","FB_IUCN","FB_UsedasBait","FB_UsedforAquaculture","FB_PriceCateg","FB_Aquarium_I","FB_Aquarium_II")
      
  # mergeall
      data_fishbase_final <- merge(nas,otherfb,by="fb_sci_name")
    
  #save 
    write.csv2(data_fishbase_final, here::here("results","02_scrap", "fishbase_table.csv"), row.names = FALSE)
    
#----
    
    
    