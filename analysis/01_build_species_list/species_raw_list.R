###################################################################################################
#'  Set up the list of species we will work with
#'  Update the list used in Langlois et al. 2022
#'
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com}
#'
#'  Produce 
#'    - RLS_species_init.csv
#'         
#' @date 2023/05/06
#' @lastrun 2023/07/01
##################################################################################################

rm(list = ls())

# Load sp_list
  sp_list_RLS <- read.csv(file = here::here("Data","Langlois_el_al_2022.csv"))
  sp_list_RLS <- rutils::rename_col(sp_list_RLS,"sp_name","rls_sci_name")
  
# Clean the names and correspondence with fishbase using the rfishbase package

  ## Deal with rls names
  Species <- gsub("_"," ",sp_list_RLS$rls_sci_name)
  Species_corrected <- rep("NA",length(Species))
  SpecCode <- rep("NA",length(Species))
  
  fb_Names <- pbmcapply::pbmclapply(1:length(Species), mc.cores = parallel::detectCores()-1, function(k){
    #k=1
    test <- rfishbase::validate_names(Species[k])
    if(length(test)==1){
      Species_corrected[k] <- test
      SpecCode[k] <- as.numeric((unique(rfishbase::species(test,fields="SpecCode"))))
    }else{
      next
    }
    c(Species_corrected[k], SpecCode[k])
  })#end of k
  
  fb_Names <- lapply(1:length(Species), function(k){ #
    #k=1
    cat("k=",k,"\n")
    test <- rfishbase::validate_names(Species[k])
    if(length(test)==1){
      Species_corrected[k] <- test
      SpecCode[k] <- as.numeric((unique(rfishbase::species(test,fields="SpecCode"))))
    }else{
      next
    }
    c(Species_corrected[k], SpecCode[k])
  })#end of k
  
  list_sp_rls <- as.data.frame(gsub(" ", "_", cbind(Species, do.call(rbind, fb_Names))))
  colnames(list_sp_rls) <- c("rls_sci_name", "fb_sci_name", "spec_code")
  list_sp_rls$spec_code <- as.numeric(list_sp_rls$spec_code)
  
    #total species drops from 2017 to 2014
    #list_sp_rls[duplicated(list_sp_rls$fb_sci_name),]
    #list_sp_rls[list_sp_rls$fb_sci_name%in%'Kyphosus_vaigiensis',]
    #list_sp_rls[list_sp_rls$fb_sci_name%in%'Pseudocaranx_dentex',]
  
    #there is a NA in list_sp_rls$fb_sci_name (for no reason : Suezichthys_devisi)
    list_sp_rls$fb_sci_name[list_sp_rls$rls_sci_name%in%"Suezichthys_devisi"] <- "Suezichthys_devisi"
  
  ## merge all and save 
  
    RLS_species_init <- merge(list_sp_rls,sp_list_RLS)
    RLS_species_init <- RLS_species_init[!duplicated(RLS_species_init$fb_sci_name),]
    RLS_species_init <- RLS_species_init[!is.na(RLS_species_init$fb_sci_name),]
    
    write.csv2(RLS_species_init,here::here("results","01_build_species_list","RLS_species_init.csv"),row.names = F)
