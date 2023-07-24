#################################################
#' Get the number of total publications from ggscholar
#' 
#' @author Nicolas Mouquet , \email{nicolas.mouquet@cnrs.fr}
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#'
#' GGfiles have been scraped by Nicolas Casajus in december 2019
#' using R package Gpack https://github.com/ahasverus/gpack
#'
#'
#################################################

  rm(list = ls())

  sp_files <- list.files(here::here("results","02_scrap","scholar","rawdata"),full.names=T)
  sp_names <- list.files(here::here("results","02_scrap","scholar","rawdata"),full.names=F)

  scholar <- do.call(rbind,pbmcapply::pbmclapply(1:length(sp_files), function(i){
    #i=1410
    id <- sp_files[i]
    
    rls_sci_name <- rutils::to_binomial_name(gsub(".xlsx","",sp_names[i]))
    sp <- openxlsx::read.xlsx(id)
    if (dim(sp)[1]>1){
    cbind.data.frame(rls_sci_name=rls_sci_name,GG_ref_nb=dim(sp)[1],GG_ref_cit=sum(as.numeric(sp$ref_citation),na.rm = T))
    } else {
      cbind.data.frame(rls_sci_name=rls_sci_name,GG_ref_nb=0,GG_ref_cit=0)
    }
  
  },mc.cores = parallel::detectCores()-1))
  
  write.csv2(scholar, here::here("results","02_scrap", "scholar_table.csv"), row.names = FALSE)
  
  
  