###################################################################################################
#'  Create a dataframe with all species synonyms
#'  will be used in gbif scrap
#'  Compute the interest indices 
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com}
#' 
#' Produce 
#'    - all_accepted_syn.csv
#'
#' @date 2022/02/06
##################################################################################################
rm(list = ls())

  path_wta_files <- here::here("results","02_scrap","wta")
  all_files <- list.files(path_wta_files)

  syn_sp <- do.call(rbind,(pbmcapply::pbmclapply(all_files,function(id)
  {
  #id <- all_files[2779]
  load(here::here(path_wta_files,id))
  fb_sci_name <- rep(WTA$Flickr$Flickr_n_images$name[1],nrow(WTA$Flickr$Flickr_n_images))
  name <- WTA$Flickr$Flickr_n_images$name
  type <- WTA$Flickr$Flickr_n_images$type
  cbind.data.frame(fb_sci_name=fb_sci_name,name=name,type=type)
},mc.cores = parallel::detectCores()-1)))

  write.csv2(syn_sp,here::here("results","02_scrap","all_accepted_syn.csv"),row.names = F)


