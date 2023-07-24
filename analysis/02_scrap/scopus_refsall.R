###################################################################################################
#'  Add the citation after the WTA files have been produced
#'  Read all the WTA files to get all the refs 
#'  Call scopus to get the citations 
#'  Re-open each WTA files to add the citations
#'  The WPA aggregated script must be run again after this procedure  
#'
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#' 
#' Produce 
#'    - WTA_aggregated.csv
#' 
#' @date 2023/05/06
##################################################################################################

rm(list = ls())

#FUNCTIONS----

#short cuts to create the names of the sp or files 

file_to_sp <- function(id){gsub(".RData","",gsub("_"," ",id))}
sp_to_file <- function(id){paste0(gsub(" ","_",id),".RData")}

#SCOPUS 

# SCOPUS (need to implement personal api keys here, best to use at least 8 different keys)
# accepted and synonyms

scopus_check <- function(api){
  do.call(rbind,lapply(api, function(api_id){
    
    temp <- httr::GET(paste0('http://api.elsevier.com/content/search/scopus?query=heart&apiKey=',api_id)) 
    head_temp <- httr::headers(temp)
    rem <- as.numeric(head_temp$`x-ratelimit-remaining`)
    cbind.data.frame(apikey=api_id,remaining=rem)
  }))
}

scopus_funk <- function(title,api){
  #title <- gsub("-"," ",all_unik_refs_todo$title[35])
  #api <- api
  
  title <- gsub("-"," ",title)
  
  
  options("elsevier_api_key" = sample(api,1))
  
  an.error.occured <- FALSE
  
  id_scop <- paste0("TITLE({",title,"})")
  
  tryCatch({
  res <- rscopus::scopus_search(query = id_scop, max_count = 1,
                               count = 1,verbose=FALSE)},
  error = function(e){
   an.error.occured <<- TRUE
  }
  )
  
  if (!an.error.occured){
    if (!is.null(res$entries[[1]]$`citedby-count`)) {
      cit_tot <- res$entries[[1]]$`citedby-count`
      scop_id <- res$entries[[1]]$`source-id`
      if(is.null(scop_id)) scop_id <- NA
      
      scout <- cbind.data.frame(scopus_id=scop_id,scopus_citation=cit_tot)
    } else {
      scout <- cbind.data.frame(scopus_id=NA,scopus_citation=NA)
    }
  } else {scout <- cbind.data.frame(scopus_id=NA,scopus_citation=NA)}
  return(scout)
}

#----

#API KEYS----

api1 <- c("01352a1da7c0c0ce38c10e9f171f630b",
         "380f20901b6ea9cf4835328cd68060b0",
         "7be627843a435062da54c10c70204381",
         "75a72ff077a0137e7269c1fde36b24d4",
         "ba8f4dd7af2644302b5c65b6a3241f7f",
         "0e612b221d73db6123bee130d0ce3a51",
         "9728caf1098713c147967b8c9e3bac12",
         "29588cb4c51e74a2920dca66e1660764",
         "92511e2f09b62725d3ae48155b9ce4e1")

api2 <- c("674879e869e2bf1636d7fba199212944",
         "d9a35576b1a23f910fa351077f2f8820",
         "a9225f30145ccac003f49db5f2d3171d",
         "dff159d88bc985adee93a325faeb0d6a",
         "6d7b03850299c7d4892c8225738687d7",
         "abe562c8136236f4f2e1c2fbd0125a1e",
         "477a7bf3fdcb7980c09ecd7c4612843f",
         "99308e077664ebc231e04767fbb3a50a",
         "6b88c7368f7f214e30725257a94e9398",
         "a5fb446c07b0324b79b7b47ba47ffe5a")

api3 <- c("e4642501a4b441bd696e2d8a655f8d53",
         "34b6a1169d1e57e459be3cecf311904e",
         "e35b58f526498357d2be2a9d74353d46",
         "8e8a7e9f7030419ac187f8ef02d514d4",
         "91d6936e2c86a275a0cda35638bb9ffa",
         "cd538933f5d80bafbe75a80a7af65172",
         "82fec7b2b7a5e97a7726436789d704d6",
         "593113b6a7bf7c8a58567b477bba7848",
         "e79317514a0f0aca0ae8c3538f819408",
         "20e480f61b8d3f02a3e3d402b5eadab7")

api4 <- c("37ac38d73e0dcb8a2fb9d5cf75714fe1",
          "2f29d49d9ed62cf29b6c5e8f169fa01a",
          "ce872c70a9bdd7b825f8854045b7b0db",
          "deff8a5417909f7de3157bcf2673a40b",
          "341097e14e2b27215b9607901e28b163",
          "2d065516d8a5ba2894e9174a018b9ba1",
          "401662d129a80b2882508fca8ef64ea8",
          "773c84985da00cfc3ef437c3a60d8c43",
          "8f40a1cfde02a16a357d971ce2ecbf85",
          "fbda27fc68382341b5de5e112032cf7f")

#----

#GET REFS FIRST RUN----
  #this need to be run the first time to generate the files 
  #that will be used afterward if the scrap is done in several 
  #sessions 

  #You need your own api keys (from Scopus) to run this code, this line source mine and is in gitignore ;) 
  source(here::here('R','apikeys.R'))

  api <- api1
  scopus_check(api)

  path_wta_files <- here::here("results","02_scrap","wta")
  all_files <- list.files(path_wta_files)
  
  ##get all the refs present in the wta files 
    all_refs <- do.call(rbind,pbmcapply::pbmclapply(all_files,function(id)
    {
    #id <- all_files[1]
    #id <- "Acanthaluteres_brownii.RData"
    load(here::here(path_wta_files,id))
    if(!is.null(dim(WTA$WOS$WOS_intel))) WTA$WOS$WOS_intel
  },mc.cores = parallel::detectCores()-1))
    
  ##remove duplicated refs and keep only the ut and the title
    
    all_unik_refs <-all_refs[!duplicated(all_refs),c("ut","title")]
    save(all_unik_refs,file=here::here("results","02_scrap","scopus","all_unik_refs_todo.RData"))
  
  #run one scrap on 100 refs and save the all_unik_refs_done.RData
    
    all_unik_refs_done <- all_unik_refs[1:100,]
    
    all_unik_refs_done$scopus_id <- NA
    all_unik_refs_done$scopus_citation <- NA
    
    for(i in 1:nrow(all_unik_refs_done)){ #
      
      if (pingr::is_online()){
        #i=1
        cat("i= ",i," still ",nrow(all_unik_refs_done)-i," to go ...\n")
        scop <- scopus_funk(all_unik_refs_done$title[i],api=api)
        if (scop$scopus_id%in%"ERROR"){
          cat('LIMIT SCRAP REACHED ... STOP ...')
          break
        }
        
        all_unik_refs_done$scopus_id[i] <- scop$scopus_id
        all_unik_refs_done$scopus_citation[i] <- scop$scopus_citation
        
        cat("   id=",all_unik_refs_done$scopus_id[i],"    cit=",all_unik_refs_done$scopus_citation[i],"\n")
        #cat("   title=",all_unik_refs$title[i],"\n")
        
      } else 
      {
        cat('NO INTERNET CONNECTION, restart at i=',i,"\n")
        break
      }
      
    } #
    
    save(all_unik_refs_done,file=here::here("results","02_scrap","scopus","all_unik_refs_done.RData"))
#----   
    
#GET REFS----
    
    api <- api1
    scopus_check(api)
    
    path_wta_files <- here::here("results","02_scrap","wta")
    all_files <- list.files(path_wta_files)
    
    #final_table <- read.csv2(here::here("results","05_assemlble_knowInt","05_Human_Interest_final_table.csv"))
    #all_files <- all_files[gsub(".RData","",all_files)%in%final_table$fb_sci_name]
    
    all_refs <- do.call(rbind,pbmcapply::pbmclapply(all_files,function(id)
    {
      #id <- all_files[722]
      #id <- "Salmo_salar.RData"
      load(here::here(path_wta_files,id))
      if(!is.null(dim(WTA$WOS$WOS_intel))) WTA$WOS$WOS_intel[,c("ut","title")]
    },mc.cores = parallel::detectCores()-1))
   
    ##remove duplicated refs and keep only the ut and the title
    
    all_unik_refs <-all_refs[!duplicated(all_refs),]
    
    ##remove the ref already done 
    
    load(file=here::here("results","02_scrap","scopus","all_unik_refs_done.RData"))
    
    all_unik_refs_to_do <- all_unik_refs[!all_unik_refs$title%in%all_unik_refs_done$title,]
    
    ##do the scrap
    
    all_unik_refs_new <- all_unik_refs_to_do
    
    all_unik_refs_new$scopus_id <- NA
    all_unik_refs_new$scopus_citation <- NA
    
    for(i in 1:nrow(all_unik_refs_new)){ #
      
      if (pingr::is_online()){
        #i=1
        cat("i= ",i," still ",nrow(all_unik_refs_new)-i," to go ...\n")
        scop <- scopus_funk(all_unik_refs_new$title[i],api=api)
        if (scop$scopus_id%in%"ERROR"){
          cat('LIMIT SCRAP REACHED ... STOP ...')
          break
        }
        
        all_unik_refs_new$scopus_id[i] <- scop$scopus_id
        all_unik_refs_new$scopus_citation[i] <- scop$scopus_citation
        
        cat("   id=",all_unik_refs_new$scopus_id[i],"    cit=",all_unik_refs_new$scopus_citation[i],"\n")
        
      } else 
      {
        cat('NO INTERNET CONNECTION, restart at i=',i,"\n")
        break
      }
      
    } #
    
    all_unik_refs_done <- rbind(all_unik_refs_done,all_unik_refs_new)
    
    save(all_unik_refs_done,file=here::here("results","02_scrap","scopus","all_unik_refs_done.RData"))
    
#----

#ADD THE CITATION TO ALL THE WTA FILES----
  
  load(file=here::here("results","02_scrap","scopus","all_unik_refs_done.RData"))

  path_wta_files <- here::here("results","02_scrap","wta")
  all_files <- list.files(path_wta_files)
      
  pbmcapply::pbmclapply(all_files,function(id)
      {
        
        #whc <- which(gsub(".RData","",gsub("_"," ",all_files)) %in% "Pomphorhynchus tereticollis")
        #id=all_files[whc]
        #id=all_files[1]
        
        load(here::here(path_wta_files,id))
        
        if (!is.null(dim(WTA$WOS$WOS_intel))) {
          
          WTA$WOS$WOS_intel$scopus_id=NA
          WTA$WOS$WOS_intel$scopus_citation=NA
          refscop <- all_unik_refs_done[all_unik_refs_done$title %in% WTA$WOS$WOS_intel$title,]
          WTA$WOS$WOS_intel$scopus_id[which(WTA$WOS$WOS_intel$title%in%refscop$title)]=refscop$scopus_id[which(refscop$title%in%WTA$WOS$WOS_intel$title)]
          WTA$WOS$WOS_intel$scopus_citation[which(WTA$WOS$WOS_intel$title%in%refscop$title)]=refscop$scopus_citation[which(refscop$title%in%WTA$WOS$WOS_intel$title)]
          
          # hirsh index of the species
          # get ref id and the number of quotes for each ref
          refs <- WTA$WOS$WOS_intel[!is.na(WTA$WOS$WOS_intel$scopus_citation),]
          
          if (nrow(refs)>0){
            # order refs according to the number of quotes
            refs           <- refs[order(as.numeric(refs$scopus_citation), decreasing = TRUE),]  
            refs$order     <- c(1:nrow(refs))  
            rownames(refs) <- refs$order  
            inmax <- c()
            for(i in 1: nrow(refs)){
              hi    <- as.numeric(refs$scopus_citation[i])
              inmax <- c(inmax, min(hi, i))} # eo for i
            # max
            h_index <- max(inmax)
            
            WTA$WOS$WOS_stat$tot_cit <- sum(as.numeric(refs$scopus_citation))
            WTA$WOS$WOS_stat$H_index <- h_index
          } else 
          {
            WTA$WOS$WOS_stat$tot_cit <- 0
            WTA$WOS$WOS_stat$H_index <- 0
          }
        } else 
        {
          WTA$WOS$WOS_stat$tot_cit <- 0
          WTA$WOS$WOS_stat$H_index <- 0
        }
        
        save(WTA,file=here::here(path_wta_files,id))
        
      },mc.cores = parallel::detectCores()-1)

#----
  
#WPA AGREGATED----
  
  path_wta_files <- here::here("results","02_scrap","wta")
  all_files <- list.files(path_wta_files)
  
  WTA_agregated <- do.call(rbind,(pbmcapply::pbmclapply(all_files, function(id)
  {
    
    #id=all_files[1]
    #id <- "Bombylius_major.RData"
    load(here::here(path_wta_files,id))
    
    if (length(WTA)==1){
      WTA <- WTA[[1]]
      save(WTA,file=here::here(path_wta_files,id))
    }
    
    WTA_agregated <-cbind.data.frame(Scientific=file_to_sp(id),
                                     FLICKR_tot=sum(WTA$Flickr$Flickr$nb_flickr, na.rm = TRUE))
    
    if (!is.null(dim(WTA$WIKI_species$wikisp_stat))){
      
      WTA$WIKI_species$wikisp_stat <- WTA$WIKI_species$wikisp_stat[!duplicated(WTA$WIKI_species$wikisp_stat),]
      
      WTA_agregated <- cbind(WTA_agregated,WIKI_views=WTA$Wikipedia$Wiki_stat[,c('total_views')],
                             WIKI_lenght=WTA$Wikipedia$Wiki_stat[,c('total_length')],
                             WIKI_nblang=WTA$WIKI_species$wikisp_stat$nb_lan,
                             WIKI_nbcom=WTA$WIKI_species$wikisp_stat$nb_com)[1,]
    } else {
      WTA_agregated <- cbind(WTA_agregated,WIKI_views=WTA$Wikipedia$Wiki_stat[,c('total_views')],
                             WIKI_lenght=WTA$Wikipedia$Wiki_stat[,c('total_length')],
                             WIKI_nblang=0,
                             WIKI_nbcom=0)
    }
    
    WTA_agregated <- cbind(WTA_agregated,WOS_all=WTA$WOS$WOS_stat$wos_all,
                           WOS_shan_fields=WTA$WOS$WOS_stat$wos_ref_shan,
                           WOS_CiteScore=WTA$WOS$WOS_stat$wos_mean_CiteScore,
                           WOS_simp_fields=WTA$WOS$WOS_stat$wos_ref_simp,
                           WOS_n_fields=WTA$WOS$WOS_stat$wos_ref_nfields,
                           WOS_tot_cit=WTA$WOS$WOS_stat$tot_cit,
                           WOS_H_index=WTA$WOS$WOS_stat$H_index)
    
    if (!is.null(WTA$NCBI$NCBI_stat)){
      
      if (length(WTA$NCBI$NCBI_stat)==3){
        
        WTA_agregated <- cbind(WTA_agregated,NCBI_nuccore=WTA$NCBI$NCBI_stat$nuccore[1],
                               NCBI_protein=WTA$NCBI$NCBI_stat$protein[1],
                               NCBI_popset=WTA$NCBI$NCBI_stat$popset[1])
        
      } else {
        
        WTA_agregated <- cbind(WTA_agregated,NCBI_nuccore=sum(WTA$NCBI$NCBI_stat[,"nuccore"], na.rm = TRUE),
                               NCBI_protein=sum(WTA$NCBI$NCBI_stat[,"protein"], na.rm = TRUE),
                               NCBI_popset=sum(WTA$NCBI$NCBI_stat[,"popset"], na.rm = TRUE))
      }
      
    } else {
      WTA_agregated <- cbind(WTA_agregated,NCBI_nuccore=0,
                             NCBI_protein=0,
                             NCBI_popset=0)
    }
    
    WTA_agregated
    
  },mc.cores = parallel::detectCores()-1)))
  
  write.csv2(WTA_agregated,here::here("results","02_scrap","WTA_agregated.csv"),row.names = F)
  
#----







