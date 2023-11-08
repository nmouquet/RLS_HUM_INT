###################################################################################################
#'  Scrap each species for FLICKR, NCBI, WIKIPEDIA and WOS
#'
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#'
#'  Produce 
#'    - one file per species, all stored in results/02_scrap/wta
#'  They are not pushed on the git repo (too much files)
#'    - WTA_agregated.csv which combine all the metrics stored in results/02_scrap/wta 
#'      in a single dataframe
#'    
#'         
#' @date 2023/05/06
##################################################################################################

rm(list = ls())

#FUNCTIONS----

#Short cuts to create the names of the sp or files 

  file_to_sp <- function(id){gsub(".RData","",gsub("_"," ",id))}
  sp_to_file <- function(id){paste0(gsub(" ","_",id),".RData")}

#Find synonyms WORMS with taxsize
  
  syn_worms <- function(id){
    # create a vector with all the synonyms associated to the accepted name of a species
    syn     <- taxize::synonyms(x = id, db = "worms", accepted = TRUE)
    syn     <- as.data.frame(syn [[1]])
    
    if(nrow(syn) !=0){
      if (!is.null(syn$scientificname)){
        syn     <- syn$scientificname
        n       <- length(syn)
        # cbind the synonyms vector to a vector full of "syn"
        type <- rep(x = "syn", n = n)
        syn  <- data.frame(name = syn, type = type)} else{ syn <- data.frame(name = NA, type = NA)}
    } else{ syn <- data.frame(name = NA, type = NA)} # eo if and eo else
    # rbind synonyms and accepted names
    
    species           <- cbind(id, "acc")
    colnames(species) <- c("name", "type")
    species           <- rbind(species, syn)
    species           <- unique(species)
    if(nrow(species) > 1 & length(which(is.na(species$name))) !=0){species <- species[-which(is.na(species$name)),]} # eo if
    species
  }
  
#Remove accent 

  accent <- function(text){
  
  text <- gsub("ä","a",text)
  text <- gsub("å","a",text)
  text <- gsub("à","a",text)
  text <- gsub("â","a",text)
  text <- gsub("ã","a",text)
  text <- gsub("á","a",text)
  text <- gsub("À","A",text)
  text <- gsub("Á","A",text)
  
  text <- gsub("é","e",text)
  text <- gsub("è","e",text)
  text <- gsub("ê","e",text)
  text <- gsub("É","E",text)
  text <- gsub("È","E",text)
  text <- gsub("Ё","e",text)
  text <- gsub("ë","e",text) 
  
  text <- gsub("ç","c",text)
  
  text <- gsub("î","i",text)
  text <- gsub("í","i",text)
  text <- gsub("ï","i",text)
  
  text <- gsub("ñ","n",text)
  
  text <- gsub("ö","o",text)
  text <- gsub("ô","o",text)
  text <- gsub("ó","o",text)
  text <- gsub("ò","o",text)
  text <- gsub("ð","o",text)
  text <- gsub("œ","oe",text)
  
  text <- gsub("ù","u",text)
  text <- gsub("û","u",text)
  text <- gsub("ú","u",text)
  text <- gsub("ü","u",text)
  
  text <- gsub("’","'",text)
  text <- gsub("\"","",text)
  
  text <-  gsub("\nEN","",text)
  
  
  text <- as.character(text)
  
  library(stringi)
  text <- stringi::stri_trans_general(text, "Latin-ASCII")
  
  return(text)
  
}

#match_SCOPUS

  match_SCOPUS <- function(source) {
  
  #source="JOURNAL OF ZOOLOGY"
  
  source_low <- tolower(source)
  
  if (source_low %in% tolower(SCOPUS_source)) {
    SCOPUS_out <- SCOPUS_source[tolower(SCOPUS_source) %in% source_low]
    check <- "NO"
  } else { 
    source_low <- gsub("the ","",source_low)
    if (source_low %in% tolower(SCOPUS_source)) {
      SCOPUS_out <- SCOPUS_source[tolower(SCOPUS_source) %in% source_low]
      check <- "NO"
    } else {check <- "YES"}
  }
  
  results <- list()
  results[1] <- check
  ifelse (check=="NO",results[2] <- SCOPUS_out,results[2] <- NA)
  if (check=="NO") return(SCOPUS_out) else return (NA)
}

#----

#FUNCTION FLICKR----

  require(httr)
  require(pipeR)
  
  ## construct_api_request
  construct_api_request <- function(text, api_k, page, period){
    paste0("https://www.flickr.com/services/rest/?method=flickr.photos.search"
           ,"&api_key=", api_k
           ,"&text=", text
           ,"&min_upload_date=", period["mindate"]
           ,"&max_upload_date=", period["maxdate"]
           ,"&page=", page
           ,"&format=json"
           ,"&nojsoncallback=1"
    )
  } # eo function
  
  ## get_api_res_text
  get_api_res_text <- function(text, api_k, page, period){
    res <- httr::GET(url = construct_api_request(text, api_k, page, period)) %>>% content( as = "text" , encoding = "UTF-8") %>>% jsonlite::fromJSON() 
    if(res$stat != "ok") stop("Error: Request returned non ok status ")
    res$photos
  } # eo function
  
  ## sample_api
  sample_api <- function(api = NULL){sample(api, 1)} # eo function

  ##Call Flickr 

  flickr <- function(id,key,period){
  
  id_flk <- gsub(pattern = " ", replacement = "+", x = as.character(id))
  response_init <- get_api_res_text(text = id_flk, api_k = sample_api(api = key),page=1,period = period) 
  if(is.null(nrow(response_init$photo))){n <- 0} else{ n <-  response_init$total}
  
  list(Scientific=id,Flickr_n_images=n,period=period )
  
} # eo function

#----

#FUNCTION WIKIPEDIA----

#get_page_views_lang
# Will search for views in wikipedia of the given name and in every language
# get the number of views for the page of 1 name in 1 language
  get_page_views_lang <- function(lan, nam = name, start_date, end_date){ 
  # dates must be in format  AAAAMMDDHH example "2015100100"
  # lan = "en" ; nam = "Apogon capricornis"
  tryCatch({res <- pageviews::article_pageviews(project = paste(lan, "wikipedia.org", sep = "."), article = nam ,start = start_date, end = end_date)
  sum(res$views)},
  error = function(e){
    res   <- data.frame(project = "wikipedia", language = lan, article = nam, access = 0, agent = 0, granularity = 0, date = 0, views = 0)
    sum(res$views)
  })
}

# get_all_wiki_views
# concatenates the number of views for 1 species for all languages studied + the total number of views
  get_all_wiki_views <- function(name, languages, start_date, end_date){

  #start_date <- "2015100100"
  #end_date <- "2023023100"
  #languages <- c("en", "es", "fr","de","ru","pl","nl","it","pt")
  #name=sp_id
  
  lang_views        <- vector(mode = "numeric", length = length(languages)+2)
  col_lang          <- paste(languages, "views", sep = "_")
  names(lang_views) <- c("name", col_lang, "total_views")  
  tot <- 0
  for (i in 1:length(languages)){
    i=1
    lang_views[i+1] <- get_page_views_lang(lan = languages[i], nam = name, start_date, end_date)
    tot <- tot + lang_views[i+1]
  }
  lang_views[length(lang_views)] <- tot
  lang_views[1] <- name
  
  #cat(name, "DONE \n")
  # lang_views <- as.data.frame(lang_views)
  lang_views
}#eo getwikidata


# get_length_wiki_articles
# Get the nomber of character in all articles concerning the species
  get_length_wiki_articles <- function(languages, name){
  
  #name <- "Pomacanthus imperator"
  #languages = c("en", "es", "fr","de")
  
  wiki_length_lang <- do.call(cbind, lapply(languages, function(lang){
    
    tryCatch({
      wp_info  <- WikipediR::page_info(language = lang, project = "wikipedia", page = name) # info on the wikipage corresponding to the species in a given language  
      num      <- names(grep(pattern = "fullurl", wp_info$query$pages, value = TRUE))            # get the id number of the page
      obj_num  <- get(x = paste0(num), pos = wp_info$query$pages)                                # Actually get the object
      page_url <- obj_num$fullurl                                                                # get the URL of the page
      # equivalent of the following lines in tidy but i think the usuall wau of coding is easiest to understand piece by piece : sample = url %>% read_html() %>% html_node('body #content #bodyContent #mw-content-text .mw-parser-output table') %>% html_table(fill = TRUE)
      wiki_text    <- rvest::read_html(page_url)                                                                          # get the content of the wiki page
      wiki_text    <- rvest::html_nodes(x = wiki_text, 'body #content #bodyContent #mw-content-text .mw-parser-output p') # get the body text of the article (if first time reading a html file, do it step by step to where you are going)
      wiki_text    <- rvest::html_text(wiki_text,trim = TRUE)                                                            # turn it into a list to be able to count the number of characters
      nb_word_wiki <- sum(lengths(strsplit(wiki_text, " ")))
      close(url(page_url))
      nb_word_wiki
    }, # end of case where it is fine
    error = function(e){
      nb_word_wiki <- 0
      return(nb_word_wiki)
    } # end of error
    ) # end of tryCatch
  })) # end of wiki_length_lang
  # sum the length of all pages in differnt languages
  total            <- sum(wiki_length_lang,na.rm = TRUE)
  # add it to the vector
  wiki_length_lang <- cbind(wiki_length_lang, total)
  # add a column with the name and set column names
  wiki_length_lang           <- cbind(name, wiki_length_lang)
  col_lang                   <- paste(languages, "length", sep = "_")
  colnames(wiki_length_lang) <- c("name", col_lang, "total_length")
  wiki_length_lang           <- as.data.frame(wiki_length_lang)
  
  #cat(name, "DONE \n")
  wiki_length_lang
}

#----

#FUNCTION WIKISPECIES----
  wikispecies <- function(id_sp){
  #id_sp <- "Cyprinus carpio"
  res <- wikitaxa::wt_wikispecies(id_sp)
  when <- Sys.Date()
  
  if (length(res)>0){
    nb_lan <- length(unique(res$langlinks$lang)) 
    if (nrow(res$common_names)>0){
      nb_com <- length(res$common_names$name)
      
      if (sum(grepl("français",tolower(res$common_names$language)))>=1){
        com_fr <- res$common_names$name[grepl("français",tolower(res$common_names$language))]
      } else com_fr=NA
      if (sum(grepl("english",tolower(res$common_names$language)))>=1){
        com_en <- res$common_names$name[grepl("english",tolower(res$common_names$language))]
      } else com_en=NA
      
    } else 
    {
      nb_com <- 0
      com_fr <- NA
      com_en <- NA
    }
    
    wikisp_stat <- cbind.data.frame(name=id_sp,nb_lan=nb_lan,nb_com=nb_com,com_fr=com_fr,com_en=com_en)
    wiki_lang <- res$langlinks$lang
    wiki_ext_links <- res$externallinks
    
    list(wikisp_stat=wikisp_stat,wiki_lang=wiki_lang,wiki_ext_links=wiki_ext_links,when=when)
  } else {
    
    list(wikisp_stat=NA,wiki_lang=NA,wiki_ext_links=NA,when=when)
  }
  
}
#----

#SCRAP THEM ALL----

  RLS_species <- read.csv2(here::here("results","01_build_species_list","RLS_species_init.csv"))
  #https://www.kaggle.com/datasets/maksymshkliarevskyi/scopus-source-list
  SCOPUS <- read.csv(here::here("data", "scopus", "CS_2019.csv"))  
  SCOPUS_source <- unique(SCOPUS$Title)
  
  path_wta_files <- here::here("results","02_scrap","wta")

  mm_sp_list <- RLS_species$fb_sci_name
  feed_the_WTA <- RLS_species$fb_sci_name[!RLS_species$fb_sci_name %in% gsub(".RData","",list.files(path_wta_files))]
  feed_the_WTA <- gsub("_"," ",feed_the_WTA)

  still=length(feed_the_WTA)
  toend <- length(feed_the_WTA)

  for (i in 1:toend) {
  #i=1
  sp_id <- feed_the_WTA[i]
  #sp_id <- "Jenkinsia lamprotaenia"
  
  still=still-1
  
  cat("==================","\n")
  cat("N=",which(feed_the_WTA %in% sp_id),"  still ",still," to scrap...","\n\n")
  
  #get the synonyms 
  cat("SYNONYMS","\n")
  ER <- TRUE
  while(ER){tryCatch({
    ids <- syn_worms(sp_id)
    ER <- FALSE},
    error = function(e){
      ER <- TRUE
      cat("Error with the scraping, wait 60s and retry ... ","\n")
      Sys.sleep(60)}
  )
  }
  
  ids$name <- accent(ids$name)
  
  #remove duplicated synomims (ex Xxxxx yyyyy yyyyy -> Xxxxx yyyyy)
  dup <- if (length(ids$name>1)){
    do.call(rbind,lapply(ids$name , function(id){
      #id=ids$name[2]
      dup_id <- unlist(strsplit(id,' '))
      if (length(dup_id)>2){
        dup_id <- dup_id[!stri_duplicated(dup_id)]
      }
      paste(dup_id,collapse = " ")
    }))
  }
  
  ids$name <-dup
  ids <- ids[!stri_duplicated(ids$name),]
  
  #remove synomyms with only two letters in common, some the sites often returns the same stats 
  
  if (length(ids$name)>1){
    
    ids_temp <- ids
    
    for (i in 2:length(ids$name)){
      
      if (stringdist::stringdist(ids$name[1], ids$name[i], method = "lv")<=2) {
        ids_temp <- ids_temp[-i,]
      }
    }
    
    ids <- ids_temp
  }
  
  #FLICKR
  cat("FLICKR","\n")
  
  # Time period to search
  mindate <- "2010-01-01"
  maxdate <- "2023-02-31"
  period  <- c(mindate = mindate, maxdate = maxdate)
  
  #You need your own api keys (from flickr) to run this code, this line source mine and is in gitignore ;) 
  source(here::here('R','apikeys.R'))
  
  #Scrap
  ER <- TRUE
  while(ER){
    flickr_scrap <- tryCatch({
      do.call(rbind,lapply(ids$name, function(id) {
        #id=ids$name[1]
        id_flk <- gsub(pattern = " ", replacement = "+", x = as.character(id))
        
        ## Run the the request on the first page and get the total number of photos
        response_init <- get_api_res_text(text = id_flk, api_k = sample_api(api = key_flickr),page=1,period = period) 
        
        if(is.null(nrow(response_init$photo))){n <- 0} else{ n <-  response_init$total}
        
        cat("  Flickr =",n,"\n")
        
        ER <<- FALSE
        cbind.data.frame(name=id,type=ids$type[ids$name==id],nb_flickr=n)
      }))},
      error = function(e){
        ER <- TRUE
        cat("Error with the scraping, wait 60s and retry ... ","\n")
        Sys.sleep(60)}
    )
  }
  
  Flickr <- list(Flickr_n_images=flickr_scrap,period=period)
  
  #WIKIPEDIA 
    cat("WIKIPEDIA","\n")
    #Together, the 10 most-viewed languages (English, German, Spanish, Russian, Japanese, French, Polish, Dutch, Italian, and Portuguese) accounted for 81.3% of page views.
    #https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/cobi.13702
    
    id_lang    <- c("en", "es", "fr","de","ru","pl","nl","it","pt")
    langs      <- c("english", "spanish", "french","german","russian","polish","dutch","italian","portuguese")
    start_date <- "2015100100" # the function article_pageviews() from the pageviews R package cannot go further in time
    end_date   <- "2022123100"
    period  <- c(mindate = 2015100100, maxdate = 2022123100)
    
    lwiki <- list(ER=TRUE,wiki_scrap=NA)
    while(lwiki$ER){
      tryCatch({
        sp_metadat        <- c(sp_id)
        names(sp_metadat) <- c("name")
        sp_dat_views            <- get_all_wiki_views(name = sp_id, languages = id_lang, start_date = start_date, end_date = end_date)
        Sys.sleep(sample(seq(0.05, 0.3, by = 0.01), 1))
        sp_dat_lenght            <- get_length_wiki_articles(name = sp_id, languages = id_lang)
        wiki_scrap <- data.frame(c(sp_metadat, sp_dat_views[2:11],sp_dat_lenght[2:11]))
        
        cols.num <- colnames(wiki_scrap)[2:21]
        wiki_scrap[cols.num] <- sapply(wiki_scrap[cols.num],as.numeric)
        wiki_scrap$total_views <- apply(wiki_scrap[, 2:10],1, sum,na.rm=TRUE)
        wiki_scrap$total_length <- apply(wiki_scrap[, 12:20],1, sum,na.rm=TRUE)
        
        cat("  Wiki_views =",wiki_scrap$total_views)
        cat("  Wiki_lenght =",wiki_scrap$total_length ,"\n")
        
        lwiki$ER <- FALSE
        lwiki$wiki_scrap <-wiki_scrap 
        lwiki
      },
      error = function(e){
        lwiki$ER <- TRUE
        cat("Error with the scraping, wait 60s and retry ... ","\n")
        Sys.sleep(60)}
      )
    }
    
    Wikipedia <- list(Wiki_stat=lwiki$wiki_scrap,period=period)
    
  #WIKISPECIES 
    cat("WIKISPECIES","\n")
    Wiki_species <- wikispecies(sp_id)
  
  #NCBI 
    cat("NCBI","\n")
    
    #You need your own ncbi keys to run this code
    ncbi_token <- Sys.getenv("NCBI_KEY")
    
    ER <- TRUE
    while(ER){
      data_ncbi <-  tryCatch({
        do.call(rbind,lapply(ids$name, function(id) {
          
          #id <- ids$name[1]
          temp          <- rentrez::entrez_global_query(term = id)
          ER <<- FALSE
          cbind.data.frame(name=id,type=ids$type[ids$name==id],t(temp))
          
        }))},
        error = function(e){
          ER <- TRUE
          cat("Error with the scraping, wait 60s and retry ... ","\n")
          Sys.sleep(60)
        })
    }
    
    NCBI <- list(NCBI_stat=data_ncbi,when=Sys.Date())
    
    Sys.sleep(sample(seq(0.05, 0.5, by = 0.01), 1))
  
  #WOS
    source(here::here("R", "wos_api.R"))
    #https://api.clarivate.com/swagger-ui/?apikey=18843364c98b6aac5d84b80749a86c8f308d31b9&url=https%3A%2F%2Fdeveloper.clarivate.com%2Fapis%2Fwoslite%2Fswagger%3FforUser%3D6338b7da0d39a94a0e33e7ff01d4901e8f219061
    
    wos_intel_all <- NULL
    
    wos_intel_all <- do.call(rbind,lapply(ids$name, function(id) {
      
      #id <- ids$name[2]
      ER <- TRUE
      while(ER){
        tryCatch({
          wos_hit <- rwoslite::wos_search(paste0("TS=\"",id,"\""))
          cat("      wos_hit= ",wos_hit,"\n")
          
          if (wos_hit>0) {
            wos_intel <- rwoslite::wos_get_records(paste0("TS=\"",id,"\""),limit = 30000)
            
          }
          ER <- FALSE
        },
        error = function(e){
          ER <- TRUE
          cat("Error with the scraping, wait 60s and retry ... ","\n")
          Sys.sleep(60)
        })
      }
      if (wos_hit>0){wos_intel}
    }))
    
    wos_intel_all <- wos_intel_all[!duplicated(wos_intel_all$title),]
    
    wos_hit <- nrow(wos_intel_all)
    
    if (!is.null(wos_hit)) {
      
      wos_scopus <- wos_intel_all$source[tolower(wos_intel_all$source) %in% tolower(SCOPUS_source)]
      
      if (length(wos_scopus)>0){
        
        wos_scopus <- do.call(rbind,parallel::mclapply(wos_scopus, function(wos_id){
          #wos_id=wos_scopus[1]
          intel <- SCOPUS[SCOPUS$Title==match_SCOPUS(wos_id),]
          Title <- intel$Title[1]
          CiteScore <- intel$CiteScore[1]
          ASJC_field <- paste(intel$Scopus.Sub.Subject.Area,collapse = "_")
          Publisher <- intel$Publisher[1]
          Open_Access <- intel$Open.Access[1]
          E.ISSN <- intel$E.ISSN[1]
          cbind.data.frame(Title=Title,CiteScore=CiteScore,ASJC_field=ASJC_field,
                           Publisher=Publisher,Open_Access=Open_Access,E.ISSN)
        },mc.cores = parallel::detectCores()-1))
        
        tot_ref_scopus <- nrow(wos_scopus)
        mean_CiteScore <- mean(wos_scopus$CiteScore,na.rm = T)
        
        sci_fields_tot_ref <- data.frame(table(unlist(strsplit(wos_scopus$ASJC_field,"_"))))
        
        fields <- as.vector(sci_fields_tot_ref$Freq)
        names(fields) <- as.vector(sci_fields_tot_ref$Var1)
        shan_fields <- vegan::diversity(fields, "shannon")
        simp_fields <- vegan::diversity(fields, "simpson")
        N_fields <- nrow(sci_fields_tot_ref)
        
      } else {
        wos_intel=wos_intel_all
        wos_scopus=NA
        tot_ref_scopus=0
        shan_fields=0
        mean_CiteScore=0
        simp_fields <- 0
        N_fields <- 0
      }
    } else {
      wos_intel=0
      wos_scopus=NA
      tot_ref_scopus=0
      shan_fields=0
      mean_CiteScore=0
      simp_fields <- 0
      N_fields <- 0
    }
    if (is.null(wos_hit)) wos_hit <- 0
    if (is.null(wos_intel_all)) wos_intel_all <- 0
    
    WOS <- list(WOS_stat=cbind.data.frame(name=sp_id,type="all",wos_all=wos_hit,wos_sco_refs=tot_ref_scopus,
                                          wos_ref_shan=shan_fields,wos_mean_CiteScore=mean_CiteScore,wos_ref_simp=simp_fields,
                                          wos_ref_nfields=N_fields),when=Sys.Date(),WOS_intel=wos_intel_all,WOS_scopus=wos_scopus)
    
 
  #Wrap and return 
  
  WTA <- list(Flickr=Flickr,Wikipedia=Wikipedia,WOS=WOS,NCBI=NCBI,WIKI_species=Wiki_species)
  
  save(WTA,file=here::here(path_wta_files,paste0(gsub(" ","_",sp_id),".RData")))
  
  cat("\n","Done","\n\n")
  
} #toend

#check one file 
  all_files <- list.files(path_wta_files)
  load(here::here(path_wta_files,all_files[1]))
  WTA
  
  all_files[!gsub(".RData","",all_files)%in%RLS_species$fb_sci_name]
  
#----
  
#RESCRAP WOS----
  
  ms.cores=parallel::detectCores()-1
  
  RLS_species <- read.csv2(here::here("results","01_build_species_list","RLS_species_init.csv"))
  SCOPUS <- read.csv(here::here("data", "scopus", "CS_2019.csv"))  
  SCOPUS_source <- unique(SCOPUS$Title)
  
  path_wta_files <- here::here("results","02_scrap","wta")
  
  mm_sp_list <- RLS_species$fb_sci_name
  #feed_the_WTA <- RLS_species$fb_sci_name[!RLS_species$fb_sci_name %in% gsub(".RData","",list.files(path_wta_files))]
  #feed_the_WTA <- gsub("_"," ",feed_the_WTA)
  
  feed_the_WTA <- gsub("_"," ",mm_sp_list)
  
  still=length(feed_the_WTA)
  toend <- length(feed_the_WTA)
  
  for (i in 1:toend) {
    #i=19
    #sp_id <- feed_the_WTA[i]
    #sp_id <- "Dicentrarchus labrax"
    
    still=still-1
    
    cat("==================","\n")
    cat("N=",which(feed_the_WTA %in% sp_id),"  still ",still," to scrap...","\n\n")
    
    #get the synonyms 
    cat("SYNONYMS","\n")
    ER <- TRUE
    while(ER){tryCatch({
      ids <- syn_worms(sp_id)
      ER <- FALSE},
      error = function(e){
        ER <- TRUE
        cat("Error with the scraping, wait 60s and retry ... ","\n")
        Sys.sleep(60)}
    )
    }
    
    ids$name <- accent(ids$name)
    
    #remove duplicated synomims (ex Xxxxx yyyyy yyyyy -> Xxxxx yyyyy)
    dup <- if (length(ids$name>1)){
      do.call(rbind,lapply(ids$name , function(id){
        #id=ids$name[2]
        dup_id <- unlist(strsplit(id,' '))
        if (length(dup_id)>2){
          dup_id <- dup_id[!stri_duplicated(dup_id)]
        }
        paste(dup_id,collapse = " ")
      }))
    }
    
    ids$name <-dup
    ids <- ids[!stri_duplicated(ids$name),]
    
    #remove synomyms with only two letters in common, some the sites often returns the same stats 
    
    if (length(ids$name)>1){
      
      ids_temp <- ids
      
      for (i in 2:length(ids$name)){
        
        if (stringdist::stringdist(ids$name[1], ids$name[i], method = "lv")<=2) {
          ids_temp <- ids_temp[-i,]
        }
      }
      
      ids <- ids_temp
    }

    #WOS
    source(here::here("R", "wos_api.R"))
    #https://api.clarivate.com/swagger-ui/?apikey=18843364c98b6aac5d84b80749a86c8f308d31b9&url=https%3A%2F%2Fdeveloper.clarivate.com%2Fapis%2Fwoslite%2Fswagger%3FforUser%3D6338b7da0d39a94a0e33e7ff01d4901e8f219061
    
    wos_intel_all <- NULL
  
    wos_intel_all <- do.call(rbind,lapply(ids$name, function(id) {
        
        #id <- ids$name[2]
        ER <- TRUE
        while(ER){
        tryCatch({
          wos_hit <- rwoslite::wos_search(paste0("TS=\"",id,"\""))
          cat("      wos_hit= ",wos_hit,"\n")
          
          if (wos_hit>0) {
            wos_intel <- rwoslite::wos_get_records(paste0("TS=\"",id,"\""),limit = 30000)
            
          }
          ER <- FALSE
        },
        error = function(e){
          ER <- TRUE
          cat("Error with the scraping, wait 60s and retry ... ","\n")
          Sys.sleep(60)
        })
        }
        if (wos_hit>0){wos_intel}
        }))
    
    wos_intel_all <- wos_intel_all[!duplicated(wos_intel_all$title),]
    
    wos_hit <- nrow(wos_intel_all)
    
    if (!is.null(wos_hit)) {

      wos_scopus <- wos_intel_all$source[tolower(wos_intel_all$source) %in% tolower(SCOPUS_source)]
      
      if (length(wos_scopus)>0){
        
        wos_scopus <- do.call(rbind,parallel::mclapply(wos_scopus, function(wos_id){
          #wos_id=wos_scopus[1]
          intel <- SCOPUS[SCOPUS$Title==match_SCOPUS(wos_id),]
          Title <- intel$Title[1]
          CiteScore <- intel$CiteScore[1]
          ASJC_field <- paste(intel$Scopus.Sub.Subject.Area,collapse = "_")
          Publisher <- intel$Publisher[1]
          Open_Access <- intel$Open.Access[1]
          E.ISSN <- intel$E.ISSN[1]
          cbind.data.frame(Title=Title,CiteScore=CiteScore,ASJC_field=ASJC_field,
                           Publisher=Publisher,Open_Access=Open_Access,E.ISSN)
        },mc.cores = ms.cores))
        
        tot_ref_scopus <- nrow(wos_scopus)
        mean_CiteScore <- mean(wos_scopus$CiteScore,na.rm = T)
        
        sci_fields_tot_ref <- data.frame(table(unlist(strsplit(wos_scopus$ASJC_field,"_"))))
        
        fields <- as.vector(sci_fields_tot_ref$Freq)
        names(fields) <- as.vector(sci_fields_tot_ref$Var1)
        shan_fields <- vegan::diversity(fields, "shannon")
        simp_fields <- vegan::diversity(fields, "simpson")
        N_fields <- nrow(sci_fields_tot_ref)
        
      } else {
        wos_intel=wos_intel_all
        wos_scopus=NA
        tot_ref_scopus=0
        shan_fields=0
        mean_CiteScore=0
        simp_fields <- 0
        N_fields <- 0
      }
    } else {
      wos_intel=0
      wos_scopus=NA
      tot_ref_scopus=0
      shan_fields=0
      mean_CiteScore=0
      simp_fields <- 0
      N_fields <- 0
    }
    
    if (is.null(wos_hit)) wos_hit <- 0
    
    if (is.null(wos_intel_all)) wos_intel_all <- 0
    
    WOS <- list(WOS_stat=cbind.data.frame(name=sp_id,type="all",wos_all=wos_hit,wos_sco_refs=tot_ref_scopus,
                                          wos_ref_shan=shan_fields,wos_mean_CiteScore=mean_CiteScore,wos_ref_simp=simp_fields,
                                          wos_ref_nfields=N_fields),when=Sys.Date(),WOS_intel=wos_intel_all,WOS_scopus=wos_scopus)
    
    #Wrap and return 
    
    load(here::here(path_wta_files,paste0(gsub(" ","_",sp_id),".RData")))
    
    WTA$WOS <- WOS
    
    save(WTA,file=here::here(path_wta_files,paste0(gsub(" ","_",sp_id),".RData")))
    
    cat("\n","Done","\n\n")
    
  } #toend

#----  
  
#WRAP & CHECK THEM ALL---- 
  
  ms.cores=parallel::detectCores()-1
  
  path_wta_files <- here::here("results","02_scrap","wta")
  all_files <- list.files(path_wta_files)
  
  ##look at particular file and modify some aberrant values
    ###NCBI these species have been modified 
        #"Bodianus_rufus",Epinephelus_maculatus" #"Hemigymnus_fasciatus"
   
      #id <- all_files[2]
      id <- "Synchiropus_splendidus.RData"
      load(here::here(path_wta_files,id))
      
      #WTA$NCBI$NCBI_stat[WTA$NCBI$NCBI_stat$name%in%"Scarus quinque-fasciatus",c(2:38)]=0
      #save(WTA,file=here::here(path_wta_files,id))
    
  
  ##I Check FLICKR (some time it get NULL for the numbers of pictures rather, need to redo the scrap)
    Check_Flickr <- do.call(rbind,(pbmcapply::pbmclapply(all_files,function(id)
    {
      # id <- all_files[20]
      load(here::here(path_wta_files,id))
      
      if (is.null(WTA$Flickr$Flickr_n_images)) id
      
    },mc.cores = ms.cores)))
    Check_Flickr
    
    #redo Check_Flickr if Check_Flickr not NULL
    lapply(as.vector(Check_Flickr), function(file_id)
    {
      
      #file_id <- as.vector(Check_Flickr)[1]
      
      load(here::here(path_wta_files,file_id))
      
      # Time period to search
      mindate <- "2010-01-01"
      maxdate <- "2022-10-31"
      period  <- c(mindate = mindate, maxdate = maxdate)
      
      # API keys (need to implement personal keys here, best to use at least 4 different keys)
      key <- c(nico  = "f7ee248d845197320076269e22ed59f7",
               franz = "25eba0325877c8d3886ea447453fab60",
               nico2 = "7b9a21587a0aeb22af42612c3022694a",
               nico3 = "e150ade03c91a6f353b244222dc7c4ba")
      
      ER <- TRUE
      while(ER){
        flickr_scrap <- tryCatch({
          do.call(rbind,lapply(WTA$NCBI$NCBI_stat$name, function(id) {
            
            #id <- WTA$NCBI$NCBI_stat$name[1]
            
            id_flk <- gsub(pattern = " ", replacement = "+", x = as.character(id))
            
            ## Run the the request on the first page and get the total number of photos
            response_init <- get_api_res_text(text = id_flk, api_k = sample_api(api = key),page=1,period = period) 
            
            if(is.null(nrow(response_init$photo))){n <- 0} else{ n <-  response_init$total}
            ER <<- FALSE
            cat("  Flickr =",id," type=",WTA$NCBI$NCBI_stat$type[WTA$NCBI$NCBI_stat$name==id],"\n")
            cbind.data.frame(name=id,type=WTA$NCBI$NCBI_stat$type[WTA$NCBI$NCBI_stat$name==id],nb_flickr=n)
          }))},
          error = function(e){
            ER <- TRUE
            cat("Error with the scraping, wait 60s and retry ... ","\n")
            Sys.sleep(60)}
        )
        
      }
      
      WTA$Flickr$Flickr_n_images <- flickr_scrap
      
      save(WTA,file=here::here(path_wta_files,file_id))
    })
    
    #redo Check_Flickr if Check_Flickr not NULL
    lapply(as.vector(Check_Flickr), function(file_id)
    {
      
      #file_id <- as.vector(Check_Flickr)[1]
      
      load(here::here(path_wta_files,file_id))
      
      # Time period to search
      mindate <- "2010-01-01"
      maxdate <- "2022-10-31"
      period  <- c(mindate = mindate, maxdate = maxdate)
      
      # API keys (need to implement personal keys here, best to use at least 4 different keys)
      key <- c(nico  = "f7ee248d845197320076269e22ed59f7",
               franz = "25eba0325877c8d3886ea447453fab60",
               nico2 = "7b9a21587a0aeb22af42612c3022694a",
               nico3 = "e150ade03c91a6f353b244222dc7c4ba")
      
      ER <- TRUE
      while(ER){
        flickr_scrap <- tryCatch({
          do.call(rbind,lapply(WTA$NCBI$NCBI_stat$name, function(id) {
            
            #id <- WTA$NCBI$NCBI_stat$name[1]
            
            id_flk <- gsub(pattern = " ", replacement = "+", x = as.character(id))
            
            ## Run the the request on the first page and get the total number of photos
            response_init <- get_api_res_text(text = id_flk, api_k = sample_api(api = key),page=1,period = period) 
            
            if(is.null(nrow(response_init$photo))){n <- 0} else{ n <-  response_init$total}
            ER <<- FALSE
            cat("  Flickr =",id," type=",WTA$NCBI$NCBI_stat$type[WTA$NCBI$NCBI_stat$name==id],"\n")
            cbind.data.frame(name=id,type=WTA$NCBI$NCBI_stat$type[WTA$NCBI$NCBI_stat$name==id],nb_flickr=n)
          }))},
          error = function(e){
            ER <- TRUE
            cat("Error with the scraping, wait 60s and retry ... ","\n")
            Sys.sleep(60)}
        )
        
      }
      
      WTA$Flickr$Flickr_n_images <- flickr_scrap
      
      save(WTA,file=here::here(path_wta_files,file_id))
    })

  ##II #Handle the doubled values everywhere (for NCBI and FLICKR)
    #sometimes it gets doubled value that need to be removed; pb with the synonyms that link to the same page in reality; 
    #a startegy is to consider that it is unlikely that too synonyms will have the same exact number so we should rather 
    #consider only on of them and set the othes to 0)
  
    pbmcapply::pbmclapply(all_files, function(idsp)
    {
      #idsp <- all_files[1]
      #idsp <- "Chlorocebus_aethiops.RData"
      load(here::here(path_wta_files,idsp))
      
      if (length(WTA)==1) {
        WTA <- WTA[[1]]
        save(WTA,file=here::here(path_wta_files,idsp))
      }
      
      if (dim(WTA$Flickr$Flickr_n_images)[1]>1){
        
        todo <- c("nb_flickr")
        for (id in todo){
          #id <- todo[1]
          tb <- table(WTA$Flickr$Flickr_n_images[,id])
          tb <- tb[names(tb)!=0]
          tb <- as.numeric(names(tb)[tb>1])
          to_r <- which(WTA$Flickr$Flickr_n_images[,id] %in% tb)
          WTA$Flickr$Flickr_n_images[,id][to_r[-1]]=0
        }
        
        todo <- colnames(WTA$NCBI$NCBI_stat)[-c(1,2)]
        for (id in todo){
          #id <- todo[1]
          tb <- table(WTA$NCBI$NCBI_stat[,id])
          tb <- tb[names(tb)!=0]
          tb <- as.numeric(names(tb)[tb>1])
          to_r <- which(WTA$NCBI$NCBI_stat[,id] %in% tb)
          WTA$NCBI$NCBI_stat[,id][to_r[-1]]=0
        }
       
        save(WTA,file=here::here(path_wta_files,idsp))
      }
    },mc.cores = ms.cores)
    
  ##IIbis #Handle the syn names with "-" in it (NCBI does not like them ... )
    
    pbmcapply::pbmclapply(all_files, function(idsp)
    {
      #idsp <- all_files[146]
      #idsp <- "Leptoscarus_vaigiensis.RData"
      load(here::here(path_wta_files,idsp))

      if(!is.null(WTA$NCBI$NCBI_stat)){
        if (dim(WTA$NCBI$NCBI_stat)[1]>1){
          
          WTA$NCBI$NCBI_stat <- WTA$NCBI$NCBI_stat[!grepl("-",WTA$NCBI$NCBI_stat$name),]
          save(WTA,file=here::here(path_wta_files,idsp))
        }
      }
     
    },mc.cores = ms.cores)
    
  ##III Construct the First WTA_agregated dataframe #add one zoom when possible
  
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
                             WOS_n_fields=WTA$WOS$WOS_stat$wos_ref_nfields)
  
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
      
    },mc.cores = ms.cores)))
    
    write.csv2(WTA_agregated,here::here("results","02_scrap","WTA_agregated.csv"),row.names = F)
  
  ##V Check values 
  ##locate the aberrant values in checkfile, correct by hand and save 
  
    ###look them all on exel and correct the Flickr if needed 
  
      chk_names <- WTA_agregated$Scientific
    
      chk_wta <- cbind.data.frame(Scientific=chk_names,
                                   FLICKR_tot=WTA_agregated$FLICKR_tot[WTA_agregated$Scientific %in% chk_names],
                                   WIKI_views=WTA_agregated$WIKI_views[WTA_agregated$Scientific %in% chk_names],
                                   WIKI_lenght=WTA_agregated$WIKI_lenght[WTA_agregated$Scientific %in% chk_names],
                                   WOS_all=WTA_agregated$WOS_all[WTA_agregated$Scientific %in% chk_names],
                                   NCBI_nuccore=WTA_agregated$NCBI_nuccore[WTA_agregated$Scientific %in% chk_names],
                                   NCBI_protein=WTA_agregated$NCBI_protein[WTA_agregated$Scientific %in% chk_names],
                                   NCBI_popset=WTA_agregated$NCBI_popset[WTA_agregated$Scientific %in% chk_names]
      )
    
      write.csv2(chk_wta,file=here::here("results","02_scrap","chk_wta.csv"),row.names = F)
    
      ####correct by hand (to be done !!)
      ## Flickr of Boops boops was over evaluated for (because of betty boops) (checked by hand and set to 120)
      ## Flickr of Coris julis had to be modified because of the syn Julis julis (set to 0)
      ## Flickr of Halichoeres brasiliensis had to be modified because of the syn Julis principis (set to 0)
    
      id <- "Halichoeres_brasiliensis.RData"
      load(here::here(path_wta_files,id))
      WTA$Flickr$Flickr_n_images
      WTA$Flickr$Flickr_n_images$nb_flickr[3]=0
      save(WTA,file=here::here(path_wta_files,id))
      
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
                               WOS_n_fields=WTA$WOS$WOS_stat$wos_ref_nfields)
        
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
        
      },mc.cores = ms.cores)))
      
      write.csv2(WTA_agregated,here::here("results","02_scrap","WTA_agregated.csv"),row.names = F)
  
    ###Look at the residual to detect outliers and recheck the data 
    ###WIKI_views vs FLICKR_tot
    ###WOS_all vs NCBI_nuccore
    ###NCBI_protein vs NCBI_nuccore
  
      model.lm = lm(log(NCBI_protein+1) ~ log(NCBI_nuccore+1),na.action=na.exclude, data=WTA_agregated) 
      summary(model.lm)
      model.res = resid(model.lm) 
      plot(fitted(model.lm), model.res)
      abline(0,0)
    
      maxres <- 5
      minres <- -5
      chk_wta <- cbind.data.frame(Scientific=c(WTA_agregated$Scientific[as.numeric(names(model.res[model.res > maxres]))],
                                                  WTA_agregated$Scientific[as.numeric(names(model.res[model.res < minres]))]),
                                     Flickr=c(WTA_agregated$FLICKR_tot[as.numeric(names(model.res[model.res > maxres]))],
                                              WTA_agregated$FLICKR_tot[as.numeric(names(model.res[model.res < minres]))]),
                                     Wiki_lenght=c(WTA_agregated$WIKI_lenght[as.numeric(names(model.res[model.res > maxres]))],
                                                   WTA_agregated$WIKI_lenght[as.numeric(names(model.res[model.res < minres]))]),
                                     Wiki_views=c(WTA_agregated$WIKI_views[as.numeric(names(model.res[model.res > maxres]))],
                                                  WTA_agregated$WIKI_views[as.numeric(names(model.res[model.res < minres]))]),
                                     WOS_all=c(WTA_agregated$WOS_all[as.numeric(names(model.res[model.res > maxres]))],
                                               WTA_agregated$WOS_all[as.numeric(names(model.res[model.res < minres]))]),
                                     NCBI_nuccore=c(WTA_agregated$NCBI_nuccore[as.numeric(names(model.res[model.res > maxres]))],
                                                    WTA_agregated$NCBI_nuccore[as.numeric(names(model.res[model.res < minres]))]),
                                     NCBI_protein=c(WTA_agregated$NCBI_protein[as.numeric(names(model.res[model.res > maxres]))],
                                                    WTA_agregated$NCBI_protein[as.numeric(names(model.res[model.res < minres]))]),
                                     NCBI_popset=c(WTA_agregated$NCBI_popset[as.numeric(names(model.res[model.res > maxres]))],
                                                   WTA_agregated$NCBI_popset[as.numeric(names(model.res[model.res < minres]))]))
      
      write.csv2(chk_wta,file=here::here("results","02_scrap","chk_wta.csv"),row.names = F)
    
#----
