#################################################
# Webscrap TWITTER
#
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com}
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#' @author Nicolas Casajus \email{nicolas.casajus@@fondationbiodiversite.fr}
#
# the scrap has been performed in 2019 with the built in function 
# get_twitter_intel, note that this function use selenium and the given 
# twitter change its credential ofen there is no guaranty that the 
# function is still working 
#
# the data have been cleaned by hand after the scrap; for some species there 
# was some obvious issue; as the function called the text of the tweet it was 
# possible to remove tweet not related to fishes 
#
# the initial list of fishes contained more species than included in the RLS data set
# here we subset this data set to build the twitter table used in the main analysis
# this sub-setting is done in the AGGREGATE AT THE SPECIES LEVEL section 
#
#################################################


# FUNCTION get_twitter_intel and LOOP SCRAP ----
#' @title Download intel from Twitter
#'
#' @description
#' This function downloads intel from  Twitter  site using Selenium
#' technology and the package \{code\link[RSelenium]{RSelenium}}.
#'
#' @param term [string] A term to search tweet for.
#' @param ntw_max [numeric] The max number of tweet to search
#' @param first_tw [numeric] The position of the starting tweet (default 1).
#' @param browser [string] The browser name ('chrome', 'firefox', 'phantomjs' or 'internet explorer')
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@gmail.com}
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#'
#' @export
#'
#' @return This function returns a data frame
#'
#' @examples
#'
#' get_twitter_intel(
#'   term = "Amphiprion ocellaris",
#'   ntw_max    = 50,
#'   first_tw   = 1,
#'   browser     = "firefox"
#' )
#' 

rm(list=ls(all=TRUE))

#
#note ( all, fr or en for languages)
get_twitter_intel <- function(
  term = NULL, ntw_max = 50, first_tw = 1, browser = "firefox",lang="all")
{
  
  term <- "Abalistes stellatus"
  ntw_max = 1000
  first_tw = 1
  browser = "firefox"
  lang="en"
  
  #'  -------------------------------------------------------------------------   @LoadAddings
  
  require("RSelenium")
  
  #'  -------------------------------------------------------------------------   @FormatSearchTerms
  
  if (lang=="all") search_terms <- gsub(" ", "%2B", term) 
  if (lang=="en") search_terms <- search_terms <- paste0(gsub(" ", "%2B", term),"%20lang%3Aen")
  if (lang=="fr") search_terms <- search_terms <- paste0(gsub(" ", "%2B", term),"%20lang%3Afr")
  
  
  #'  -------------------------------------------------------------------------   @RunSeleniumServer
  Sys.sleep(1)
  rs_driver <- rsDriver(
    port     = 4567L,
    browser  = browser,
    verbose  = FALSE
  )
  
  cat(paste0("\n>>> Searching Tweets for: \"", search_terms, "\"\n\n"))
  cat(paste0("  -[x] Opening ", browser, "\n"))
  
  url <- paste0(
    "https://twitter.com/",
    "search",
    "?q=", search_terms,
    "&src=typed_query"
  )
  
  #'  -------------------------------------------------------------------------   @OpenURLInBrowser
  
  rs_client <- rs_driver$client
  
  rs_client$navigate(url)
  Sys.sleep(sample(1:3, 1))
  rs_client$refresh() #close the log window
  Sys.sleep(sample(1:3, 1))
  rs_client$mouseMoveToLocation(x = sample(1:300, 1), y = sample(1:300, 1), webElement = NULL) #move mouse to random location 
  
  cat(paste0("  -[x] Browsing: \"", url, "\"\n"))
  
  #'  -------------------------------------------------------------------------   @ScrollDown
  
  thumb_links <- rs_client$findElements(using = "class", value = "ProfileTweet-action--favorite")
  new_links   <- TRUE
  
  if (length(thumb_links)>0) {
    while (length(thumb_links) < (ntw_max + first_tw - 1) && new_links) {
      
      scroll_down <- rs_client$findElement(using = "css", value = "body")
      scroll_down$sendKeysToElement(list(key = "end"))
      
      previous_links <- thumb_links
      
      Sys.sleep(sample(1:3, 1))
      
      #move the mouse Use the random module to choose youR X,Y. Maybe do it multiple times.
      
      thumb_links <- rs_client$findElements(using = "class", value = "ProfileTweet-action--favorite")
      
      if (length(thumb_links)==length(previous_links)) break
    }#while (length(thumb_links)
    
    #'  -------------------------------------------------------------------------   @GetTwitterIntel
    
    cat(paste0("  -[x] Web scraping: \"", url, "\"\n"))
    thumb_retweet <- rs_client$findElements(using = "class", value = "ProfileTweet-action--retweet")
    thumb_favorite <- rs_client$findElements(using = "class", value = "ProfileTweet-action--favorite")
    thumb_reply<- rs_client$findElements(using = "class", value = "ProfileTweet-action--reply")
    thumb_date <- rs_client$findElements(using = "class", value = "tweet-timestamp")
    thumb_user <- rs_client$findElements(using = "class", value = "fullname")
    thumb_text <- rs_client$findElements(using = "class", value = "TweetTextSize")
    
    #extract_date
    
    extract_text <- function(thumb){
      unlist(
        sapply(
          thumb,
          function(x){
            x <- x$findElement(using = "class", value = "tweet-text")
            x$getElementText()
          }
        )
      )
    }
    
    extract_date <- function(thumb){
      unlist(
        sapply(
          thumb,
          function(x){
            x <- x$findElement(using = "class", value = "ProfileTweet-actionCount")
            x$getElementText()
          }
        )
      )
    }
    
    #not use for the moment 
    extract_user <- function(thumb){
      us <- unlist(
        sapply(
          thumb,
          function(x){
            x <- x$findElement(using = "class", value = "ProfileTweet-actionCount")
            x$getElementText()
          }
        )
      )
      us[!us == ""]
    }
    
    extract <- function(thumb){
      unlist(
        sapply(
          thumb[seq(2,length(thumb),by=2)],
          function(x){
            x <- x$findElement(using = "class", value = "ProfileTweet-actionCount")
            x$getElementText()
          }
        )
      )
    }
    
    data_tw <- cbind.data.frame(retweet=as.numeric(gsub("Retweeter|\\n","",extract(thumb_retweet))), 
                                favorite=as.numeric(gsub("J'aime|\\n","",extract(thumb_favorite))), 
                                reply=as.numeric(gsub("Répondre|\\n","",extract(thumb_reply))),
                                date=extract_date(thumb_date),
                                text=extract_text(thumb_text)
    )
  } else { 
    data_tw <- cbind.data.frame(retweet=NA, favorite=NA, reply=NA, date=NA, text=NA)
  }
  
  data_tw <- cbind.data.frame(data_tw,term=rep(term,dim(data_tw)[1])) 
  
  #format the data_tw
  data_tw$date <- as.character(data_tw$date)
  data_tw$term <- as.character(data_tw$term)
  #data_tw$user <- as.character(data_tw$user)
  data_tw$text <- as.character(data_tw$text)
  for (i in 1:length(data_tw$date)) {if (nchar(data_tw$date[i])<9) data_tw$date[i] <- paste(data_tw$date[i],"2019")}
  data_tw$date <- gsub("janv.", "janvier", data_tw$date)
  data_tw$date <- gsub("févr", "février", data_tw$date)
  data_tw$date <- gsub("avr.", "avril", data_tw$date)
  data_tw$date <- gsub("juil.", "juillet", data_tw$date)
  data_tw$date <- gsub("sept.", "septembre", data_tw$date)
  data_tw$date <- gsub("oct.", "octobre", data_tw$date)
  data_tw$date <- gsub("nov.", "novembre", data_tw$date)
  data_tw$date <- gsub("déc.", "décembre", data_tw$date)
  
  require(lubridate)
  data_tw$date <- parse_date_time(data_tw$date, orders = "%d/%m/%Y")
  
  #'  -------------------------------------------------------------------------   @CloseSeleniumServer
  cat(paste0("  -[x] Closing ", browser, "\n"))
  rs_client$close()
  xxx <- rs_driver$server$stop()
  rm(rs_client)
  
  
  #'  -------------------------------------------------------------------------   @ReturnResults
  return(data_tw)
  
}

#-----

#SCRAP OVER ALL SPECIES----

  #prepare the list of species file ##run only once !! 
  sp_list <- read.csv(here::here("results","01_build_species_list","all_accepted_syn.csv"), header = TRUE)
  sp_list <- data.frame(sp_list,TW="NO")
  sp_list$TW <- as.character(sp_list$TW)

  #Do the scrap (change VPN every 40 scrap)
  
  ##run only once !! 
  twitter <- data.frame(retweet=NA,favorite=NA,reply=NA,date=NA,term=NA)
  write.csv(twitter,here::here("results","02_scrap","twitter","02_twitter_species_table.csv"),row.names=FALSE)
  
  ##loop the scrap 
  
  incr=2 #number of scrap in a loop 
  ntw_max=2000
  system.time(
    for (j in which(sp_list$TW=='NO')[1]:(which(sp_list$TW=='NO')[1]+incr)){
      j=1
      sp_list <- read.csv("sp_list.csv",stringsAsFactors=FALSE)
      dt_tw <- get_twitter_intel(term=sp_list$name[j],ntw_max = ntw_max)
      twitter <- read.csv("twitter.csv",colClasses = c("numeric","numeric","numeric","factor","factor"))
      twitter <- rbind(twitter,dt_tw)
      if (j==1) twitter <- twitter[-1,]
      write.csv(twitter,"twitter.csv",row.names=FALSE)
      sp_list$TW[j]= "YES"
      write.csv(sp_list,"sp_list.csv",row.names=FALSE)
    }
  )
#----

# CHECK TWEETS----
#  temporary files (not stored were created in this script)

  ## create two files to be able to open them with excel or numbers to check the tweets
  ### names with 50 tweets or more
  higher_fifty_names <- tw_total_sorted$species[which(tw_total_sorted$tot_tw >= 50)]
  higher_fifty       <-  twitter[which(twitter$term %in% higher_fifty_names),]
  write.csv(higher_fifty, file = here::here("results","02_scrap","twitter", "02_more_50_tweets.csv"), row.names = FALSE)
  
  ### names with less than 50  tweets
  lower_fifty_names <- tw_total_sorted$species[which(tw_total_sorted$tot_tw < 50)]
  lower_fifty       <-  twitter[which(twitter$term %in% lower_fifty_names),]
  
  # Manually check the tweets for the names for which 100 or more tweets have been found
  
  ## Load cleaned table for terms with more than 50 tweets. cleaned until 100
  more_fifty      <- read.csv(here::here("results","02_scrap","twitter", "02_more_50_tweets_clean.csv"))
  more_fifty      <- more_fifty[-which(more_fifty$term %in% c("", " ", NA)),]
  more_fifty$term <- factor(more_fifty$term)
  
  ## Re-aggregate
  twitter2   <- rbind(more_fifty, lower_fifty)
  
  ## remove unused tables
  rm(higher_fifty, higher_fifty_names, lower_fifty, lower_fifty_names, more_fifty)
  file.remove(here::here("results","02_scrap","twitter", "02_more_50_tweets.csv"))
  
  ## how many tweets per term now?
  tw_total2           <- data.frame(table(twitter2$term))
  colnames(tw_total2) <- c("species","tot_tw")
  tw_total_sorted2    <- tw_total2[order(tw_total2$tot_tw, decreasing = TRUE),] 
  table(tw_total_sorted2$tot_tw)
  head(tw_total_sorted2)
  
  ## term with duplicated word (we know the resuts for them are unsure)
  duplicated_names <- c()
  for ( i in 1 : nrow(tw_total_sorted2)){
    # i <- 1
    t <- tw_total_sorted2$species[i]
    term <- tolower(t)
    term <- strsplit(x = term, split = " ")
    term <- term[[1]]
    if(length(which(duplicated(term) == TRUE)) != 0){ 
      duplicated_names <- c(duplicated_names, as.character(t))
    } # eo if
  } # eo for
  
  to_check <- setdiff(duplicated_names, as.character(tw_total$species[which(tw_total$tot_tw >=100)]))
  rm(duplicated_names, i, t, term)
  
  tw_tocheck <- twitter2[which(twitter2$term %in% to_check),]
  tw_ok      <- twitter2[-which(twitter2$term %in% to_check),]
  write.csv(tw_tocheck, here::here("results","02_scrap","twitter", "02_duplicated_to_check.csv"), row.names = FALSE)
  
  ## Load table cleaned for names with 100 tweets or more and/or duplicated words
  dupl_ok      <- read.csv(here::here("results","02_scrap","twitter", "02_duplicated_checked.csv"))
  dupl_ok      <- dupl_ok[-which(dupl_ok$term %in% c("", " ", NA)),2:ncol(dupl_ok)]
  dupl_ok$term <- factor(dupl_ok$term)
  
  ## Re-aggregate
  twitter3   <- rbind(dupl_ok, tw_ok)
  write.csv(x = twitter3, file = here::here("results","02_scrap","twitter", "02_twitter3.csv"), row.names = FALSE)
  
  ## remove unused tables
  rm(to_check, tw_tocheck, tw_ok, dupl_ok)
  file.remove(here::here("results","02_scrap","twitter", "02_duplicated_to_check.csv"))
  
  ## how many tweets per term now?
  tw_total3           <- data.frame(table(twitter3$term))
  colnames(tw_total3) <- c("species","tot_tw")
  tw_total_sorted3    <- tw_total3[order(tw_total3$tot_tw, decreasing = TRUE),] 
  table(tw_total_sorted3$tot_tw)
  head(tw_total_sorted3)
  
  ## check 50 random names
  belly_name   <- tw_total_sorted3$species[which(tw_total_sorted3$tot_tw < 100 & tw_total_sorted3$tot_tw > 1)]
  random50     <- sample.int(n = length(belly_name), size = 50)
  random_names <- belly_name[random50]
  
  # on the 50 random names checked, we found unapropriated tweets for 8. But 2 with "(". We checked all the species with "(" so now we have 6 on 50 ==> 12% of the data for which some tweets are not related to the name
  # if we remove the names for which we have more than 100 tweets that we checked and the species for which we have only 1 tweet or no tweet at all we are left with the "bellu" : 3232 names
  # 12% of 3232 = 388 ==> in the whole dataset 388 names for which we collected unrelated tweets
  # In total we have 12730 names. (388/12730)*100 = 3%
  # The percentage of error in the collection of tweets is rather low. OK
  
#----
  
# AGGREGATE AT THE SPECIES LEVEL AND SAVE ----

  ## Load tables
    ### Twitter
      twitter_clean      <- read.csv(here::here("results","02_scrap","twitter","02_twitter_clean_names.csv"))
      twitter_clean      <- twitter_clean[-which(twitter_clean$term %in% c("", " ", NA)),]
      twitter_clean$term <- as.character(twitter_clean$term)
      #for (i in 1:nrow(check)){twitter_clean$term[which(twitter_clean$term == check$search[i])] <- check$replace.by[i]}
    ### Names
      all_accepted_syn <- read.csv2(here::here("results","02_scrap", "all_accepted_syn.csv"))
    
    ### Merge
      twitter_clean           <- merge(x = twitter_clean, y = all_accepted_syn, by.x = "term", by.y = "name")
      twitter_clean           <- twitter_clean[order(twitter_clean$fb_sci_name),]  
      twitter_clean$order     <- c(1:nrow(twitter_clean))  
      rownames(twitter_clean) <- twitter_clean$order
      twitter_clean           <- twitter_clean[,c(7,1,8,2,3,4,6)]
      
  ## Number of tweet, retweet, favorite and reply per name
    ### Tweets 
      for (i in 1:length(twitter_clean$text)) {
        if (is.na(twitter_clean$text[i])) {twitter_clean$nb_tw[i]=0} else { twitter_clean$nb_tw[i]=1}
      }
      
      t_sp           <- aggregate(twitter_clean$nb_tw, by = list(name = twitter_clean$fb_sci_name), FUN = sum, na.rm = TRUE)
      colnames(t_sp) <- c("fb_sci_name", "TW_tweet")
  
    ### Re-tweets
      rt_sp           <- aggregate(twitter_clean$retweet, by = list(name = twitter_clean$fb_sci_name), FUN = sum, na.rm = TRUE)
      colnames(rt_sp) <- c("fb_sci_name", "TW_retweet")
    ### Favorites
      fav_sp           <- aggregate(twitter_clean$favorite, by = list(name = twitter_clean$fb_sci_name), FUN = sum, na.rm = TRUE)
      colnames(fav_sp) <- c("fb_sci_name", "TW_favorite")
    ### Replies
      rep_sp           <- aggregate(twitter_clean$reply, by = list(name = twitter_clean$fb_sci_name), FUN = sum, na.rm = TRUE)
      colnames(rep_sp) <- c("fb_sci_name", "TW_reply")
  
  ## Bind everything at the species level
    twitter_sp <- merge(t_sp, rt_sp, by = "fb_sci_name")
    twitter_sp <- merge(twitter_sp, fav_sp, by = "fb_sci_name")
    twitter_sp <- merge(twitter_sp, rep_sp, by = "fb_sci_name")

  #save all 
    
  write.csv2(x = twitter_sp, file = here::here("results","02_scrap","twitter","02_twitter_species_table.csv"), row.names = FALSE)

  #23 species were missing (after the taxonomic check with fishbase was made) and have been added by hand
  #the final twitter file is twitter_final.csv

#----