###################################################################################################
#'  Assemble all gathered data of human interest
#'  Compute the research effort and public attention dimensions 
#'
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#' 
#'  Produce 
#'    - Figure 2A, 2B
#'    - Figure S1 
#'    - 05_Human_Interest_final_table.csv
#'
#'         
#' @date 2023/05/06
##################################################################################################

rm(list = ls())

#FUNCTIONS----

#function norm01

norm01 <- function(dat){
  min_dat <- min(dat,na.rm=TRUE)
  max_dat <- max(dat,na.rm=TRUE)
  dat <- dat+abs(min_dat)
  min_dat <- min(dat,na.rm=TRUE)
  max_dat <- max(dat,na.rm=TRUE)
  dat <- (dat-min_dat)/(max_dat-min_dat)
  return(dat)
}

#function identifyPch
identifyPch <- function(x, y = NULL, n = length(x), plot = FALSE, pch = 19, ...)
{
  xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
  sel <- rep(FALSE, length(x))
  while(sum(sel) < n) {
    ans <- identify(x[!sel], y[!sel], labels = which(!sel), n = 1, plot = plot, ...)
    if(!length(ans)) break
    ans <- which(!sel)[ans]
    points(x[ans], y[ans], pch = pch)
    sel[ans] <- TRUE
  }
  ## return indices of selected points
  which(sel)
}

#----

#Assemble RLS_final----

#RLS data set from Langlois et al. 2022
  RLS_species <- read.csv2(here::here("results","01_build_species_list","RLS_species_init.csv"))

  #sp_list_RLS <- read.csv(file = here::here("Data","Langlois_el_al_2022.csv"))
  
  #sum(RLS_species$fb_sci_name%in%"Caranx_bartholomaei")
  
  #sp_list_RLS$sp_name[!sp_list_RLS$sp_name%in%RLS_species$rls_sci_name]


  ##select the var of interest from the initial RLS data set from Langlois et al. 2022
  RLS_species_var <- c("rls_sci_name","fb_sci_name","spec_code","esthe_score",
                     "file_name","copyright","rls","max_length","trophic_level",
                     "thermal_mp_5min_95max","thermal_95thmax","trophic_group",
                     "water_column","diel_activity","habitat","iucn_code",
                     "Importance","fishery_importance")

  RLS_Langlois <- RLS_species[,RLS_species_var]
  
#Twitter
  twitter <- read.csv2(here::here("results","02_scrap","twitter","twitter_final.csv"))
  twitter$fb_sci_name <- gsub(" ","_",twitter$fb_sci_name)
  
#Phylo
  phylo <- read.csv(here::here("results","04_phylo","table_phylo.csv"))
  phylo <- rutils::rename_col(phylo,"name","fb_sci_name")
  phylo$fb_sci_name<- gsub(" ","_",phylo$fb_sci_name)
  
#WTA
  WTA_agregated <- read.csv2(here::here("results","02_scrap","WTA_agregated.csv"))
  WTA_agregated <- rutils::rename_col(WTA_agregated,"Scientific","fb_sci_name")
  WTA_agregated$fb_sci_name<- gsub(" ","_",WTA_agregated$fb_sci_name)

#FISHBASE
  
  fishbase <- read.csv2(here::here("results","02_scrap","fishbase_table.csv"))
  fishbase$fb_sci_name<- gsub(" ","_",fishbase$fb_sci_name)

#GBIF 
  
  gbif005 <- read.csv(here::here("results","03_gbif","species_range_size_005.csv"))
  gbif005 <- gbif005[,c("fb_sci_name","GBIF_NC_records","GBIF_NC_n_cells_005")]
  
  gbif01 <- read.csv(here::here("results","03_gbif","species_range_size_01.csv"))
  gbif01 <- gbif01[,c("fb_sci_name","GBIF_NC_n_cells_01")]
  
  gbif <- merge(gbif005,gbif01)
  gbif$fb_sci_name <- gsub(" ","_",gbif$fb_sci_name)
  
#SCHOLARS
  
  scholar <- read.csv2(here::here("results","02_scrap", "scholar_table.csv"))
  scholar$rls_sci_name <- gsub(" ","_",scholar$rls_sci_name)
  
    ##there was 26 species which do not have data in scholar which were scraped by hand on ggscholar 
    newcholar <-read.csv2(here::here("results","02_scrap", "newcholar.csv")) 
  
  scholar <- rbind(scholar,newcholar)
  
#Assemble all 
  
  RLS_final <- merge(RLS_Langlois,twitter,by="fb_sci_name")
  RLS_final <- merge(RLS_final,phylo,by="fb_sci_name")
  RLS_final <- merge(RLS_final,WTA_agregated,by="fb_sci_name")
  RLS_final <- merge(RLS_final,fishbase,by="fb_sci_name")
  RLS_final <- merge(RLS_final,gbif,by="fb_sci_name")
  RLS_final <- merge(RLS_final,scholar,by="rls_sci_name")
  
#Remove some species that were not really associated to reefs, they were present in 
  #the RLS dataset but occured in only few sites and should not be considered here 
  
  remsp <- c("Elagatis_bipinnulata","Euthynnus_affinis","Euthynnus_lineatus","Mola_mola",
             "Salmo_salar","Thunnus_albacares","Gadus_morhua")
  
  RLS_final <- RLS_final[!RLS_final$fb_sci_name%in%remsp,]
  
#---- 
  
#Taxonomic information----
  
  taxo <- c("fb_sci_name","rls_sci_name","kingdom","phylum","class","order","family","genus")
  final_table <- RLS_final[,taxo]
  
#----
  
#Species attributes (Figure S1)----
  
  ##Aesthe 
    final_table$Aesthetic <- norm01(RLS_final[,"esthe_score"])
  
  ##AGE_mean
    final_table$Evol_age <- norm01(log10(RLS_final[,"ages_mean" ]))

  ##Habitat - Coralness & Benthicness (Figure S1)
    ## to transform the variable habitat (coral, sand, rock) (water column) into a continuous variable we performed a MCA analysis with both the variable habitat (coral, sand, rock)
    ## (the NA were removed)
    ## and the variable water column; the first axis of the MCA (33.1%) is related to the position in the water column (from open water, high values, to bottom, low values) and will be 
    ## renamed Benthicness (inverse of the axis values)
    ## and second axis (21.1%) is clearly
    ## related to the substrate coral->rock->sand and will be called "Coralness" 

    dat_habitat <-data.frame(fb_sci_name=RLS_final$fb_sci_name)
    dat_habitat$water_column <- RLS_final$water_column
    dat_habitat$habitat <- RLS_final$habitat
    rownames(dat_habitat) <- dat_habitat$fb_sci_name
    #dat_habitat$water_column[dat_habitat$water_column=="pelagic site attached"]="pelagic"
    #dat_habitat$water_column[dat_habitat$water_column=="pelagic non-site attached"]="pelagic"
    dat_habitat <- dat_habitat[complete.cases(dat_habitat),]

    mca_habitat <- FactoMineR::MCA(dat_habitat[,-1],graph = FALSE)
    library(ggplot2)
     factoextra::fviz_screeplot (mca_habitat, addlabels = TRUE, ylim = c (0, 45))
     factoextra::fviz_mca_biplot (mca_habitat, repel = TRUE,
                                  ggtheme = theme_minimal(),axes = c(1, 2),geom.ind=c("point"))
      
     figS1 <- factoextra::fviz_mca_biplot(mca_habitat,
                                 axes = c(1, 2),
                                 geom          = "point",
                                 col.ind="blue",
                                 repel         = TRUE,
                                 col.var       = "#575656",
                                 geom.var      = c("arrow"),
                                 alpha.ind     = 1,
                                 ggtheme       = theme_bw(),
                                 pointsize = 2)+
       theme(legend.position ="none",
             axis.title.y = element_text(size=14, face="bold"),
             axis.text.y = element_text(size=12),
             axis.title.x = element_text(size=14, face="bold"),
             axis.text.x = element_text(size=12))+
       xlim(-2, 5)
     ggsave(here::here("tables_figures","FIG_S1.tiff"),device="tiff",figS1,width = 17, height = 17, units = "cm",dpi=300)
     
     figS1_text <- factoextra::fviz_mca_biplot(mca_habitat,
                                               axes = c(1, 2),
                                               geom          = "point",
                                               col.ind="blue",
                                               repel         = TRUE,
                                               col.var       = "#575656",
                                               geom.var      = c("arrow", "text"),
                                               alpha.ind     = 1,
                                               ggtheme       = theme_bw(),
                                               pointsize = 2)+
       theme(legend.position ="none",
             axis.title.y = element_text(size=14, face="bold"),
             axis.text.y = element_text(size=12),
             axis.title.x = element_text(size=14, face="bold"),
             axis.text.x = element_text(size=12))+
       xlim(-2, 5)
     ggsave(here::here("tables_figures","FIG_S1_text.tiff"),device="tiff",figS1_text,width = 17, height = 17, units = "cm",dpi=300)
     
     #from the MCA there 
      
      final_table$Coralness=NA
      final_table$Coralness[final_table$fb_sci_name %in% dat_habitat$fb_sci_name] <-norm01((-1*as.numeric(mca_habitat$ind$coord[,2])))
      final_table$Benthicness[final_table$fb_sci_name %in% dat_habitat$fb_sci_name] <-norm01((-1*as.numeric(mca_habitat$ind$coord[,1])))

  ##Habitat - (diel_activity)  
    
    diel_activity <- RLS_final$diel_activity
    diel_activity[diel_activity %in% "day"] <- 1
    diel_activity[diel_activity %in% "night"] <- 0
    
    final_table$Diel_activity=norm01(as.numeric(diel_activity))
    
  ##Length
    
    final_table$Length <- norm01(RLS_final[,"max_length"])

  ##Trophic_level
    
    final_table$Trophic_level <- norm01(RLS_final[,"trophic_level"])

  ##Thermal : thermal_mp_5min_95max 
    
    final_table$Temperature <- norm01(abs(log10(0.02+(1-RLS_final[,"thermal_mp_5min_95max"]))))

  ##Species range & occurrences 
    
    final_table$Range_005 <- norm01(log10(1+RLS_final$GBIF_NC_n_cells_005))
    
#----
    
#Human Interest ----
    
    ##Scientific literature  
    
      wos_tot_ref <- norm01(log10(1+RLS_final$WOS_all))
      schol_tot_ref <- norm01(log10(1+RLS_final$GG_ref_nb))
      
      final_table$Sci_fields <- norm01(RLS_final$WOS_simp_fields)
      final_table$H_index <- norm01(log10(1+RLS_final$WOS_H_index))
      final_table$Tot_pubs <- norm01(apply(data.frame(wos_tot_ref,schol_tot_ref),1,mean,na.rm=TRUE))

      ###illustrate that GG_ref_cit provide more of the grey litterature than WOS_tot_cit 
      
        # ggplot(final_table, aes(x=wos_tot_ref, y=schol_tot_cit))+ 
        #   geom_point(size=1)+
        #   geom_smooth(method = lm,colour="gray")+
        #   theme_bw()+ xlim(0, 1)+ylim(0, 1)+
        #   geom_abline (slope=1, linetype = "dashed", color="black",alpha=0.5)
        #   

    ##NCBI 
      final_table$NCBI <- norm01(apply(data.frame(norm01(log10(RLS_final$NCBI_nuccore+1)),norm01(log10(RLS_final$NCBI_protein+1))),1,mean,na.rm=TRUE))
      
    ##Wikipedia
      final_table$Wiki_views <- norm01(log10(RLS_final$WIKI_views+1))

    ##Fish_base NAs
      final_table$FishBase <- 1-norm01(RLS_final$FB_nas)
   
    ##Twitter 
      final_table$Twitter <-  norm01(apply(data.frame(norm01(log10(RLS_final$TW_tweet+1)),norm01(log10(RLS_final$TW_retweet+1)),norm01(log10(RLS_final$TW_favorite+1))),1,mean,na.rm=TRUE))
      
    ##FLICKR
      final_table$Flickr <- norm01(log10(RLS_final$FLICKR_tot+1))
      
#----
      
#The two Dimensions of Human interest (FIGURE 2 A and B) ---- 
      
    final_table <- final_table[order(final_table$Range_005),]
      
    varid <- c("fb_sci_name")
    varscale <- c("Tot_pubs","Sci_fields","H_index","Wiki_views","Twitter","Flickr","NCBI","FishBase")
    data_scale <-  cbind.data.frame(fb_sci_name=final_table[,varid],Range=final_table$Range_005,scale(final_table[,varscale]))
    data_scale <- data_scale[complete.cases(data_scale), ]
    pca_field<-ade4::dudi.pca(data_scale[,c(-1,-2)], scannf=FALSE, nf = 8)
    
    res.ind <- factoextra::get_pca_ind(pca_field)
    
    colors_grad <- c('#4575b4', '#74add1', '#abd9e9', '#e0f3f8',
                     '#abe9c7', '#74d186', '#45b455', '#156b15')
    
    #factoextra::fviz_eig(pca_field,main = "Eigenvalues")
    
    ##FIGURE 2A
      library(ggplot2)
      fig2a <- factoextra::fviz_pca_biplot(pca_field,
                                    axes = c(1, 2),
                                    col.ind       = data_scale$Range,
                                    geom          = "point",
                                    gradient.cols = colors_grad,
                                    repel         = TRUE,
                                    col.var       = "#575656",
                                    geom.var      = c("arrow"),
                                    alpha.ind     = 1,
                                    ggtheme       = theme_bw(),
                                    pointsize = 2)+
          labs(col="Range", title = " ") +
          theme(legend.position ="none",
              axis.title.y = element_text(size=14, face="bold"),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size=14, face="bold"),
              axis.text.x = element_text(size=12))
  
      ggsave(here::here("tables_figures","Fig2a.tiff"),device="tiff",fig2a,width = 17, height = 17, units = "cm",dpi=300)

      fig2a_text <- factoextra::fviz_pca_biplot(pca_field,
                                           axes = c(1, 2),
                                           col.ind       = data_scale$Range,
                                           geom          = "point",
                                           gradient.cols = colors_grad,
                                           repel         = TRUE,
                                           col.var       = "#575656",
                                           geom.var      = c("arrow", "text"),
                                           alpha.ind     = 0.9,
                                           ggtheme       = theme_bw(),
                                           pointsize     = 2)+
        labs(col="Range", title = " ") +
        theme(legend.position="none",
              axis.title.y = element_text(size=14,face="bold"),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size=14,face="bold"),
              axis.text.x = element_text(size=12))
      
      ggsave(here::here("tables_figures","fig2a_text.tiff"),device="tiff",fig2a_text,width = 17, height = 17, units = "cm",dpi=300)
      
      ###find examples to illustrate the figure 
      
        # df <- data.frame(x=pca_field$li$Axis1, y=pca_field$li$Axis2)
        # rownames(df) <- data_scale$fb_sci_name
        # plot(df)
        # abline(v=0)
        # abline(h=0)
        # id_fish <- identifyPch(x=df$x,y=df$y)
        # rownames(df)[id_fish]

    ##We then computed a pca with only each subset of variables 
    ##and used the coordinates on the first axis of the PCA (this take the correlations of variables within each subset of variables)
    
      varid <- c("fb_sci_name")
      varkno <- c("Tot_pubs","Sci_fields","H_index","NCBI","FishBase") # "NCBI","FB_nas"
      varpub <- c("Wiki_views","Twitter","Flickr") # "Wiki_length"
    
      ###Academic knowledge 
        data_scale <-  cbind.data.frame(fb_sci_name=final_table[,varid],scale(final_table[,varkno]))
        data_scale <- data_scale[complete.cases(data_scale), ]
        pca_field<-ade4::dudi.pca(data_scale[,-1], scannf=FALSE, nf = 5)
        
        ####look at the position of the variables to addapt the norm01(res.ind$coord$Dim.1) / norm01(-1*res.ind$coord$Dim.1)
        
          # factoextra::fviz_pca_biplot(pca_field,
          #                             axes = c(1, 2),
          #                             #col.ind       = data_scale$Range,
          #                             geom          = "point",
          #                             gradient.cols = colors_grad,
          #                             repel         = TRUE,
          #                             col.var       = "#575656",
          #                             geom.var      = c("arrow", "text"),
          #                             alpha.ind     = 0.9,
          #                             ggtheme       = theme_bw(),
          #                             pointsize     = 2)

        #### compute the acad knowledge 
          res.ind <- factoextra::get_pca_ind(pca_field)
          df <- data.frame(x=res.ind$coord$Dim.1, y=res.ind$coord$Dim.2)
          rownames(df) <- data_scale$fb_sci_name
          final_table$acad[which(final_table$fb_sci_name %in% rownames(df))] <- norm01(-1*res.ind$coord$Dim.1)

      ###Public interest 
        data_scale <-  cbind.data.frame(fb_sci_name=final_table[,varid],scale(final_table[,varpub]))
        data_scale <- data_scale[complete.cases(data_scale), ]
        pca_field<-ade4::dudi.pca(data_scale[,-1], scannf=FALSE, nf = 3)
        
        ####look at the position of the variables to addapt the norm01(res.ind$coord$Dim.1) / norm01(-1*res.ind$coord$Dim.1)
          # factoextra::fviz_pca_biplot(pca_field,
          #                             axes = c(1, 2),
          #                             #col.ind       = data_scale$Range,
          #                             geom          = "point",
          #                             gradient.cols = colors_grad,
          #                             repel         = TRUE,
          #                             col.var       = "#575656",
          #                             geom.var      = c("arrow", "text"),
          #                             alpha.ind     = 0.9,
          #                             ggtheme       = theme_bw(),
          #                             pointsize     = 2)

        #### compute the public attention
          res.ind <- factoextra::get_pca_ind(pca_field)
          df <- data.frame(x=res.ind$coord$Dim.1, y=res.ind$coord$Dim.2)
          rownames(df) <- data_scale$fb_sci_name
          final_table$public[which(final_table$fb_sci_name %in% rownames(df))] <- norm01(-1*res.ind$coord$Dim.1)
      
      ###Mean interest 
        final_table$interest <- norm01((final_table$acad+final_table$public)/2)
        
      ###Correlation between acad and public
        
        #cor.test(final_table$acad, final_table$public, method = "pearson", alternative = "less") # r=0.66 p<0.001
        
    ##FIGURE 2b
    
      ###Compute the quantiles (how much species at the intersection of both top 10% acad and public)
      ###and find some exemples species for illustration 

        Q_public <- as.numeric(quantile(x <- final_table$public,probs = seq(0, 1, 0.1))[10])
        Q_acad <- as.numeric(quantile(x <- final_table$acad,probs = seq(0, 1, 0.1))[10])
          
        top_both <- sum((final_table$acad>Q_acad) & (final_table$public>Q_public))
          #top_both = 111 species = 4.6%
        top_acad <- sum((final_table$acad>Q_acad))-top_both
        top_public <- sum((final_table$public>Q_public))-top_both
    
        Top_int_1 <- final_table$fb_sci_name[order(final_table$interest,decreasing = T)][1]
        #Top_int_2 <-  final_table$fb_sci_name[order(final_table$interest,decreasing = T)][2]
        #Top_acad_1 <- final_table$fb_sci_name[order(final_table$acad,decreasing = T)][1]
        #Top_acad_2 <- final_table$fb_sci_name[order(final_table$acad,decreasing = T)][2]
        #Top_public_1 <-  final_table$fb_sci_name[order(final_table$public,decreasing = T)][1]
        #Top_public_2 <-  final_table$fb_sci_name[order(final_table$public,decreasing = T)][2]

      ### find coordinates of fish to illustrate Acad_no_public (high acad min public) & Public_no_acad (high public min acad)

          # df <- data.frame(x=final_table$acad,y=final_table$public)
          # rownames(df) <- final_table$fb_sci_name
          # plot(df)
          # abline(v=Q_acad)
          # abline(h=Q_public)
          # id_fish <- identifyPch(x=df$x,y=df$y)
          # rownames(df)[id_fish]

        Acad_no_public <- "Jenkinsia_lamprotaenia"
        Public_no_acad <- "Paraplesiops_bleekeri"
        toppub <- "Synchiropus_splendidus"
        topacad <-"Epinephelus_coioides"
  
      ###Draw the figure 
  
        library(ggplot2)
        fig2b <- ggplot(final_table, aes(x=acad, y=public, color=Range_005)) +
                    geom_point(alpha=1,size=2) +
                    scale_colour_gradientn(colours = colors_grad)+
                    xlim(0,1)+ylim(0,1)+
                    xlab("Research effort")+ylab("Public attention")+
                    geom_hline(yintercept=Q_public, linetype="dashed", 
                               color = "black", size=0.5)+
                    geom_vline(xintercept=Q_acad, linetype="dashed", 
                               color = "black", size=0.5)+
                    theme_bw()+
                    theme(legend.position = c(0.09, 0.84),
                          axis.title.y = element_text(size=14,face="bold"),
                          axis.text.y = element_text(size=12),
                          axis.title.x = element_text(size=14,face="bold"),
                          axis.text.x = element_text(size=12))+
                    labs(color=NULL)+
                    geom_abline (slope=1, linetype = "dashed", color="black",alpha=0.5)+
                    geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%Top_int_1],y=final_table$public[final_table$fb_sci_name%in%Top_int_1]), 
                               shape=1,
                               color='black',
                               size=4)+
                    #geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%Top_int_2],y=final_table$public[final_table$fb_sci_name%in%Top_int_2]), 
                    #           shape=1,
                    #           color='black',
                    #           size=4)+
                    geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%topacad],y=final_table$public[final_table$fb_sci_name%in%topacad]), 
                               shape=1,
                               color='black',
                               size=4)+
                    #geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%Top_acad_2],y=final_table$public[final_table$fb_sci_name%in%Top_acad_2]),
                    #           shape=1,
                    #           color='black',
                    #           size=4)+
                    geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%toppub],y=final_table$public[final_table$fb_sci_name%in%toppub]), 
                               shape=1,
                               color='black',
                               size=4)+
                    #geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%Top_public_2],y=final_table$public[final_table$fb_sci_name%in%Top_public_2]),
                    #           shape=1,
                    #           color='black',
                    #           size=4)+
                    geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%Acad_no_public],y=final_table$public[final_table$fb_sci_name%in%Acad_no_public]),
                               shape=1,
                               color='black',
                               size=4)+
                    geom_point(aes(x=final_table$acad[final_table$fb_sci_name%in%Public_no_acad],y=final_table$public[final_table$fb_sci_name%in%Public_no_acad]),
                               shape=1,
                               color='black',
                               size=4)
        
        ggsave(here::here("tables_figures","Fig2b.tiff"),device="tiff",fig2b,width = 18, height = 17, units = "cm",dpi=300)
        

        ###Corelation acad public  
          #cor.test(final_table$acad, final_table$public, method = "pearson", alternative = "greater") # r=-0.7 p<0.001
          #r=0.67 p<0.001
        
#----

#IUCN---- 

      # assessments <- read.csv(here::here('data','iucn','assessments.csv'))
      # 
      # sub_iucn <- assessments[assessments$scientificName %in% gsub("_"," ",final_table$fb_sci_name),c("scientificName","redlistCategory")]
      # colnames(sub_iucn) <- c("fb_sci_name","IUCN")
      # THR  <- c("Critically Endangered", "Vulnerable", "Endangered")
      # LC <- c("Least Concern", "Near Threatened","Lower Risk/least concern")
      # DD <- c("Data Deficient")
      # for(i in 1 : nrow(sub_iucn)){
      #   if(sub_iucn$IUCN[i] %in% THR) sub_iucn$IUCN[i] <- "TH"
      #   if(sub_iucn$IUCN[i] %in% LC) sub_iucn$IUCN[i] <- "LC"
      #   if(sub_iucn$IUCN[i] %in% DD) sub_iucn$IUCN[i] <- "DD"
      # }
      # sub_iucn$fb_sci_name <- gsub(" ","_",sub_iucn$fb_sci_name)
      # 
      # final_table <- merge(final_table,sub_iucn,all.x = T)
      # final_table$IUCN[is.na(final_table$IUCN)] <- "NE"
      # 
      final_table$IUCN[order(final_table$fb_sci_name)]=RLS_final$FB_IUCN[order(RLS_final$fb_sci_name)]
      THR  <- c("CR", "VU", "EN")
      LC <- c("LC", "NT","LR/lc")
      NE <- c("N.E.")
      for(i in 1 : nrow(final_table)){
        if(final_table$IUCN[i] %in% THR) final_table$IUCN[i] <- "TH"
        if(final_table$IUCN[i] %in% LC) final_table$IUCN[i] <- "LC"
        if(final_table$IUCN[i] %in% NE) final_table$IUCN[i] <- "NE"
      }

      #final_table$IUCN[final_table$fb_sci_name%in%"Pollachius_virens"] <- "LC"
      #final_table$IUCN[final_table$fb_sci_name%in%"Myoxocephalus_scorpius"] <- "LC"
      
#----
      
#Climatic Risk----
#Data from Boyce, Daniel et al. (2022), A climate risk index for marine life, Dryad, Dataset, https://doi.org/10.5061/dryad.7wm37pvwr
      
    risk_table <- read.csv(file = here::here("data","Climat_risk", "Boyce_etal_2022_NATCC.csv"))
    risk_table <- risk_table[risk_table$SPname %in% gsub("_"," ",final_table$fb_sci_name),] 
   
    ClimVuln_SSP126 <- risk_table[risk_table$Experiment%in%"SSP126",]
    ClimVuln_SSP585 <- risk_table[risk_table$Experiment%in%"SSP585",]
    
    ClimVuln_SSP126 <- ClimVuln_SSP126[,c("SPname","ClimVuln")]
    ClimVuln_SSP585 <- ClimVuln_SSP585[,c("SPname","ClimVuln")]
    colnames(ClimVuln_SSP126) <- c("fb_sci_name","ClimVuln_SSP126")
    colnames(ClimVuln_SSP585) <- c("fb_sci_name","ClimVuln_SSP585")
    ClimVuln_SSP126$fb_sci_name <- gsub(" ","_",ClimVuln_SSP126$fb_sci_name)
    ClimVuln_SSP585$fb_sci_name <- gsub(" ","_",ClimVuln_SSP585$fb_sci_name)
    
    final_table <- merge(final_table,ClimVuln_SSP126,all=T)
    final_table <- merge(final_table,ClimVuln_SSP585,all=T)

#----
      
#Human_uses----
 
    #fishery_importance
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% "highly commercial"] <- 6
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% "commercial"] <- 5
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% "minor commercial"] <- 3
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% "subsistence fisheries"] <- 2
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% "of no interest"] <- 1
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% "of potential interest"] <- 2
      RLS_final$FB_Importance[RLS_final$FB_Importance %in% c("unknown","NA", NA) ] <- 1
      
      #table(RLS_final$FB_Importance)
      
      Fishery <- norm01(as.numeric(RLS_final$FB_Importance))
      
      #price_category
      
      RLS_final$FB_PriceCateg[RLS_final$FB_PriceCateg %in% "very high"] <- 4
      RLS_final$FB_PriceCateg[RLS_final$FB_PriceCateg %in% "high"] <- 3
      RLS_final$FB_PriceCateg[RLS_final$FB_PriceCateg %in% "medium"] <- 2
      RLS_final$FB_PriceCateg[RLS_final$FB_PriceCateg %in% "low"] <- 1
      RLS_final$FB_PriceCateg[RLS_final$FB_PriceCateg %in% c("unknown","NA", NA) ] <- 0
      
      Price <- norm01(as.numeric(RLS_final$FB_PriceCateg))
      
      #table(RLS_final$FB_PriceCateg)
      
      #aquaculture
      
      RLS_final$FB_UsedforAquaculture[RLS_final$FB_UsedforAquaculture %in% "commercial"] <- 4
      RLS_final$FB_UsedforAquaculture[RLS_final$FB_UsedforAquaculture %in% c("experimental","likely future use")] <- 2
      RLS_final$FB_UsedforAquaculture[RLS_final$FB_UsedforAquaculture %in% "never/rarely"] <- 1
      RLS_final$FB_UsedforAquaculture[RLS_final$FB_UsedforAquaculture %in% c("NA", NA) ] <- 0
      
      Aquaculture <- norm01(as.numeric(RLS_final$FB_UsedforAquaculture))
      
      #table(RLS_final$FB_UsedforAquaculture)
      
      #Aquarium :
      
      RLS_final$FB_Aquarium[RLS_final$FB_Aquarium_I %in% "commercial"] <- 3
      RLS_final$FB_Aquarium[RLS_final$FB_Aquarium_I %in% "public aquariums"] <- 2
      RLS_final$FB_Aquarium[RLS_final$FB_Aquarium_I %in% "never/rarely"] <- 1
      RLS_final$FB_Aquarium[RLS_final$FB_Aquarium_I %in% "potential"] <- 1
      RLS_final$FB_Aquarium[RLS_final$FB_Aquarium_I %in% c("NA", NA) ] <- 0
      
      Aquarium <- norm01(as.numeric(RLS_final$FB_Aquarium))
      
      #table(RLS_final$FB_Aquarium)
      
      #bind them all 
      
      Fishery <- cbind.data.frame(fb_sci_name=RLS_final$fb_sci_name,
                                  Fishery=Fishery,Price=Price,
                                  Aquaculture=Aquaculture,Aquarium=Aquarium)
                                  
      final_table <- merge(final_table,Fishery)

#----

#Save all ----

    write.csv2(final_table,here::here("results","05_assemlble_knowInt","05_Human_Interest_final_table.csv"),row.names = FALSE)

#---- 
      