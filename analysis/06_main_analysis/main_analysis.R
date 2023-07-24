###################################################################################################
#'  Main analysis 
#'  
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#'
#'  Produce 
#'  - Figure 3
#'  - Figure 4
#'  - pagel_acad.csv and pagel_public.csv
#'  - Figure 5
#'  - Figure 6
#'  - Figure S2
#'         
#' @date 2023/05/06
##################################################################################################

rm(list = ls())
library(ggplot2)
final_table <- read.csv2(here::here("results","05_assemlble_knowInt","05_Human_Interest_final_table.csv"))

#FUNCTIONS----

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

#function BRT
BRT_values <- function(varcor,cor_id,data_tab,n.trees,learning.rate,train.fraction,bag.fraction){
  
  # varcor <- c("Aesthe","Coralness","Benthicness","Diel_activity","Length","Trophic_level","Thermal","Fishery","Aquaculture","Aquarium","Price")
  # range <- "Range_01"
  # varcor <- c(varcor,range)
  # cor_id="acad"
  # data_tab <- final_table
  
  require(dismo)
  require(car)
  require(gbm)
  
  data_brt <- na.omit(data_tab[,c(varcor,cor_id)])
  
  gbm1 <- gbm::gbm(as.formula(paste(varcor," ~ ", paste(cor_id, collapse= "+"))), data = data_brt,
              distribution = "gaussian", n.trees = n.trees, shrinkage = learning.rate,
              interaction.depth = 3, bag.fraction = bag.fraction, train.fraction = train.fraction,
              n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE,
              verbose = FALSE, n.cores = parallel::detectCores()-1)
  
  
  perf_gbm1  <-  gbm.perf(gbm1, method = "cv")
  
  data_brt_prediction_1 <- stats::predict(
    # the model from above
    object = gbm1, 
    # the testing data
    newdata = data_brt,
    # this is the number we calculated above
    n.trees = perf_gbm1)
  rmse_fit1 <- Metrics::rmse(actual = data_brt[,varcor], 
                             predicted = data_brt_prediction_1)
  
  R2_brt <- (cor(data_brt_prediction_1, data_brt[,varcor]))^2
  
  best.iter <- gbm.perf(gbm1, method = "OOB")
  Imp_var <- summary(gbm1, n.trees = best.iter)
  
  #Partial plots 
  
  Par_dep <- lapply(Imp_var$var,function(id){
    # id <- Imp_var$var[1]
    treezy::partial_dependence(gbm1, var=id)
  })
  names(Par_dep) <- Imp_var$var
  
  n=dim(data_brt[1])
  
  a <- list(n,R2_brt,Imp_var,Par_dep)
  names(a) <- c("n","R2_brt","Imp_var","Par_dep")
  return(a)
  
}

#Function to visualize the familly position on the tree 
#obj2 must be obtained before using this function 
visu_fam <- function(name_fam) {     
  
  # load packages
  require("phytools") # for sims and ASRs
  require("ggtree")
  table <- phylo_table[phylo_table$sp_name %in% set100[[100]]$tip.label,]
  node <- phytools::findMRCA(set100[[100]],
                             as.character(table$sp_name[which(as.character(table$family) == name_fam)]),
                             type = "node")
  
  plot(obj2, ftype = "off", type = "phylogram", outline = FALSE, legend = FALSE,#phylogram
       fsize = c(0.2,1), lwd = 1.5, offset = 5, xlim = c(-180,300))
  
  phytools::cladelabels(text = name_fam,
                        node        = node,
                        cex         = 2,
                        orientation = "horizontal",
                        wing.length=5,offset = 2)
}

#----

#BRT analysis FIGURE 3, FIGURE PARTIAL PLOTS----

data_tab <- final_table
names(data_tab)[names(data_tab) == 'Range_005'] <- 'Range'
names(data_tab)[names(data_tab) == 'Evol_age'] <- 'Species_age'
names(data_tab)[names(data_tab) == 'Length'] <- 'Body_size'


id_var <- c("Aesthetic","Coralness","Benthicness","Diel_activity","Body_size","Trophic_level","Temperature","Fishery","Aquaculture","Aquarium","Price","Species_age")
range <- "Range"
id_var <- c(id_var,range)

  set.seed(1234)
  acad_brt <- BRT_values(varcor="acad",cor_id =id_var,data_tab =data_tab,n.trees=1000,learning.rate=0.01,train.fraction=0.75,bag.fraction=0.5)    
  public_brt <- BRT_values(varcor="public",cor_id =id_var,data_tab =data_tab,n.trees=1000,learning.rate=0.01,train.fraction=0.75,bag.fraction=0.5)    
  #note that the number of species is n = acad_brt$n (2243) NA have been removed ... 

  acad_brt$Imp_var$var = factor(acad_brt$Imp_var$var, levels = rev(acad_brt$Imp_var$var), ordered = TRUE)
  public_brt$Imp_var$var = factor(public_brt$Imp_var$var, levels = rev(acad_brt$Imp_var$var), ordered = TRUE)

  a <- ggplot(acad_brt$Imp_var, aes(x=var, y=rel.inf,fill=100-rel.inf))+ 
    geom_bar(stat="identity", position="dodge")+ coord_flip()+
    #scale_fill_gradient2(low = "white",high = "blue") +
    scale_fill_viridis_c(begin = 0.5,
                         end = 1)+
    ylab("Variable Importance (%)")+
    xlab("")+
    ylim(0,65)+
    ggtitle(paste0("Research effort"," r2 = ",round(acad_brt$R2_brt,2)))+
    guides(fill=F)+ theme_bw()+
    theme(plot.title = element_text(size=18, face="bold"),
          axis.text.y = element_text(size=13, face="bold"),
          axis.title.x = element_text(size=15, face="bold"),
          axis.text.x = element_text(size=10))
  
  b <- ggplot(public_brt$Imp_var, aes(x=var, y=rel.inf,fill=100-rel.inf))+ #reorder(var,rel.inf)
    geom_bar(stat="identity", position="dodge")+ coord_flip()+
    scale_fill_viridis_c(begin = 0.5,
                         end = 1)+
    ylab("Variable Importance (%)")+
    xlab("")+
    ylim(0,65)+
    ggtitle(paste0("Public attention"," r2 = ",round(public_brt$R2_brt,2)))+
    guides(fill=F)+ theme_bw()+
    theme(plot.title = element_text(size=18, face="bold"),
          axis.text.y = element_text(size=13, face="bold"),
          axis.title.x = element_text(size=15, face="bold"),
          axis.text.x = element_text(size=10))
  
  fig3_main <- gridExtra::grid.arrange(a,b,ncol=2)
  
  ggsave(file=here::here("tables_figures","FIG_3.tiff"), fig3_main,width = 35, height = 18, dpi = 300, units = "cm", device='tiff') 
  
  l_acad_brt <- lapply(as.character(acad_brt$Imp_var$var[1:6]), function(id)
  {
    ggplot(acad_brt$Par_dep[[id]], aes(x=value, y=fitted_function))+ 
      geom_point(shape=21,fill="#eeeeee",color="#808080") +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
      ylab("Fitted values")+xlab("values")+ggtitle(paste0(names(acad_brt$Par_dep[id]),"  ( ",round(acad_brt$Imp_var$rel.inf[acad_brt$Imp_var$var%in%id],1)," %)"))+
      theme_bw()+
      theme(plot.title = element_text(size=11, face="bold"))
  })
  
  l_public_brt <- lapply(as.character(public_brt$Imp_var$var[1:6]), function(id)
  {
    ggplot(public_brt$Par_dep[[id]], aes(x=value, y=fitted_function))+ 
      geom_point(shape=21,fill="#eeeeee",color="#808080") +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
      ylab("Fitted values")+xlab("values")+ggtitle(paste0(names(public_brt$Par_dep[id]),"  ( ",round(public_brt$Imp_var$rel.inf[public_brt$Imp_var$var%in%id],1)," %)"))+
      theme_bw()+
      theme(plot.title = element_text(size=11, face="bold"))
  })
  
  library(gridExtra)
  fig3_left <- do.call("grid.arrange", c(l_acad_brt, ncol=2))
  ggsave(file=here::here("tables_figures","FIG_3_left.tiff"), fig3_left,width = 11, height = 15, dpi = 300, units = "cm", device='tiff') 
  
  fig3_right <- do.call("grid.arrange", c(l_public_brt, ncol=2))
  ggsave(file=here::here("tables_figures","FIG_3_right.tiff"), fig3_right,width = 11, height = 15, dpi = 300, units = "cm", device='tiff') 
  
  
#FIGURE PARTIAL PLOTS
  
  l_acad_brt <- lapply(as.character(acad_brt$Imp_var$var[1:12]), function(id)
  {
    ggplot(acad_brt$Par_dep[[id]], aes(x=value, y=fitted_function))+ 
      geom_point(shape=21,fill="#eeeeee",color="#808080") +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
      ylab("Fitted values")+xlab("values")+ggtitle(paste0(names(acad_brt$Par_dep[id]),"  (relative influence= ",round(acad_brt$Imp_var$rel.inf[acad_brt$Imp_var$var%in%id],1)," %)"))+
      theme_bw()+
      theme(plot.title = element_text(size=8, face="bold"))
  })
  
  l_public_brt <- lapply(as.character(public_brt$Imp_var$var[1:12]), function(id)
  {
    ggplot(public_brt$Par_dep[[id]], aes(x=value, y=fitted_function))+ 
      geom_point(shape=21,fill="#eeeeee",color="#808080") +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
      ylab("Fitted values")+xlab("values")+ggtitle(paste0(names(public_brt$Par_dep[id]),"  (relative influence= ",round(public_brt$Imp_var$rel.inf[public_brt$Imp_var$var%in%id],1)," %)"))+
      theme_bw()+
      theme(plot.title = element_text(size=8, face="bold"))
  })

  #library(gridExtra)
  #figS1_A <- do.call("grid.arrange", c(l_acad_brt, ncol=3))
  #ggsave(file=here::here("tables_figures","FIG_S1_A.tiff"), figS1_A,width = 20, height = 16, dpi = 300, units = "cm", device='tiff') 
  
  #figS1_B <- do.call("grid.arrange", c(l_public_brt, ncol=3))
  #ggsave(file=here::here("tables_figures","FIG_S1_B.tiff"), figS1_B,width = 20, height = 16, dpi = 300, units = "cm", device='tiff') 

#----

#Phylogenetic analyses FIGURE 4----

  classif           <- read.csv(here::here("results","04_phylo", "classif.csv"))
  colnames(classif)[1] <- "sp_name"

  ## Scaridae is a subfamily of the Labridae (F. Leprieur personal comment)
  classif$family[classif$family == "Scaridae"] <- "Labridae"

  phylo_table          <- classif[, c("sp_name", "family", "order")]
  phylo_table$family   <- factor(phylo_table$family)
  phylo_table$sp_name  <- gsub(" ","_",as.character(phylo_table$sp_name))
  phylo_table$tip_name <- paste(phylo_table$family, phylo_table$sp_name, sep = "_")

  rownames(phylo_table) <- gsub("_"," ",phylo_table$sp_name)
  
  temp_table <- final_table[,c("acad","public")]
  rownames(temp_table) <- final_table$fb_sci_name
  
  phylo_table <- merge(phylo_table,temp_table,by="row.names",all.x = T)
  
  final_table$family <- factor(final_table$family)
  final_table$sp_name <- gsub(" ","_",as.character(final_table$fb_sci_name))
  final_table$tip_name <- paste(final_table$family, final_table$sp_name, sep = "_")

  ##Family
  ##here we get intel on the families with highest acad and public values 
  
    #number of families 140
    #length(unique(final_table$family[!(is.na(final_table$Evol_age))]))

    fam_acad <- aggregate(final_table$acad, by=list(Familly=final_table$family), FUN=mean,na.rm=T)
    colnames(fam_acad) <- c('Familly','acad')
    fam_public <- aggregate(final_table$public, by=list(Familly=final_table$family), FUN=mean,na.rm=T)
    colnames(fam_public) <- c('Familly','public')
  
    family <- data.frame(table(final_table$family))
    colnames(family) <- c('Familly','nb_species')
  
    family_acad <- merge(fam_acad,family,by="Familly")
    family_public <- merge(fam_public,family,by="Familly")
  
    family_acad <- subset(family_acad,family_acad$nb_species>20)
    family_acad <- family_acad[order(family_acad$acad,decreasing = T),]
    family_public <- subset(family_public,family_public$nb_species>20)
    family_public <- family_public[order(family_public$public,decreasing = T),]
  
  ##FIGURE 4a, b
  ## build the phylo_table for the variable of interest and select the species set
  ## all the section must be run for each variable of interest

    var_interet <- "public" #can be public or acad
    phylo_table <- final_table

    ### Scaridae is a subfamily of the Labridae according to F. Leprieur so we merge them
      phylo_table$family[phylo_table$family == "Scaridae"] <- "Labridae"
      phylo_table$family                                   <- factor(phylo_table$family)
      phylo_table$fb_sci_name                             <- as.character(phylo_table$fb_sci_name) # 2671 species left

    ## add underscores to acc_sci_names (used later)
      phylo_table$name     <- gsub(pattern = " ", replacement = "_", x = phylo_table$fb_sci_name)
      phylo_table$tip_name <- paste(phylo_table$family, phylo_table$name, sep="_")

    ##load the phylogeny tree for all fishes
      load(here::here("results","04_phylo", "set100.RData"))

    ##Plot the var of interest on one tree 
      usedTree <- set100[[100]]

      ###remove the species which are not in usedtree
      
        rm_sp <- usedTree$tip.label[!usedTree$tip.label %in% phylo_table$name]
      
        new_tree <-usedTree
        for (i in 1: length(rm_sp)){
          new_tree <- ape::drop.tip(new_tree, rm_sp[i], trim.internal = TRUE, subtree = FALSE,
                                    root.edge = 0, rooted = ape::is.rooted(new_tree), collapse.singles = TRUE,
                                    interactive = FALSE)
        }
  
        #sum(usedTree$tip.label %in% new_tree$tip.label)
  
      ### Order species according to the variable of interest to set the color gradient in contmap 
      ### (use the ranks rather than the absolute values to get a more balanced gradient)
      
        phylo_table       <- phylo_table[order(phylo_table[,var_interet]),] # order according to esth score
        var_ordered <- c(1:length(new_tree$tip.label))
        names(var_ordered) <- phylo_table$name[phylo_table$name %in% set100[[100]]$tip.label]
  
      ###build the map 
  
        obj2          <- phytools::contMap(new_tree, var_ordered, plot = FALSE, sig = 2, res = 200)
        # obj2          <- phytools::setMap(obj2, rev(c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#e0f3f8',
        #                                               '#abd9e9', '#74add1', '#4575b4', '#313695'))) # invert color map (use when needed)
        # 
        obj2          <- phytools::setMap(obj2, rev(c('#b41d14','#d73027', '#f46d43', '#fdae61', '#fee090', '#e0f3f8',
                                                '#abd9e9', '#74add1', '#4575b4', '#313695','#292e7f','#121657'))) # invert color map (use when needed)
        if (var_interet == "acad") fig <- "FIGURE_4_A.jpg"
        if (var_interet == "public") fig <- "FIGURE_4_B.jpg"
        
          ####Select the family to highlight before plotting the figure 
  
            fam_acad <- aggregate(final_table$acad, by=list(Familly=final_table$family), FUN=mean,na.rm=T)
            colnames(fam_acad) <- c('Familly','acad')
            fam_public <- aggregate(final_table$public, by=list(Familly=final_table$family), FUN=mean,na.rm=T)
            colnames(fam_public) <- c('Familly','public')
            
            family <- data.frame(table(final_table$family))
            colnames(family) <- c('Familly','nb_species')
            
            family_sub <- merge(fam_acad,family,by="Familly")
            family_sub <- merge(family_sub,fam_public,by="Familly")
            
            family_sub <- subset(family_sub,family_sub$nb_species>10)
            family_sub[order(family_sub$acad,decreasing = T),]
            family_sub[order(family_sub$public,decreasing = T),]
            
            #visu_fam(name_fam=c("Batrachoididae")) #Pseudochromidae
            
          ####Prepare the labels 
            
            if (var_interet == "acad") {
              fam_high <- c("Scombridae","Carangidae","Sparidae","Lutjanidae","Sebastidae")
              fam_low <-c("Blenniidae","Tripterygiidae","Gobiidae","Cheilodactylidae","Batrachoididae","Labridae")
              
            } else {
              fam_high <- c("Carangidae","Scombridae","Pomacanthidae","Balistidae","Acanthuridae","Chaetodontidae")
              fam_low <-c("Blenniidae","Tripterygiidae","Gobiidae","Cheilodactylidae","Lethrinidae","Batrachoididae","Labridae")
            }
            
            table <- phylo_table[phylo_table$sp_name %in% set100[[100]]$tip.label,]
            table_high <- table[which(table$family %in% fam_high),]
            table_high <- table_high[!is.na(table_high$family),]
            
            table_low <- table[which(table$family %in% fam_low),]
            table_low <- table_low[!is.na(table_low$family),]
            
            labels_fam_high <- as.character(unique(table_high$family))
            labels_fam_high  <- unlist(lapply(labels_fam_high, function(x){
              # x <- "Gempylidae"
              node        <-  phytools::findMRCA(set100[[100]],
                                                 as.character(table_high$sp_name[which(as.character(table_high$family) == x)]),
                                                 type = "node")
              names(node) <- x
              node
              }))
            labels_fam_high  <- labels_fam_high[order(labels_fam_high, decreasing = FALSE)]
            
            labels_fam_low <- as.character(unique(table_low$family))
            labels_fam_low  <- unlist(lapply(labels_fam_low, function(x){
              # x <- "Gempylidae"
              node        <-  phytools::findMRCA(set100[[100]],
                                                 as.character(table_low$sp_name[which(as.character(table_low$family) == x)]),
                                                 type = "node")
              names(node) <- x
              node
              }))
            labels_fam_low  <- labels_fam_low[order(labels_fam_low, decreasing = FALSE)]
            
          ####Plot the tree
            jpeg(here::here("tables_figures", fig),width = 20, height = 20, units = "cm", res = 500)
            
            plot(obj2, ftype = "off", type = "phylogram", outline = FALSE, legend = FALSE,#phylogram
                 fsize = c(0.2,1), lwd = 1.5, offset = 5, xlim = c(-220,300))
            phytools::add.color.bar(100, obj2$cols, title = "Rank",
                                    lims = obj2$lims, digits = 3, prompt = FALSE, x = -150,
                                    y = 150, lwd = 4, fsize = 1, subtitle = "")
            # for(i in 1:length(labels_fam_high)){
            #   phytools::cladelabels(text = paste0(names(labels_fam_high)[i]),
            #                         node        = labels_fam_high[i],
            #                         cex         = 1,
            #                         orientation = "horizontal",
            #                         wing.length=5,offset = 2)
            # } # eo for i
            # for(i in 1:length(labels_fam_low)){
            #   phytools::cladelabels(text = paste0(names(labels_fam_low)[i]),
            #                         node        = labels_fam_low[i],
            #                         cex         = 1,
            #                         orientation = "horizontal",
            #                         wing.length=5,offset = 2)
            # } # eo for i
            
            dev.off()
  
          ####Choose examples for the figure 
        
            ex <- phylo_table[phylo_table$family=="Scombridae",c("fb_sci_name","acad","public")]
            ex[order(ex$acad,decreasing = T),]
            ex[order(ex$public,decreasing = T),]

    ##Pagel Lambda
    ###Need to run the previous section before 

      var_interet <- "public"
      phylo_table_pagel <- phylo_table[phylo_table$name %in% set100[[1]]$tip.label,]

      trait             <- phylo_table_pagel[,var_interet]
      trait             <- sample(phylo_table_pagel[,var_interet])
      names(trait)      <- phylo_table_pagel$name

      #### Compute Pagel's lambda for the 100 trees (20 mins with 11 cores)
      ####(length(set100)
      #### produce pagel_acad.csv and pagel_public.csv
      
        start.time <- Sys.time()
        pagel <- do.call(rbind, pbmcapply::pbmclapply(1:100,function(i){
          
          set_100_elague <- set100[[i]]
          varinter        <- phylo_table_pagel[,var_interet]
          #varinter        <- sample(phylo_table_pagel[,var_interet])
          names(varinter) <- phylo_table_pagel$name
          SGNL         <- phytools::phylosig(set_100_elague, varinter, method = "lambda", test = TRUE)
          res <- cbind.data.frame(SGNL$lambda,SGNL$P)
          names(res) <- c('lambda','p-value')
          return(res)
          
        },mc.cores = parallel::detectCores()-1))
        end.time   <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
  
        mean(pagel$lambda) #acad = 0.62 public = 0.45
        
        lambda <- data.frame(lambda=pagel$lambda)
        write.csv2(lambda,here::here("results","06_main_analysis",paste0("pagel_",var_interet,".csv")))
  
        ggplot(pagel, aes(x=lambda)) +
          geom_density(fill="gray")+
          geom_vline(aes(xintercept=mean(lambda)), color="blue",
                     linetype="dashed")+
          labs(title=var_interet,x=var_interet, y = "Density")+xlim(0,1)+
          theme_classic()
        
#----   

#IUCN analysis FIGURE 5----

  final_table$IUCN <- as.factor(final_table$IUCN)
  final_table$IUCN <- factor(final_table$IUCN , levels=c("TH","NE", "DD", "LC"))

  ## IUCN models
  
    #acad 
      model <- lm(acad ~ IUCN, data = final_table)
      ANOVA <- aov(model)
      #car::leveneTest(ANOVA)  #not homogeneity of variance, need to do a kruskal_test
      modelk <- rstatix::kruskal_test(acad ~ IUCN, data = final_table)
      res_acad <- rstatix::dunn_test(acad ~ IUCN, p.adjust.method = "bonferroni", data = final_table)
 
    #public
      model <- lm(public ~ IUCN, data = final_table)
      ANOVA <- aov(model)
      #car::leveneTest(ANOVA)  #not homogeneity of variance, need to do a kruskal_test
      modelk <- rstatix::kruskal_test(public ~ IUCN, data = final_table)
      res_public <- rstatix::dunn_test(public ~ IUCN, p.adjust.method = "bonferroni", data = final_table)

  ## FIGURE 5 (1200 x 650)

    myColors <- ifelse(levels(final_table$IUCN)=="TH" , "#FA625F" , 
                       ifelse(levels(final_table$IUCN)=="NE", "#eeeeee",
                              ifelse(levels(final_table$IUCN)=="LC", "#56b658",
                                     "#B8B8B8" )))
    
    library(ggplot2)
    
    ###Figure of IUCN status box plots 
      
      axiss <- 9
      axist <- 13
        
      a <- ggplot(final_table, aes(x = IUCN, y = acad,colors=IUCN))+
        geom_boxplot(
          fill=myColors,
          width = .5, 
          outlier.shape = NA
        ) +  ylab("Research effort") +
        theme_bw() + ylim(0,1.1)+
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.2))+
        theme(axis.text = element_text(size=axiss),
              axis.title = element_text(size=axist))+
        ggdist::stat_halfeye(
          adjust = .5,
          width = .6,
          .width = 0,
          justification = -.3,
          alpha = .1,
          fill='blue',
          point_colour = NA)+
        geom_point(
          size = 1,
          alpha = .2,
          col="gray",
          position = position_jitter(
            seed = 1, width = .1
          ))
      
      b <- ggplot(final_table, aes(x = IUCN, y = public,colors=IUCN))+
        geom_boxplot(
          fill=myColors,
          width = .5, 
          outlier.shape = NA
        ) + ylab("Public attention") +
        theme_bw() + ylim(0,1.1)+
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.2))+
        theme(axis.text = element_text(size=axiss),
              axis.title = element_text(size=axist))+
        ggdist::stat_halfeye(
          adjust = .5,
          width = .6,
          .width = 0,
          justification = -.3,
          alpha = .1,
          fill='blue',
          point_colour = NA)+
        geom_point(
          size = 1.5,
          alpha = .3,
          col='gray',
          position = position_jitter(
            seed = 1, width = .1
          ))
      
      ###Figure by IUCN class

        library(dplyr)
        THR_final_table <- final_table %>% filter(IUCN=="TH")
        LC_final_table <- final_table %>% filter(IUCN=="LC")
        DD_final_table <- final_table %>% filter(IUCN=="DD")
        NE_final_table <- final_table %>% filter(IUCN=="NE")
        
        Q_public <- as.numeric(quantile(x <- final_table$public,probs = seq(0, 1, 0.1))[10])
        Q_acad <- as.numeric(quantile(x <- final_table$acad,probs = seq(0, 1, 0.1))[10])
        
        sum((THR_final_table$acad>Q_acad) & (THR_final_table$public>Q_public)) # 15 
        100*(sum((THR_final_table$acad>Q_acad) & (THR_final_table$public>Q_public)))/nrow(THR_final_table) # 7.9%

        sum((LC_final_table$acad>Q_acad) & (LC_final_table$public>Q_public)) # 86 
        100*(sum((LC_final_table$acad>Q_acad) & (LC_final_table$public>Q_public)))/nrow(LC_final_table) # 4.7
        
        sum((DD_final_table$acad>Q_acad) & (DD_final_table$public>Q_public)) # 4 
        100*(sum((DD_final_table$acad>Q_acad) & (DD_final_table$public>Q_public)))/nrow(DD_final_table) # 5.7
        
        sum((NE_final_table$acad>Q_acad) & (NE_final_table$public>Q_public)) # 5 
        100*(sum((NE_final_table$acad>Q_acad) & (NE_final_table$public>Q_public)))/nrow(NE_final_table) # 1.5
        
        lc <- grid::grobTree(grid::textGrob(paste0("(LC) Least Concern, n= ",dim(LC_final_table)[1]), x=0.03,  y=1.15, hjust=-0.1,
                                            gp=grid::gpar(col="black", fontsize=12)))
        thr <- grid::grobTree(grid::textGrob(paste0("(TH) Threatened, n= ",dim(THR_final_table)[1]), x=0.03,  y=1.15, hjust=-0.1,
                                             gp=grid::gpar(col="black", fontsize=12)))
        dd <- grid::grobTree(grid::textGrob(paste0("(DD) Data Deficient, n= ",dim(DD_final_table)[1]), x=0.03,  y=1.15, hjust=-0.1,
                                            gp=grid::gpar(col="black", fontsize=12)))
        ne <- grid::grobTree(grid::textGrob(paste0("(NE) Not Evaluated, n= ",dim(NE_final_table)[1]), x=0.03,  y=1.15, hjust=-0.1,
                                            gp=grid::gpar(col="black", fontsize=12)))
        
        ## find coordinates of fish to illustrate the figure
        # df <- data.frame(x=LC_final_table$acad, y=LC_final_table$public)
        # rownames(df) <- LC_final_table$fb_sci_name
        # plot(df)
        # abline(v=Q_public)
        # abline(h=Q_acad)
        # id_fish <- identifyPch(x=df$x,y=df$y)
        # rownames(df)[id_fish]
        
        sub_LC_final_table <- LC_final_table[LC_final_table$fb_sci_name %in% c("Dicentrarchus_labrax","Chelon_auratus","Synchiropus_splendidus"),]
        
        lc <- ggplot(LC_final_table, aes(x=acad, y=public)) + geom_point(shape=19,col="#56b658",size=2,alpha=0.4)+ 
          stat_ellipse(linetype="dashed",level = 0.90)+
          geom_hline(yintercept=Q_public, linetype="dashed", color = "gray")+ 
          geom_vline(xintercept=Q_acad, linetype="dashed", color = "gray")+
          theme_bw()+
          theme(axis.text = element_text(size=axiss),
                axis.title = element_text(size=axist),
                panel.grid.minor = element_blank())+
          xlim(-0.05,1.25)+ylim(-0.05,1.35)+
          scale_y_continuous(limits = c(0, 1.35), breaks = seq(0, 1, by = 0.2))+
          scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2))+
          xlab("Research effort")+ylab("Public attention")+
          geom_point(data=sub_LC_final_table, aes(x=acad, y=public),shape=1, color="black",size=4,stroke = 0.8)+
          geom_rect(
            fill="#56b658",alpha=0.25,
            #mapping=aes_string(x="month", y="big"),
            xmin=-0.06,
            xmax=1.26,
            ymin=1.26,
            ymax=1.42
          )#+annotation_custom(lc)
        
        ## find coordinates of fish to illustrate the figure
        # df <- data.frame(x=THR_final_table$acad, y=THR_final_table$public)
        # rownames(df) <- THR_final_table$fb_sci_name
        # plot(df)
        # abline(v=Q_public)
        # abline(h=Q_acad)
        # id_fish <- identifyPch(x=df$x,y=df$y)
        # rownames(df)[id_fish]
        #Bottom Aioliops_brachypterus
        #Top left Epinephelus_itajara Top right Melanogrammus_aeglefinus
        
        sub_THR_final_table <- THR_final_table[THR_final_table$fb_sci_name %in% c("Aioliops_brachypterus","Epinephelus_itajara","Melanogrammus_aeglefinus"),]
        
        th <- ggplot(THR_final_table, aes(x=acad, y=public)) + geom_point(shape=19,col="#f94444",size=2,alpha=0.4)+ 
          stat_ellipse(linetype="dashed",level = 0.90)+
          geom_hline(yintercept=Q_public, linetype="dashed", color = "gray")+ 
          geom_vline(xintercept=Q_acad, linetype="dashed", color = "gray")+
          theme_bw()+
          theme(axis.text = element_text(size=axiss),
                axis.title = element_text(size=axist),
                panel.grid.minor = element_blank())+
          xlim(-0.05,1.25)+ylim(-0.05,1.35)+
          scale_y_continuous(limits = c(0, 1.35), breaks = seq(0, 1, by = 0.2))+
          scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2))+
          xlab("Research effort")+ylab("Public attention")+
          geom_point(data=sub_THR_final_table, aes(x=acad, y=public),shape=1, color="black",size=4,stroke = 0.8)+
          geom_rect(
            fill="#FA625F",alpha=0.25, 
            #mapping=aes_string(x="month", y="big"), 
            xmin=-0.06,
            xmax=1.26,
            ymin=1.26,
            ymax=1.42
          )#+annotation_custom(thr)
        
        ## find coordinates of fish to illustrate the figure
        # df <- data.frame(x=DD_final_table$acad, y=DD_final_table$public)
        # rownames(df) <- DD_final_table$fb_sci_name
        # plot(df)
        # abline(v=0)
        # abline(h=0)
        # id_fish <- identifyPch(x=df$x,y=df$y)
        # rownames(df)[id_fish]
        #Top top Epinephelus_lanceolatus ; top left Zeus_faber ; top right Ocyurus_chrysurus
        sub_DD_final_table <- DD_final_table[DD_final_table$fb_sci_name %in% c("Ocyurus_chrysurus", "Zeus_faber", "Epinephelus_lanceolatus"),]
        
        dd <- ggplot(DD_final_table, aes(x=acad, y=public)) + geom_point(shape=19, col="#808080",size=2,alpha=0.5)+ 
          stat_ellipse(linetype="dashed",level = 0.90)+
          geom_hline(yintercept=Q_public, linetype="dashed", color = "gray")+ 
          geom_vline(xintercept=Q_acad, linetype="dashed", color = "gray")+
          theme_bw()+
          xlim(-0.05,1.25)+ylim(-0.05,1.35)+
          theme(axis.text = element_text(size=axiss),
                axis.title = element_text(size=axist),
                panel.grid.minor = element_blank())+
          scale_y_continuous(limits = c(0, 1.35), breaks = seq(0, 1, by = 0.2))+
          scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2))+
          xlab("Research effort")+ylab("Public attention")+
          geom_point(data=sub_DD_final_table, aes(x=acad, y=public),shape=1, color="black",size=4,stroke = 0.8)+
          geom_rect(
            fill="#B8B8B8",alpha=0.25, 
            #mapping=aes_string(x="month", y="big"), 
            xmin=-0.06,
            xmax=1.26,
            ymin=1.26,
            ymax=1.42
          )#+annotation_custom(dd)
        
        ## find coordinates of fish to illustrate the figure
        # df <- data.frame(x=NE_final_table$acad, y=NE_final_table$public)
        # rownames(df) <- NE_final_table$fb_sci_name
        # plot(df)
        # abline(v=0)
        # abline(h=0)
        # id_fish <- identifyPch(x=df$x,y=df$y)
        # rownames(df)[id_fish]
        #top left Plotosus_lineatus ; top right Boreogadus_saida
        #bottom Squamicreedia_obtusa

        sub_NE_final_table <- NE_final_table[NE_final_table$fb_sci_name %in% c("Squamicreedia_obtusa", "Plotosus_lineatus", "Boreogadus_saida"),]
        
        ne <- ggplot(NE_final_table, aes(x=acad, y=public)) + geom_point(shape=21, fill="#f1efef",color="#7c7b7b",size=2,alpha=0.6)+ 
          stat_ellipse(linetype="dashed",level = 0.90)+
          geom_hline(yintercept=Q_public, linetype="dashed", color = "gray")+ 
          geom_vline(xintercept=Q_acad, linetype="dashed", color = "gray")+
          theme_bw()+
          xlim(-0.05,1.25)+ylim(-0.05,1.35)+
          theme(axis.text = element_text(size=axiss),
                axis.title = element_text(size=axist),
                panel.grid.minor = element_blank())+
          scale_y_continuous(limits = c(0, 1.35), breaks = seq(0, 1, by = 0.2))+
          scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2))+
          xlab("Research effort")+ylab("Public attention")+
          geom_point(data=sub_NE_final_table, aes(x=acad, y=public),shape=1, color="black",size=4,stroke = 0.8)+
          geom_rect(
            fill="#eeeeee",alpha=0.25, 
            #mapping=aes_string(x="month", y="big"), 
            xmin=-0.06,
            xmax=1.26,
            ymin=1.26,
            ymax=1.42
          )#+annotation_custom(ne)
        
      #### arrange and save 
        
        fig5 <- gridExtra::grid.arrange(a,th,ne,b,dd,lc,ncol=3)
        
        ggsave(file=here::here("tables_figures","FIG_5.tiff"), fig5,width = 25, height = 15, dpi = 300, units = "cm", device='tiff') 
  

#----  

#Climate vulnerability  FIGURE 6----
#data from https://www.nature.com/articles/s41558-022-01437-y

  library(dplyr)
  library(ggplot2)
      
  final_table$IUCN <- as.factor(final_table$IUCN)
  final_table$IUCN <- factor(final_table$IUCN , levels=c("TH","NE", "DD", "LC"))
  THR_final_table <- final_table %>% filter(IUCN=="TH")
  
  #sum(!is.na(final_table$ClimVuln_SSP585)) 2094 fishes
  
  #top left (Dicentrarchus_labrax) top right (Totoaba_macdonaldi) bottom left (Paratrachichthys_trailli) bottom right (Springeratus_polyporatus)
  sub_acad <- final_table[final_table$fb_sci_name %in% c("Dicentrarchus_labrax", "Springeratus_polyporatus","Totoaba_macdonaldi","Paratrachichthys_trailli"),]
  
  #top left (Myoxocephalus_scorpius) top right (Stereolepis_gigas) bottom left (Decapterus_muroadsi) bottom right (Rypticus_courtenayi)
  sub_public <- final_table[final_table$fb_sci_name %in% c("Myoxocephalus_scorpius", "Rypticus_courtenayi","Decapterus_muroadsi","Stereolepis_gigas"),]
  
  my_color_grad <- rev(c('#4575b4', '#74add1', '#abd9e9', '#e0f3f8',
                         '#abe9c7', '#74d186', '#45b455', '#156b15'))
  
  a <- ggplot(final_table, aes(x=ClimVuln_SSP585, y=acad,color=1-Range_005))+ geom_point(size=2)+
    scale_color_gradientn(colours = my_color_grad)+
    geom_smooth(method = lm,colour="gray")+
    #geom_hline(yintercept=0.5, linetype="dashed", color = "gray")+ 
    #geom_vline(xintercept=0.5, linetype="dashed", color = "gray")+
    theme_bw()+ xlim(0.25, 0.75)+ylim(-0.19, 1)+
    theme(axis.text = element_text(size=9),
          axis.title = element_text(size=13))+
    geom_point(data=THR_final_table, aes(x=ClimVuln_SSP585, y=acad),shape=1, color="#f94444",size=2,alpha=0.9)+
    geom_smooth(
      aes(x = ClimVuln_SSP585, y =acad),
      color = "#FA9593",
      method = "lm",
      data = final_table[final_table$IUCN=="TH",],
      se = FALSE
    ) +
    geom_point(data=sub_acad, aes(x=ClimVuln_SSP585, y=acad),shape=1, color="#5C5C5C",size=4,stroke = 0.8)+
    ylab("Research effort")+xlab("Climate vulnerability (SSP5-8.5)")+
    theme(legend.position = "none")
  
  b <- ggplot(final_table, aes(x=ClimVuln_SSP585, y=public,color=1-Range_005))+ geom_point(size=2)+
    scale_color_gradientn(colours = my_color_grad)+
    geom_smooth(method = lm,colour="gray")+
    #geom_hline(yintercept=0.5, linetype="dashed", color = "gray")+ 
    #geom_vline(xintercept=0.5, linetype="dashed", color = "gray")+
    theme_bw()+ xlim(0.25, 0.75)+ylim(-0.19, 1)+
    theme(axis.text = element_text(size=9),
          axis.title = element_text(size=13))+
    geom_point(data=THR_final_table, aes(x=ClimVuln_SSP585, y=public),shape=1, color="#f94444",size=2,alpha=0.9)+
    geom_smooth(
      aes(x = ClimVuln_SSP585, y =public),
      color = "#FA9593",
      method = "lm",
      data = final_table[final_table$IUCN=="TH",],
      se = FALSE
    ) +
    geom_point(data=sub_public, aes(x=ClimVuln_SSP585, y=public),shape=1, color="#5C5C5C",size=4,stroke = 0.8)+
    ylab("Public attention")+xlab("Climate vulnerability (SSP5-8.5)")+
    theme(legend.position = "none")

  fig6 <- gridExtra::grid.arrange(a,b,ncol=2)
  ggsave(file=here::here("tables_figures","FIG_6.tiff"), fig6,width = 25, height = 10, dpi = 300, units = "cm", device='tiff') 
  
  #correlation with acad
    #cor.test(final_table$ClimVuln_SSP585, final_table$acad, method = "pearson", alternative = "less") # r=-0.7 p<0.001
    #ALL acad r=-0.30 p<0.001
    #cor.test(THR_final_table$ClimVuln_SSP585, THR_final_table$acad, method = "pearson", alternative = "less") # r=-0.7 p<0.001
    #TH acad r=-0.58 p<0.001
    #cor.test(final_table$ClimVuln_SSP585, final_table$public, method = "pearson", alternative = "less") # r=-0.7 p<0.001
    #ALL public r=-0.3 p<0.001
    #cor.test(THR_final_table$ClimVuln_SSP585, THR_final_table$public, method = "pearson", alternative = "less") # r=-0.7 p<0.001
    #TH public r=-0.53 p<0.001
  
  ### find coordinates of fish to illustrate
    # identifyPch <- function(x, y = NULL, n = length(x), plot = FALSE, pch = 19, ...)
    # {
    #   xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
    #   sel <- rep(FALSE, length(x))
    #   while(sum(sel) < n) {
    #     ans <- identify(x[!sel], y[!sel], labels = which(!sel), n = 1, plot = plot, ...)
    #     if(!length(ans)) break
    #     ans <- which(!sel)[ans]
    #     points(x[ans], y[ans], pch = pch)
    #     sel[ans] <- TRUE
    #   }
    #   ## return indices of selected points
    #   which(sel)
    # }
    # 
    # df <- data.frame(x=final_table$ClimVuln_SSP585, y=final_table$public)
    # rownames(df) <- final_table$fb_sci_name
    # plot(df)
    # id_fish <- identifyPch(x=df$x,y=df$y)
    # rownames(df)[id_fish]

#----
  
#Climate vulnerability  FIGURE S2----
  #data from https://www.nature.com/articles/s41558-022-01437-y
  
  library(dplyr)
  library(ggplot2)
  
  final_table$IUCN <- as.factor(final_table$IUCN)
  final_table$IUCN <- factor(final_table$IUCN , levels=c("TH","NE", "DD", "LC"))
  THR_final_table <- final_table %>% filter(IUCN=="TH")
  
  my_color_grad <- rev(c('#4575b4', '#74add1', '#abd9e9', '#e0f3f8',
                         '#abe9c7', '#74d186', '#45b455', '#156b15'))
  
 
  a <- ggplot(final_table, aes(x=ClimVuln_SSP126, y=acad,color=1-Range_005))+ geom_point(size=2)+
    scale_color_gradientn(colours = my_color_grad)+
    geom_smooth(method = lm,colour="gray")+
    #geom_hline(yintercept=0.5, linetype="dashed", color = "gray")+ 
    #geom_vline(xintercept=0.5, linetype="dashed", color = "gray")+
    theme_bw()+ xlim(0.25, 0.75)+ylim(-0.19, 1)+
    theme(axis.text = element_text(size=9),
          axis.title = element_text(size=13))+
    geom_point(data=THR_final_table, aes(x=ClimVuln_SSP126, y=acad),shape=1, color="#f94444",size=2,alpha=0.9)+
    geom_smooth(
      aes(x = ClimVuln_SSP126, y =acad),
      color = "#FA9593",
      method = "lm",
      data = final_table[final_table$IUCN=="TH",],
      se = FALSE
    )+
    ylab("Research effort")+xlab("Climate vulnerability (SSP1-2.6)")+
    theme(legend.position = "none")
  
  b <- ggplot(final_table, aes(x=ClimVuln_SSP126, y=public,color=1-Range_005))+ geom_point(size=2)+
    scale_color_gradientn(colours = my_color_grad)+
    geom_smooth(method = lm,colour="gray")+
    #geom_hline(yintercept=0.5, linetype="dashed", color = "gray")+ 
    #geom_vline(xintercept=0.5, linetype="dashed", color = "gray")+
    theme_bw()+ xlim(0.25, 0.75)+ylim(-0.19, 1)+
    theme(axis.text = element_text(size=9),
          axis.title = element_text(size=13))+
    geom_point(data=THR_final_table, aes(x=ClimVuln_SSP126, y=public),shape=1, color="#f94444",size=2,alpha=0.9)+
    geom_smooth(
      aes(x = ClimVuln_SSP126, y =public),
      color = "#FA9593",
      method = "lm",
      data = final_table[final_table$IUCN=="TH",],
      se = FALSE
    ) +
    ylab("Public attention")+xlab("Climate vulnerability (SSP1-2.6)")+
    theme(legend.position = "none")
  
  figS2 <- gridExtra::grid.arrange(a,b,ncol=2)
  ggsave(file=here::here("tables_figures","FIG_S2.tiff"), figS2,width = 25, height = 10, dpi = 300, units = "cm", device='tiff') 
  
  #corelation with ClimVuln_SSP126
  #cor.test(final_table$ClimVuln_SSP126, final_table$acad, method = "pearson", alternative = "less") # r=-0.7 p<0.001
  #ALL acad r=-0.19 p<0.001
  #cor.test(THR_final_table$ClimVuln_SSP126, THR_final_table$acad, method = "pearson", alternative = "less") # r=-0.7 p<0.001
  #TH acad r=-0.45 p<0.001
  #cor.test(final_table$ClimVuln_SSP126, final_table$public, method = "pearson", alternative = "less") # r=-0.7 p<0.001
  #ALL public r=-0.23 p<0.001
  #cor.test(THR_final_table$ClimVuln_SSP126, THR_final_table$public, method = "pearson", alternative = "less") # r=-0.7 p<0.001
  #TH public r=-0.43 p<0.001
  
 
#----














