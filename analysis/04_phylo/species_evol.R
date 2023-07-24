###################################################################################################
#'  Get classif, phylotree and compute ED and evol age
#'
#' @author Nicolas Mouquet , \email{nicolas.mouquet@@cnrs.fr}
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com}
#'  
#'    
#'  Produce 
#'    - classif.csv
#'    - set100.RData
#'    - table_phylo.csv
#'    
#'         
#' @date 2023/05/06
##################################################################################################

rm(list = ls())
#source(here::here("R", "00_functions.R"))
RLS_species <- read.csv2(here::here("results","01_build_species_list","RLS_species_init.csv"))
require(dplyr) # for the pipe

remsp <- c("Elagatis_bipinnulata","Euthynnus_affinis","Euthynnus_lineatus","Mola_mola",
           "Salmo_salar","Thunnus_albacares","Gadus_morhua")

RLS_species <- RLS_species[!RLS_species$fb_sci_name%in%remsp,]


#FUNCTIONS----

#' Get Classif
#' 
#' @description 
#' Extract classification of a list of species from the WORMS database
#'
#' @param names A character vector. Species names
#'
#' @return A `data.frame` with the following variables: `kingdom`, `phylum`, 
#'   `class`, `order`, `family`, `genus`, and `species`.
#'   
#' @export

get_classif <- function(names) {
  
  A <- data.frame(name = names, kingdom = NA, phylum = NA, class = NA, 
                  order = NA, family = NA, genus = NA, species = NA)
  
  for (i in 1:length(names)) {
    
    classi <- taxize::classification(names[i], db = "worms")
    classi <- classi[[1]]
    
    if (length(classi) == 1) {
      
      A[i, "kingdom"] <- NA
      A[i, "phylum"]  <- NA
      A[i, "class"]   <- NA
      A[i, "order"]   <- NA
      A[i, "family"]  <- NA
      A[i, "genus"]   <- NA
      A[i, "species"] <- NA
      
    } else {
      
      kingdom    <- classi$name[classi$rank == "Kingdom"]
      phylum     <- classi$name[classi$rank == "Phylum"]
      class      <- classi$name[classi$rank == "Class"]
      order      <- classi$name[classi$rank == "Order"]
      family     <- classi$name[classi$rank == "Family"]
      genus      <- classi$name[classi$rank == "Genus"]
      species    <- classi$name[classi$rank == "Species"]
      
      A[i, "kingdom"] <- kingdom
      A[i, "phylum"]  <- phylum    
      A[i, "class"]   <- class
      A[i, "order"]   <- order     
      A[i, "family"]  <- family    
      A[i, "genus"]   <- genus     
      A[i, "species"] <- species   
    }
  }
  
  A
}


#
#' Get Ages
#' 
#' @description 
#' Computes the age of all the species on the leaves of a phylgenetic tree.
#'
#' @param tree A phylogenetic object
#'
#' @return A vector of ages of the leaves of tree
#' 
#' @export

get_ages <- function(tree) {
  
  nsp <- length(tree$tip.label) # number of species
  
  tips <- which(tree$edge[ , 2] <= nsp) # get the starting and ending nodes of each edge
  # security to take only the ones inferior to the number of species ie "leaves" 
  
  ages <- tree$edge.length[tips] # the age of a species is actually the length of the edges from
  # the first node of the phylo tree to the species' leaf
  
  names(ages) <- tree$tip.label
  
  ages
}

#----

# Classification -----
# This gets the classification for all species in the bdd
# The first line takes more than an hour (taxize)

  sp_names <- gsub("_"," ",RLS_species$fb_sci_name)
  
  classif <- get_classif(sp_names)
  
  write.csv(x = classif, file = here::here("results","04_phylo", "classif.csv"),
            row.names = FALSE)
  
  classif<- read.csv(here::here("results","04_phylo", "classif.csv"))
  
  colnames(classif) <- c("name", colnames(classif[-1]))
  
  ## Scaridae is a subfamily of the Labridae (F. Leprieur personal communication) Fabien Leprieur <fableprieur@gmail.com>
    classif$family[classif$family == "Scaridae"] <- "Labridae"

# ----

# Generate Phylogenetic tree ----

  phylo_table          <- classif[, c("name", "family", "order")]
  phylo_table$family   <- factor(phylo_table$family)
  phylo_table$sp_name  <- gsub(" ","_",as.character(phylo_table$name))
  phylo_table$tip_name <- paste(phylo_table$family, phylo_table$name, sep = "_")
  
  # Download the phylogenetic tree for all fishes to compute the age
    set100_all  <- fishtree::fishtree_complete_phylogeny(phylo_table$name)
    ##Requested 2408 but only found 2307 species.
    
  # Dropping names not in list
    set100 <- pbmcapply::pbmclapply(set100_all,function(x){
      #x <- set100_all[[1]]
      ape::drop.tip(x, x$tip.label[!is.element(x$tip.label,as.character(gsub(" ","_",phylo_table$name)))])
    }, mc.cores = parallel::detectCores()-1)
  
  # Save
    save(set100, file =  here::here("results","04_phylo","set100.RData"))

# ----

# Species ages ----

  # load the 100 trees
    load(here::here("results","04_phylo", "set100.RData"))

  # Compute the age of all species found by fishtree (for each of the 100 trees)
    ages      <- do.call(cbind, pbmcapply::pbmclapply(set100, get_ages, mc.cores = parallel::detectCores()-1)) 
    ages_mean <- apply(ages, 1, mean, na.rm = T) # mean across the trees
    ages_mean <- ages_mean[match(phylo_table$sp_name, names(ages_mean))]
    phylo_table$ages_mean <- ages_mean
    
# ----

#Merge and save----
    
    table_phylo <- merge(classif,phylo_table[,c("name",'ages_mean')])
    write.csv(table_phylo,here::here("results","04_phylo", "table_phylo.csv"),row.names = FALSE)
#----
