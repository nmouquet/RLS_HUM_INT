#Not used in the analysis (was created only to check the IUCN status get from FISHBASE
#against the assessments.csv file downloaded from IUCN red list website)

rm(list = ls())
assessments <- read.csv(here::here('data','iucn','assessments.csv'))
final_table <- read.csv2(here::here("results","05_assemlble_knowInt","05_Human_Interest_final_table.csv"))

sum(gsub("_"," ",final_table$fb_sci_name) %in% assessments$scientificName)