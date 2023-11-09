# Reef Life Survey and Human Interest 

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Research compendium to reproduce analyses and figures of the following article:

> Low human interest for the most at-risk reef fishes worldwide, by Mouquet N., Langlois J., Casajus C., Auber A., Flandrin U.,Guilhaumon F., Loiseau N., McLean M., Receveur A., Stuart Smith R.D. & Mouillot, D. submitted to Science Advances in July 2023.

  
## Content

This repository is structured as follow:

- [`data/`](https://github.com/nmouquet/RLS_HUM_INT/tree/master/data):
contains data required to reproduce figures and tables

- [`analysis/`](https://github.com/nmouquet/RLS_HUM_INT/tree/main/analysis/):
contains subfolders organized by theme. Each folder contains R scripts to run 
specific analysis

- [`results/`](https://github.com/nmouquet/RLS_HUM_INT/tree/main/results):
follows the structure of analyses. Contains intermediate results and the 
numeric results used to produce the figures

- [`tables_figures/`](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures):
contains the figures and tables produced for the article

- [`R/`](https://github.com/nmouquet/RLS_HUM_INT/tree/main/R):
contains R functions developed for this project

- [`DESCRIPTION`](https://github.com/nmouquet/RLS_HUM_INT/tree/main/DESCRIPTION):
contains project metadata (author, date, dependencies, etc.)

- [`make.R`](https://github.com/nmouquet/RLS_HUM_INT/tree/main/make.R):
main R script to run the entire project by calling each R script stored in the `analyses/` folder


## Workflow
    
### Build species list

The script [`analysis/01_build_species_list/species_raw_list.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/01_build_species_list/species_raw_list.R)

- uses the species list from Langlois _et al._ (2022), checks the names and the presence on [Fishbase](https://www.fishbase.se)
- creates the [`results/01_build_table/all_accepted_syn.csv`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/results/01_build_species_list/RLS_species_init.csv) table that contains all the accepted synonyms for the species.

**Note:** this table is created by combining the information recorded in the folder `results/02_scrap/wta/` (not uploaded on GitHub). Contact us (nicolas.mouquet@cnrs.fr) to access these data.
  
  
### Get data from Flickr, Wikipedia, NCBI, WOS & Scopus

The script [`analysis/02_scrap/Scrap_them_all.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/02_scrap/Scrap_them_all.R) retrieves data from different databases.



###### FLICKR 

Queries were performed on species accepted name and synonyms.

We used the [Flickr API](https://www.flickr.com/services/api/) in R with the R package [`httr`](https://cran.r-project.org/package=httr). The time frame was 2010-01-01 to 2023-02-31.



###### WIKIPEDIA 

Queries were performed on species accepted names only.

We recorded views for the 10 most-viewed languages (English, German, Spanish, Russian, Japanese, French, Polish, Dutch, Italian, and Portuguese) accounted for 81.3% of page views (Mittermeier _et al._ 2021).

We used the [Wikimedia API](https://wikimedia.org/api/rest_v1/) with the R package [`pageviews`](https://cran.r-project.org/package=pageviews). The time frame was 2015-10-01 2022-12-31.



###### NCBI 

Queries were performed on species accepted name and synonyms.

We used the [Entrez NCBI API](https://www.ncbi.nlm.nih.gov/books/NBK25501/) for nucleotide and protein sequences with the R package [`rentrez`](https://cran.r-project.org/package=rentrez).



###### WOS & SCOPUS

Queries were performed on species accepted name and synonyms.

We used the [WOS Lite API](https://developer.clarivate.com/apis/woslite) with the R package [`rwoslite`](https://github.com/FRBCesab/rwoslite) to retrieve number of references in Web of Science for each species and the metadata of each reference (titles, journal, etc.). When the title of the article was indexed in [Scopus](https://www.scopus.com), we could get the number of citations for each article and the ASJC field code of the journal with the script [`analysis/02_scrap/scopus_refsall.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/02_scrap/scopus_refsall.R). 




### Get data from Twitter

The script [`analysis/02_scrap/twitter.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/01_build_species_list/species_raw_list.R) was used to retrieved data from [Twitter](https://twitter.com).

Queries were performed on species accepted name and synonyms.

The scrap has been performed in 2019 with the built-in function `get_twitter_intel()`. Note that this function use the R package [`RSelenium`](https://cran.r-project.org/package=RSelenium) but because Twitter often changes its credential, there is no guaranty that the function is still working.



### Get data from FishBase

The script [`analysis/02_scrap/fishbase.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/02_scrap/fishbase.R) was used to retrieved data from [FishBase](https://www.fishbase.se/search.php).

Queries were performed on species accepted names only.

This script uses the functions `species()` and `stocks()` from the R package [`rfishbase`](https://cran.r-project.org/package=rfishbase).



### Get data from GBIF

Scripts in [`analysis/03_gbif_homerange/`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/03_gbif_homerange/)
were used to 1) retrieve GBIF data using the R package [`rgbif`](https://cran.r-project.org/package=rgbif) and 2) compute species home range.



### Get species evolutionary data

The script [`analysis/04_phylo/species_evol.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/04_phylo/species_evol.R) was used to retrieve species classification, phylogenetic tree and to compute evolutionary age.



### Combine data

The script [`analysis/05_assemble_knowInt/assemble_knoint.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/05_assemble_knowInt/assemble_knoint.R) combines all human interest metrics and species attributes in a single file ([`results/05_assemlble_knowInt/05_Human_Interest_final_table.csv`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/results/05_assemlble_knowInt/05_Human_Interest_final_table.csv)).

It produces the [Figure S1](https://github.com/nmouquet/RLS_HUM_INT/blob/main/tables_figures/FIG_S1.tiff)

It computes the two dimensions of knowledge (academic knowledge and public interest) and produces the [Figure 2A](https://github.com/nmouquet/RLS_HUM_INT/blob/main/tables_figures/Fig2a.tiff) and the [Figure 2B](https://github.com/nmouquet/RLS_HUM_INT/blob/main/tables_figures/Fig2b.tiff).



### Analysis

The script [`analysis/06_main_analysis/main_analysis.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/06_main_analysis/main_analysis.R) run the main analysis of this study. Details can be found in the section methods of the associated article.

It produces [Figures 3-6](https://github.com/nmouquet/RLS_HUM_INT/blob/main/tables_figures/), [Figure S2](https://github.com/nmouquet/RLS_HUM_INT/blob/main/tables_figures/FIG_S2.tiff) and tables [`results/06_main_analysis/pagel_acad.csv`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/results/06_main_analysis/pagel_acad.csv) and [`results/06_main_analysis/pagel_public.csv`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/results/06_main_analysis/pagel_public.csv).



## Results  

The file [`results/05_assemlble_knowInt/05_Human_Interest_final_table.csv`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/results/05_assemlble_knowInt/05_Human_Interest_final_table.csv) contains all the information used in this study. You are welcome to use it by citing properly our work, but even more welcome to contact us (nicolas.mouquet@cnrs.fr) if you want to collaborate :smiley:
  
Some files were not uploaded on GitHub as they were too big. They can be provided on demand:

- `results/02_scrap/wta/`
- `results/03_gbif/gbif/`
- `results/03_scrap/scholar/rawdata/`



## Figures and Tables

Figures and Tables will be stored in `figures_tables/`.

The following Figures and Tables can be reproduced with the script indicated in brackets (all in [`analysis/`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/)):
    
- Figure 1 has been produced with other means.
- [Figures 2A](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures), [2B](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) were produced by [`05_assemble_knowint/assemble_knoint.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/05_assemble_knowint/assemble_knoint.R)

- [Figures 3A](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures), [3B](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) were produced by [`06_main_analysis/main_analysis.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/06_main_analysis/main_analysis.R)
- [Figure 4](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) was produced by [`06_main_analysis/main_analysis.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/06_main_analysis/main_analysis.R). Note that the final figure was produced by other means.
- [Figure 5](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) was produced by [`06_main_analysis/main_analysis.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/06_main_analysis/main_analysis.R)
- [Figure 6](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) was produced by [`06_main_analysis/main_analysis.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/06_main_analysis/main_analysis.R)

- Table 1 has been produced by other means.

- [Figure S1](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) was produced by [`05_assemble_knowint/assemble_knoint.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/05_assemble_knowint/assemble_knoint.R)

- [Figure S2](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) was produced by [`05_assemble_knowint/assemble_knoint.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/05_assemble_knowint/assemble_knoint.R)

- [Figure S3](https://github.com/nmouquet/RLS_HUM_INT/tree/main/tables_figures) was produced by [`06_main_analysis/main_analysis.R`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/analysis/06_main_analysis/main_analysis.R)



## Credits

Drawings of fishes used in the figures were created with the platform [DreamStudio](https://dreamstudio.ai/) using several images of each species as a baseline for training and using the keyword “Fish”, a stretch of 70% and 35 steps of variations. Resulting images are licensed under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/deed.en)). They are stored in [`data/images/`](https://github.com/nmouquet/RLS_HUM_INT/blob/main/data/images/).
  


## References

Langlois J, Guilhaumon F, Baletaud F, Casajus N, de Almeida Braga C, Fleuré V, Kulbicki M, Loiseau N, Mouillot D, Renoult JP, Stahl A, Stuart-Smith RD, Tribot A-S & Mouquet N (2022) The aesthetic value of reef fishes is globally mismatched to their conservation priorities. **PLoS Biology**, 20, e3001640. DOI: [10.1371/journal.pbio.3001640](https://doi.org/10.1371/journal.pbio.3001640).

Mittermeier JC, Correia R, Grenyer R, Toivonen T & Roll U (2021) Using Wikipedia to measure public interest in biodiversity and conservation **Conservation Biology**, 35, 412-423. DOI: [10.1111/cobi.13702](https://doi.org/10.1111/cobi.13702)
