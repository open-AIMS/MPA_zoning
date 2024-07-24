## Zoning analyses (Green vs Blue), Sector/Year combination.
## Responses:
##   - HC    : group summary table
##   - SC    : group summary table
##   - A     : group summary table
##   - Total abundance         : fish05 (family)
##   - Large fish abundance    : fish05 (family)
##   - Pomacentridae abundance : fish05 (family)
##   - Coral trout density     : fish05 (species code)
##   - Trout Biomass           : fish05 (species code)
##   - Secondary Target abundance : fish05 (family)
##   - Herbivore                  : fish05 (family)
## Output:
##   data.frame: cellmeans etc
##   figures: individual sectors

## Prior to running please review the following
## - parameters/density_table.csv
## - parameters/L-W co-eff.csv
## - parameters/MPA_paper trophic groups.csv
## - parameters/webGroups.csv
## - parameters/model.settings.txt

## There are two broad purposes for these scripts.
## purpose='Reports'
##   This is to generate a Report on MPA Zoning every two years
## purpose='Web'
##   This is to generate Sector specific outputs for webpages
##   As such, there is a global variable (sector) that indicates
##   which sector to model.
##   - sector can either be the name of an actual sector or
##     'All' in which case, all sectors will be modelled in a single model

## ---- libraries
library(dplyr)
library(tidyr)
library(reshape)
library(nlme)
library(data.table)
library(gmodels)
library(ggplot2)
library(gridExtra)
library(scales)
library(gtable)
library(MASS)
library(lme4)
library(arm)
library(brms)
library(coda)
library(INLA)
library(grid)
library(purrr)
library(png)
library(stringr)
library(ggtext)
library(magick)
source('../R/helperFunctions.R')
## ----end

## ---- settings
purpose <- 'Reports' #'Web' #'Reports'
sector <- 'All' #'Cairns' #'All'
PNG <- FALSE
MAX_REPORT_YEAR <- 2024
report_year <- 2024
## ----end

## ---- paths
MPA_create_paths(purpose, report_year)
## ----end

################################################################
## Extract data sources from the databases                    ##
## Exclude data for TRANSECT_NO 6 and REPORT_YEAR 2017        ##
## Also exclude data for which there is no RAP_REEF_PAIR data ##
##                                                            ##
## output                                                     ##
##   data/*.csv                                               ##
################################################################
MPA_getData()

########################################################################
## Load the extracted data and make a slight change to one field name ##
########################################################################
fish <- MPA_loadFishData() %>%
  dplyr:::rename(SumOfABUNDANCE = SUMOFABUNDANCE)
benthos <- MPA_loadBenthosData() %>%
  dplyr:::rename(SumOfCover = SUMOFCOVER)

#####################################################################
## Pre-process the data                                            ##
## - clean up latitude and longitude (if they exist)               ##
## - create both numeric (Year) and categorical (cYear) version of ##
##    REPORT_YEAR                                                  ##
## - create a tidy version of RAP_OPEN_CLOSED (Zone)               ##
## - create a model safe version of RAP_REEF_PAIR (Pair)           ##
## - create a model safe version of REEF_NAME (Reef)               ##
## - create a model safe version of SITE_NO (Site)                 ##
## - create a model sage version of TRANSECT_NO (Transect)         ##
## - create a tidy version of A_SECTOR (Sector) which for Reports  ##
##    data, combines Cairns and Innisfail together                 ##
## - create a tidy version of FISH_CODE (Species)                  ##
#####################################################################
fish <-MPA_cleanImport(fish, purpose)
benthos <-MPA_cleanImport(benthos, purpose)
## It seems like in 2020 two new Reefs have been added that are not
## part of the zone pairing (John Brewer Reef and Myrmidon Reef) As
## they are not part of the pairing, they break the models etc.  So I
## will exclude them.
fish <- fish %>% dplyr::filter(!is.na(RAP_REEF_PAIR))
benthos <- benthos %>% dplyr::filter(!is.na(RAP_REEF_PAIR))

######################################################################
## Assign modelling groups to the fish data for Reports purpose     ##
## - generate Species richness data                                 ##
## - express counts (Values) relative to sampling AREA (calculate   ##
##    densities)                                                    ##
## - create Biomass and Length groups for the various fish groups   ##
##    including Coral trout and Secondary targets                   ##
######################################################################
if (purpose == 'Reports') fish <- MPA_group(fish)
##############################################################
## Assign modelling groups to the fish data for Web purpose ##
##############################################################
if (purpose == 'Web') fish <- MPA_webgroups(fish)
#################################################
## Assign modelling groups to the benthos data ##
#################################################
benthos <- MPA_benthosgroups(benthos)

##################################################################
## Aggregate the fish data to Transect level (should already be ##
## Transect level)                                              ##
##################################################################
fish<-MPA_transectAgg(fish) 

########################################################################
## Aggregate data to site level                                       ##
## - For LENGTH as well as A, HC and SC aggregate with mean otherwise ##
##    sum                                                             ##
########################################################################
fish.site <- MPA_siteAgg(fish)
benthos.site <- MPA_siteAgg(benthos)

##################################################
## Bind together fish and benthos data and save ##
##################################################
data <- fish.site %>% bind_rows(benthos.site) %>% as.data.frame()
## data = fish %>% bind_rows(benthos) %>% as.data.frame
if (purpose=='Reports') save(data, file = paste0(processed_data_path, "/reports.data.RData"))
if (purpose=='Web') save(data, file = paste0(processed_data_path, "'/web.data.RData"))

###################################################################
## Generate a model safe version of Year that can be used in     ##
## autocorrelation models.                                       ##
## Note also that there were some surveys conducted in 2017 that ##
## should not be part of this analyses - remove them.            ##
###################################################################
data <- data %>% mutate(Time=Year-min(Year)) %>% filter(cYear!=2017) %>% droplevels()

#################################################################################
## If this is for the web analysis, then we may wish to run on a single Sector ##
#################################################################################
sec <- 'All'
if (purpose=='Web' & sector!='All') {
    data <- data %>% filter(Sector==sector) %>% droplevels()
    sec <- sector
}

##########################################################################
## Define the y-axis labels (may want to put this in a parameters/ file ##
##########################################################################
labels <- MPA_makeLabels(type = purpose)
titles <- MPA_makeTitles(type = purpose) 

save(labels, titles, file = paste0(processed_data_path, "/labels.RData"))
save(data, file = paste0(processed_data_path, "/data.RData"))
load(file = paste0(processed_data_path, "/data.RData"))

source("10_analysis.R")
source("20_summarise_models.R")


