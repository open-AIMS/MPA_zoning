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

## ---- settings
purpose <- 'Reports' #'Web' #'Reports'
sector <- 'All' #'Cairns' #'All'
PNG <- FALSE
MAX_REPORT_YEAR <- 2024
report_year <- 2024
sec <- "All"
## ----end

## ---- paths
MPA_create_paths(purpose, report_year)
## ----end

load(file = paste0(processed_data_path, "/labels.RData"))
load(file = paste0(processed_data_path, "/data.RData"))

########################################################################
## Load the extracted data and make a slight change to one field name ##
########################################################################
fish = MPA_loadFishData() %>% dplyr:::rename(SumOfABUNDANCE = SUMOFABUNDANCE)
benthos = MPA_loadBenthosData() %>% dplyr:::rename(SumOfCover = SUMOFCOVER)

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
fish <-MPA_cleanImport(fish,purpose)
benthos <-MPA_cleanImport(benthos,purpose)
## It seems like in 2020 two new Reefs have been added that are not
## part of the zone pairing (John Brewer Reef and Myrmidon Reef) As
## they are not part of the pairing, they break the models etc.  So I
## will exclude them.
fish <- fish %>% dplyr::filter(!is.na(RAP_REEF_PAIR))
benthos <- benthos %>% dplyr::filter(!is.na(RAP_REEF_PAIR))


#####################################################################
## Filter the data to just the original pairs.  This is so we can  ##
## compare the results to when all the reefs are in and therefore  ##
## show what would happen without a design change.                 ##
## Essentially we only keep the NEW_RAP_PAIRs that are less than   ##
## 29.                                                             ##
#####################################################################
fish <- fish |> dplyr::filter(NEW_RAP_PAIR < 29) |>
  droplevels()
benthos <- benthos |> dplyr::filter(NEW_RAP_PAIR < 29) |>
  droplevels()

######################################################################
## Assign modelling groups to the fish data for Reports purpose     ##
## - generate Species richness data                                 ##
## - express counts (Values) relative to sampling AREA (calculate   ##
##    densities)                                                    ##
## - create Biomass and Length groups for the various fish groups   ##
##    including Coral trout and Secondary targets                   ##
######################################################################
if (purpose=='Reports') fish <- MPA_group(fish)
##############################################################
## Assign modelling groups to the fish data for Web purpose ##
##############################################################
if (purpose=='Web') fish <- MPA_webgroups(fish)
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
data = fish.site %>% bind_rows(benthos.site) %>% as.data.frame
## data = fish %>% bind_rows(benthos) %>% as.data.frame
if (purpose=='Reports') save(data, file='data/reports.data_design_influence.RData')
if (purpose=='Web') save(data, file='data/web.data_design_influence.RData')

###################################################################
## Generate a model safe version of Year that can be used in     ##
## autocorrelation models.                                       ##
## Note also that there were some surveys conducted in 2017 that ##
## should not be part of this analyses - remove them.            ##
###################################################################
data = data %>% mutate(Time=Year-min(Year)) %>% filter(cYear!=2017) %>% droplevels

#################################################################################
## If this is for the web analysis, then we may wish to run on a single Sector ##
#################################################################################
sec='All'
if (purpose=='Web' & sector!='All') {
    data = data %>% filter(Sector==sector) %>% droplevels
    sec=sector
}

##########################################################################
## Define the y-axis labels (may want to put this in a parameters/ file ##
##########################################################################
labels=MPA_makeLabels(type=purpose)
titles=MPA_makeTitles(type=purpose) 

save(data, file='data/data_design_influence.RData')
load(file='data/data_design_influence.RData')

##########################################################
## Run the analyses, looping through each of the Groups ##
##########################################################
## cm <- vector('list', length=length(names(labels)[20:37]))
cm <- vector('list', length=length(names(labels)))
names(cm) <- names(labels)
cm.inla <- cm.all <- cm.all.year <- cm.gam <- cm
model.settings <- read.table(file='parameters/model.settings.txt',
                             header=TRUE,sep=',', strip.white=TRUE)
j = 0
for (i in names(labels)) {  
    j=j+1
    ms = model.settings %>% filter(Group==i)
    dat = data %>% filter(Group==i, Year <= MAX_REPORT_YEAR) |> droplevels()
    cat(paste0('\n## ',i,'\n'))   
    cat('### Raw means\n')
    cellmeans<-MPA_rawMeans(dat)
    cm[[i]] <- cellmeans
    cellmeans <- MPA_sector_levels4plotting(cellmeans)
    p = MPA_RAPPlot(cellmeans, ytitle=labels[[i]], title=titles[[i]], stat='mean',purpose=purpose)
    MPA_SAVE_PLOTS(p, filename = paste0('figures/',purpose,'/RAPPlot_',i,'_',sec,'_raw_design_influence'), PNG = FALSE)
    
    cat('### Fit INLA model\n')
    ## Exclude Sector/Year/Zone combinations that have no fish (not
    ## surveyed)
    dat <- MPA_remove_combinations_without_fish(dat)

    ## Reorder the levels such that a more data rich sector is the
    ## first level in order to stabalise models
    dat <- MPA_sector_levels4modelling(dat)
    
    ## Establish the model formula
    inla.form <- Y~Sector*cYear*Zone+
        f(Pair, model='iid') +
        f(Reef, model='iid') +
        f(Site, model='iid')
    ## Prepare the data according to the response type    
    if (grepl('.*Biomass', i)) {
        dat.g <- dat %>% filter(Value > 0)
        dat.c <- rbind(dat, dat.g)
        Y <- rbind(
            cbind(as.numeric(dat$Value > 0), NA),
            cbind(NA, dat.g$Value/1000))
        dat.mod <- dat.c
    } else if (grepl('.*Length', i)) {
        dat.mod <- dat
        Y <- dat.mod$Value
    } else { # Abundance
        dat.mod <- dat %>% mutate(Value=as.integer(ceiling(Value)))
        Y <- dat.mod$Value
    }
    regs<-
      dat.mod %>% group_by(Sector,cYear,Zone) %>%
      summarise(Sum=sum(Value)) %>%
      as.data.frame() %>%
      filter(Sum==0) %>%
      dplyr::select(-Sum)
    ## Handle a special case
    if(i =="Lethrinus miniatus and L. nebulosus (Redthroat and Spangled emperors)") {
        dat = dat %>% filter(Sector!='Cairns') %>% droplevels
        dat.mod = dat.mod %>% filter(Sector!='Cairns') %>% droplevels
        regs<-
            dat.mod %>% group_by(Sector,cYear,Zone) %>%
            summarise(Sum=sum(Value)) %>%
            as.data.frame() %>%
            filter(Sum==0) %>%
            dplyr::select(-Sum)
        Y <- dat.mod$Value
    }

    cat('### Fit the INLA model\n')
    dat.inla<-MPA_inla(data=dat,
                       data.mod = dat.mod,
                       Y = Y,
                       inla.form = inla.form,
                       fam=as.character(ms[,'INLA.family']),
                       link=as.character(ms[,'INLA.link']))
    save(dat.inla, file=paste0('data/dat.inla_',i,'_',sec,'_design_influence.RData'))
    load(file=paste0('data/dat.inla_',i,'_',sec,'_design_influence.RData'))

    cat('### Estimate cellmeans and effects\n')
    data.mod <- dat.mod
    cellmeans.inla <- MPA_inla.cellmeans(dat = dat,
                                         dat.inla,
                                         regs,
                                         mult=1)
    cm.inla[[i]] <- cellmeans.inla
    cat('### INLA modelled means\n\n')
                                        #MPA_rawPlot(cellmeans.inla[[1]], ytitle=labels[[i]])
    cellmeans.inla[[1]] <- MPA_sector_levels4plotting(cellmeans.inla[[1]])
    cellmeans.inla[[1]] <- cellmeans.inla[[1]] %>% filter(Sector != 'Cooktown-Lizard') %>% droplevels()
    p=MPA_RAPPlot(cellmeans.inla[[1]], ytitle=labels[[i]], title=titles[[i]],purpose=purpose)
    ggsave(filename=paste0('figures/',purpose,'/RAPPlot_',i,'_',sec,'_inla_design_influence.pdf'), p,width=10, height=6)

    cm.all[[i]] <- cellmeans.inla$effects_overall %>% 
        mutate(Group=i)
}

save(cm.inla, file='data/cm.inla_design_influence.RData')
save(cm.all, file='data/cm.all_design_influence.RData')
load(file='data/cm.all_design_influnce.RData')
load(file='data/cm.inla_design_influence.RData')

## The compilation plots are generated in summary.R

