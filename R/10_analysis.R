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

##########################################################
## Run the analyses, looping through each of the Groups ##
##########################################################
## cm <- vector('list', length=length(names(labels)[20:37]))
cm <- vector('list', length=length(names(labels)))
names(cm) <- names(labels)
cm.inla <- cm.all <- cm.all.year <- cm.gam <- cm
model.settings <- read.table(file = paste0(params_path, "/model.settings.txt"),
                             header=TRUE,sep=',', strip.white=TRUE)
j <- 0
for (i in names(labels)) {  
    j<-j+1
    ms <- model.settings %>% filter(Group==i)
    dat <- data %>% filter(Group==i, Year <= MAX_REPORT_YEAR) |> droplevels()
    cat(paste0('\n## ',i,'\n'))   
    cat('### Raw means\n')
    cellmeans<-MPA_rawMeans(dat)
    cm[[i]] <- cellmeans
    cellmeans <- MPA_sector_levels4plotting(cellmeans)
    p <- MPA_RAPPlot(cellmeans, ytitle=labels[[i]], title=titles[[i]], stat='mean',purpose=purpose)
    MPA_SAVE_PLOTS(p, filename = paste0(fig_path, "/RAPPlot_",i,'_',sec,'_raw'), PNG = FALSE)
    
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
        dat <- dat %>% filter(Sector!='Cairns') %>% droplevels
        dat.mod <- dat.mod %>% filter(Sector!='Cairns') %>% droplevels
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
    save(dat.inla, file=paste0(modelled_data_path, '/dat.inla_',i,'_',sec,'.RData'))
    load(file<-paste0(modelled_data_path, '/dat.inla_',i,'_',sec,'.RData'))

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
    p<-MPA_RAPPlot(cellmeans.inla[[1]], ytitle=labels[[i]], title=titles[[i]],purpose=purpose)
    ggsave(filename<-paste0(fig_path, '/RAPPlot_',i,'_',sec,'_inla.pdf'), p,width=10, height=6)

    cm.all[[i]] <- cellmeans.inla$effects_overall %>% 
        mutate(Group=i)
}

save(cm.inla, file = paste0(modelled_data_path, '/cm.inla.RData'))
save(cm.all, file = paste0(modelled_data_path, '/cm.all.RData'))
load(file = paste0(modelled_data_path, '/cm.all.RData'))
load(file = paste0(modelled_data_path, '/cm.inla.RData'))

## The compilation plots are generated in summary.R
