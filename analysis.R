## TODO
## 1. Need to increase the MCMC sampling chain parameters
## 2. autcorrelation

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

purpose = 'Web' #'Reports'
sector = 'Pompeys' #'All'
    
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
source('helperFunctions.R')

## Make sure all the directories and subdirectories are in place
if (!dir.exists('figures')) dir.create('figures')
if (!dir.exists('data')) dir.create('data')

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
fish = MPA_loadFishData() %>% dplyr:::rename(SumOfABUNDANCE = SUMOFABUNDANCE)
benthos = MPA_loadBenthosData() %>% dplyr:::rename(SumOfCover = SUMOFCOVER)



## Eventually there will be a loop that will filter the data according to groups
## - Web Data
##   - HC    : group summary table
##   - SC    : group summary table
##   - A     : group summary table
##   - Total abundance         : fish05 (family)
##   - Large fish abundance    : fish05 (family)
##   - Pomacentridae abundance : fish05 (family)
##   - Coral trout density     : fish05 (species code)
## - Report Data
##   - Trout abundance
##   - Trout biomass
##   - Secondary abundance
##   - Secondary biomass
##   - multiple other functional groups

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

######################################################################
## Assign modelling groups to the fish data for Reports purpose     ##
## - generate Species richness data                                 ##
## - express counts (Values) relative to sampling AREA (calculate   ##
##    densities)                                                    ##
## - create Biomass and Length groups for Coral Trout and Secondary ##
##    targets                                                       ##
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
if (purpose=='Reports') save(data, file='data/reports.data.RData')
if (purpose=='Web') save(data, file='data/web.data.RData')

###############################################################
## Generate a model safe version of Year that can be used in ##
## autocorrelation models                                    ##
###############################################################
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

##########################################################
## Run the analyses, looping through each of the Groups ##
##########################################################
cm = list()
cm.inla = list()
cm.stan=list()
model.settings = read.table(file='parameters/model.settings.txt', header=TRUE,sep=',', strip.white=TRUE)
for (i in names(labels)) {
    ms = model.settings %>% filter(Group==i)
    dat = data %>% filter(Group==i)
    cat(paste0('##',i,'\n'))   
    cat('### Raw means\n')
    cellmeans<-MPA_rawMeans(dat)
    cm[[i]] <- cellmeans
    #MPA_rawPlot(cellmeans, ytitle=labels[[i]], stat='mean')
    p = MPA_RAPPlot(cellmeans, ytitle=labels[[i]], title=titles[[i]], stat='mean',purpose=purpose)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_raw.pdf'), p,width=5, height=3)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_raw.png'), p,width=5, height=3, dpi=300)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_raw.jpg'), p,width=5, height=3, dpi=300)
    
    cat('\n\nINLA========================\n')
    dat.inla<-MPA_inla(dat,fam=as.character(ms[,'INLA.family']), link=as.character(ms[,'INLA.link']))
    save(dat.inla, file=paste0('data/dat.inla_',i,'_',sec,'.RData'))
    cellmeans.inla <- MPA_inla.cellmeans(dat,dat.inla)
    cm.inla[[i]] <- cellmeans.inla
    cat('### INLA modelled means\n\n')
                                        #MPA_rawPlot(cellmeans.inla[[1]], ytitle=labels[[i]])
    p=MPA_RAPPlot(cellmeans.inla[[1]], ytitle=labels[[i]], title=titles[[i]],purpose=purpose)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_INLA.pdf'), p,width=5, height=3)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_INLA.png'), p,width=5, height=3, dpi=300)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_INLA.jpg'), p,width=5, height=3, dpi=300)
    
    cat('\n\n')
    cat('### INLA modelled sector means\n\n')
    MPA_sectorPlot(cellmeans.inla[['cellmeans.sector']], ytitle=labels[[i]])
    cat('STAN (via brms)===============\n')
    dat.stan <- MPA_stan(dat, cellmeans, family=ms[,'stan.family'])
    save(dat.stan, file=paste0('data/dat.stan_',i,'_',sec,'.RData'))
    cellmeans.stan = MPA_cellmeans_stan(dat.stan)
    #MPA_rawPlot(cellmeans.stan[[1]], ytitle=labels[[i]])
    p=MPA_RAPPlot(cellmeans.stan[[1]], ytitle=labels[[i]], title=titles[[i]],purpose=purpose)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_stan.pdf'), p,width=5, height=3)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_stan.png'), p,width=5, height=3, dpi=300)
    ggsave(filename=paste0('figures/RAPPlot_',i,'_',sec,'_stan.jpg'), p,width=5, height=3, dpi=300)
    
    MPA_sectorPlot(cellmeans.stan[['SectorZoneMeans']], ytitle=labels[[i]])
    cm.stan[[i]] <- cellmeans.stan    
    cat('\\clearpage\n\n')

    save(cm,file=paste0('data/cm_',sec,'.RData'))
    save(cm.inla,file=paste0('data/cm.inla_',sec,'.RData'))
    save(cm.stan,file=paste0('data/cm.stan_',sec,'.RData'))  
}
save(cm,file='data/cm.RData')
save(cm.inla,file='data/cm.inla.RData')
save(cm.stan,file='data/cm.stan.RData')  
