
MPA_getData <- function() {
    ## Fish data
    MPA_getFishData()

    ## Benthos data
    MPA_getBenthosData()
}


MPA_getBenthosData <- function() {
        writeLines("
SELECT V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR,
 V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR,
 V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.TRANSECT_NO, BENTHOS_SUMMARY_ZEROS.GROUP_CODE,
Sum(BENTHOS_SUMMARY_ZEROS.COVER) AS SumOfCOVER
FROM V_RM_SAMPLE INNER JOIN BENTHOS_SUMMARY_ZEROS ON V_RM_SAMPLE.SAMPLE_ID = BENTHOS_SUMMARY_ZEROS.SAMPLE_ID
WHERE (((V_RM_SAMPLE.P_CODE)='RAP' Or (V_RM_SAMPLE.P_CODE)='RMRAP')
 AND ((V_RM_SAMPLE.VISIT_NO) Is Not Null))
GROUP BY V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME,
 V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR, V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.TRANSECT_NO,
 BENTHOS_SUMMARY_ZEROS.GROUP_CODE, V_RM_SAMPLE.VISIT_NO
HAVING (((BENTHOS_SUMMARY_ZEROS.GROUP_CODE)='HC' Or (BENTHOS_SUMMARY_ZEROS.GROUP_CODE) Like 'SC' Or
 (BENTHOS_SUMMARY_ZEROS.GROUP_CODE) Like 'A') AND ((V_RM_SAMPLE.VISIT_NO) Is Not Null))
ORDER BY V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.SITE_NO
",'data/benthos.sql')
    system("java -jar scripts/dbExport.jar data/benthos.sql data/benthos.csv reef reefmon")

        benthos <- read.csv('data/benthos.csv', strip.white=TRUE)
        benthos = benthos %>% filter(TRANSECT_NO!='6',!is.na(RAP_REEF_PAIR)) %>%
            filter(REPORT_YEAR!=2017) # ARLINGTON REEF (OPEN) sampled in 2017, but should not be included in the analyses
        head(benthos)
        save(benthos, file='data/benthos.RData')
}

MPA_getFishData <- function() {
    writeLines("
SELECT V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR,
  V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR,
  V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO,
  RM_FISH05.FAMILY, RM_FISH05.FISH_CODE, Sum(RM_FISH05.ABUNDANCE) AS SumOfABUNDANCE, RM_FISH05.LENGTH
FROM V_RM_SAMPLE INNER JOIN RM_FISH05 ON V_RM_SAMPLE.SAMPLE_ID = RM_FISH05.SAMPLE_ID
WHERE (((V_RM_SAMPLE.P_CODE)='RAP' Or (V_RM_SAMPLE.P_CODE)='RMRAP')
 AND ((V_RM_SAMPLE.VISIT_NO) Is Not Null))
GROUP BY V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME,
 V_RM_SAMPLE.RAP_OPEN_CLOSED, V_RM_SAMPLE.REPORT_YEAR, V_RM_SAMPLE.SITE_NO,
 V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO, RM_FISH05.FAMILY, RM_FISH05.FISH_CODE,
 RM_FISH05.LENGTH, V_RM_SAMPLE.P_CODE, V_RM_SAMPLE.VISIT_NO
HAVING (((RM_FISH05.FISH_CODE) Not Like 'APR_VIRE' And
         (RM_FISH05.FISH_CODE) Not Like 'PDA.*' And
         (RM_FISH05.FISH_CODE) Not Like 'CHA_LUNS' And
         (RM_FISH05.FISH_CODE) Not Like 'ACA_SP' And
         (RM_FISH05.FISH_CODE) Not Like 'CHA_OXYC' And
         (RM_FISH05.FISH_CODE) Not Like 'CHA_SEME' And
         (RM_FISH05.FISH_CODE) Not Like 'CHL_LABI' And
         (RM_FISH05.FISH_CODE) Not Like 'CHS_FRON' And
         (RM_FISH05.FISH_CODE) Not Like 'CRO_ALTI' And
         (RM_FISH05.FISH_CODE) Not Like 'CTE_BINO' And
         (RM_FISH05.FISH_CODE) Not Like 'DIP_BIFA' And
         (RM_FISH05.FISH_CODE) Not Like 'GNA_AURO' And
         (RM_FISH05.FISH_CODE) Not Like 'GRA_ALBI' And
         (RM_FISH05.FISH_CODE) Not Like 'GYN_SPP' And
         (RM_FISH05.FISH_CODE) Not Like 'IST_DECO' And
         (RM_FISH05.FISH_CODE) Not Like 'LET_ERUS' And
         (RM_FISH05.FISH_CODE) Not Like 'NAS_ANBR' And
         (RM_FISH05.FISH_CODE) Not Like 'PMS_OLIG' And
         (RM_FISH05.FISH_CODE) Not Like 'POM_PAVO' And
         (RM_FISH05.FISH_CODE) Not Like 'POM_UN' And
         (RM_FISH05.FISH_CODE) Not Like 'PSU_TUKA' And
         (RM_FISH05.FISH_CODE) Not Like 'SAR_RUBR' And
         (RM_FISH05.FISH_CODE) Not Like 'SCA_SP'))
ORDER BY V_RM_SAMPLE.RAP_REEF_PAIR, V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.REPORT_YEAR,
 V_RM_SAMPLE.SITE_NO, V_RM_SAMPLE.SAMPLE_DATE, V_RM_SAMPLE.TRANSECT_NO",
           'data/fish.sql')
    system("java -jar scripts/dbExport.jar data/fish.sql data/fish.csv reef reefmon")
    fish <- read.csv('data/fish.csv', strip.white=TRUE)
    fish = fish %>% filter(TRANSECT_NO!='6') %>%
        filter(REPORT_YEAR!=2017) # ARLINGTON REEF (OPEN) sampled in 2017, but should not be included in the analyses
    head(fish)
    save(fish, file='data/fish.RData')
}

MPA_loadFishData <- function() {
    load(file='data/fish.RData')
    fish
}
MPA_loadBenthosData <- function() {
    load(file='data/benthos.RData')
    benthos
}

MPA_cleanImport <- function(data,purpose) {
    if(!is.null(data$SITE_LAT)) data$LAT <- as.numeric(as.character(data$SITE_LAT))
    if(!is.null(data$SITE_LONG)) data$LONG <- as.numeric(as.character(data$SITE_LONG))

    data = data %>%
        mutate(
            Year=REPORT_YEAR
           #,dYear=Year
           ,cYear=factor(Year)
           ,Zone=factor(RAP_OPEN_CLOSED, levels=c('C','O'), labels=c('Closed','Open'))
           ,Pair=factor(RAP_REEF_PAIR)
           ,Reef=factor(interaction(RAP_REEF_PAIR,REEF_NAME))
           ,Site=factor(interaction(RAP_REEF_PAIR,REEF_NAME,SITE_NO))
           ,Transect=factor(interaction(RAP_REEF_PAIR,REEF_NAME,SITE_NO,TRANSECT_NO))
        )
    if (purpose=='Reports') data=data %>%
                                mutate(Sector=dplyr:::recode(A_SECTOR, CA='CAIN', IN='CAIN')
                                      ,Sector=factor(Sector, levels=c('CAIN','TO','PO','SW','CB'), labels=c('Cairns','Townsville','Pompeys','Swains','Cap-Bunkers')))
    if (purpose=='Web') data=data %>%
                            mutate(Sector=factor(A_SECTOR, levels=c('CA','IN','TO','PO','SW','CB'), labels=c('Cairns','Innisfail','Townsville','Pompeys','Swains','Cap-Bunkers')))
    if(exists('FISH_CODE', where=data)) data=data %>% mutate(Species=gsub('\\.','_',FISH_CODE))
    
    ## data = data %>%
    ##     mutate(
    ##         Year=REPORT_YEAR
    ##        #,dYear=Year
    ##        ,Sector=dplyr:::recode(A_SECTOR, CA='CAIN', IN='CAIN')
    ##        ,Sector=factor(Sector, levels=c('CAIN','TO','PO','SW','CB'), labels=c('Cairns','Townsville','Pompeys','Swains','Cap-Bunkers'))
    ##        ,cYear=factor(Year)
    ##        ,Zone=factor(RAP_OPEN_CLOSED, levels=c('C','O'), labels=c('Closed','Open'))
    ##        ,Pair=factor(RAP_REEF_PAIR)
    ##        ,Reef=factor(interaction(RAP_REEF_PAIR,REEF_NAME))
    ##        ,Site=factor(interaction(RAP_REEF_PAIR,REEF_NAME,SITE_NO))
    ##        ,Transect=factor(interaction(RAP_REEF_PAIR,REEF_NAME,SITE_NO,TRANSECT_NO))
    ##        ,Species=gsub('\\.','_',FISH_CODE)
    ##     )
    data
}


## Standardize all so that the response is Value
## It is either:
## - density
## - length (Coral Trout and Secondary targets)
## - biomass per unit area (Coral Trout and Secondary targets)
MPA_group <- function(data) {
    groups = read.csv('parameters/MPA paper trophic groups.csv', strip.white=TRUE)
    lw_conv<- read.csv("parameters/L-W co-effs.csv", header=T, sep=",", strip.white=T)
    
    groups$Species = groups$Code
    groups$Group=groups$Trophic

    richness = data %>%
        group_by(Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Species) %>%
        summarize(SumOfABUNDANCE=sum(SumOfABUNDANCE,na.rm=TRUE)) %>% ungroup %>%
        MPA_speciesRichness() %>%
        mutate(Group='Species Richness', Value=Richness) %>%
        dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value) 
    
    data=data %>% mutate(Value=SumOfABUNDANCE) %>%
        left_join(groups) %>%
        MPA_calculateDensities() %>% dplyr::select(-Area) %>%
        split(.$Group)
    
    ## For Coral Trout and Secondary targets, create additional LENGTH and BIOMASS items
    data[['Coral Trout Biomass']] = MPA_calculateBiomass(data[['Coral Trout']],lw_conv) %>%
        mutate(Value=Biomass, Group='Coral Trout Biomass') %>% dplyr::select(-a,-b,-Biomass)
    data[['Coral Trout Length']] = data[['Coral Trout']] %>%
        mutate(Value=LENGTH,Group='Coral Trout Length')
    data[['Secondary targets Biomass']] = MPA_calculateBiomass(data[['Secondary targets']],lw_conv) %>%
        mutate(Value=Biomass, Group='Secondary targets Biomass') %>% dplyr::select(-a,-b,-Biomass)
    data[['Secondary targets Length']] = data[['Secondary targets']] %>%
        mutate(Value=LENGTH,Group='Secondary targets Length')

    do.call('rbind', data) %>%
        dplyr::select(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Value) %>%
        bind_rows(richness) %>%
        filter(!is.na(Value))
}


MPA_speciesRichness <- function(data) {
    d=data %>% dplyr:::select(Sector,SHELF,Pair,Zone,Reef,Site,Transect,Year,cYear,Species,SumOfABUNDANCE) %>%
        mutate(Present=ifelse(is.na(SumOfABUNDANCE) | SumOfABUNDANCE==0,0,1)) %>% dplyr:::select(-SumOfABUNDANCE) %>%
            spread(key=Species,value=Present)
    d= d %>% mutate(Richness=d %>% rowwise() %>% dplyr:::select(matches('^[A-Z]{3,3}_[A-Z]{3,4}$')) %>% rowSums(na.rm=TRUE))
    d
}

MPA_calculateDensities <- function(data) {
        densities = read.csv('parameters/density_table.csv', strip.white=TRUE) %>%
        mutate(Taxa=gsub('\\_','.',Taxa)) %>% dplyr:::rename(Species=Taxa,Area=Convert)
        data %>% left_join(densities) %>%
            mutate(Area=ifelse(is.na(Area),4,Area),Value=Value*Area)
}

MPA_calculateBiomass <- function(data, lw_conv){
    data = data %>% #gather(key='Species', value='Value', contains('_')) %>%
        left_join(lw_conv) %>%
            mutate(Biomass=SumOfABUNDANCE*a*LENGTH^b)
    data
}

## First breaks the data.frame up into a list with item membership determined by filters
## defined in parameters/webGroups.csv
## Then the list is bound back into a single data.frame.
## The resulting data frame will be longer than the original if any entries (FAMILY OR FISH_CODE)
## are defined in multiple groups.
## Value represents abundance only
MPA_webgroups <- function(data) {
    webgroups = read.table('parameters/webGroups.csv', sep=';', header=TRUE)
    data1 = list()
    for (i in 1:nrow(webgroups)) {
        data1[[as.character(webgroups[i,1])]] =
            data %>% dplyr::filter_(.dots=as.character(webgroups[i,2])) %>%
            mutate(Group=as.character(webgroups[i,1])) %>% droplevels %>%
            group_by(Group,Sector,SHELF,Year,cYear,Zone,Pair,Reef,Site,Transect) %>%
            summarize(Value=sum(SumOfABUNDANCE,na.rm=TRUE)) %>% ungroup
    }
    do.call('rbind', data1)
}

MPA_benthosgroups <- function(data) {
    data %>% dplyr::filter(GROUP_CODE %in% c('HC','A','SC')) %>% droplevels %>%
        mutate(Group=GROUP_CODE,Value=SumOfCover)
}

MPA_transectAgg <- function(data) {
    data %>% group_by(Group,Sector,SHELF,Pair,Zone,Reef,Site,Transect,cYear,Year) %>%
        summarize(Value=ifelse(unique(Group) %in% c('Coral Trout Length', 'Secondary target Length'), mean(Value,na.rm=TRUE),
                                sum(Value,na.rm=TRUE)))
}

MPA_siteAgg <- function(data) {
    data %>% group_by(Group,Sector,SHELF,Pair,Reef,Site,Zone,cYear,Year) %>%
        summarize(Value=ifelse(unique(Group) %in% c('Coral Trout Length', 'Secondary target Length','A','HC','SC'), mean(Value,na.rm=TRUE),
                                sum(Value,na.rm=TRUE)))
}


MPA_makeLabels <- function(type='Reports') {
    labels = list() 
    if (type=='Reports') {
        labels[['Coral Trout Biomass']] = expression(Trout~biomass~(kg~per~1000~m^2))
        labels[['Coral Trout']] = expression(Trout~(per~1000~m^2))
        labels[['Coral Trout Length']] = expression(Trout~Length~(cm))
        labels[['Secondary targets Biomass']] = expression(Secondary~targets~biomass~(kg~per~1000~m^2))
        labels[['Secondary targets']] = expression(Secondary~targets~(per~1000~m^2))
        labels[['Secondary targets Length']] = expression(Secondary~targets~Length~(cm))
        labels[['Scrapers']] = expression(Scrapers~(per~1000~m^2))
        labels[['Croppers']] = expression(Croppers~(per~1000~m^2))
        labels[['Farmers']] = expression(Farmers~(per~1000~m^2))
        labels[['Planktivores']] = expression(Planktivores~(per~1000~m^2))
        labels[['Detritivores']] = expression(Detritivores~(per~1000~m^2))
        labels[['Benthic foragers']] = expression(Benthic~forager~(per~1000~m^2))
        labels[['Excavators']] = expression(Excavators~(per~1000~m^2))
        labels[['Obligate corallivores']] = expression(Obligate~corallivores~(per~1000~m^2))
        labels[['Omnivorous Pomacentridae']] = expression(Omnivorous~Pomacentridae~(per~1000~m^2))
        
        labels[['Species Richness']] = expression(Species~Richness~(per~site))
        
        labels[['A']] = expression(Macroalgae~cover~('%'))
        labels[['HC']] = expression(Hard~coral~cover~('%'))
        labels[['SC']] = expression(Soft~coral~cover~('%'))
    } else {
        labels[['large']] = expression(Median~abundance~per~Site)
        labels[['small']] = expression(Median~abundance~per~Site)
        labels[['total']] = expression(Median~abundance~per~Site)
        labels[['trout']] = expression(Median~abundance~per~Site)
        labels[['herbivores']] = expression(Median~abundance~per~Site)
        labels[['secondary_targets']] = expression(Median~abundance~per~Site)
        labels[['A']] = expression(Macroalgae~cover~('%'))
        labels[['HC']] = expression(Hard~coral~cover~('%'))
        labels[['SC']] = expression(Soft~coral~cover~('%'))
    }
    labels
}

MPA_makeTitles <- function(type='Reports') {
    titles = list() 
    if (type=='Reports') {
        titles[['Coral Trout Biomass']] = ''
        titles[['Coral Trout']] = ''
        titles[['Coral Trout Length']] = ''
        titles[['Secondary targets Biomass']] = ''
        titles[['Secondary targets']] = ''
        titles[['Secondary targets Length']] = ''
        titles[['Scrapers']] = ''
        titles[['Croppers']] = ''
        titles[['Farmers']] = ''
        titles[['Planktivores']] = ''
        titles[['Detritivores']] = ''
        titles[['Benthic foragers']] = ''
        titles[['Excavators']] = ''
        titles[['Obligate corallivores']] = ''
        titles[['Omnivorous Pomacentridae']] = ''
        
        titles[['Species Richness']] = ''
        
        titles[['A']] = ''
        titles[['HC']] = ''
        titles[['SC']] = ''
    } else {
        titles[['large']] = 'Large fish density from fixed site surveys'
        titles[['small']] = 'Small fish density from fixed site surveys'
        titles[['total']] = 'Total fish density from fixed site surveys'
        titles[['trout']] = 'Coral trout density from fixed site surveys'
        titles[['herbivores']] = 'Herbivore density from fixed site surveys'
        titles[['secondary_targets']] = 'Secondary target density from fixed site surveys'
        titles[['A']] = 'Algal cover from fixed site surveys'
        titles[['HC']] = 'Hard coral cover from fixed site surveys'
        titles[['SC']] = 'Soft coral cover from fixed site surveys'
    }
    titles
}


MPA_rawMeans <- function(data) {
    data %>% group_by(Sector,Zone,cYear) %>%
        summarize(Mean=mean(Value, na.rm=TRUE),
                  lower=ci(Value,na.rm=TRUE)[2],
                  upper=ci(Value,na.rm=TRUE)[3]
                  )
}

## Still need to put the Report data purpose figures in..
MPA_RAPPlot <- function(dat, ytitle, title, stat='median', purpose='Web') {
    if (purpose=='Web') {
        if(stat=='mean') {
            dat = dat %>% mutate(Value=Mean)  
        } else {
            dat = dat %>% mutate(Value=Median)
            #ytitle=substitute(ytitle,list(Mean=Median))
        }
        max.y = max(dat$upper, na.rm=TRUE)*1.25
        #if (grepl('cover',ytitle)) max.y=100
        
        p <-ggplot(dat, aes(y=Value, x=cYear, fill=Zone,color=Zone)) +
            geom_blank() + #aes(x=1,y=0))+
            geom_line(aes(x=as.numeric(cYear)),position=position_dodge(width=0.1))+
            geom_linerange(aes(ymin=lower, ymax=upper),position=position_dodge(width=0.1), show.legend=FALSE)+
            geom_point(position=position_dodge(width=0.1), size=2)+
                                        #facet_grid(~Sector, switch='x')+
            scale_fill_manual('', breaks=c('Closed','Open'), labels=c('Fishing prohibited','Open to fishing'),values=c('forestgreen','blue'))+
            scale_color_manual('', breaks=c('Closed','Open'), labels=c('Fishing prohibited','Open to fishing'),values=c('forestgreen','blue'))
                                        #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
        if (any(all.vars(ytitle) == 'cover')) p=p+scale_y_continuous(ytitle, breaks=seq(0,100,by=20),labels=seq(0,100,by=20), limits=c(0,max.y))
        if (!any(all.vars(ytitle) == 'cover')) p = p+scale_y_continuous(ytitle, labels=comma, limits=c(0,max.y))
        p=p+scale_x_discrete('Year')+
            theme_classic() +
            ggtitle(title)
        p<-p+theme(legend.position=c(1,1), legend.justification=c(1,1),
                   legend.direction = 'horizontal', legend.background=element_blank(),
                   strip.background=element_blank(),strip.text.x=element_text(size=12),
                   axis.title.y=element_text(vjust=1.5),
                   panel.spacing=unit(1,unit='lines'), axis.text.x=element_text(size=10),
                   axis.line.y=element_line(), axis.line.x=element_line(),strip.placement='outside')
    }
    ## if(stat=='mean') {
    ##     p <-ggplot(dat, aes(y=Mean, x=cYear, fill=Zone, shape=Zone,linetype=Zone)) +
    ##         geom_blank()+
    ##         geom_line(aes(x=as.numeric(cYear)))+
    ##         geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1, linetype=1)+geom_point()+
    ##             facet_grid(~Sector, switch='x')+
    ##                 scale_fill_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c('black','white'))+
    ##                     scale_shape_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(21,21))+
    ##                         scale_linetype_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(1,2))+
    ##                                     #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
    ##                             scale_y_continuous(ytitle, labels=comma)+
    ##                                     #scale_x_continuous('', breaks=2006:2014)+
    ##                                 theme_classic()
    ## } else {
    ##     p <-ggplot(dat, aes(y=Median, x=cYear, fill=Zone, shape=Zone,linetype=Zone)) +
    ##         geom_blank()+
    ##         geom_line(aes(x=as.numeric(cYear)))+
    ##         geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1, linetype=1)+geom_point()+
    ##             facet_grid(~Sector,switch='x')+
    ##                 scale_fill_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c('black','white'))+
    ##                     scale_shape_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(21,21))+
    ##                         scale_linetype_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(1,2))+
    ##                                     #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
    ##                             scale_y_continuous(ytitle, labels=comma)+
    ##                                     #scale_x_continuous('', breaks=2006:2014)+
    ##                                 theme_classic()
    ## }

    #g <- ggplotGrob(p)
    #print(p)
    #grid.newpage()
                                        #grid.draw(rbind(g[c(1,4:5)], g[1:3],size="first"))
    p
}


MPA_inla <- function(data,fam='nbinomial',link='log') {
    if (fam=='beta') data = data %>% mutate(Value=Value/100)
    n.1 = 1:nrow(data) # indices of data
    dd <- expand.grid(Sector=levels(data$Sector),Year=levels(data$cYear),Zone=levels(data$Zone))
    n<-nrow(dd)
    m <- matrix(0,n/2,n)
    for (i in 1:n/2) m[i,i] <- -1
    for (i in 1:n/2) m[i,i+(n/2)] <- 1
    N <- nrow(data)
    LP <-inla.make.lincombs(Predictor=cbind(matrix(0,n/2,N),m ), '(Intercept)'=rep(0,n/2))

    #Xmat = model.matrix(~Sector*Year*Zone, data=dd)[1:2,]
    #LP=inla.make.lincomb(as.data.frame(Xmat))
    newdata <- cbind(Value=NA,expand.grid(Sector=levels(data$Sector),cYear=levels(data$cYear),Zone=levels(data$Zone)), Pair=NA, Reef=NA,Site=NA)
    dat <- rbind(data %>% dplyr:::select(Value,Sector,cYear,Zone,Pair,Reef,Site), newdata) %>% as.data.frame
    n.2 = (nrow(data)+1):nrow(dat)
    ## newdata3 <- cbind(Biomass=NA,expand.grid(Sector=levels(data$Sector),Year=NA,Zone=levels(data$Zone)), Pair=NA, Reef=NA,Site=NA)
    ## n.3 = (nrow(dat)+1):(nrow(dat)+nrow(newdata3))
    ## dat <- rbind(dat, newdata3) %>% as.data.frame
    INLA:::inla.dynload.workaround()
    if (grepl('beta',fam)) dat = dat %>% mutate(Value=ifelse(Value==0,0.01,Value))
    if (length(unique(data$Sector))>1) {
        dat.inla <- inla(Value~Sector*cYear*Zone+
                             f(Pair, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Reef, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Site, model='iid'), #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))),
                         data=dat,
                                        #control.fixed=list(expand.factor.strategy="inla",mean=0, prec=0.01, mean.intercept=0, prec.intercept=0.01),
                                        #control.family=list(hyper=list(theta1=list(prior='loggamma', param=c(0.01,0.01)),
                                        #                        theta2=list(prior='gaussian', param=c(0,0.01)))),
                                        #control.family=list(hyper=list(theta=list(prior='loggamma', param=c(0.1,0.1)))),
                         family=fam,#'nbinomial',
                         control.family=list(link=link),
                         control.predictor = list(compute=TRUE, link=1),
                         control.inla=list(lincomb.derived.only=TRUE))#,
                                        #                     lincomb=LP)
    } else {
           dat.inla <- inla(Value~cYear*Zone+
                             f(Pair, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Reef, model='iid') + #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))) +
                             f(Site, model='iid'), #,hyper=list(theta=list(prior='loggamma', params=c(0,0.01)))),
                         data=dat,
                                        #control.fixed=list(expand.factor.strategy="inla",mean=0, prec=0.01, mean.intercept=0, prec.intercept=0.01),
                                        #control.family=list(hyper=list(theta1=list(prior='loggamma', param=c(0.01,0.01)),
                                        #                        theta2=list(prior='gaussian', param=c(0,0.01)))),
                                        #control.family=list(hyper=list(theta=list(prior='loggamma', param=c(0.1,0.1)))),
                         family=fam,#'nbinomial',
                         control.family=list(link=link),
                         control.predictor = list(compute=TRUE, link=1),
                         control.inla=list(lincomb.derived.only=TRUE))
    }
    list(Inla=dat.inla,N=N,newdata=newdata,n.1=n.1,n.2=n.2)
}


MPA_inla.cellmeans <- function(dat,model) {
    #n1 = nrow(dat)+1
    #n2 = n1 + nrow(model$newdata)-1
    cellmeans = cbind(model[['newdata']],
        model[[1]]$summary.linear.predictor[model[['n.2']],]
                      )
    mult=1
    if (model[[1]]$all.hyper$family[[1]]$label=='zeroinflatednbinomial1') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta1$from.theta
    if (model[[1]]$all.hyper$family[[1]]$label=='nbinomial') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta
    if (model[[1]]$all.hyper$family[[1]]$label=='gamma') inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta
    #if (model[[1]]$all.hyper$family[[1]]$label=='zeroinflatedbinomial1') {inv.link=model[[1]]$all.hyper$family[[1]]$hyper$theta$from.theta; mult=100;}
    if (model[[1]]$all.hyper$family[[1]]$label=='binomial') {inv.link=binomial()$linkinv; mult=100;}
    if (model[[1]]$all.hyper$family[[1]]$label=='beta') {inv.link=binomial()$linkinv; mult=100;}
    #if (model[[1]]$all.hyper$family[[1]]$label=='betabinomial') {inv.link=binomial()$linkinv; mult=100;}
    
    cellmeans$mean=inv.link(cellmeans$mean) * mult
    cellmeans$Median=inv.link(cellmeans[,'0.5quant']) * mult
    cellmeans$lower = inv.link(cellmeans[,'0.025quant']) * mult
    cellmeans$upper = inv.link(cellmeans[,'0.975quant']) * mult

    ## Percent Effects (Zone) for each Year/Sector
    aa=inv.link(t(plyr:::ldply(model[[1]]$marginals.linear.predictor[model[['n.2']]], function(x) {
        inla.rmarginal(10000,x)
    })[,-1]))*mult
    ndat.p=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone)) #%>% arrange(Year,Zone)
    #nn=expand.grid(Sector=levels(dat$Sector),Year=levels(dat$Year))
    ndat.p = as.matrix((dd=(cbind(ndat.p,t(aa)) %>% arrange(Sector,cYear)))[,-1:-3])
    s1=ndat.p[seq(1,nrow(ndat.p),by=2),]; s2=ndat.p[seq(2,nrow(ndat.p),by=2),];
    
    #ndat.p = cbind(dd[,1:3],plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
    #    data.frame(Mean=mean(x),Median=median(x),t(quantile(x,p=c(0.025,0.975))),'p>0'=length(x[x>0])/length(x))
    #}))
    ndat.p = cbind(dd[seq(1,nrow(dd),by=2),1:2],plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
        data.frame(Mean=mean(x),Median=median(x),t(quantile(x,p=c(0.025,0.975))),'p>0'=length(x[x>0])/length(x))
    }))
    ## ndat = cbind(ndat,t(aa)) %>% group_by(Sector,Year) %>% do({
    ##     x=.
    ##     xx=as.matrix((x[1,-1:-3]-x[2,-1:-3])/x[2,-1:-3])
    ##     data.frame(Mean=mean(xx), Median=median(xx),t(quantile(xx,p=c(0.025,0.975))),
    ##                    'p>0'=length(xx[xx>0])/length(x))
    ## })  %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    ndat=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    if (length(unique(ndat$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat)
    if (length(unique(ndat$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat)
    Xmat=as.matrix(cbind(ndat,Xmat) %>% dplyr:::select(-Zone) %>% group_by(cYear,Sector) %>% do({
        x=.
        data.frame(x[1,-1:-2]-x[2,-1:-2])
    }) %>% ungroup %>% dplyr:::select(-Sector,-cYear))
    ndat=ndat %>% ungroup %>% dplyr:::select(-Zone) %>% distinct %>% bind_cols(
        plyr:::adply(aa %*% t(Xmat),2,function(x) {
            data.frame(Mean=mean(x), Median=median(x),t(quantile(x,p=c(0.025,0.975))),
                       'p>0'=length(x[x>0])/length(x))
        })
    ) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ## Sector cell means
    ndat.sector=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    if (length(unique(ndat.sector$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat.sector)
    if (length(unique(ndat.sector$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat.sector)
    Xmat=cbind(ndat.sector,Xmat) %>% dplyr:::select(-cYear) %>% group_by(Zone,Sector) %>% summarize_all(funs(mean)) %>% ungroup
    cellmeans.sector=Xmat %>% dplyr:::select(Sector,Zone)
    Xmat=as.matrix(Xmat %>% dplyr:::select(-Sector,-Zone))
    cellmeans.sector=cbind(cellmeans.sector,
        plyr:::adply((aa %*% t(Xmat)),2,function(x) {
            data.frame(Mean=mean(x), Median=median(x),t(quantile(x,p=c(0.025,0.975))),t(quantile(x,p=c(0.25,0.75)))
                       )
        })
    )%>% dplyr:::rename(lower=X2.5., upper=X97.5., lower.1=X25., upper.1=X75.)
    ## cellmeans.sector = cbind(model[['newdata3']],
    ##     model[[1]]$summary.linear.predictor[model[['n.3']],]
    ##                          )
    
    ##Sector effect sizes
    aa.sector = aa %*% t(Xmat)
    ndat.p.sector=expand.grid(Sector=levels(dat$Sector),Zone=levels(dat$Zone))
    ndat.p.sector = as.matrix((dd=(cbind(ndat.p.sector,t(aa.sector)) %>% arrange(Sector)))[,-1:-2])
    s1=ndat.p.sector[seq(1,nrow(ndat.p.sector),by=2),]; s2=ndat.p.sector[seq(2,nrow(ndat.p.sector),by=2),];
    ndat.p.sector = cbind(dd[,1:2],plyr:::adply(100*(s1-s2)/s2, 1, function(x) {
        data.frame(Mean=mean(x),Median=median(x),t(quantile(x,p=c(0.025,0.975))),'p>0'=length(x[x>0])/length(x))
    }))

    
    ## ndat.sector=expand.grid(Sector=levels(dat$Sector),Zone=levels(dat$Zone))
    ## ndat.sector = cbind(ndat.sector,t(aa.sector)) %>% group_by(Sector) %>% do({
    ##     x=.
    ##     xx=as.matrix((x[1,-1:-2]-x[2,-1:-2])/x[2,-1:-2])
    ##     data.frame(Mean=mean(xx), Median=median(xx),t(quantile(xx,p=c(0.025,0.975))),
    ##                    'p>0'=length(xx[xx>0])/length(x))
    ## })  %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    ndat.sector=expand.grid(Sector=levels(dat$Sector),Zone=levels(dat$Zone))
    if (length(unique(ndat.sector$Sector))>1) Xmat = model.matrix(~-1+Sector:Zone, data=ndat.sector)
    if (length(unique(ndat.sector$Sector))==1) Xmat = model.matrix(~-1+Zone, data=ndat.sector) 
    Xmat=as.matrix(cbind(ndat.sector,Xmat) %>% dplyr:::select(-Zone) %>% group_by(Sector) %>% do({
        x=.
        data.frame(x[1,-1]-x[2,-1])
    }) %>% ungroup %>% dplyr:::select(-Sector))
    ndat.sector=ndat.sector %>% ungroup %>% dplyr:::select(-Zone) %>% distinct %>% bind_cols(
        plyr:::adply(aa.sector %*% t(Xmat),2,function(x) {
            data.frame(Mean=mean(x), Median=median(x),t(quantile(x,p=c(0.025,0.975))),
                       'p>0'=length(x[x>0])/length(x))
        })
    ) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    #Overall Zone means
    ndat.zone=expand.grid(Sector=levels(dat$Sector),cYear=levels(dat$cYear),Zone=levels(dat$Zone))
    if (length(unique(ndat.zone$Sector))>1) Xmat = model.matrix(~-1+Sector:cYear:Zone, data=ndat.zone)
    if (length(unique(ndat.zone$Sector))==1) Xmat = model.matrix(~-1+cYear:Zone, data=ndat.zone)
    Xmat=cbind(ndat.zone,Xmat) %>% dplyr:::select(-cYear,-Sector) %>% group_by(Zone) %>% summarize_all(funs(mean)) %>% ungroup
    cellmeans.zone=Xmat %>% dplyr:::select(Zone)
    Xmat=as.matrix(Xmat %>% dplyr:::select(-Zone))
    cellmeans.zone=cbind(cellmeans.zone,
        plyr:::adply(aa %*% t(Xmat),2,function(x) {
            data.frame(Mean=mean(x), Median=median(x),t(quantile(x,p=c(0.025,0.975)))
                       )
        })
    )%>% dplyr:::rename(lower=X2.5., upper=X97.5.)

    ##Zone effect sizes
    aa.zone = aa %*% t(Xmat)
    ndat.p.zone=expand.grid(Zone=levels(dat$Zone))
    ndat.p.zone = as.matrix((dd=(cbind(ndat.p.zone,t(aa.zone))))[,-1])
    s1=ndat.p.zone[seq(1,nrow(ndat.p.zone),by=2),]; s2=ndat.p.zone[seq(2,nrow(ndat.p.zone),by=2),];
    x=100*(s1-s2)/s2
    ndat.p.zone = data.frame(Mean=mean(x),Median=median(x),t(quantile(x,p=c(0.025,0.975))),'p>0'=length(x[x>0])/length(x))
    
    ## ndat.zone=expand.grid(Zone=levels(dat$Zone))
    ## ndat.zone = cbind(ndat.zone,t(aa.zone)) %>% do({
    ##     x=.
    ##     xx=as.matrix((x[1,-1]-x[2,-1])/x[2,-1])
    ##     data.frame(Mean=mean(xx), Median=median(xx),t(quantile(xx,p=c(0.025,0.975))),
    ##                    'p>0'=length(xx[xx>0])/length(x))
    ## })  %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    ndat.zone=expand.grid(Zone=levels(dat$Zone))
    Xmat = model.matrix(~-1+Zone, data=ndat.zone)    
    Xmat=as.matrix(cbind(ndat.zone,Xmat) %>% dplyr:::select(-Zone) %>% do({
        x=.
        data.frame(x[1,]-x[2,])
    }) %>% ungroup)
    ndat.zone=plyr:::adply(aa.zone %*% t(Xmat),2,function(x) {
            data.frame(Mean=mean(x), Median=median(x),t(quantile(x,p=c(0.025,0.975))),
                       'p>0'=length(x[x>0])/length(x))
        }) %>% dplyr:::rename(lower=X2.5., upper=X97.5.)
    
    list(cellmeans=cellmeans,ndat=ndat,ndat.p=ndat.p,
         cellmeans.sector=cellmeans.sector, ndat.sector=ndat.sector,ndat.p.sector=ndat.p.sector,
         cellmeans.zone=cellmeans.zone,ndat.zone=ndat.zone,ndat.p.zone=ndat.p.zone)
}


MPA_sectorPlot <- function(dat, ytitle) {
    p <-ggplot(dat, aes(y=Median, x=Sector, fill=Zone, shape=Zone)) +
        #geom_line()+
        geom_errorbar(aes(ymin=lower, ymax=upper),width=0.01, linetype=1,position=position_dodge(width=0.1))+
        geom_linerange(aes(ymin=lower.1, ymax=upper.1), size=1, position=position_dodge(width=0.1)) +
            geom_point(position=position_dodge(width=0.1))+
                scale_fill_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c('black','white'))+
                    scale_shape_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(21,21))+
                        scale_linetype_manual('Zone', breaks=c('Closed','Open'), labels=c('Reserve','Non-reserve'),values=c(1,2))+
                            #scale_y_continuous(expression(paste(Trout~biomass~"(per 1000", m^2, ")", sep="")), labels=comma)+
                                scale_y_continuous(ytitle, labels=comma)+
                                        #scale_x_continuous('', breaks=2006:2014)+
                                theme_classic()
    p<-p+theme(legend.position=c(1,1), legend.justification=c(1,1), strip.background=element_blank(),strip.text.x=element_text(size=12),
               axis.title.y=element_text(vjust=1.5), panel.spacing=unit(1,unit='lines'), axis.text.x=element_text(size=10),
               axis.line.y=element_line(), axis.line.x=element_line())
    print(p)
}


MPA_makePriors <- function(cellmeans, data, link='log') {
    link=eval(parse(text=link))
    priors=list()
                                        #priors$intercept=c(mu=log(cellmeans[1,'Mean']), sd=log(2*cellmeans[1,'upper']-cellmeans[1,'lower']))
    #priors$intercept=c(mu=round(log(median(data$Value)),2), sd=round(log(sd(data$Value)),2))
    priors$intercept=c(round(link(median(data$Value)),2), abs(round(link(sd(data$Value)),2)))

    if (length(unique(data$Sector))>1) Xmat = model.matrix(~Sector*cYear*Zone, data=cellmeans)
    if (length(unique(data$Sector))==1) Xmat = model.matrix(~cYear*Zone, data=cellmeans)
    coefs = cellmeans$Mean
    #b<-solve(t(X)%*%X)%*%t(X)%*%y
    #OR
    b<-solve(crossprod(Xmat), crossprod(Xmat,coefs))
    priors$b = c(0, abs(round((sd(b[-1])),2)))
    priors
}


MPA_stan <- function(dat, cellmeans,family='zero_inflated_negbinomial') {
   
    if (grepl('*.link=logit.*',family)) {dat1 = dat %>% mutate(Value=Value/100)
    }else if (grepl('^Gamma.*',family)) {dat1 = dat %>% mutate(Value=Value)
    }else dat1 = dat %>% mutate(Value=as.integer(Value))

    if (grepl('Beta',family)) dat1 = dat1 %>% mutate(Value=ifelse(Value==0,0.01,Value))

    link=gsub('.*link=(.*)\\)','\\1',family)
    priors = MPA_makePriors(cellmeans,data=dat1, link=link)

    prior =  prior=c(prior_string(paste0("normal(",paste(priors$intercept, collapse=','),")"), class='Intercept'),
                     prior_string(paste0("normal(",paste(priors$b, collapse=','),")"), class='b'),
                     prior(cauchy(0,5), class='sd')
                     )
    if (grepl('(^zero_inflated_negbinomial.*|^negbinomial)',family)) {
        prior = c(prior,
                  prior(gamma(0.01,0.01), class='shape')
                  )
    }
    if (grepl('^zero_inflated_negbinomial.*',family)) {
        prior=c(prior,prior(beta(1,1), class='zi'))
    }
    if (length(unique(dat1$Sector))>1) form = Value~Sector*cYear*Zone+(1|Pair) + (1|Reef) + (1|Site)
    if (length(unique(dat1$Sector))==1) form = Value~cYear*Zone+(1|Pair) + (1|Reef) + (1|Site)
    print(form)
    if (grepl('^Beta.*',family)) {
        dat.stan = brm(form, data=dat1,
                       family=Beta(link='logit'), iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    } else if (grepl('^Gamma.*',family)) {
                dat.stan = brm(form, data=dat1,
                       family=Gamma(link='log'), iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    } else if (grepl('^zero_inflated_negbinomial.*',family)) {
       dat.stan = brm(form, data=dat1,
                       family='zero_inflated_negbinomial', iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    } else {
        dat.stan = brm(form, data=dat1,
                       family='negbinomial', iter = 2000, warmup = 1000, thin=3, chains=3,
                       prior=prior)
    }
    
    dat.stan
}

MPA_cellmeans_stan <- function(dat.stan) {
    coefs = fixef(dat.stan, summary=FALSE)
    ##Cellmeans
    dat <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat$Sector))>1) dat <- cbind(dat, model.matrix(~Sector*cYear*Zone, data=dat))
    if (length(unique(dat$Sector))==1) dat <- cbind(dat, model.matrix(~cYear*Zone, data=dat))
    #xmat<- as.matrix(plyr::ddply(dat,~Sector*cYear*Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat %>% group_by(Sector,cYear,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-cYear,-Zone) %>% as.matrix
    if (dat.stan$family$link=='log') cellmeans.mcmc<-exp(coefs %*% t(xmat))
    if (dat.stan$family$link=='logit') cellmeans.mcmc<-invlogit(coefs %*% t(xmat))*100
    if (dat.stan$family$link=='identity') cellmeans.mcmc<-coefs %*% t(xmat)
    
    #if(trans=='exp') cellmeans.mcmc<-exp(coefs %*% t(xmat))
    #if(trans=='logit') cellmeans.mcmc<-invlogit(coefs %*% t(xmat))
    #if(trans=='gaussian') cellmeans.mcmc<-(coefs %*% t(xmat))
    dat.cellmeans <- plyr::adply(cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })
    
    #aa<-plyr::ddply(dat,~Sector+cYear+Zone, plyr::numcolwise(mean))
    aa = dat %>% group_by(Sector,cYear,Zone) %>% summarize_if(is.numeric, mean) %>% as.data.frame
    
    dat <- cbind(aa[,1:3],dat.cellmeans)
    if (length(unique(dat$Sector)) > 1) dat$Sector <- factor(dat$Sector, labels=c('Cairns','Townsville','Pompeys','Swains','Cap-Bunkers'))
    dat$dcYear <- as.Date(paste(dat$cYear,'-01-01',sep=''))#as.numeric(as.character(dat$cYear))

    ## Closed higher than open
    co.mcmc <- cellmeans.mcmc[,seq(1,ncol(cellmeans.mcmc),by=2)] - cellmeans.mcmc[,seq(2,ncol(cellmeans.mcmc),by=2)]
    co.dat <- apply(co.mcmc,2,function(x) {
        length(x[x>0])/length(x)
    })
    co.dat <- cbind(expand.grid(cYear=seq(min(year(dat$dcYear)),max(year(dat$dcYear)),by=2),Sector=c("CAIN","TO","PO","SW","CB")), Prob=co.dat)

    ## Effects sizes
    co.effect <- plyr::adply(co.mcmc,2,function(x){data.frame(Mean=mean(x), HPDinterval(as.mcmc(x)),t(quantile(x,p=c(0.025,0.975))))})
    co.effect <- cbind(expand.grid(cYear=seq(min(year(dat$dcYear)),max(year(dat$dcYear)),by=2),Sector=c("CAIN","TO","PO","SW","CB")), co.effect)
    
    ## Means per Sector
    dat.s <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat.s$Sector)) > 1) dat.s <- cbind(dat.s, model.matrix(~Sector*cYear*Zone, data=dat.s))
    if (length(unique(dat.s$Sector))== 1) dat.s <- cbind(dat.s, model.matrix(~cYear*Zone, data=dat.s))
    #xmat<- as.matrix(plyr::ddply(dat.s,~Sector*Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat.s %>% group_by(Sector,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-Zone,-cYear) %>% as.matrix
    #if(trans=='exp') sector.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    #if(trans=='logit') sector.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))
                                        #if(trans=='gaussian') sector.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat))
    if (dat.stan$family$link=='log') sector.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    if (dat.stan$family$link=='logit')sector.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))*100
    if (dat.stan$family$link=='identity')sector.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat)))
    sector.cellmeans <- plyr::adply(sector.cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })
    facts <- expand.grid(Zone=levels(data$Zone),Sector=levels(data$Sector))
    dat.s <- cbind(facts,sector.cellmeans)

    ## Effects per sector
    if (length(unique(dat.s$Sector))>1) {
        co.mcmc.s <- sector.cellmeans.mcmc[,c(1,3,5,7,9)] - sector.cellmeans.mcmc[,c(2,4,6,8,10)]
        co.effect.s <- plyr::adply(co.mcmc.s,2,function(x){data.frame(Mean=mean(x), HPDinterval(as.mcmc(x)),t(quantile(x,p=c(0.025,0.975))))})
        co.effect.s <- cbind(expand.grid(Sector=c("CAIN","TO","PO","SW","CB")), co.effect.s)
    
    ## Probabilities Closed higher than open per sector
    co.dat.s <- apply(co.mcmc.s,2,function(x) {
        length(x[x>0])/length(x)
    })
    co.dat.s <- cbind(expand.grid(Sector=c("CAIN","TO","PO","SW","CB")), Prob=co.dat.s)
    } else {
        co.dat.s = NULL
        co.effect.s=NULL
    }
    
    ## Overall closed higher than open probabilities
    co.mcmc1 <- as.vector(cellmeans.mcmc[,seq(1,ncol(cellmeans.mcmc),by=2)]) - as.vector(cellmeans.mcmc[,seq(2,ncol(cellmeans.mcmc),by=2)])
    co <- length(co.mcmc1[co.mcmc1>0])/length(co.mcmc1)

    ## Overall means
    dat.o <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
    if (length(unique(dat.o$Sector))>1) dat.o <- cbind(dat.o, model.matrix(~Sector*cYear*Zone, data=dat.o))
    if (length(unique(dat.o$Sector))==1) dat.o <- cbind(dat.o, model.matrix(~cYear*Zone, data=dat.o))
    #xmat<- as.matrix(plyr::ddply(dat.o,~Zone,plyr::colwise(mean))[,-1:-3])
    xmat = dat.o %>% group_by(Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-Zone,-cYear) %>% as.matrix
    if(dat.stan$family$link=='log') overall.cellmeans.mcmc<-exp(coefs %*% t(as.matrix(xmat)))
    if(dat.stan$family$link=='logit') overall.cellmeans.mcmc<-invlogit(coefs %*% t(as.matrix(xmat)))*100
    if(dat.stan$family$link=='identity') overall.cellmeans.mcmc<-(coefs %*% t(as.matrix(xmat)))
    ## overall.cellmeans <- adply(overall.cellmeans.mcmc,2,function(x){
    ##     data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
    ##                t(ci(as.mcmc(x))))
    ## })
    overall.cellmeans <- plyr::adply(overall.cellmeans.mcmc,2,function(x){
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.50),
                   t(ci(x)))
    })
    facts.o <- expand.grid(Zone=levels(data$Zone))
    dat.o <- cbind(facts.o,overall.cellmeans)

    ## Overall effects
    MCMCsum <- function(x) {
        data.frame(Median=median(x), HPDinterval(as.mcmc(x)),HPDinterval(as.mcmc(x),prob=0.5),t(quantile(x,p=c(0.025,0.975))))
    }
    o.mcmc <- overall.cellmeans.mcmc[,c(1)] - overall.cellmeans.mcmc[,c(2)]
    o.effect <-MCMCsum(o.mcmc)
    o.effect <- rbind(o.effect, MCMCsum(overall.cellmeans.mcmc[,c(1)] - overall.cellmeans.mcmc[,c(2)])/(overall.cellmeans.mcmc[,c(2)]))
    rownames(o.effect) <- c('Difference', 'Percent difference')

    list(cellmeans=dat,
         Closed_VS_Open_effect=co.effect,
         Closed_VS_Open_prob=co.dat,
         SectorZoneMeans=dat.s,
         Sector_Closed_VS_Open_effects=co.effect.s,
         Sector_Closed_VS_Open_prob=co.dat.s,
         OverallZoneMeans=dat.o,
         Overall_Closed_VS_Open_effect.effect=o.effect,
         Overall_Closed_VS_Open_prob=co
         )
}
