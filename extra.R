## The idea of this analysis is to run the trout biomass model with random intercept/slope
## for reef such that we can predict trout biomass for each reef/year.
## Mike needs this to use in an analysis that looks at the influence of distance to the reef
## to see if the difference in Zoning effect size is related to non-compliance (as measured by
## distance needed to travel to get to the reef)..
load(file='data/reports.data.RData')
data = data %>% mutate(Time=Year-min(Year)) %>% filter(cYear!=2017) %>% droplevels
dat = data %>% filter(Group=="Coral Trout Biomass")

cellmeans<-MPA_rawMeans(dat)

form = Value~Sector*cYear*Zone+(1|Pair) + (cYear|Reef) + (1|Site)
dat.stan = brm(form, data=dat1,
                        family='zero_inflated_negbinomial', iter = 2000, warmup = 1000, thin=3, chains=3,
               prior=prior)

coefs = fixef(dat.stan, summary=TRUE)[,1]
coefs.r = ranef(dat.stan, summary=TRUE)$Reef

dat <- expand.grid(Sector=levels(data$Sector), Zone=levels(data$Zone), cYear=levels(data$cYear))
if (length(unique(dat$Sector))>1) dat <- cbind(dat, model.matrix(~Sector*cYear*Zone, data=dat))
if (length(unique(dat$Sector))==1) dat <- cbind(dat, model.matrix(~cYear*Zone, data=dat))
                                        #xmat<- as.matrix(plyr::ddply(dat,~Sector*cYear*Zone,plyr::colwise(mean))[,-1:-3])
xmat = dat %>% group_by(Sector,cYear,Zone) %>% summarize_all(mean) %>% ungroup %>% dplyr::select(-Sector,-cYear,-Zone) %>% as.matrix
if (dat.stan$family$link=='log') cellmeans.mcmc<-exp(coefs %*% t(xmat))
if (dat.stan$family$link=='logit') cellmeans.mcmc<-invlogit(coefs %*% t(xmat))*100
if (dat.stan$family$link=='identity') cellmeans.mcmc<-coefs %*% t(xmat)

newdata = dat1 %>% dplyr::select(Sector, SHELF, Pair, Reef, Zone, cYear) %>% distinct
newdata=cbind(newdata,predict(dat.stan, re_formula=~(cYear|Reef), newdata=newdata))
cellmeans

#aa %>% filter(cYear==2006, Sector=='Cairns', Zone=='Open')


newdata %>% #filter(Sector=='Cairns', SHELF=='M') %>%
    ggplot(aes(y=Estimate, x=cYear, group=Reef, color=Zone)) + geom_line() + facet_grid(Sector~SHELF, scales='free')

newdata %>% #filter(Sector=='Cairns', SHELF=='M') %>%
    ggplot(aes(y=Estimate, x=cYear, group=Reef, color=Pair)) + geom_line() + facet_grid(Sector~SHELF, scales='free')

save(newdata, file='data/newdata.RData')
