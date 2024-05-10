library(gamm4)
library(data.table)

dat = fread('datasets/mobility.affect.covid.missingness.11.3.23.csv',data.table = F)

# convert character variables back to factor
dat$OS[dat$OS == ''] = NA
dat$OS = factor(dat$OS)
print(nrow(dat))
print(length(unique(dat$SVID)))

dat$sex[dat$sex == ''] = NA
dat$sex = factor(dat$sex)


#build array
args <- commandArgs(trailingOnly = TRUE)
args<-as.integer(args)

index = args[1]

outcomes = data.frame(pheno = c(rep(c('numpts','distance','pa.av','na.av'),times = 2)),
		      smooth = c(rep('date', times = 4),rep('cases',times = 4)))
x = outcomes[index,1]
smooth = outcomes[index,2]
col.index = grep(x, colnames(dat))
names(dat)[col.index] = 'outcome'


if(x == 'numpts' | x == 'distance' & smooth == 'date'){
outcome.gamm <- gamm4(outcome ~ sex + relative.age + wknd + OS + loc.prop.miss + pa.prop.miss +
                     s(days.post.covid, k = 35), 
                     random = ~(1 | family/SVID),  data = dat)
	

saveRDS(outcome.gamm, paste0('models/',x,'gamm4.days','.11.8.23.rds'))
	}

if(x == 'numpts' | x == 'distance' & smooth == 'cases'){
outcome.gamm <- gamm4(outcome ~ sex + relative.age + wknd + OS + loc.prop.miss + pa.prop.miss + 
                     s(weekly.cases.per.100k, k = 10),
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('models/',x,'gamm4.cases','11.8.23.rds'))
	}


if(x == 'pa.av' | x == 'na.av'& smooth == 'date'){

outcome.gamm <- gamm4(outcome ~ sex + relative.age + OS + loc.prop.miss + pa.prop.miss + 
                     s(days.post.covid, k = 35),
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('models/',x,'.gamm4.days','.11.8.23.rds'))
	}

if(x == 'pa.av' | x == 'na.av'& smooth == 'cases'){
outcome.gamm <- gamm4(outcome ~ sex + relative.age + OS + loc.prop.miss + pa.prop.miss +
                     s(weekly.cases.per.100k, k = 11), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('models/',x,'.gamm4.cases','.11.8.23.rds'))
}


