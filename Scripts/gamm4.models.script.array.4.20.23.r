library(gamm4)
library(data.table)

dat = fread('datasets/mobility.affect.covid.5.19.23.csv',data.table = F)

# convert character variables back to factor
dat$OS.version[dat$OS.version == ''] = NA
dat$OS.version = factor(dat$OS.version)
print(nrow(dat))
print(length(unique(dat$user_id)))

dat$sex[dat$sex == ''] = NA
dat$sex = factor(dat$sex)

# natural log scale population density

dat$pop.density.scaled = log(dat$pop.density)

#build array
args <- commandArgs(trailingOnly = TRUE)
args<-as.integer(args)

index = args[1]

outcomes = c('numpts','distance','pa.av','na.av')
x = outcomes[index]
col.index = grep(x, colnames(dat))
names(dat)[col.index] = 'outcome'


if(x == 'numpts' | x == 'distance'){
outcome.gamm <- gamm4(outcome ~ sex + age + month + wknd + pop.density.scaled+ OS.version + 
                     s(days.post.covid, k = 35), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'gamm4.days','.5.31.23.rds'))

outcome.gamm <- gamm4(outcome ~ sex + age + month + wknd + pop.density.scaled+ OS.version + 
                     s(cases.weekly.per.100k, k = 20), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('models/',x,'gamm4.cases','.5.31.23.rds'))


}


if(x == 'pa.av' | x == 'na.av'){

outcome.gamm <- gamm4(outcome ~ sex + age + month + pop.density.scaled+ OS.version + 
                     s(days.post.covid, k = 35), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('models/',x,'.gamm4.days','.5.31.23.rds'))

outcome.gamm <- gamm4(outcome ~ sex + age + month + pop.density.scaled+ OS.version + 
                     s(cases.weekly.per.100k, k = 20), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('models/',x,'.gamm4.cases','.5.31.23.rds'))
}


