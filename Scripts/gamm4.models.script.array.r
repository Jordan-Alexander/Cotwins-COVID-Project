library(gamm4)
library(data.table)

dat = fread('mobility.affect.covid.1.26.22.csv', data.table = F)
dat$case.count.time.interaction = dat$cases.weekly.per.100k * dat$days.post.covid

#build array
args <- commandArgs(trailingOnly = TRUE)
args<-as.integer(args)

index = args[1]

outcomes = c('numpts','distance','pa.av','na.av')
x = outcomes[index]
col.index = grep(x, colnames(dat))
names(dat)[col.index] = 'outcome'


outcome.gamm <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS + 
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0('gamm4.',x,'.1.26.23.rds'))


if(x == 'pa.av'| x == 'na.av'){
     outcome.gamm2 <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS +
                                 numpts + distance +
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)
     saveRDS(outcome.gamm2, paste0('gamm4.',x,'.mobility.1.26.23.rds'))}


