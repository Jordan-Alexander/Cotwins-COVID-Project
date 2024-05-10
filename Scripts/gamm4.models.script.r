library(gamm4)
library(data.table)

dat = fread('mobility.affect.covid.1.26.22', data.table = F)


dat$case.count.time.interaction = dat$cases.weekly.per.100k * dat$days.post.covid

numpts.gamm <- gamm4(numpts ~ sex + age + season + wknd + pop.density+ OS + 
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(numpts.gamm, 'gamm4.pts.1.26.23.rds')

dist.gamm <- gamm4(distance ~ sex + age + season + wknd + pop.density+ OS +
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(dist.gamm, 'gamm4.dist.1.26.23.rds')

pa.av.gamm <- gamm4(pa.av ~ sex + age + season + wknd + pop.density+ OS +
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(pa.av.gamm, 'gamm4.pa.av.1.26.23.rds')

na.av.gamm <- gamm4(na.av ~ sex + age + season + wknd + pop.density+ OS +
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(pa.av.gamm, 'gamm4.na.av.1.26.23.rds')


pa.av.mobility.gamm <- gamm4(pa.av ~ sex + age + season + wknd + pop.density+ OS + 
                                  numpts + distance +
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(pa.av.mobility.gamm, 'gamm4.pa.av.mobility.1.26.23.rds')


na.av.mobility.gamm <- gamm4(na.av ~ sex + age + season + wknd + pop.density+ OS + 
                                  numpts + distance +
                     s(days.post.covid, k = 10) + s(cases.weekly.per.100k, k = 10), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(na.av.mobility.gamm, 'gamm4.na.av.mobility.1.26.23.rds')
