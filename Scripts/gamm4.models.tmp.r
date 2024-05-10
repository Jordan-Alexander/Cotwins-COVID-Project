library(gamm4)
library(data.table)

dat = fread('mobility.affect.covid.2.21.csv', data.table = F)
print(nrow(dat))
print(length(unique(dat$user_id)))

#build array
args <- commandArgs(trailingOnly = TRUE)
args<-as.integer(args)

index = args[1]

outcomes = c('numpts','distance')

#,'pa.av','na.av')

x = outcomes[index]
col.index = grep(x, colnames(dat))
names(dat)[col.index] = 'outcome'


# if(x == 'numpts' | x == 'distance'){
# outcome.gamm <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS + 
#                      s(days.post.covid, k = 35), 
#                      random = ~(1 | family/SVID),  data = dat)
# 
# saveRDS(outcome.gamm, paste0(x,'gamm4.days','.3.21.23.rds'))
# 
# outcome.gamm <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS + 
#                      s(cases.weekly.per.100k, k = 10), 
#                      random = ~(1 | family/SVID),  data = dat)
# 
# saveRDS(outcome.gamm, paste0(x,'gamm4.cases','.3.21.23.rds'))


}


if(x == 'pa.av' | x == 'na.av'){

outcome.gamm <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS + 
                     s(days.post.covid, k = 35), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'gamm4.days','.3.21.23.rds'))

# outcome.gamm <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS + 
#                      s(cases.weekly.per.100k, k = 10), 
#                      random = ~(1 | family/SVID),  data = dat)
# 
# saveRDS(outcome.gamm, paste0(x,'gamm4.cases','.3.21.23.rds'))


# 
#      outcome.gamm2 <- gamm4(outcome ~ sex + age + season + wknd + pop.density+ OS +
#                                  numpts + distance +
#                      s(days.post.covid, k = 35) + s(cases.weekly.per.100k, k = 10), 
#                      random = ~(1 | family/SVID),  data = dat)
#  
#     saveRDS(outcome.gamm2, paste0(x,'gamm4.','.mobility.3.21.23.rds'))
    }


