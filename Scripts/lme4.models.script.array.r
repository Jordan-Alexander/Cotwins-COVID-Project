library(lme4)
library(data.table)

dat = fread('mobility.affect.covid.1.26.22.csv', data.table = F)
dat$case.count.time.interaction = dat$cases.weekly.per.100k * dat$days.post.covid
dat$log_cases = log(dat$cases.weekly.per.100k + 1)
#build array
args <- commandArgs(trailingOnly = TRUE)
args<-as.integer(args)

index = args[1]

outcomes = c('numpts','distance','pa.av','na.av')
x = outcomes[index]
col.index = grep(x, colnames(dat))
names(dat)[col.index] = 'outcome'

# covid models

# cases only, not log transformed
outcome.lme1 <- lmer(outcome ~ sex + age + OS +
                           season + wknd +
                           pop.density + 
                           cases.weekly.per.100k+
                           (1|family/SVID),
                      data=dat)

saveRDS(outcome.lme1, file = paste0(index,'.cases.lme.1.27.23.rds'))

# cases only, log transformed

outcome.lme2 <- lmer(outcome ~ sex + age + OS +
                           season + wknd +
                           pop.density + 
                           log_cases +
                           (1|family/SVID),
                      data=dat)

saveRDS(outcome.lme2, file = paste0(index,'.log.cases.lme.1.27.23.rds'))


# cases and time, no interaction
outcome.lme3 <- lmer(outcome ~ sex + age + OS +
                           season + wknd +
                           pop.density + 
                           cases.weekly.per.100k+
                           days.post.covid +
                           (1|family/SVID),
                      data=dat)

saveRDS(outcome.lme3, file = paste0(index,'.cases.time.lme.1.27.23.rds'))

# log cases and time, no interaction

outcome.lme4 <- lmer(outcome ~ sex + age + OS +
                           season + wknd +
                           pop.density + 
                           log_cases+
                           days.post.covid +
                           (1|family/SVID),
                      data=dat)

saveRDS(outcome.lme4, file = paste0(index,'.log.cases.time.lme.1.27.23.rds'))


# cases and time with interaction

outcome.lme5 <- lmer(outcome ~ sex + age + OS +
                           season + wknd +
                           pop.density + 
                           cases.weekly.per.100k * days.post.covid+
                           (1|family/SVID),
                      data=dat)

saveRDS(outcome.lme5, file = paste0(index,'.cases.time.interact.lme.1.27.23.rds'))

# log cases and time with interaction

outcome.lme6 <- lmer(outcome ~ sex + age + OS +
                           season + wknd +
                           pop.density + 
                           log_cases * days.post.covid+
                           (1|family/SVID),
                      data=dat)

saveRDS(outcome.lme6, file = paste0(index,'.log.cases.time.interact.lme.1.27.23.rds'))