setwd("C:/Users/alexa877/Documents/UMN Courses/Research/Covid Location project/RAship 2022/Datasets")
library(gamm4)
library(lme4)
library(data.table)
library(ggplot2)
library(dplyr)

dat = fread('staypoints.covid.11.16.22.csv', data.table = F)
dat$case.count.time.interaction = dat$cases.weekly.per.100k * dat$days.post.covid

get.gamm.plot = function(mod,xvar){
          outcome_pred <- predict(mod$gam, se.fit = T)
          
          outcome_plot_data = tibble(
               x = mod$gam$model[,xvar],
               outcome = outcome_pred$fit,
               lower = outcome_pred$fit - 1.96 * outcome_pred$se.fit,
               upper = outcome_pred$fit + 1.96 * outcome_pred$se.fit)
          
          
          outcome_plot = ggplot(outcome_plot_data, aes(x, outcome)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2, outline.type = 'full') +
               geom_line()
          
          return(outcome_plot)}
               
     
           
     


#build array
args <- commandArgs(trailingOnly = TRUE)
args<-as.integer(args)

index = args[1]

outcomes = c('numpts','distance','pa.av','na.av')
x = outcomes[index]
col.index = grep(x, colnames(dat))
names(dat)[col.index] = 'outcome'

users = unique(dat$user_id)

outcome.gamm <- gamm4(outcome~s(days.post.covid, k = 30), 
                        random = ~(1 | family/SVID),
                      data = dat)

saveRDS(outcome.gamm, paste0(x,'.gamm4.simple.days.1.26.23.rds'))

plot = get.gamm.plot(outcome.gamm,'days.post.covid')
ggsave(paste0(x,'.gamm4.simple.days.plot.png'),plot, device = 'png')


outcome.gamm <- gamm4(outcome~s(cases.weekly.per.100k, k = 30), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'.gamm4.simple.cases.1.26.23.rds'))

plot = get.gamm.plot(outcome.gamm,'cases.weekly.per.100k')
ggsave(paste0(x,'.gamm4.simple.cases.plot.png'),plot, device = 'png')



outcome.gamm <- gamm4(outcome~s(cases.weekly.per.100k, k = 30) + s(days.post.covid, k = 30), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'.gamm4.simple.cases.days.1.26.23.rds'))



outcome.gamm <- gamm4(outcome~s(cases.weekly.per.100k, k = 30) + s(days.post.covid, k = 30) + 
                      s(case.count.time.interaction, k = 30), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'.gamm4.allsmooths.1.26.23.rds'))

plot = get.gamm.plot(outcome.gamm,'cases.weekly.per.100k')
ggsave(paste0(x,'.gamm4.allsmooths.cases.plot.png'),plot, device = 'png')

plot = get.gamm.plot(outcome.gamm,'days.post.covid')
ggsave(paste0(x,'.gamm4.allsmooths.days.plot.png'),plot, device = 'png')

plot = get.gamm.plot(outcome.gamm,'case.count.time.interaction')
ggsave(paste0(x,'.gamm4.allsmooths.cases.days.interaction.plot.png'),plot, device = 'png')

