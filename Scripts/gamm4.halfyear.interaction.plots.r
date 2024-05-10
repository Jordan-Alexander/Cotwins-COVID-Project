# case by covid interaction models, categorical

library(gamm4)
library(lme4)
library(data.table)
library(ggplot2)
library(dplyr)

dat = fread('mobility.affect.covid.post.Jan.2020.csv', data.table = F)
nrow(dat)
length(unique(dat$user_id))

get.gamm.plot = function(mod,xvar){
          outcome_pred <- predict(mod$gam, se.fit = T)
          
          outcome_plot_data = tibble(
               x = mod$gam$model[,xvar],
               outcome = outcome_pred$fit,
               lower = outcome_pred$fit - 1.96 * outcome_pred$se.fit,
               upper = outcome_pred$fit + 1.96 * outcome_pred$se.fit)
          
          
          outcome_plot = ggplot(outcome_plot_data, aes(x, outcome, color = halfyear)) +
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


if(x == 'pa.av'|x == 'na.av'){

outcome.gamm <- gamm4(outcome~s(cases.weekly.per.100k,by = halfyear, k = 35), 
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'.gamm4.cases.halfyear.interaction.3.3.23.rds'))

plot = get.gamm.plot(outcome.gamm,'cases.weekly.per.100k')
ggsave(paste0(x,'.gamm4.halfyear.interaction.3.plot.png'),plot, device = 'png')}




if(x == 'numpts'|x == 'distance'){

outcome.gamm <- gamm4(outcome~s(cases.weekly.per.100k,by = halfyear, k = 35),
		     family = poisson,  
                     random = ~(1 | family/SVID),  data = dat)

saveRDS(outcome.gamm, paste0(x,'.gamm4.cases.halfyear.interaction.3.3.23.rds'))

plot = get.gamm.plot(outcome.gamm,'cases.weekly.per.100k')
ggsave(paste0(x,'.gamm4.halfyear.interaction.3.plot.png'),plot, device = 'png')}

