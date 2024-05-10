# set directory ----
setwd("/panfs/jay/groups/1/vrie0006/alexa877/CovidLocationProject/RAship.2022")
# load packages ----
# create effects plot for GAMM4 data
library(gratia)
library(ggeffects)
library(lubridate)
library(dplyr)
library(gamm4)
library(mgcv)
library(ggplot2)
library(data.table)
library(cowplot)
library(sjPlot)
library(htmltools)

# create covid plot to add to date plots


# smooth national covid cases using past week average
# moving average function ----
moving.avg = function(vec, nlag, nas.as.0s = T){
  tmp = shift(vec,c(0:(nlag-1)))
  
  df = matrix(nrow = length(tmp[[1]]), ncol = nlag)
  for(i in 1:nlag){
    df[,i] = tmp[[i]]
  }
  if(nas.as.0s == T){
    df[which(is.na(df)==T)] = 0
  }
  df = as.data.frame(df)
  df$V8 = rowMeans(df)
  return(df$V8)
}
# prepare covid data for plotting ----
covid = fread('datasets/covid_daily_time_series_8_12_US_longform.csv')
head(covid)

covid.natl = covid %>% group_by(date) %>% 
  summarise(cases = sum(cases)) %>%
  mutate(dailycases = cases - lag(cases)) %>%
  mutate(weekly.avg.cases = moving.avg(dailycases,7),
         date = ymd(date))%>%
  mutate(days.post.covid = as.numeric(date - ymd('2020-01-22')))



# input model, name of smooth term, number of points to evaluate derivative of smooth function
# output plot of smooth and its first derivative
# smooth plots function ----




get.smooths.plot.date = function(model, 
                                 smooth,
                                 pheno,
                                 color,
                                 deriv_pts = 1000){
  # input model, name of smooth term, number of points to evaluate derivative of smooth function
  # output plot of smooth and its first derivative
  
  # create derivative plot data ----
  model.pred = derivatives(model,
                           term = smooth,
                           level = 0.99,
                           n = deriv_pts)
  model.pred$sig = NA
  model.pred$sig[model.pred$lower >0] = 'positive'
  model.pred$sig[model.pred$upper <0] = 'negative'
  model.pred$sig[model.pred$upper >=0 & model.pred$lower <= 0] = 'n.s.'
  model.pred$sig = factor(model.pred$sig, levels = c('n.s.','negative','positive'),
                          labels = c('Slope is not significant (p>0.01)',
                                     'Slope is negative (p<0.01)',
                                     'Slope is positive (p<0.01)'))
  # function to plot date smooths ----
  
  # matching dates with main plot data
  model.pred = model.pred[model.pred$data >= min(model$model$days.post.covid),]
  model.pred = model.pred[model.pred$data <= max(model$model$days.post.covid),]
  model.pred$date = as_date(model.pred$data, 
                            origin = ymd('2020-01-26'))
  model.pred$date = round(model.pred$date, digits = 0)
  
  
  # create residualized plot data (date) ----
  model.main = ggpredict(model, terms = 'days.post.covid')
  
  model.main = model.main[model.main$x >= min(model$model$days.post.covid),]
  model.main = model.main[model.main$x <= max(model$model$days.post.covid),]
  model.main$date = as_date(model.main$x, 
                            origin = ymd('2020-01-26'))
  model.main$date = ymd(model.main$date)
  model.pred$date = ymd(model.pred$date)
  
  sig = left_join(model.main, model.pred[,c("date","sig")])
  sig = sig[is.na(sig$sig)==F,]
  
  
  # create plots (date) ----
  xmin = min(model.pred$date)
  xmax = max(model.pred$date)
  ymax.d = max(model.pred$upper) + 0.25*sd(model.pred$derivative)
  ymin.d = min(model.pred$lower) - 0.25*sd(model.pred$derivative)
  
  ymax.m = max(model.main$conf.high) + 0.25*sd(model.main$predicted)
  ymin.m = min(model.main$conf.low) - 0.25*sd(model.main$predicted)
  
  # rescale covid data to plot with both plots
  covid.natl = subset(covid.natl, date <= xmax)
  scale.deriv = (ymax.d-ymin.d)/(1.25*max(covid.natl$weekly.avg.cases))
  scale.main = (ymax.m-ymin.m)/(1.25*max(covid.natl$weekly.avg.cases))
  covid.rescale = covid.natl %>%
    mutate(cases.scaled.deriv = weekly.avg.cases*scale.deriv + ymin.d,
           cases.scaled.main = weekly.avg.cases*scale.main + ymin.m,
           date = as_date(days.post.covid, origin = '2020-01-22')) %>%
    subset(date <= xmax)
  
  
  
  
  plot.deriv = 
    ggplot(NULL, aes(x = date)) +
    geom_line(data = model.pred, 
              aes(y = derivative),color = 'black')+
    geom_ribbon(data = model.pred, 
                aes(ymin = lower, ymax = upper),
                fill = color, 
                alpha = .2)  +
    geom_line(data = covid.rescale, 
              aes(y = cases.scaled.deriv), 
              color = 'black',
              alpha = 0.2)+
    geom_ribbon(data = covid.rescale, 
                aes(ymin = ymin.d, ymax = cases.scaled.deriv),
                fill = 'black', 
                alpha = .3) +
    
    #geom_abline(intercept = 0, slope = 0, color = 'black') +
    geom_vline(xintercept = ymd('2020-01-22'), color = 'red') +
    geom_vline(xintercept = ymd('2020-03-11'), color = 'blue') +
    xlab('date')+
    ylab(pheno) +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 limits = c(xmin,xmax))+
    ylim(ymin.d,ymax.d)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plot.model = 
    ggplot(NULL, aes(x = date)) +
    geom_line(data = sig, 
              aes(y = predicted),color = 'black')+
    
    geom_ribbon(data = sig, 
                aes(ymin = conf.low, ymax = conf.high),
                fill = color, 
                alpha = .3)+
    geom_line(data = covid.rescale, 
              aes(y = cases.scaled.main), 
              color = 'black',
              alpha = 0.2)+
    geom_ribbon(data = covid.rescale, 
                aes(ymin = ymin.m, ymax = cases.scaled.main),
                fill = 'black', 
                alpha = .3) +
    geom_vline(xintercept = ymd('2020-01-22'), color = 'red') +
    geom_vline(xintercept = ymd('2020-03-11'), color = 'blue') +
    xlab('date')+
    ylab(pheno) +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 limits = c(xmin,xmax))+
    ylim(ymin.m,ymax.m)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  plot.model = plot.model + geom_point(data = sig,
                                       aes(x =date,
                                           y = predicted,
                                           color = sig),
                                       size = 0.5
  )+
    scale_color_manual(
      values = c('gray','red','blue'))+
    theme(legend.position = 'bottom',
          legend.key = element_rect(fill =NA,color = NA))+
    labs(color = '')
  
  
  plot = cowplot::plot_grid(plot.model,plot.deriv,ncol = 1,
                            rel_heights = c(0.6,0.4))
  
  return(plot.model)
}





get.smooths.plot.cases = function(model, 
                                  smooth,
                                  pheno,
                                  color,
                                  deriv_pts = 1000){
  # input model, name of smooth term, number of points to evaluate derivative of smooth function
  # output plot of smooth and its first derivative
  
  # create derivative plot data ----
  model.pred = derivatives(model,
                           term = smooth,
                           level = 0.99,
                           n = deriv_pts)
  model.pred$sig = NA
  model.pred$sig[model.pred$lower >0] = 'positive'
  model.pred$sig[model.pred$upper <0] = 'negative'
  model.pred$sig[model.pred$upper >=0 & model.pred$lower <= 0] = 'n.s.'
  model.pred$sig = factor(model.pred$sig, levels = c('n.s.','negative','positive'),
                          labels = c('Slope is not significant (p>0.01)',
                                     'Slope is negative (p<0.01)',
                                     'Slope is positive (p<0.01)'))
  # function for case count smooths ----
  
  
  model.main = ggpredict(model, terms = 'cases.weekly.per.100k')
  model.main = model.main[model.main$x <=2000,]    
  model.pred = model.pred[model.pred$data <= 2000,]
  
  sig = model.main %>% mutate(cases = round(x, digits = 0))
  model.pred$cases = round(model.pred$data, digits = 0)
  sig = left_join(sig,model.pred[,c('cases','sig')])
  
  sig = sig[is.na(sig$sig)==F,]
  
  plot.deriv = 
    ggplot(model.pred, aes(x = data,y = derivative)) +
    geom_line(color = 'black')+
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = color, 
                alpha = .2)+
    ylab('f\'(x)')+
    xlab('Weekly county-level cases per 100,000')
  
  plot.model = 
    ggplot(model.main, aes(x = x,y = predicted)) +
    geom_line(color = 'black') +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = color, 
                alpha = .2) +
    ylab(pheno)+
    xlab('Weekly county-level cases per 100,000')
  
  plot.model = plot.model + geom_point(data = sig,
                                       aes(x =cases,
                                           y = predicted,
                                           color = sig),
                                       size = 1
  )+
    scale_color_manual(
      values = c('gray','red','blue'))+
    theme(legend.position = 'top',
          legend.key = element_rect(fill =NA,color = NA))+
    labs(color = '')

  
  # (Optional, add monthly averages and CIs to plot) ----
  # create plot data
  
  
    
  # plot = cowplot::plot_grid(plot.model,plot.deriv,ncol = 1,
  #                           rel_heights = c(0.6,0.4))
  # 
  return(plot.model)
}



# date smooths ----

# number of derivative points is number of days of data
data = fread('datasets/mobility.affect.covid.missingness.11.3.23.csv')

firstday.affect = as.numeric(min(data$date[!is.na(data$pa.av)]))
lastday.affect = as.numeric(max(data$date[!is.na(data$pa.av)]))
ndays.affect = length(firstday.affect:lastday.affect)


firstday.mobility = as.numeric(min(data$date[!is.na(data$numpts)]))
lastday.mobility = as.numeric(max(data$date[!is.na(data$numpts)]))
ndays.mobility = length(firstday.mobility:lastday.mobility)



## positve affect days plots ----
pa.mod = readRDS('models/pa.av.gamm4.days.11.8.23.rds')
#pa.mod.2 = readRDS("pa.av.gamm4.days.4.20.23.rds")
#mgcv::gam.check(pa.mod$gam)

#emmeans(as.emmGrid(pa.mod.2$mer), specks = 'month')

pa.days.plot = get.smooths.plot.date(pa.mod$gam,
                                     smooth = 's(days.post.covid)',
                                     pheno = 'Positive Affect',
                                     color = 'blue',
                                     deriv_pts = 2*ndays.affect)



ggsave('plots/pa.days.plot.11.8.23.png',
       pa.days.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')

## negative affect days plots ----
na.mod = readRDS('models/na.av.gamm4.days.11.8.23.rds')
#mgcv::gam.check(na.mod$gam)

na.days.plot = get.smooths.plot.date(na.mod$gam,
                                     smooth = 's(days.post.covid)',
                                     pheno = 'Negative Affect',
                                     color = 'red',
                                     deriv_pts = 2*ndays.affect)


#rm(na.mod)

ggsave('plots/na.days.plot.11.8.23.png',
       na.days.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')

## numpts days plots ----
pts.mod = readRDS('models/numptsgamm4.days.11.8.23.rds')
#pts.check.days =mgcv::gam.check(pts.mod$gam)

pts.days.plot = get.smooths.plot.date(pts.mod$gam,
                                      smooth = 's(days.post.covid)',
                                      pheno = 'Daily Locations Visited',
                                      color = 'purple',
                                      deriv_pts = 2*ndays.mobility)


ggsave('plots/pts.days.plot.11.8.png',
       pts.days.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')


## distance days plots ----

dist.mod = readRDS('models/distancegamm4.days.11.8.23.rds')
#mgcv::gam.check(dist.mod$gam)
dist.days.plot = get.smooths.plot.date(dist.mod$gam,
                                       smooth = 's(days.post.covid)',
                                       pheno = 'Daily Distance Travelled',
                                       color = 'green',
                                       deriv_pts = 2*ndays.mobility)

ggsave('plots/dist.days.plot.11.8.png',
       dist.days.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')

# Mixed Effects Models Table, Date ----

mer_mods.days = tab_model(pts.mod$mer,dist.mod$mer,pa.mod$mer,na.mod$mer,
                          pred.labels = c('Intercept', 
                                          'Sex (Male)', 
                                          'Relative Age',
                                          'Weekend',
                                          'OS (Android v1)',
                                          'OS (iOS v0)',
                                          'OS (iOS v1)',
                                          '% Missing Location Data',
                                          '% Missing Affect Surveys',
                                          's(Date)'), 
                          dv.labels = c('Daily Locations Vistied',
                                        'Daily Distance Travelled',
                                        'Positive Affect',
                                        'Negative Affect')
)


save_html(mer_mods.days,'covid.days.lmer.models.table.html')



# case count smooths ----

## positve affect cases plots ----
pa.mod = readRDS('models/pa.av.gamm4.cases.11.8.23.rds')
#mgcv::gam.check(pa.mod$gam)
pa.cases.plot = get.smooths.plot.cases(pa.mod$gam,
                                       smooth = 's(cases.weekly.per.100k)',
                                       pheno = 'Positive Affect',
                                       color = 'blue',
                                       deriv_pts = 4000)

ggsave('plots/pa.cases.plot.11.8.png',
       pa.cases.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')

## negative affect cases plots ----
na.mod = readRDS('models/na.av.gamm4.cases.11.8.23.rds')
#mgcv::gam.check(na.mod$gam)
na.cases.plot = get.smooths.plot.cases(na.mod$gam,
                                       smooth = 's(cases.weekly.per.100k)',
                                       pheno = 'Negative Affect',
                                       color = 'red',
                                       deriv_pts = 4000)

ggsave('plots/na.cases.plot.11.8.png',
       na.cases.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')


## numpts cases plots ----
pts.mod = readRDS('models/numptsgamm4.cases.3.31.23.rds')
#mgcv::gam.check(pts.mod$gam)
pts.cases.plot = get.smooths.plot.cases(pts.mod$gam,
                                        smooth = 's(cases.weekly.per.100k)',
                                        pheno = 'Daily Locations Visited',
                                        color = 'purple',
                                        deriv_pts = 4000)

ggsave('plots/pts.cases.plot.11.8.png',
       pts.cases.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')


## distance cases plots ----

dist.mod = readRDS('models/distancegamm4.cases.11.8.23.rds')
#mgcv::gam.check(dist.mod$gam)
dist.cases.plot = get.smooths.plot.cases(dist.mod$gam,
                                         smooth = 's(cases.weekly.per.100k)',
                                         pheno = 'Daily Distance Travelled',
                                         color = 'green',
                                         deriv_pts = 4000)

ggsave('plots/dist.cases.plot.11.8.png',
       dist.cases.plot,
       device = 'png',
       height = 6, 
       width = 10, 
       units = 'in')

# Mixed Effects Models Table, Cases ----

mer_mods.cases = tab_model(pts.mod$mer,dist.mod$mer,pa.mod$mer,na.mod$mer,
                           pred.labels = c('Intercept', #1
                                           'Sex (Male)', #2
                                           'Age', #3
                                           'Month (August)', #4
                                           'Month (December)', #5
                                           'Month (February)', #6
                                           'Month (January)', #7
                                           'Month (July)',#8
                                           'Month (June)', #9 
                                           'Month (March)', #10
                                           'Month (May)', #11
                                           'Month (November)', #12
                                           'Month (October)', #13
                                           'Month (September)', #14
                                           'Weekend', #15
                                           'Population density', #16
                                           'OS (Android v0)', #17
                                           'OS (Android v1)', #18
                                           'OS (iOS v0)', #19
                                           'OS (iOS v1)',#20
                                           's(Case Count)'), #21 
                           order.terms = c(1,2,3,7,6,10,11,9,8,4,14,13,12,5,15,16,17,18,19,20,21),
                           dv.labels = c('Daily Locations Vistied',
                                         'Daily Distance Travelled',
                                         'Positive Affect',
                                         'Negative Affect')
)


save_html(mer_mods.cases,'covid.case.count.lmer.models.table.html')

