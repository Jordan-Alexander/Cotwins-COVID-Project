# Case count year interaction plots

# top ----
# Plot case count year interactions from mixed effects models
library(dplyr)
library(ggplot2)
library(ggeffects)
library(ggthemes)
library(cowplot)


# pts ----

mean.pts = 3.157869
sd.pts = 1.966859

mean.cases = 157.8789
sd.cases = 249.0444

pts = readRDS('~/CovidLocationProject/RAship.2022/models/numpts_casecount_year_interaction_lme_mod.rds')
covidvals = seq(-1,3,0.01)
pts.pred = ggpredict(pts, terms = c('weekly.cases.per.100k [covidvals]','Covid_Year_Fac')) %>%
  as.data.frame() %>%
  mutate(predicted = predicted*sd.pts + mean.pts,
         conf.low = conf.low*sd.pts + mean.pts,
         conf.high = conf.high*sd.pts + mean.pts,
         x = x*sd.cases + mean.cases) %>%
  mutate(conf.low = case_when(conf.low >= 0 ~ conf.low,
                              conf.low < 0 ~ 0),
         category = rep(
             c('2020-01-20 -\n2020-05-01','2021-01-20 -\n2021-05-01','2022-01-20 -\n2022-04-18'), 
             times = length(covidvals))
         )

pts.plot = ggplot(data = pts.pred) + geom_line(aes(x = x, y = predicted, color = category))+
  geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = category), alpha = 0.3)+
  ylab('Locations Visited/Day') +
  xlab('') +
  scale_y_continuous(breaks = seq(0,3.5,0.5),limits = c(0,3.5))+
  scale_x_continuous(breaks = seq(0,1000,200),limits = c(0,1000)) + 
  theme_clean() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = 'bottom',
        legend.title = element_blank())



# dist ----

mean.dist = 16.31367
sd.dist = 38.49916

mean.cases = 157.8789
sd.cases = 249.0444

dist = readRDS('~/CovidLocationProject/RAship.2022/models/distance_casecount_year_interaction_lme_mod.rds')
covidvals = seq(-1,4,0.01)
dist.pred = ggpredict(dist, terms = c('weekly.cases.per.100k [covidvals]','Covid_Year_Fac')) %>%
  as.data.frame() %>%
  mutate(predicted = predicted*sd.dist + mean.dist,
         conf.low = conf.low*sd.dist + mean.dist,
         conf.high = conf.high*sd.dist + mean.dist,
         x = x*sd.cases + mean.cases) %>%
  mutate(conf.low = case_when(conf.low >= 0 ~ conf.low,
                              conf.low < 0 ~ 0),
         category = rep(
           c('2020-01-20 -\n2020-05-01','2021-01-20 -\n2021-05-01','2022-01-20 -\n2022-04-18'), 
           times = length(covidvals))
  )

dist.plot = ggplot(data = dist.pred) + geom_line(aes(x = x, y = predicted, color = category))+
  geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = category), alpha = 0.3)+
  scale_y_continuous(breaks = seq(0,22,2),limits = c(0,22))+
  scale_x_continuous(breaks = seq(0,1000,200),limits = c(0,1000)) + 
  theme_clean() +
  ylab('Daily Travel Distance (km)') +
  xlab(' ') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = 'bottom',
        legend.title = element_blank())

# pa ----

mean.pa = 2.834021
sd.pa = 0.7221136

mean.cases = 157.8789
sd.cases = 249.0444

pa = readRDS('~/CovidLocationProject/RAship.2022/models/pa.av_casecount_year_interaction_lme_mod.rds')
covidvals = seq(-1,4,0.01)
pa.pred = ggpredict(pa, terms = c('weekly.cases.per.100k [covidvals]','Covid_Year_Fac')) %>%
  as.data.frame() %>%
  mutate(predicted = predicted*sd.pa + mean.pa,
         conf.low = conf.low*sd.pa + mean.pa,
         conf.high = conf.high*sd.pa + mean.pa,
         x = x*sd.cases + mean.cases) %>%
  mutate(conf.low = case_when(conf.low >= 0 ~ conf.low,
                              conf.low < 0 ~ 0),
         category = rep(
           c('2020-01-20 -\n2020-05-01','2021-01-20 -\n2021-05-01','2022-01-20 -\n2022-04-18'), 
           times = length(covidvals))
  )

pa.plot = ggplot(data = pa.pred) + geom_line(aes(x = x, y = predicted, color = category))+
  geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = category), alpha = 0.3)+
  scale_y_continuous(breaks = seq(0,4,0.5),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,1200,200),limits = c(0,1200)) + 
  theme_clean() +
  ylab('Positive Affect (0-5)') +
  xlab(' ') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = 'bottom',
        legend.title = element_blank())


# na ----

mean.na = 2.069254
sd.na = 0.8184926

mean.cases = 157.8789
sd.cases = 249.0444

na = readRDS('~/CovidLocationProject/RAship.2022/models/na.av_casecount_year_interaction_lme_mod.rds')
covidvals = seq(-1,4,0.01)
na.pred = ggpredict(na, terms = c('weekly.cases.per.100k [covidvals]','Covid_Year_Fac')) %>%
  as.data.frame() %>%
  mutate(predicted = predicted*sd.na + mean.na,
         conf.low = conf.low*sd.na + mean.na,
         conf.high = conf.high*sd.na + mean.na,
         x = x*sd.cases + mean.cases) %>%
  mutate(conf.low = case_when(conf.low >= 0 ~ conf.low,
                              conf.low < 0 ~ 0),
         category = rep(
           c('2020-01-20 -\n2020-05-01','2021-01-20 -\n2021-05-01','2022-01-20 -\n2022-04-18'), 
           times = length(covidvals))
  )

na.plot = ggplot(data = na.pred) + geom_line(aes(x = x, y = predicted, color = category))+
  geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = category), alpha = 0.3)+
  scale_y_continuous(breaks = seq(0,3,0.5),limits = c(0,3))+
  scale_x_continuous(breaks = seq(0,1200,200),limits = c(0,1200)) + 
  theme_clean() +
  ylab('Negative Affect (0-5)') +
  xlab(' ') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = 'bottom',
        legend.title = element_blank())

# plot grid ----

fig4.plot = cowplot::plot_grid(pts.plot, dist.plot,
                               pa.plot, na.plot,
                               nrow = 2)


ggsave(filename = "~/CovidLocationProject/RAship.2022/plots/figure4.lme.date.covid.interaction.mods.png",
       fig4.plot,
       height = 8,
       width = 10,
       units = c('in'),
       device = 'png')



# LME Table ----
library(sjPlot) # we only load these at this step to keep from interfering w/ cowplot functions
library(htmltools)


mer_mods = tab_model(pts,dist,pa,na,
                      pred.labels = c('Intercept',
                                    'Sex (Male)',
                                    'Age on 01/20/2020',
                                    'OS (Android)',
                                   '% Missing Locations',
                                    '% Missing Affect Surveys',
                                    '01/20 - 05/01 2021',
                                    '01/20 - 05/01 2022',
                                   'Past-week county-level COVID-19 cases per 100,000',
                                   'Past-week county-level COVID-19 cases per 100,000 X
                                   01/20 - 05/01 2021',
                                   'Past-week county-level COVID-19 cases per 100,000 X
                                   01/20 - 05/01 2022'),
                     dv.labels = c('Daily Locations Vistied',
                                   'Daily Distance Travelled',
                                   'Positive Affect',
                                   'Negative Affect'),
                     show.obs = FALSE, show.r2 = F,show.se = T,show.ci = F,show.re.var=T,digits = 3,
                     title = 'Supplementary Table S4. Effect of Year on Affect/Mobility')



save_html(mer_mods,'Date.Casecount.LME.Tables.html')





# bottom ----