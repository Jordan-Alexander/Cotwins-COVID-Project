# top ----
# Create Bar plots of mixed effects models
library(dplyr)
library(ggplot2)
library(ggeffects)
library(ggthemes)
library(cowplot)

# pts ----

pts = readRDS('~/CovidLocationProject/RAship.2022/models/numpts_categorial_date_lme_mod.rds')
summary(pts)
mean.pts = 3.242958
sd.pts = 1.981685

pts.pred = ggpredict(pts, terms = 'Covid_Year_Fac') %>% 
  as.data.frame() %>%
  mutate(predicted = predicted*sd.pts + mean.pts,
         conf.low = conf.low*sd.pts + mean.pts,
         conf.high = conf.high*sd.pts + mean.pts)

pts.pred$category = c('2019-01-20 -\n2019-05-01',
                      '2020-01-20 -\n2020-05-01',
                      '2021-01-20 -\n2021-05-01',
                      '2022-01-20 -\n2022-04-18')


plot.pts = ggplot(pts.pred) + 
  geom_point(aes(x = category, y = predicted, color = category, size = 2), show.legend = F) +
  geom_errorbar(aes(x = category, ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = pts.pred$predicted[1]),linetype = 'dashed',color = 'red')+
  theme_clean() +
  ylab('Locations Visted/Day') + xlab('') +
  ggtitle('Daily Locations Visited') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=8),
        plot.title = element_text(size=16))+
  scale_y_continuous(breaks = seq(2,4,0.5),limits = c(2,4))


# distance ----

dist = readRDS('~/CovidLocationProject/RAship.2022/models/distance_categorial_date_lme_mod.rds')
summary(dist)
mean.dist = 17.07425
sd.dist = 39.23025

dist.pred = ggpredict(dist, terms = 'Covid_Year_Fac') %>% 
  as.data.frame() %>%
  mutate(predicted = predicted*sd.dist + mean.dist,
         conf.low = conf.low*sd.dist + mean.dist,
         conf.high = conf.high*sd.dist + mean.dist)

dist.pred$category = c('2019-01-20 -\n2019-05-01',
                      '2020-01-20 -\n2020-05-01',
                      '2021-01-20 -\n2021-05-01',
                      '2022-01-20 -\n2022-04-18')


plot.dist = ggplot(dist.pred) + 
  geom_point(aes(x = category, y = predicted, color = category, size = 2), show.legend = F) +
  geom_errorbar(aes(x = category, ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = dist.pred$predicted[1]),linetype = 'dashed',color = 'red')+
  theme_clean() +
  ylab('Kilometers') + xlab('') +
  ggtitle('Daily Travel Distance') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=8),
        plot.title = element_text(size=16))+
  scale_y_continuous(breaks = seq(12,22,2),limits = c(12,22))

# pa ----

pa = readRDS('~/CovidLocationProject/RAship.2022/models/pa.av_categorial_date_lme_mod.rds')
summary(pa)
mean.pa = 2.882635
sd.pa = 0.7393253

pa.pred = ggpredict(pa, terms = 'Covid_Year_Fac') %>% 
  as.data.frame() 

pa.pred$category = c('2019-01-20 -\n2019-05-01',
                       '2020-01-20 -\n2020-05-01',
                       '2021-01-20 -\n2021-05-01',
                       '2022-01-20 -\n2022-04-18')


plot.pa = ggplot(pa.pred) + 
  geom_point(aes(x = category, y = predicted, color = category, size = 2), show.legend = F) +
  geom_errorbar(aes(x = category, ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = pa.pred$predicted[1]),linetype = 'dashed',color = 'red')+
  theme_clean() +
  ylab('Standardized β') + xlab('') +
  ggtitle('Postive Affect ') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=8),
        plot.title = element_text(size=16))+
  scale_y_continuous(breaks = seq(-0.25,0.25,0.1),limits = c(-0.25,0.25))

# na ----

na = readRDS('~/CovidLocationProject/RAship.2022/models/na.av_categorial_date_lme_mod.rds')
summary(na)
mean.na = 2.068255
sd.na = 0.8080795

na.pred = ggpredict(na, terms = 'Covid_Year_Fac') %>% 
  as.data.frame() 

na.pred$category = c('2019-01-20 -\n2019-05-01',
                     '2020-01-20 -\n2020-05-01',
                     '2021-01-20 -\n2021-05-01',
                     '2022-01-20 -\n2022-04-18')


plot.na = ggplot(na.pred) + 
  geom_point(aes(x = category, y = predicted, color = category, size = 2), show.legend = F) +
  geom_errorbar(aes(x = category, ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9)) +
  geom_hline(aes(yintercept = 0)) + 
  geom_hline(aes(yintercept = na.pred$predicted[1]),linetype = 'dashed',color = 'red')+
  theme_clean() +
  ylab('Standardized β') + xlab('') +
  ggtitle('Negative Affect ') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=8),
        plot.title = element_text(size=16))+
  scale_y_continuous(breaks = seq(-0.1,0.3,0.1),limits = c(-0.1,0.35))

# plot grid ----

fig1.plot = cowplot::plot_grid(plot.pts, plot.dist,
                   plot.pa, plot.na,
                   nrow = 2)


ggsave(filename = "~/CovidLocationProject/RAship.2022/plots/figure1.lme.date.mods.png",
         fig1.plot,
       height = 8,
       width = 8,
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
                                            '01/20 - 05/01 2020',
                                            '01/20 - 05/01 2021',
                                            '01/20 - 05/01 2022'),
                           dv.labels = c('Daily Locations Vistied',
                                         'Daily Distance Travelled',
                                         'Positive Affect',
                                         'Negative Affect'),
                     show.obs = FALSE, show.r2 = F,show.se = T,show.ci = F,digits = 3,
                     title = 'Supplementary Table S2. Effect of Year on Affect/Mobility')



save_html(mer_mods,'Categorical.Date.LME.Tables.html')


c(0.13108009,0.07395745,0.11857222,0.02785167)^2
c(0.13607266,0.24189445,0.25996737, 0.06656964)^2

c(0.5424955,0.4220184,0.6723728,0.4456861)^2
c(0.8852166,0.8103310,1.0297255,1.1675486)^2

c(0.5032784,0.1748140,0.6101952,0.4144581)^2
c(0.7767936,0.8130158,1.0252216,0.7949725)^2
# bottom ----