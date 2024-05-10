# Create Gamm Plots for Presentation

setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Models")
library(ggplot2)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
# numpts gam cases

numpts <- readRDS("numpts.gamm4.simple.cases.2.7.23.rds")

summary(numpts$gam)

numpts_pred = predict(numpts$gam, se.fit = T)
numpts_plot_data = tibble(cases = numpts$gam$model[,'cases.weekly.per.100k'],
               numpts = numpts_pred$fit,
               lower = numpts_pred$fit - 1.96 * numpts_pred$se.fit,
               upper = numpts_pred$fit + 1.96 * numpts_pred$se.fit)

numpts_plot_data = numpts_plot_data[numpts_plot_data$cases < 2500,]
          
numpts_plot = ggplot(numpts_plot_data, aes(cases, numpts)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'red',
                           outline.type = 'full') +
     geom_line(color = 'red')+ 
     theme_classic() +
     ylab('Daily Locations Visited') +
     xlab('Weekly Covid-19 cases per 100,000 People (County Level)')

ggsave('numpts.cases.smooth.presentation.png',
       numpts_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

# numpts gam days

numpts <- readRDS("numpts.gamm4.simple.days.2.7.23.rds")

summary(numpts$gam)

numpts_pred = predict(numpts$gam, se.fit = T)
numpts_plot_data = tibble(days = numpts$gam$model[,'days.post.covid'],
               numpts = numpts_pred$fit,
               lower = numpts_pred$fit - 1.96 * numpts_pred$se.fit,
               upper = numpts_pred$fit + 1.96 * numpts_pred$se.fit)

numpts_plot_data$days = ymd('2020-01-26') + numpts_plot_data$days

numpts_plot = ggplot(numpts_plot_data, aes(days, numpts)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'red',
                           outline.type = 'full') +
     geom_line(color = 'red')+ 
     theme_classic() +
     geom_vline(xintercept = ymd('2020-01-26'), color = 'Black', alpha = 1) +
     ylab('Daily Locations Visited') +
     xlab('Date')

ggsave('numpts.days.smooth.presentation.png',
       numpts_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

rm(numpts)



# distance gam cases

distance <- readRDS("distance.gamm4.simple.cases.2.7.23.rds")

summary(distance$gam)

distance_pred = predict(distance$gam, se.fit = T)
distance_plot_data = tibble(cases = distance$gam$model[,'cases.weekly.per.100k'],
               distance = distance_pred$fit,
               lower = distance_pred$fit - 1.96 * distance_pred$se.fit,
               upper = distance_pred$fit + 1.96 * distance_pred$se.fit)

distance_plot_data =distance_plot_data[distance_plot_data$cases < 2500,]

distance_plot = ggplot(distance_plot_data, aes(cases, distance)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'blue',
                           outline.type = 'full') +
     geom_line(color = 'blue')+ 
     theme_classic() +
     ylab('Daily Distance Travelled') +
     xlab('Weekly Covid-19 cases per 100,000 People (County Level)')

ggsave('distance.cases.smooth.presentation.png',
       distance_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

rm(distance)


# PA by days plot

PA <- readRDS("pa.av.gamm4.simple.days.2.7.23.rds")

summary(PA$gam)

PA_pred = predict(PA$gam, se.fit = T)
PA_plot_data = tibble(days = PA$gam$model[,'days.post.covid'],
               PA = PA_pred$fit,
               lower = PA_pred$fit - 1.96 * PA_pred$se.fit,
               upper = PA_pred$fit + 1.96 * PA_pred$se.fit)

PA_plot_data$days = ymd('2020-01-26') + PA_plot_data$days

PA_plot = ggplot(PA_plot_data, aes(days, PA)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'green',
                           outline.type = 'full') +
     geom_line(color = 'green')+ 
     theme_classic() +
     geom_vline(xintercept = ymd('2020-01-26'), color = 'Black', alpha = 1) +
     ylab('Positive Affect') +
     xlab('Date')

ggsave('PA.days.smooth.presentation.png',
       PA_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

rm(PA)



# PA by cases plot

PA <- readRDS("pa.av.gamm4.simple.cases.2.7.23.rds")

summary(PA$gam)

PA_pred = predict(PA$gam, se.fit = T)
PA_plot_data = tibble(cases = PA$gam$model[,'cases.weekly.per.100k'],
               PA = PA_pred$fit,
               lower = PA_pred$fit - 1.96 * PA_pred$se.fit,
               upper = PA_pred$fit + 1.96 * PA_pred$se.fit)

PA_plot_data= PA_plot_data[PA_plot_data$cases < 2500,]

PA_plot = ggplot(PA_plot_data, aes(cases, PA)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'green',
                           outline.type = 'full') +
     geom_line(color = 'green')+ 
     theme_classic() +
     ylab('Positive Affect') +
     xlab('Weekly Covid-19 cases per 100,000 People (County Level)')

ggsave('PA.cases.smooth.presentation.png',
       PA_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

rm(PA)




# na.av by days plot

na.av <- readRDS("na.av.gamm4.simple.days.2.7.23.rds")

summary(na.av$gam)

na.av_pred = predict(na.av$gam, se.fit = T)
na.av_plot_data = tibble(days = na.av$gam$model[,'days.post.covid'],
               na.av = na.av_pred$fit,
               lower = na.av_pred$fit - 1.96 * na.av_pred$se.fit,
               upper = na.av_pred$fit + 1.96 * na.av_pred$se.fit)

na.av_plot_data$days = ymd('2020-01-26') + na.av_plot_data$days

na.av_plot = ggplot(na.av_plot_data, aes(days, na.av)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'orange',
                           outline.type = 'full') +
     geom_line(color = 'orange')+ 
     theme_classic() +
     geom_vline(xintercept = ymd('2020-01-26'), color = 'Black', alpha = 1) +
     ylab('Negative Affect') +
     xlab('Date')

ggsave('na.av.days.smooth.presentation.png',
       na.av_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

rm(na.av)



# na.av by cases plot

na.av <- readRDS("na.av.gamm4.simple.cases.2.7.23.rds")

summary(na.av$gam)

na.av_pred = predict(na.av$gam, se.fit = T)
na.av_plot_data = tibble(cases = na.av$gam$model[,'cases.weekly.per.100k'],
               na.av = na.av_pred$fit,
               lower = na.av_pred$fit - 1.96 * na.av_pred$se.fit,
               upper = na.av_pred$fit + 1.96 * na.av_pred$se.fit)

na.av_plot_data= na.av_plot_data[na.av_plot_data$cases < 2500,]

na.av_plot = ggplot(na.av_plot_data, aes(cases, na.av)) +
               geom_ribbon(aes(ymax = upper, ymin = lower), 
                           alpha = 0.2, 
                           fill = 'orange',
                           outline.type = 'full') +
     geom_line(color = 'orange')+ 
     theme_classic() +
     ylab('Negative Affect') +
     xlab('Weekly Covid-19 cases per 100,000 People (County Level)')

ggsave('na.av.cases.smooth.presentation.png',
       na.av_plot, 
       device = 'png',
       width = 7,
       height = 7,
       units = 'in')

rm(na.av)