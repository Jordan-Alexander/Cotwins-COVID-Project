# don't actually run this locally, musing to document msi activity
setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Models")
library(dplyr)
library(ggplot2)
library(ggExtra)
library(gamm4)

numpts = readRDS('gamm4.numpts.1.26.23.rds')
distance = readRDS('gamm4.distance.1.26.23.rds')
pa.av = readRDS('gamm4.pa.av.1.26.23.rds')
na.av = readRDS('gamm4.na.av.1.26.23.rds')

pa.av.mobility = readRDS('gamm4.pa.av.mobility.1.26.23.rds')
na.av.mobility = readRDS('gamm4.na.av.mobility.1.26.23.rds')




numpts_pred <- predict(numpts$gam, se.fit = T)
numpts_plot_data <-
  tibble(
    days.post.covid = numpts$gam$model$days.post.covid,
    cases.weekly.per.100k = numpts$gam$model$cases.weekly.per.100k,
    numpts = numpts_pred$fit,
    lower = numpts_pred$fit - 1.96 * numpts_pred$se.fit,
    upper = numpts_pred$fit + 1.96 * numpts_pred$se.fit
  )



numpts_days_plot <- ggplot(numpts_plot_data, aes(days.post.covid, numpts)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2, outline.type = 'full') +
  geom_line() + geom_point(alpha = 0) +
  scale_color_brewer("", palette = "Set1") +
  scale_fill_brewer("", palette = "Set1") +
  labs(
    x = "Days_pre/post 01-22-2020",
    y = "Average Locations per Day")


numpts_days_plot <- ggplot(numpts_plot_data, aes(days.post.covid, numpts)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2, outline.type = 'full') +
  geom_line() + geom_point(alpha = 0) +
  labs(
    x = "Days_pre/post 01-22-2020",
    y = "Average Locations per Day")


numpts_cases_plot <- ggplot(numpts_plot_data, aes(cases.weekly.per.100k, numpts)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2, outline.type = 'full') +
  geom_line() + geom_point(alpha = 0) +
  scale_color_brewer("", palette = "Set1") +
  scale_fill_brewer("", palette = "Set1") +
  labs(
    x = "Days_pre/post 01-22-2020",
    y = "Average Locations per Day")



#ggsave('pts.days.1.26.23.png',plot = numpts_days_plot, device = 'png')