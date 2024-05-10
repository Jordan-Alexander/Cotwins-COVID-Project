### Plot generalized additive mixture models of affect before and during the pandemic

setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)
library(gamm4)
library(mgcv)


getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

panas.long<-read.csv('PANASphone.long.csv')
head(panas.long)
panas.long$timestamp = ymd(panas.long$timestamp)

# compute mean pa.av and na.av

avg <- panas.long %>% group_by(SVID) %>% summarise(pa.av = mean(pa.av),
                                                   na.av = mean(na.av),
                                                   bestzygos = first(bestzygos))
mean(avg$pa.av,na.rm = T)
sd(avg$pa.av,na.rm = T)


# add sex and date of birth to data
dems = read.csv('CoTwinsDemographics.JA.add.dates.081722.csv') %>%
     select(c(SVID,Family,age_at_intake,Sex1, bestzygos, birth_date, test_date))


panas.long = left_join(panas.long,dems)



# create days since start of pandemic variable (operationalized as the day the NBA closed)
covid.startdate<-ymd('2020-03-11')

panas.long$covid.days<-as.numeric(panas.long$timestamp) - as.numeric(covid.startdate)

panas.long<- select(panas.long, c(SVID,Family,bestzygos,pa.av,na.av,timestamp,Sex1, birth_date, covid.days))

str(panas.long)
panas.long$SVID = as.factor(panas.long$SVID)
panas.long$Family = as.factor(panas.long$Family)
panas.long$Sex1 = factor(panas.long$Sex1, labels = c('Female','Male'))
panas.long$birth_date<-ymd(panas.long$birth_date)
panas.long$age.covid.start<- (as.numeric(covid.startdate) - as.numeric(panas.long$birth_date))/365.25
panas.long$season<-getSeason(panas.long$timestamp)
nrow(panas.long)
panas.long<-na.omit(panas.long)
nrow(panas.long)

age.start = panas.long %>% group_by(SVID) %>% summarise(age = first(age.covid.start),
                                                        zyg = first(bestzygos))
mean(age.start$age)
sd(age.start$age)
mean(age.start$age[age.start$zyg == 'MZ'])
sd(age.start$age[age.start$zyg == 'MZ'])

mean(age.start$age[age.start$zyg == 'DZ'])
sd(age.start$age[age.start$zyg == 'DZ'])

mean(age.start$age[age.start$zyg == 'OS'])
sd(age.start$age[age.start$zyg == 'OS'])


### gamm of positive affect before and during the pandemic

# played around with k until I found one that wasn't too restrictive. 
# lower k values miss the large decline in positive affect at the start of the pandemic

pa.gamm <- gamm4(pa.av ~ s(covid.days, k = 30), 
                 random = ~(1 | Family/SVID),  data = panas.long)


pa.gamm.corrections <- gamm4(pa.av ~ s(covid.days, k = 30) +
                             Sex1 + season + age.covid.start, 
                             random = ~(1 | Family/SVID),  data = panas.long)




summary(pa.gamm.corrections$gam)
pdf('gamm.pa.pdf')
par(mfrow = c(2,1))
plot.gam(pa.gamm$gam)
plot.gam(pa.gamm.corrections$gam)
dev.off()



# now we do the same for negative affect


na.gamm <- gamm4(na.av ~ s(covid.days, k = 30), 
                 random = ~(1 | Family/SVID),  data = panas.long)


na.gamm.corrections <- gamm4(na.av ~ s(covid.days, k = 30) +
                             Sex1 + season + age.covid.start, 
                             random = ~(1 | Family/SVID),  data = panas.long)


plot.gam(na.gamm$gam)
plot.gam(na.gamm.corrections$gam)


summary(na.gamm.corrections$gam)
pdf('gamm.na.pdf')
par(mfrow = c(2,1))
plot.gam(na.gamm$gam)
plot.gam(na.gamm.corrections$gam)
dev.off()



# plot again with ggplot

pa_pred = predict(pa.gamm$gam, se.fit = T)

pa_plot_data <-
  tibble(
    covid.days = as_date(panas.long$covid.days, origin=covid.startdate),
    pa = pa_pred$fit,
    lower = pa_pred$fit - 1.96 * pa_pred$se.fit,
    upper = pa_pred$fit + 1.96 * pa_pred$se.fit
  )

pa_plot <- 
  ggplot(pa_plot_data, aes(covid.days, pa)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), 
              color = 'purple', fill = 'purple', alpha = 0.2, 
              outline.type = 'full') +
  geom_line(color = 'purple') + geom_point(alpha = 0)+
  geom_vline(xintercept=as.numeric(pa_plot_data$covid.days[64]+days(9)),
                colour="blue")+
     geom_vline(xintercept=as.numeric(pa_plot_data$covid.days[65]+days(7)),
                colour="red")+
     geom_vline(xintercept=as.numeric(pa_plot_data$covid.days[79]+days(8)),
                colour="green") +
  xlab("") +
  ylab("Positive Affect") +
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))
                          

                                                       
na_pred = predict(na.gamm$gam, se.fit = T)

na_plot_data <-
  tibble(
    covid.days = as_date(panas.long$covid.days, origin=covid.startdate),
    na = na_pred$fit,
    lower = na_pred$fit - 1.96 * na_pred$se.fit,
    upper = na_pred$fit + 1.96 * na_pred$se.fit
  )

na_plot <- 
  ggplot(na_plot_data, aes(covid.days, na)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), 
              color = 'Gold', fill = 'Gold', alpha = 0.2, 
              outline.type = 'full') +
  geom_line(color = 'Gold') + geom_point(alpha = 0)+
     geom_vline(xintercept=as.numeric(pa_plot_data$covid.days[64]+days(9)),
                colour="blue")+
     geom_vline(xintercept=as.numeric(pa_plot_data$covid.days[65]+days(7)),
                colour="red")+
     geom_vline(xintercept=as.numeric(pa_plot_data$covid.days[79]+days(8)),
                colour="green") +
  xlab("") +
  ylab("Negative Affect") +
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))


plot_grid(pa_plot,na_plot, nrow = 2, align = 'hv')
     
# geom_line() + geom_point(alpha = 0) +
  # scale_color_brewer("", palette = "Set1") +
  # scale_fill_brewer("", palette = "Set1") +
  # labs(
  #   x = "",
  #   y = "positive affect") + 
  # theme(legend.direction = "horizontal", legend.position = c(0.55, 0.05),
  #       legend.background = element_rect(fill=alpha('white', 0)),
  #       legend.key = element_rect(alpha(0.5)))+
  # theme(panel.background = element_rect(fill = 'white'),
  #       panel.border = element_blank(), 
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.line = element_line(size = 0.3, linetype = "solid",
  #                                colour = "black"))


pa_plot <- ggMarginal(
  pa_plot,
  margins = "x",
  type = "histogram")

save_plot("figs/par_mon_trajectory.pdf", par_mon_plot, base_aspect_ratio = 1.2)




