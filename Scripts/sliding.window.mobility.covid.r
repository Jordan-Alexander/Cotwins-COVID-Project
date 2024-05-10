# Plot Mobility data over time
setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)

move.long<-read.csv('mobility.measures.10.21.22.csv')
move.long$timestamp = as_date(move.long$date, origin = '1970-01-01')
head(move.long)
move.long$distance[move.long$numpts == 1] = 0

# plot mobility over time

# first take a daily average
daily.avg<-move.long %>% group_by(timestamp) %>%
     summarise(pts.avg = mean(numpts),
               dst.avg = mean(distance)) %>% ungroup()


pts <- ggplot(daily.avg, aes(x = timestamp, y = pts.avg)) +
                  geom_line() + xlab('') + ylab('Daily Locations Visited')


dst <- ggplot(daily.avg, aes(x = timestamp, y = dst.avg)) +
                  geom_line() + xlab('') + ylab('Daily Distance Travelled')


# pretty messy, let's try a sliding window plot

window.plotdata<-data.frame()
day1 = min(move.long$timestamp,na.rm = T)
days<-seq.Date(from = min(move.long$timestamp,na.rm = T),
               to = max(move.long$timestamp,na.rm = T),
               by = "5 days")

for(i in 1:(length(days)-2)){
     from = days[i]
     to = days[i] + days(10)
     days.tmp<-seq.Date(from = from,
                        to = to,
                        by = 'day')
     obs<-subset(move.long, timestamp %in% days.tmp)
     row = data.frame(day = days[i+1],
                      pts.av = mean(obs$numpts,na.rm = T),
                      dst.av = mean(obs$distance, na.rm =T),
                      n = nrow(obs))
     
     window.plotdata =rbind(window.plotdata,row)}
                      
                      
# plot sliding window plot

pts.window <- ggplot(window.plotdata, aes(x = day, y = pts.av)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('Daily Locations Visited')

dst.window <- ggplot(window.plotdata, aes(x = day, y = dst.av)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('Daily Distance Travelled')

sample_size = ggplot(window.plotdata, aes(x = day, y = n)) + geom_col()+ xlab('')

window = plot_grid(pts.window,dst.window, nrow = 2, ncol = 1)


# we're missing all locations between 09-11-2020 and 10-26-2020
# figure out what's going on here
# UPDATE: repulled, these are now included

days[1] = min(move.long$timestamp, na.rm = T)


# plot with Covid Case Counts:

covid<-read.csv('time_series_covid19_confirmed_8_12_US.csv')
names(covid)
nrow(covid)

table(covid$Province_State)


# colorado dataset
ncol(covid)
names(covid)

days = names(covid)[12:ncol(covid)]

covid.colorado = data.frame()
covid.tmp = subset(covid, Province_State == 'Colorado') 
for(i in days){
     row = data.frame(day = i,
                      cases = sum(covid.tmp[,i]))
     covid.colorado = rbind(covid.colorado, row)}

covid.colorado$day<-gsub('X','', covid.colorado$day)
covid.colorado$day<-gsub('\\.','-', covid.colorado$day)

covid.colorado$daily.cases <- covid.colorado$cases - lag(covid.colorado$cases)
covid.colorado$day<-mdy(covid.colorado$day)

# append 2019 days to covid.colorado

first = min(move.long$timestamp,na.rm = T)

tmp = data.frame(day = seq.Date(from = first,
                                to = ymd('2020-01-21'),
                                by = 'days'),
                 cases = 0,
                 daily.cases = 0)

covid.colorado <-rbind(tmp, covid.colorado)                 
head(covid.colorado)
# truncate to match move data
covid.colorado = subset(covid.colorado, day <= max(move.long$timestamp, na.rm = T))


caseplot = ggplot(covid.colorado, aes(x = day, y = daily.cases)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('Daily Case Count (CO)')



window = plot_grid(sample_size, pts.window,dst.window,caseplot, nrow = 4, ncol = 1, align = 'hv')
window
ggsave(filename = 'sliding.window.move.covid.colorado.png',window,width = 8,height = 11, units = c('in'))



#
#a = rnorm(n = 5000, m = 0, sd = 1)
#b = rnorm(n = 5000, m = 0.4, sd = 1)
#df = data.frame(a = a,b = b)

#ggplot(df) + geom_histogram(aes(a), color = 'blue', alpha = 0.5)+
#     geom_histogram(aes(b), color = 'red', alpha = 0.5)





# national dataset
ncol(covid)
names(covid)

days = names(covid)[12:ncol(covid)]

covid.usa = data.frame()
for(i in days){
     row = data.frame(day = i,
                      cases = sum(covid[,i]))
     covid.usa = rbind(covid.usa, row)}

covid.usa$day<-gsub('X','', covid.usa$day)
covid.usa$day<-gsub('\\.','-', covid.usa$day)

covid.usa$daily.cases <- covid.usa$cases - lag(covid.usa$cases)
covid.usa$day<-mdy(covid.usa$day)

# append 2019 days to covid.usa

first = min(move.long$timestamp,na.rm = T)

tmp = data.frame(day = seq.Date(from = first,
                                to = ymd('2020-01-21'),
                                by = 'days'),
                 cases = 0,
                 daily.cases = 0)

covid.usa <-rbind(tmp, covid.usa)                 
head(covid.usa)
# truncate to match move data
covid.usa = subset(covid.usa, day <= max(move.long$timestamp, na.rm = T))

# create plot data
covid.usa.plotdata<-data.frame()
day1 = min(window.plotdata$day,na.rm = T)
days<-seq.Date(from = ymd(day1),
               to = ymd(max(window.plotdata$day, na.rm = T)),
               by = "5 days")

covid.usa$day = ymd(covid.usa$day)
i = 1
for(i in 1:(length(days)-2)){
     from = days[i]
     to = days[i] + days(10)
     days.tmp<-seq.Date(from = from,
                        to = to,
                        by = 'day')

     obs<- covid.usa[which(covid.usa$day %in% days.tmp),]
     
     row = data.frame(day = days[i+1],
                      daily.cases = mean(obs$daily.cases,na.rm = T),
                      n = nrow(obs))
     
     covid.usa.plotdata =rbind(covid.usa.plotdata,row)}
covid.usa.plotdata$log.cases = log(covid.usa.plotdata$daily.cases)

caseplot = ggplot(covid.usa.plotdata, aes(x = day, y = log.cases)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('ln(Daily Case Count) (National)')





window = plot_grid(sample_size, pts.window,dst.window,caseplot, nrow = 4, ncol = 1, align = 'hv')
window
ggsave(filename = 'sliding.window.move.natlog.covid.USA.png',window,width = 8,height = 11, units = c('in'))



### what about percent change in Daily covid cases:

# calculate 14 day moving average:
library(zoo)

covid.usa$two.week.avg = rollmean(covid.usa$daily.cases, k = 14, fill = NA) 
covid.usa$pct.chg.two.week = (covid.usa$two.week.avg - lag(covid.usa$two.week.avg, 13))/lag(covid.usa$two.week.avg, 1) 
covid.usa$pct.chg.two.week[is.na(covid.usa$pct.chg.two.week)==T] = 0

# 14 day moving average plot data

caseplot.pct = ggplot(covid.usa, aes(x = day, y = pct.chg.two.week)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('% Chg (14-day avg) (National)')


window.pct = plot_grid(sample_size,pts.window,dst.window,caseplot.pct, align = 'hv', nrow = 4, ncol = 1)
ggsave(filename = 'sliding.window.pct.change.covid.USA.png',window.pct,width = 8,height = 11, units = c('in'))

### regress out seasonality and weekend for distance:

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

weekday.weekend = function(date){
     day = wday(date, label = T)
     wknd = rep(NA, times = length(day))
     wknd[day %in% c('Sat','Sun')] = 'weekend'
     wknd[!(day %in% c('Sat','Sun'))] = 'weekday'
     wknd = factor(wknd, levels = c('weekend','weekday'))
     return(wknd)}


move.long$season = getSeason(move.long$timestamp)
move.long$wknd = weekday.weekend(move.long$timestamp)

mod = lm(distance ~ season + wknd, data = move.long)
move.long$dist.covars = mod$residuals + mod$coefficients[1]
head(move.long)



window.plotdata<-data.frame()
day1 = min(move.long$timestamp,na.rm = T)
days<-seq.Date(from = min(move.long$timestamp,na.rm = T),
               to = max(move.long$timestamp,na.rm = T),
               by = "5 days")

for(i in 1:(length(days)-2)){
     from = days[i]
     to = days[i] + days(10)
     days.tmp<-seq.Date(from = from,
                        to = to,
                        by = 'day')
     obs<-subset(move.long, timestamp %in% days.tmp)
     row = data.frame(day = days[i+1],
                      pts.av = mean(obs$numpts,na.rm = T),
                      dst.av = mean(obs$distance, na.rm =T),
                      dst.av.covars = mean(obs$dist.covars, na.rm = T),
                      n = nrow(obs))
     window.plotdata =rbind(window.plotdata,row)}

dst.covars.window <- ggplot(window.plotdata, aes(x = day, y = dst.av.covars)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('Daily Distance Travelled (Covariate Adjusted)')

sample_size = ggplot(window.plotdata, aes(x = day, y = n)) + geom_col()+ xlab('')

plot_grid(dst.window,dst.covars.window, nrow = 2, ncol = 1)


# ok let's try log transforming

move.long$logdist = log(move.long$distance+1)
mod = lm(logdist ~ season + wknd, data = move.long)
move.long$logdist.covars = mod$residuals + mod$coefficients[1]
head(move.long)

window.plotdata<-data.frame()
day1 = min(move.long$timestamp,na.rm = T)
days<-seq.Date(from = min(move.long$timestamp,na.rm = T),
               to = max(move.long$timestamp,na.rm = T),
               by = "5 days")

for(i in 1:(length(days)-2)){
     from = days[i]
     to = days[i] + days(10)
     days.tmp<-seq.Date(from = from,
                        to = to,
                        by = 'day')
     obs<-subset(move.long, timestamp %in% days.tmp)
     row = data.frame(day = days[i+1],
                      pts.av = mean(obs$numpts,na.rm = T),
                      dst.av = mean(obs$distance, na.rm =T),
                      dst.av.log.cov = mean(obs$logdist.covars, na.rm = T),
                      n = nrow(obs))
     window.plotdata =rbind(window.plotdata,row)}

log.dst.covars.window <- ggplot(window.plotdata, aes(x = day, y = dst.av.log.cov)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[274]-days(1)), # March 11, 2020
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[277]-days(2)), # March 25, 2020
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[331]-days(11)), # December 11, 2020
                colour="green") + 
     xlab('') + ylab('ln Daily Distance Travelled (Covariate Adjusted)')

sample_size = ggplot(window.plotdata, aes(x = day, y = n)) + geom_col()+ xlab('')

plot_grid(dst.window,log.dst.covars.window, nrow = 2, ncol = 1)
