# Plot PANAS data over time
setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)

panas.long<-read.csv('Datasets/survey_PANAS_responses_20220728.csv')
head(panas.long)
panas.long$timestamp = date(panas.long$SUBMIT_DATE)





# plot positive affect over time

# first take a daily average
daily.avg<-panas.long %>% group_by(timestamp) %>%
     summarise(pa.avg = mean(pa.av),
               na.avg = mean(na.av)) %>% na.omit()  %>% ungroup()

daily.avg$timestamp<-ymd(daily.avg$timestamp)

pa <- ggplot(daily.avg, aes(x = timestamp, y = pa.avg)) +
                  geom_line() + xlab('') + ylab('positive affect')


na <- ggplot(daily.avg, aes(x = timestamp, y = na.avg)) +
                  geom_line() + xlab('') + ylab('positive affect')


# pretty messy, let's try a sliding window plot



window.plotdata<-data.frame()
day1 = min(panas.long$timestamp,na.rm = T)
days<-seq.Date(from = min(panas.long$timestamp,na.rm = T),
               to = max(panas.long$timestamp,na.rm = T),
               by = "5 days")

for(i in 1:(length(days)-2)){
     from = days[i]
     to = days[i] + days(10)
     days.tmp<-seq.Date(from = from,
                        to = to,
                        by = 'day')
     obs<-subset(panas.long, timestamp %in% days.tmp)
     row = data.frame(day = days[i+1],
                      pa.av = mean(obs$pa.av,na.rm = T),
                      na.av = mean(obs$na.av, na.rm =T),
                      n = nrow(obs))
     
     window.plotdata =rbind(window.plotdata,row)}
                      
                      
# plot sliding window plot

pa.window <- ggplot(window.plotdata, aes(x = day, y = pa.av)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[80]-days(1)),
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[83]-days(2)),
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[135]-days(1)),
                colour="green") + 
     xlab('') + ylab('Positive Affect')

na.window <- ggplot(window.plotdata, aes(x = day, y = na.av)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[80]-days(1)),
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[83]-days(2)),
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[135]-days(1)),
                colour="green") + 
     xlab('') + ylab('Negative Affect')
sample_size = ggplot(window.plotdata, aes(x = day, y = n)) + geom_col()+ xlab('')

window = plot_grid(pa.window,na.window, nrow = 2, ncol = 1)


days[1] = min(panas.long$timestamp, na.rm = T)


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

first = min(panas.long$timestamp,na.rm = T)

tmp = data.frame(day = seq.Date(from = first,
                                to = ymd('2020-01-21'),
                                by = 'days'),
                 cases = 0,
                 daily.cases = 0)

covid.colorado <-rbind(tmp, covid.colorado)                 
head(covid.colorado)
# truncate to match PANAS data
covid.colorado = subset(covid.colorado, day <= max(panas.long$timestamp, na.rm = T))





window = plot_grid(sample_size, pa.window,na.window,caseplot, nrow = 4, ncol = 1, align = 'hv')
window
ggsave(filename = 'sliding.window.panas.covid.colorado.png',window,width = 8,height = 11, units = c('in'))



#
a = rnorm(n = 5000, m = 0, sd = 1)
b = rnorm(n = 5000, m = 0.4, sd = 1)
df = data.frame(a = a,b = b)

ggplot(df) + geom_histogram(aes(a), color = 'blue', alpha = 0.5)+
     geom_histogram(aes(b), color = 'red', alpha = 0.5)





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

first = min(panas.long$timestamp,na.rm = T)

tmp = data.frame(day = seq.Date(from = first,
                                to = ymd('2020-01-21'),
                                by = 'days'),
                 cases = 0,
                 daily.cases = 0)

covid.usa <-rbind(tmp, covid.usa)                 
head(covid.usa)
# truncate to match PANAS data
covid.usa = subset(covid.usa, day <= max(panas.long$timestamp, na.rm = T))

cov.window.plot = data.frame()

for(i in 1:(length(days)-2)){
     from = days[i]
     to = days[i] + days(10)
     days.tmp<-seq.Date(from = from,
                        to = to,
                        by = 'day')
     obs<-subset(covid.usa, day %in% days.tmp)
     row = data.frame(day = days[i+1],
                      daily.av = mean(obs$daily.cases,na.rm = T),
                      n = nrow(obs))
     
     cov.window.plot =rbind(cov.window.plot,row)}


cov.window.plot$log.daily = log(cov.window.plot$daily.av)

caseplot = ggplot(cov.window.plot, aes(x = day, y = log.daily)) +
     geom_line() +
     geom_vline(xintercept=as.numeric(window.plotdata$day[80]-days(1)),
                colour="blue")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[83]-days(2)),
                colour="red")+
     geom_vline(xintercept=as.numeric(window.plotdata$day[135]-days(1)),
                colour="green") + 
     xlab('') + ylab('ln(Daily Case Count) (USA)')


window = plot_grid(sample_size, pa.window,na.window,caseplot, nrow = 4, ncol = 1, align = 'hv')
window
ggsave(filename = 'sliding.window.panas.covid.USA.png',window,width = 8,height = 11, units = c('in'))






