setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Datasets")
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(gamm4)
library(stringr)


#load data

dat = fread('mobility.affect.covid.11.16.22.csv',data.table = F) %>%
     select(c(user_id,SVID,fips, date, numpts,distance, daily.cases,county, Year_Week, 
              bestzygos,pa.av,na.av,daily.cases.per100k, 
              cases.weekly, cases.weekly.per.100k,POPESTIMATE2020))

dat$family = substr(dat$SVID, start = 1, stop = 6)

dat$cases.weekly[dat$date < ymd('2020-01-22')] = 0
dat$cases.weekly.per.100k[dat$date < ymd('2020-01-22')] = 0

#per Kelly's paper (Hu et al., 2021) 
#let's model mobility via GAMMs with smooth terms associated with 
# days since the start of the pandemic, day of the week, weekend, county, and state
# and parametric coefficients associated w/ new county cases, national new cases, season
# county population density, gender, age, and median income

# first let's compute the remaining variables here
dat$date = ymd(dat$date)
# days since start of pandemic (2020-01-22)
dat$days.post.covid = ymd(dat$date) - ymd('2020-01-22')
dat$days.post.covid = as.numeric(dat$days.post.covid)

# day of the week
dat$wkday = wday(ymd(dat$date)) %>% factor(levels = 1:7, labels = c('Sunday','Monday',
                                                                    'Tuesday','Wednesday',
                                                                    'Thursday','Friday',
                                                                    'Saturday'))

# weekend
dat$wknd = NA
dat$wknd[as.numeric(dat$wkday) != 1|as.numeric(dat$wkday) != 7] = 0
dat$wknd[as.numeric(dat$wkday) == 1|as.numeric(dat$wkday) == 7] = 1
dat$wknd = factor(dat$wknd,levels = 0:1, labels = c('weekday','weekend'))

# state
dat$state = str_extract(dat$county, '.+,')
dat$state = gsub(',','',dat$state)

# season
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}
dat$season = getSeason(dat$date)
head(dat)

# age
dems = fread('CoTwinsDemographics.JA.add.dates.081722.csv', data.table = F) %>%
     select(c(SVID,birth_date))

dat = left_join(dat,dems)
dat$age = as.numeric(dat$date - ymd(dat$birth_date))/365.25

# sex
dat$sex = substr(dat$SVID,start =8, stop = 8) %>% factor(levels = c(0,1), 
                                                         labels = c('female','male'))


# population density
area = readxl::read_xls('LND01.xls') %>% select(STCOU,LND110210D)
names(area) = c('fips', 'area')
area[area$area == 0,]

area$area[area$fips == 30113] = 244.95
area$area[area$fips == 51560] = 3.07
area$area[area$fips == 51780] = 5.55
summary(area$area)

area$fips = as.integer(area$fips)
dat = left_join(dat, area)
dat$pop.density = dat$POPESTIMATE2020/dat$area
summary(dat$pop.density)
nrow(dat[dat$pop.density == Inf,])
summary(dat$POPESTIMATE2020)
# % republican votes

president = read.csv('countypres_2000-2020.csv') %>% subset(year == 2020) %>%
     mutate(vote.pct = candidatevotes/totalvotes)

repubs = president %>% subset(party == 'REPUBLICAN') %>%
     select(county_fips, vote.pct)

names(repubs) = c('fips','pct.repub.votes.2020')

dat = left_join(dat,repubs)
head(dat)

# smooths: time index, weekday, county, state
# parametric coefficients: weekly covid cases per 100k, sex, age, season, operating system
# population density, % republican voters, weekend/weekday  

# add operating system
osdat = read.csv('OS-byMonth-Kelly.csv', colClasses= c('numeric','character','character'))
dat$my = paste0(month(dat$date),'-',year(dat$date))
dat$user_id = as.numeric(dat$user_id)
length(setdiff(dat$user_id,osdat$user_id))

dat = left_join(dat,osdat)

dat$case.count.time.interaction = dat$cases.weekly.per.100k * dat$days.post.covid

write.csv(dat, 'mobility.affect.covid.1.26.22.csv')

# create version with scaled smooth variables
dat2 = dat
dat2$cases.weekly.per.100k = scale(dat$cases.weekly.per.100k)
dat2$days.post.covid = scale(dat2$days.post.covid)
dat2$case.count.time.interaction = dat2$cases.weekly.per.100k * dat2$days.post.covid
write.csv(dat2, 'mobility.affect.covid.scaled.smoothvars.1.26.22.csv')






