setwd("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022\\Datasets")

library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(data.table)

stp = fread('staypoint.full.10.21.22.csv')
# append SVIDs to stp
svids = read_xlsx('id_mapping_reduced.xlsx')
names(svids)[2] = 'user_id'

covid = fread('covid_daily_time_series_8_12_US_longform.csv')


# compute pct change in covid cases per day and per week




stpcovid = fread('staypoints.covid.11.16.22.csv')
head(stpcovid)

# save juan dataset
stpcovid2 = left_join(stpcovid, svids[,c(1,2)]) %>% select(!c(user_id)) 
stpjuan = stpcovid2 %>% select(c(SVID,latitude,longitude, arvT,levT,date,county,fips))
fwrite(stpjuan, 'staypoints.fips.coded.9.19.23.csv')

# set all cases before 1-22-2020 to 0

stpcovid$daily.cases[stpcovid$date <= ymd('2020-01-22')] = 0
sum(is.na(stpcovid$daily.cases))


tmp = stpcovid[which(is.na(stpcovid$daily.cases)),]
sum(is.na(tmp$fips))
# remaining ~20,000 NAs on daily cases are outside the country, let's remove these
stpcovid = stpcovid[which(!is.na(stpcovid$daily.cases)),] 

# let's do some sanity checks on daily cases
max(stpcovid$daily.cases)
min(stpcovid$daily.cases)
summary(stpcovid$daily.cases)
hist(stpcovid$daily.cases)
hist(stpcovid$daily.cases[stpcovid$daily.cases > 0])
hist(stpcovid$daily.cases[stpcovid$daily.cases > 100])
length(stpcovid$daily.cases[stpcovid$daily.cases > 100])/
     nrow(stpcovid[stpcovid$daily.cases > 0])
#38% of days with any cases have more than 100 cases


hist(stpcovid$daily.cases[stpcovid$daily.cases > 1000])
length(stpcovid$daily.cases[stpcovid$daily.cases > 1000])/
     nrow(stpcovid[stpcovid$daily.cases > 0])
#25% of days with any cases have more than 1000


hist(stpcovid$daily.cases[stpcovid$daily.cases > 5000])
length(stpcovid$daily.cases[stpcovid$daily.cases > 5000])/
     nrow(stpcovid[stpcovid$daily.cases > 0])
#0.2% of days with any cases have more than 5000. 


# very, very right skewed. most counties are ~0 most of the time. 


# Now let's add hospitalizations etc. 

covid.other = fread('County_Year_Week.csv')

# merge by fips code and week of the year

covid.other$Year_Week = paste(covid.other$Year, 
                              covid.other$Week,
                              sep = '_')

names(covid.other)[4] = 'fips'

stpcovid$Year_Week = paste(year(stpcovid$date),
                           week(stpcovid$date),
                           sep = '_')
head(stpcovid)

stpcovid = left_join(stpcovid, covid.other)


stpcovid = stpcovid %>% select(!c(OBJECTID,County_Year_Week,
                                  First_year,Year,Week,
                                  SVI,cases,State))

fwrite(stpcovid,'staypoints.covid.11.16.22.csv')


# compute spatial variables

degreesToRadians <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}

distanceInKmBetweenEarthCoordinates <- function(lat1, lon1, lat2, lon2) {
  earthRadiusKm <- 6371
  dLat <- degreesToRadians(lat2-lat1)
  dLon <- degreesToRadians(lon2-lon1)
  lat1 <- degreesToRadians(lat1)
  lat2 <- degreesToRadians(lat2)
  a <- (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2))
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- earthRadiusKm * c
  return(distance)
}



#######################
#Compute DSB Variables#
#######################

# compute mode

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

#daily distance function

degreesToRadians <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}

distanceKm <- function(lat1, lon1, lat2, lon2) {
  earthRadiusKm <- 6371
  dLat <- degreesToRadians(lat2-lat1)
  dLon <- degreesToRadians(lon2-lon1)
  lat1 <- degreesToRadians(lat1)
  lat2 <- degreesToRadians(lat2)
  a <- (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2))
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- earthRadiusKm * c
  return(distance)
}


dailydistance<-function(data)
  {
    if (nrow(data)<2){
      ddistance<-NA
      return(ddistance)
    }
    data<-data[order(data$arvT),]
    distance<-distanceKm(data$latitude[1:(nrow(data)-1)],data$longitude[1:(nrow(data)-1)],
                         data$latitude[2:nrow(data)], data$longitude[2:nrow(data)])
    ddistance<-sum(distance)
    return(ddistance)
    
  }




# return dataframe of location measures

users<-unique(stpcovid$user_id)
days<-unique(stpcovid$date)

i = users[1]
j = days[10]
dat2 = data.frame()
for (i in users){
    temp<-stpcovid[stpcovid$user_id == i,]
  
    for (j in days)
      {
        daypts<- temp[temp$date == j,]
        
        if(nrow(daypts)==0)
          {
            next
          }
        
        row<-data.frame(user_id = i, 
                        date = j, 
                        numpts = nrow(daypts), 
                        distance = dailydistance(daypts),
                        daily.cases = mean(daypts$daily.cases,na.rm = T),
                        county = getmode(daypts$county),
                        Year_Week = getmode(daypts$Year_Week),
                        fips = getmode(daypts$fips))
        
        #if(which(users==i)==1)
         # {
          #  dat2<-row
     #     }else
      #      {
              dat2<-rbind(dat2, row)
       #     }
      }
    print(which(users==i))  
      
    
  }

fwrite(dat2, 'mobility.measures.covid.cases.11.16.22.csv')
#dat2 = fread('mobility.measures.covid.cases.11.16.22.csv')


dat2 = fread('mobility.measures.covid.cases.11.16.22.csv')
tmp = left_join(dat2, covid.other)


tmp = tmp %>% select(!c(OBJECTID,County_Year_Week,
                        First_year,Year,Week,
                        SVI,State))

tail(tmp)

dat2 = tmp
fwrite(dat2, 'mobility.measures.covid.cases.11.16.22.csv')



### Add Affect to Dataset ###

panas.long<-read.csv('PANASphone.long.csv')
panas.long$date = as_date(panas.long$SUBMIT_DATE)

panas.long$Year_Week = paste(year(panas.long$date),
                             week(panas.long$date),
                             sep = '_')

# need to append SVIDs to dat2

ids = read_excel('ID_mapping_test.xlsx')
# one id is duplicated, remove
ids = ids[c(1:680, 682:902),]
names(ids)[2] = 'user_id'
tmp = left_join(dat2, ids)
dat2 = tmp
rm(tmp)


# append affect to dat2
#remove userid from panas
panas.long = panas.long %>% select(!c('user_id'))
names(panas.long)[6] = 'survey_date'

# average multiple surveys from same week
panas.long = panas.long %>% group_by(SVID, Year_Week) %>%
     summarise(bestzygos = getmode(bestzygos),
               survey_date = first(survey_date),
               pa.av = mean(pa.av, na.rm = T),
               na.av = mean(na.av, na.rm = T))



tmp = left_join(dat2, panas.long)
dat2 = tmp
rm(tmp)

fwrite(dat2,'mobility.affect.covid.11.16.22.csv')


# scale daily case count by population

pop = read.csv('co-est2021-alldata.csv')
dat2 = fread('mobility.affect.covid.11.16.22.csv')

# make fips codes

# add 0s in front of county vals less than 3 digits long
pop$COUNTY = as.character(pop$COUNTY)

for(i in 1:nrow(pop)){
     if(nchar(pop$COUNTY[i]) == 3){next}
     if(nchar(pop$COUNTY[i]) ==2){pop$COUNTY[i] = paste0('0',pop$COUNTY[i])}
     if(nchar(pop$COUNTY[i]) ==1){pop$COUNTY[i] = paste0('00',pop$COUNTY[i])}}

pop$fips = paste0(pop$STATE,pop$COUNTY)

pop = pop %>% select(c(fips,POPESTIMATE2020)) %>% mutate(fips = as.integer(fips))

dat2 = left_join(dat2,pop)     


# report daily cases as daily cases per 100k people

dat2$daily.cases.per100k = dat2$daily.cases*100000/dat2$POPESTIMATE2020
dat2$date = as_date(dat2$date, origin = ymd('1970-01-01'))
dat2 = dat2 %>% select(!c(weekly.cases.per100k))

tmp = subset(dat2, date > ymd('2020-06-01'))
head(tmp)
dat2$distance[dat2$numpts == 1] = 0
dat2 = subset(dat2, dat2$distance < 1000)



# compute weekly average cases per 100k

# creating long county level covid dataset since apparently I haven't done this yet
covid<-read.csv('time_series_covid19_confirmed_8_12_US.csv') %>%
     gather(date, cases, 12:length(names(covid)))

covid$date = gsub('X','',covid$date)
covid$date = mdy(covid$date)
covid$YEAR_WEEK = paste(year(covid$date),
                        week(covid$date),
                        sep = '_')

# compute daily cases
covid2 = covid[order(covid$FIPS,covid$date),]
covid2$firstobs = NA
covid2$firstobs[covid2$FIPS == lag(covid2$FIPS)] = F
covid2$firstobs[covid2$FIPS != lag(covid2$FIPS)] = T
covid2$firstobs[1] = T


# calculate interval
covid2$daily.cases = covid2$cases - lag(covid2$cases)
covid2$daily.cases[covid2$firstobs == T] = NA
covid3 = covid2 %>% select(c('FIPS','Combined_Key','date','cases','firstobs','YEAR_WEEK','daily.cases'))
covid3 = covid3[is.na(covid3$FIPS) == F,]
covid3$daily.cases[covid3$daily.cases < 0] = 0
covid3$daily.cases[covid3$firstobs == TRUE] = 0
fwrite(covid3,'covid.long.county.8.12.csv')




# compute weekly averages by fips code
tmp3 = covid3 %>% group_by(FIPS, YEAR_WEEK) %>% 
     summarise(weekly.cases = sum(daily.cases))

tmp3$YEAR_WEEK = gsub('2020_1$','2020_01',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2020_2$','2020_02',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2020_3$','2020_03',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2020_4$','2020_04',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2020_5$','2020_05',tmp3$YEAR_WEEK)

tmp3$YEAR_WEEK = gsub('2021_1$','2021_01',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2021_2$','2021_02',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2021_3$','2021_03',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2021_4$','2021_04',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2021_5$','2021_05',tmp3$YEAR_WEEK)

tmp3$YEAR_WEEK = gsub('2022_1$','2022_01',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2022_2$','2022_02',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2022_3$','2022_03',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2022_4$','2022_04',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('2022_5$','2022_05',tmp3$YEAR_WEEK)


tmp3$YEAR_WEEK = gsub('_6','_06',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('_7','_07',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('_8','_08',tmp3$YEAR_WEEK)
tmp3$YEAR_WEEK = gsub('_9','_09',tmp3$YEAR_WEEK)


tmp3 = tmp3[order(tmp3$FIPS,tmp3$YEAR_WEEK),]

tmp3$firstobs = NA
tmp3$firstobs[tmp3$FIPS == lag(tmp3$FIPS)] = F
tmp3$firstobs[tmp3$FIPS != lag(tmp3$FIPS)] = T
tmp3$firstobs[1] = T

tmp3$pct.change.weekly = (tmp3$weekly.cases + 1  - (lag(tmp3$weekly.cases) + 1))/
     (lag(tmp3$weekly.cases)+1)

tmp3$pct.change.weekly[tmp3$firstobs == T] = NA
names(tmp3)

# append these to the main dataset
dat3 = left_join(dat2, tmp3)

# scale weekly cases by 100k
dat3$cases.weekly.per.100k = dat3$cases.weekly/dat3$POPESTIMATE2020 *100000

fwrite(dat3,'mobility.affect.covid.11.16.22.csv')








     
     
     
     
     



