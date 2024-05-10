# append all the location data files together and then clean the data
library(data.table)

filenames = c(
     'userLocation_2015-01-01_2015-01-31.csv',  'userLocation_2018-03-01_2018-03-31.csv',
     'userLocation_2015-02-01_2015-02-28.csv',  'userLocation_2018-04-01_2018-04-30.csv',
     'userLocation_2015-03-01_2015-03-31.csv',  'userLocation_2018-05-01_2018-05-31.csv',
     'userLocation_2015-04-01_2015-04-30.csv',  'userLocation_2018-06-01_2018-06-30.csv',
     'userLocation_2015-05-01_2015-05-31.csv',  'userLocation_2018-07-01_2018-07-31.csv',
     'userLocation_2015-06-01_2015-06-30.csv',  'userLocation_2018-08-01_2018-08-31.csv',
     'userLocation_2015-07-01_2015-07-31.csv',  'userLocation_2018-09-01_2018-09-30.csv',
     'userLocation_2015-08-01_2015-08-31.csv',  'userLocation_2018-10-01_2018-10-31.csv',
     'userLocation_2015-09-01_2015-09-30.csv',  'userLocation_2018-11-01_2018-11-30.csv',
     'userLocation_2015-10-01_2015-10-31.csv',  'userLocation_2018-12-01_2018-12-31.csv',
     'userLocation_2015-11-01_2015-11-30.csv',  'userLocation_2019-01-01_2019-01-31.csv',
     'userLocation_2015-12-01_2015-12-31.csv',  'userLocation_2019-02-01_2019-02-28.csv',
     'userLocation_2016-01-01_2016-01-31.csv',  'userLocation_2019-03-01_2019-03-31.csv',
     'userLocation_2016-02-01_2016-02-29.csv',  'userLocation_2019-04-01_2019-04-30.csv',
     'userLocation_2016-03-01_2016-03-31.csv',  'userLocation_2019-05-01_2019-05-31.csv',
     'userLocation_2016-04-01_2016-04-30.csv',  'userLocation_2019-06-01_2019-06-30.csv',
     'userLocation_2016-05-01_2016-05-31.csv',  'userLocation_2019-07-01_2019-07-31.csv',
     'userLocation_2016-06-01_2016-06-30.csv',  'userLocation_2019-08-01_2019-08-31.csv',
     'userLocation_2016-07-01_2016-07-31.csv',  'userLocation_2019-09-01_2019-09-30.csv',
     'userLocation_2016-08-01_2016-08-31.csv',  'userLocation_2019-10-01_2019-10-31.csv',
     'userLocation_2016-09-01_2016-09-30.csv',  'userLocation_2019-11-01_2019-11-30.csv',
     'userLocation_2016-10-01_2016-10-31.csv',  'userLocation_2019-12-01_2019-12-31.csv',
     'userLocation_2016-11-01_2016-11-30.csv',  'userLocation_2020-01-01_2020-01-31.csv',
     'userLocation_2016-12-01_2016-12-31.csv',  'userLocation_2020-02-01_2020-02-29.csv',
     'userLocation_2017-01-01_2017-01-31.csv',  'userLocation_2020-03-01_2020-03-31.csv',
     'userLocation_2017-02-01_2017-02-28.csv',  'userLocation_2020-04-01_2020-04-30.csv',
     'userLocation_2017-03-01_2017-03-31.csv',  'userLocation_2020-05-01_2020-05-31.csv',
     'userLocation_2017-04-01_2017-04-30.csv',  'userLocation_2020-06-01_2020-06-30.csv',
     'userLocation_2017-05-01_2017-05-31.csv',  'userLocation_2020-07-01_2020-07-31.csv',
     'userLocation_2017-06-01_2017-06-30.csv',  'userLocation_2020-08-01_2020-08-31.csv',
     'userLocation_2017-07-01_2017-07-31.csv',  'userLocation_2020-09-01_2020-09-30.csv',
     'userLocation_2017-08-01_2017-08-31.csv',  'userLocation_2020-09-10_2021-02-04_v2 (1).csv',
     'userLocation_2017-09-01_2017-09-30.csv',  'userLocation_20210204_thru_20210331.csv',
     'userLocation_2017-10-01_2017-10-31.csv',  'userLocation_20210401_thru_20210630.csv',
     'userLocation_2017-11-01_2017-11-30.csv',  'userLocation_20210701_thru_20210930.csv',
     'userLocation_2017-12-01_2017-12-31.csv',  'userLocation_20211001_thru_20211231.csv',
     'userLocation_2018-01-01_2018-01-31.csv',  'userLocation_20220101_thru_20220418.csv',
     'userLocation_2018-02-01_2018-02-28.csv')


filenames = sort(filenames)
filenames = filenames[1:12]
filenames

df<-data.frame()
i = filenames[2]

for(i in filenames){
          print(i)
     tmp = tryCatch(expr = read.csv(i),
                    error=function(cond) {
            print('file not found')
            return(NA)}
            )
     if(is.na(tmp) == T) next
     names(tmp) = paste0('V',1:ncol(tmp))
     df = rbind(df,tmp)
     }

fwrite(df, 'locdatfull.9.15.csv')




# clean locdatfull



names(df)<-c('id','creation_date','user_id',
	     'latitude','longitude','sample_time',
             'accuracy','user_token','sample_timezone',
             'processed','timestamp_type','protocol_version')

df$longitude<-as.numeric(df$longitude)
df$accuracy<-as.numeric(df$accuracy)
df$sample_time<-ymd_hms(df$sample_time)

df$creation_date<-ymd_hms(df$creation_date)

#remove duplicated points, incomplete points, and points from the future:
nrow(df)

df.tmp<- df[!duplicated(select(df,c(user_id,latitude,longitude,sample_time,accuracy))),]
df.tmp<- subset(df.tmp,is.na(latitude) == F)
df.tmp<- subset(df.tmp,is.na(longitude)== F)
df.tmp<- subset(df.tmp, sample_time < creation_date)
df.tmp<-subset(df.tmp, accuracy < 500)

nrow(df.tmp)

nrow(df)-nrow(df.tmp)
rm(df)
#remove points that imply excessively large speed values (>700mph)


#haversine and archaversine formulas

havsin<-function(theta){sin(theta/2)^2}

arc.havsin<-function(hav){ 2*asin(sqrt(hav))}

#degrees to radians

rad<- function(theta){theta*pi/180}

#great circle distance formula between latitudes and longitudes for distances on earth (simplified, assumes earth is spherical) 

distance<- function(lat1, long1, lat2,long2){

       # converting latitudes and longitudes from degrees to radians
  
     lat1<-rad(lat1)
     lat2<-rad(lat2)
     long1<-rad(long1)
     long2<-rad(long2)
     
     #compute haversine of the central angle between the two locations
  
     hav.theta = havsin(lat2-lat1) + cos(lat1)*cos(lat2)*havsin(long2-long1)
     
     #compute the great circle distance from the haversine
     d = arc.havsin(hav.theta)*6371
     return(d)
     }

# compute speed function

speed<-function(lat1,long1,time1, lat2, long2, time2){
     d<-distance(lat1,long1,lat2,long2)
     time<-as.duration(time2-time1)
     time<-as.numeric(time)/3600
     speed<-d/(time)
     return(speed)
     }

     
# calculate speed for each point

df.tmp<-setorder(df.tmp,user_id, sample_time)

# check if this is the first entry for this participant
df.tmp$firstpoint<-NA
df.tmp$firstpoint[1]<-T
df.tmp$firstpoint[(df.tmp$user_id == shift(df.tmp$user_id, n = 1L, type = 'lag')) == F]<- T

# calculate speed
df.tmp$speed<-NA

df.tmp$distance<-distance(lat1 = shift(df.tmp$latitude, n = 1L, type = 'lag'),
                          long1= shift(df.tmp$longitude, n = 1L, type = 'lag'),
                          lat2 = df.tmp$latitude,
                          long2 = df.tmp$longitude)


df.tmp$speed<-speed(lat1 = shift(df.tmp$latitude, n = 1L, type = 'lag'),
                    long1= shift(df.tmp$longitude, n = 1L, type = 'lag'),
                    time1 = shift(df.tmp$sample_time, n = 1L, type = 'lag'),
                    lat2 = df.tmp$latitude,
                    long2 = df.tmp$longitude,
                    time2 = df.tmp$sample_time)


df.tmp[df.tmp$firstpoint == T,c('distance','speed')]<-NA

df.tmp<-subset(df.tmp, speed < 700) 
nrow(df.tmp)


fwrite(df.tmp,'userlocations.cleaned.9.15.22.csv')




