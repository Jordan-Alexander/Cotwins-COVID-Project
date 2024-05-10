setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(readxl)
library(dplyr)
library(lubridate)
library(psych)
library(ggplot2)

# load PANAS data
df<-read.csv('PANASphone.long.csv')


# load participant birthdays
dems<-read_xlsx('Wave2_paper_all.xlsx')
dems$bday<-paste(dems$Birth_Year,dems$Birth_Month,dems$Birth_Day, sep = '-')
dems2<-read.csv('dems.full.sample.zyg.2.8.csv')

dems.tmp<-dems %>% select(ID1, Sex1,bday)
dems.tmp$bday<-ymd(dems.tmp$bday)
dems.tmp2<-dems2 %>% select(SVID, Sex1, Birth_Date)
names(dems.tmp)<-names(dems.tmp2)
dems3<-rbind(dems.tmp,dems.tmp2)
dems3<-dems3[!duplicated(dems3$SVID),]


# append birthdays to PANAS
length(unique(dems3$SVID))
length(unique(df$SVID))


df<-left_join(df, unique(dems3))
df$date<-as_date(df$SUBMIT_DATE)
df<-df[!duplicated(df[,c('SVID','date')]),]


# remove surveys that took more than 5 minutes to fill out
#df<-subset(df, TOTAL_TIME < 300)
nrow(df)


# check reliability of PANAS
# first positive Affect
pa<-c('PANAS_3',
      'PANAS_5',
      'PANAS_7',
      'PANAS_8',
      'PANAS_10')

alpha(df[,pa])

na<-c('PANAS_1',
      'PANAS_2',
      'PANAS_4',
      'PANAS_6',
      'PANAS_9')

alpha(df[,na])

# plot histograms
pdf('affect.histograms.pdf')
par(mfrow = c(2,1))
hist(df$pa.av)
hist(df$na.av)

dev.off()

# convert affect to weekly wideform:
# positive affect
df$wy<-paste(week(df$SUBMIT_DATE),year(df$SUBMIT_DATE),sep = '-')
df.tmp.pa<-select(df, SVID, bestzygos,pa.av, wy)
df.tmp.pa$SVID<-as.factor(df.tmp.pa$SVID)

df.wide.pa<-tidyr::spread(df.tmp.pa[,c('SVID','pa.av','wy')], wy, pa.av)
df.wide.pa<-df.wide.pa[,c('SVID',intersect(weeks$wy,df.tmp.pa$wy))]
head(df.wide.pa)


# negative affect
df.tmp.na<-select(df, SVID, bestzygos,na.av, wy)
df.tmp.na$SVID<-as.factor(df.tmp.na$SVID)
df.wide.na<-tidyr::spread(df.tmp.na[,c('SVID','na.av','wy')], wy, na.av)
df.wide.na<-df.wide.na[,c('SVID',intersect(weeks$wy,df.tmp.na$wy))]
head(df.wide.na)


# compute ICC for pa.av
ICC(select(df.wide.pa, !c(SVID)))

# compute ICC for na.av
ICC(select(df.wide.na, !c(SVID)))



# sliding window plot:

df.swp<-group




# normative sample of PANAS data







# calculate variances over time (5 number summaries for each participant, and variance) 




















# spaghetti plot
weeks<-c(paste0(9:53,'-2019'),
         paste0(1:53,'-2020'),
         paste0(1:53,'-2021'),
         paste0(1:29,'-2022'))
weeks = data.frame(wy = weeks,
                   weeknum = 1:180)

df.tmp<-left_join(df.tmp, weeks)
df.tmp$weeknum = factor(df.tmp$weeknum, labels = intersect(weeks$wy,df.tmp$wy))



#overplotted like crazy, plot ~10 people

df.tmp.sub <-df.tmp[df.tmp$SVID %in% sample(unique(df.tmp$SVID), 10),]

splot.pa.sub<-ggplot(df.tmp.sub, aes(x = weeknum, y = pa.av, colour = SVID)) +
               geom_line() + theme(legend.position = 'none')










