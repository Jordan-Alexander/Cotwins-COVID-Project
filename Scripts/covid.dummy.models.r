# Plot PANAS data over time
setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(dplyr)
library(lme4)

library(ggplot2)
library(lubridate)
library(cowplot)

panas.long<-read.csv('PANASphone.long.csv')
head(panas.long)
panas.long$timestamp = date(panas.long$SUBMIT_DATE)

# add family to PANAS:

panas.long$family = substr(panas.long$SVID, start = 1, stop = 6)


#### take a pre-post covid average

#### define covid as first date of covid case in united states

panas.long$covid.dummy = NA
panas.long$covid.dummy[panas.long$timestamp > ymd('2020-03-11')] = 1
panas.long$covid.dummy[panas.long$timestamp <= ymd('2020-03-11')] = 0

# check covid.dummy worked

cvd.avg = panas.long %>% group_by(timestamp) %>% summarise(dummy = mean(covid.dummy))

ggplot(data = cvd.avg) + geom_line(mapping = aes(x = timestamp, y = dummy))

# it did!



### test significance of covid.dummy on positive and negative affect with random individual and family intercepts

mod.pa = lmer(pa.av ~ covid.dummy + (1|SVID:family), data = panas.long)
summary(mod.pa)

mod.na = lmer(na.av ~ covid.dummy + (1|SVID:family), data = panas.long)
summary(mod.na)


### rerun restricting to November, 2019 - July, 2020
date.int = interval(ymd('2019-11-11'),ymd('2020-07-11'))
panas.long$interval = NA
panas.long$interval[panas.long$timestamp %within% date.int] = 1
panas.long$interval[!(panas.long$timestamp %within% date.int)] = 0

# check if interval worked
interval.plot = panas.long %>% group_by(timestamp) %>% summarise(interval = mean(interval))
ggplot(data = interval.plot) + geom_line(mapping = aes(x = timestamp, y = interval))
# it did

# rerun restricting to 4 months before and 4 months after covid
panas.tmp = panas.long[panas.long$interval == 1,]
nrow(panas.tmp)

mod.pa.4month = lmer(pa.av ~ covid.dummy + (1|SVID:family), data = panas.tmp)
summary(mod.pa.4month)

mod.na.4month = lmer(na.av ~ covid.dummy + (1|SVID:family), data = panas.tmp)
summary(mod.na.4month)

# effect sizes
-0.10180/sd(panas.tmp$pa.av, na.rm = T)
0.03190/sd(panas.tmp$na.av, na.rm = T)


### Now the same for mobility

move.long<-read.csv('mobility.measures.9.9.22.csv')
move.long$timestamp = as_date(move.long$date, origin = '1970-01-01')
head(move.long)
move.long$distance[move.long$numpts == 1] = 0


## add family and SVID
dems = read.csv('idmapper.3.10.csv')
dems$id2 = as.numeric(dems$ALT_ID)/10^19
move.long$id2 = move.long$user_id/10^19
panas.long$id2 = panas.long$user_id/10^19
dems2.tmp = panas.long %>% select(c(id2, SVID, family))

length(setdiff(move.long$id2,dems$id2))
length(setdiff(move.long$id2, dems2.tmp$id2))

nrow(move.long)
move.long <- left_join(move.long, dems)
nrow(move.long)
move.long$family = substr(move.long$SVID, start = 1, stop = 6)

length(unique(move.long$SVID))

# we're missing ~121 participants, look into this later

# make covid dummy
move.long$covid.dummy = NA
move.long$covid.dummy[move.long$timestamp > ymd('2020-03-11')] = 1
move.long$covid.dummy[move.long$timestamp <= ymd('2020-03-11')] = 0


# check covid.dummy worked

cvd.avg = move.long %>% group_by(timestamp) %>% summarise(dummy = mean(covid.dummy))

ggplot(data = cvd.avg) + geom_line(mapping = aes(x = timestamp, y = dummy))

# it did!



### test significance of covid.dummy on positive and negative affect with random individual and family intercepts




### test mobility on positive and negative affect


# first combine affect and mobility sets

head(move.long)
head(panas.long)

panas.tmp = panas.long %>% select(c(SVID, family, 
                                    covid.dummy, timestamp, 
                                    pa.av, na.av))

move.tmp = move.long %>% select(c(SVID, family, 
                                    covid.dummy, timestamp, 
                                    numpts, distance))

full.tmp = left_join(panas.tmp, move.tmp)



# test effect of mobility on affect
pa.pts = lmer(pa.av ~ covid.dummy + numpts + (1|SVID:family), data = full.tmp)
summary(pa.pts)

na.pts = lmer(na.av ~ covid.dummy + numpts + (1|SVID:family), data = full.tmp)
summary(na.pts)

pa.dst = lmer(pa.av ~ distance + covid.dummy + (1|SVID:family), data = full.tmp)
summary(pa.dst)

na.dst = lmer(na.av ~ distance + covid.dummy + (1|SVID:family), data = full.tmp)
summary(na.dst)


mod.dst = lmer(distance ~ covid.dummy + (1|SVID:family), data = move.long)
summary(mod.dst)



























