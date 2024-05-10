setwd("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022\\Datasets")
library(dplyr)
library(lubridate)
library(data.table)
library(lme4)
library(lmerTest)
library(readr)

# load data (see commented out section for changes from 5 19 data)
#dat = fread('mobility.affect.covid.11.3.23', data.table =  F)
# set to work with to preserve original
#dat2 = dat
#dat = fread('mobility.affect.covid.5.19.23.csv', data.table =  F)
# reduce to model-relevant variables and clean up some earlier merge issues
# dat.model = dat %>% select(SVID,family,bestzygos,
#                            birth_date,age,sex,OS,OS.version,
#                            date,Year_Week,Month_Year = my,Month_Day = md,month,
#                            wkday,wknd,season,days.post.covid,
#                            numpts,distance,pa.av,na.av,
#                            fips,state,county,POPESTIMATE2020,area,pop.density,
#                            daily.cases,daily.cases.per100k,
#                            weekly.cases = cases.weekly,
#                            weekly.cases.per.100k = cases.weekly.per.100k) %>%
#      unique() # get rid of duplicated cases introduced during previous data merge

# save new cleaned up dataset
#fwrite(dat.model, 'mobility.affect.covid.11.3.23.csv')

dat = fread('mobility.affect.covid.missingness.11.3.23.csv', data.table =  F)
dat2 = dat %>% select(c(SVID,date)) %>% unique()

dat2$date = ymd(dat2$date)

### Report percent of missing days per participant ###
missdays = dat2  %>% group_by(SVID) %>% 
     summarise(first = min(date, na.rm = T),
               last = max(date, na.rm = T),
               n = n()) %>% filter(SVID != '')

missdays$maxdays = missdays$last - missdays$first + 1

# what proportion of possible days are missing across the whole sample?

1 - sum(missdays$n)/sum(as.numeric(missdays$maxdays))

summary(missdays$n)
sd(missdays$n)



missdays$pctmissing = 1- missdays$n/as.numeric(missdays$maxdays)
summary(missdays$pctmissing)

# median participant is missing 39.9% of days, first quartile missing 23.6%, 3rd missing 62.0%
hist(missdays$pctmissing)
# right skewed, and kurtotic


# interval length
setorder(dat2, SVID, date)
dat2 = dat2[is.na(dat$SVID) == F,]

# flag first observation for each participant
dat2$firstobs = NA
dat2$firstobs[dat2$SVID == lag(dat2$SVID)] = F
dat2$firstobs[dat2$SVID != lag(dat2$SVID)] = T
dat2$firstobs[1] = T


# calculate interval
dat2$interval = dat2$date - lag(dat2$date)
dat2$interval[dat2$firstobs == T] = NA

summary(as.numeric(dat2$interval))

length(dat2$interval[dat2$interval == 1])/length(dat2$interval)
# 85% of intervals are only 1 day apart
length(dat2$interval[dat2$interval <= 2])/length(dat2$interval)
# 94.8% 2 days apart or less
length(dat2$interval[dat2$interval <= 7])/length(dat2$interval)
# 99.0% a week apart or less


# average and maximum intervals apart
tmp = dat2 %>% subset(firstobs == F) %>% 
     group_by(SVID) %>% 
     summarise(mean.int = mean(interval, na.rm = T),
               median.int = median(interval, na.rm = T),
               max.int = max(interval, na.rm = T))

tmp = filter(tmp, SVID != '')
head(tmp)
mean(tmp$mean.int)
summary(tmp$mean.int)
# median participant has a mean interval of 1.7 days, 1st quartile of 1.314 and 3rd quartile of 2.65
# average participant has a mean interval of 5.9 days, skewed by outlying participants
hist(as.numeric(tmp$mean.int))

mean(tmp$max.int)
#average participant has a max interval of 246.9 days.
# reflects many participants being rerecruited into the study after a period of non-participation. 




# # what if we restrict to period where we have affect data?
# 
# min(dat2$date[is.na(dat2$pa.av) == F])
# dat.tmp = dat2[dat2$date >= ymd('2019-02-05'),]
# 
# 
# ### Report percent of missing days per participant ###
# missdays = dat.tmp %>% group_by(user_id) %>% 
#      summarize(first = min(date, na.rm = T),
#                last = max(date, na.rm = T),
#                n = n())
# 
# missdays$maxdays = missdays$last - missdays$first + 1
# 
# missdays$pctmissing = 1- missdays$n/as.numeric(missdays$maxdays)
# summary(missdays)
# # average participant is missing 39.2% of days, first quartile missing 17.6%, 3rd missing 57.3%
# hist(missdays$pctmissing)
# # right skewed, and kurtotic
# 
# 
# # interval length
# setorder(dat.tmp, user_id, date)
# dat.tmp = dat.tmp[is.na(dat.tmp$user_id) == F,]
# 
# # flag first observation for each participant
# dat.tmp$firstobs = NA
# dat.tmp$firstobs[dat.tmp$user_id == lag(dat.tmp$user_id)] = F
# dat.tmp$firstobs[dat.tmp$user_id != lag(dat.tmp$user_id)] = T
# dat.tmp$firstobs[1] = T
# 
# 
# # calculate interval
# dat.tmp$interval = dat.tmp$date - lag(dat.tmp$date)
# dat.tmp$interval[dat.tmp$firstobs == T] = NA
# 
# summary(as.numeric(dat.tmp$interval))
# 
# length(dat.tmp$interval[dat.tmp$interval == 1])/length(dat.tmp$interval)
# # 85% of intervals are only 1 day apart
# length(dat.tmp$interval[dat.tmp$interval <= 2])/length(dat.tmp$interval)
# # 94% 2 days apart or less
# length(dat.tmp$interval[dat.tmp$interval <= 7])/length(dat.tmp$interval)
# # 99% a week apart or less
# 
# 
# # average and maximum intervals apart
# tmp = dat.tmp %>% subset(firstobs == F) %>% 
#      group_by(user_id) %>% 
#      summarise(mean.int = mean(interval, na.rm = T),
#                median.int = median(interval, na.rm = T),
#                max.int = max(interval, na.rm = T))
# 
# head(tmp)
# mean(tmp$mean.int)
# # average participant has a mean interval of 5 days, skewed by outlying participants
# hist(as.numeric(tmp$mean.int))
# 
# mean(tmp$max.int)
# #average participant has a max interval of greater than 101 days, right skewed
# hist(as.numeric(tmp$max.int))
# summary(as.numeric(tmp$max.int))


# missing affect data
affect.data = fread('PANASphone.07.28.22.long.csv')

dat3 = affect.data %>% mutate(date = ymd(timestamp)) %>% select(c(SVID,date,pa.av)) %>% filter(is.na(pa.av) == F) %>% unique()
### Report percent of missing days per participant ###
missdays.pa = dat3  %>% group_by(SVID) %>% 
     summarise(first = min(date, na.rm = T),
               last = max(date, na.rm = T),
               n = n()) %>% filter(SVID != '')

missdays.pa$maxdays = ceiling((missdays.pa$last - missdays.pa$first + 1)/14)
missdays.pa$n[missdays.pa$n > as.numeric(missdays.pa$maxdays)] =
     as.numeric(missdays.pa$maxdays[missdays.pa$n > as.numeric(missdays.pa$maxdays)])

# what proportion of possible days are missing across the whole sample?

sum(missdays.pa$n)
summary(missdays.pa$n)

1 - sum(missdays.pa$n)/sum(as.numeric(missdays.pa$maxdays))





missdays.pa$pctmissing = 1- missdays.pa$n/as.numeric(missdays.pa$maxdays)
summary(missdays.pa$pctmissing)



dat3 = affect.data %>% mutate(date = ymd(timestamp)) %>% select(c(SVID,date,na.av)) %>% filter(is.na(na.av) == F) %>% unique()
### Report percent of missing days per participant ###
missdays.na = dat3  %>% group_by(SVID) %>% 
     summarise(first = min(date, na.rm = T),
               last = max(date, na.rm = T),
               n = n()) %>% filter(SVID != '')

missdays.na$maxdays = ceiling((missdays.na$last - missdays.na$first + 1)/14)
missdays.na$n[missdays.na$n > as.numeric(missdays.na$maxdays)] =
     as.numeric(missdays.na$maxdays[missdays.na$n > as.numeric(missdays.na$maxdays)])

# what proportion of possible days are missing across the whole sample?

sum(missdays.na$n)
summary(missdays.na$n)

1 - sum(missdays.na$n)/sum(as.numeric(missdays.na$maxdays))





missdays.na$pctmissing = 1- missdays.na$n/as.numeric(missdays.na$maxdays)
summary(missdays.na$pctmissing)




# association between missingness and outcome variables


# append missingness
pa.missing = missdays.pa %>% select(SVID,pa.miss = pctmissing) %>% mutate(pa.miss = 100*pa.miss)
na.missing = missdays.na %>% select(SVID,na.miss = pctmissing) %>% mutate(na.miss = 100*na.miss)
geo.missing = missdays %>% select(SVID,loc.miss = pctmissing) %>% mutate(loc.miss = 100*loc.miss)

dat.lmer = dat %>%
     mutate(rel_age = as.numeric(ymd(birth_date))) %>% 
     mutate(rel_age = (rel_age - min(rel_age,na.rm = T))/365.25) %>% 
     left_join(pa.missing) %>% left_join(na.missing) %>% left_join(geo.missing) %>%
     select(SVID,family,bestzygos,
            birth_date,age,relative.age = rel_age,sex,OS,OS.version,
            date,Year_Week,Month_Year,Month_Day,month,
            wkday,wknd,season,days.post.covid,
            pa.prop.miss=pa.miss,na.prop.miss=na.miss,loc.prop.miss=loc.miss, 
            numpts,distance,pa.av,na.av,
            fips,state,county,POPESTIMATE2020,area,pop.density,
            daily.cases,daily.cases.per100k,
            weekly.cases,
            weekly.cases.per.100k)

#fwrite(dat.lmer, 'mobility.affect.covid.missingness.11.3.23.csv')

# save missing

# missingness correlations
miscors = geo.missing %>% left_join(pa.missing) %>% left_join(na.missing)
cor(miscors[,2:4], use = 'pairwise.complete.obs')


# missingness models
sink('C:/Users/alexa877/Documents/UMN/Research/Covid Location project/RAship 2022/missingness.outputs.txt')
# covariates
geo.miss.mod = lm(loc.miss ~ sex + OS + rel_age + pop.density, data = dat.lmer)
summary(geo.miss.mod)
# male participants had 2.1% less missing data.
# android users had 44% more missing data
# older participants had 0.40% less missing data per year of additional age relative to their peers
aff.miss.mod = lm(pa.miss ~ sex + OS + rel_age + pop.density, data = dat.lmer)
summary(aff.miss.mod)
# male participants had 2.8% more missing data
# iOS users had -0.96% less missing data
# Older participants had 1.15% less missing data per year of age relative to their peers.

# numpts
mod.pts.loc = lmer(numpts ~ (1|family/SVID) + loc.miss + OS +sex +rel_age + pop.density, data = dat.lmer)
mod.pts.aff = lmer(numpts ~ (1|family/SVID) + pa.miss + OS + sex + rel_age + pop.density, data = dat.lmer)
summary(mod.pts.loc)
anova(mod.pts.loc)
# A 1% increase in location missingess reduces estimated locations per day by .01
summary(mod.pts.aff)
# missing affect data is unrelated to locations per day

# distance
mod.dist.loc = lmer(distance ~ (1|family/SVID) + loc.miss + OS +sex +rel_age + pop.density, data = dat.lmer)
mod.dist.aff = lmer(distance ~ (1|family/SVID) + pa.miss + OS + sex + rel_age + pop.density, data = dat.lmer)
summary(mod.dist.loc)
# A 1% increase in location missingess reduces estimated daily travel distance by 0.11 km/day
summary(mod.dist.aff)
# missing affect data is not significantly associated with to daily travel distance 


# pa.av
mod.pa.loc = lmer(pa.av ~ (1|family/SVID) + loc.miss + OS +sex +rel_age + pop.density, data = dat.lmer)
mod.pa.aff = lmer(pa.av ~ (1|family/SVID) + pa.miss + OS + sex + rel_age + pop.density, data = dat.lmer)
summary(mod.pa.loc)
# location missingness is not significantly associated with pa.av
summary(mod.pa.aff)
# missing affect data is n.s. w/ pa.av

# na.av
mod.na.loc = lmer(na.av ~ (1|family/SVID) + loc.miss + OS +sex +rel_age + pop.density, data = dat.lmer)
mod.na.aff = lmer(na.av ~ (1|family/SVID) + pa.miss + OS + sex + rel_age + pop.density, data = dat.lmer)
summary(mod.na.loc)
# location missingness is not significantly associated with pa.av
summary(mod.na.aff)
# missing affect data is n.s. w/ pa.av
sink()




# attrition analyses
dems <- read_csv("cotwins_demographics_reduced_long_9_19_23.csv")

dat = dat %>% select(!c(birth_date)) %>% left_join(dems)

# demographic effects
dat.dems = dat %>% 
     select(c(SVID,family,age_at_intake,sex,OS,loc.prop.miss,pa.prop.miss,
              relative.age,hispcombo,racecombo,incomecombo,educombo)) %>%
     unique()

dat.dems$OS = factor(dat.dems$OS, levels = c('Android','iOS'))

dat.dems = dat.dems %>% 
     mutate(income.cont = case_when(incomecombo == '<$20k' ~ 10,
                                    incomecombo == '$21K-$30K' ~ 25,
                                    incomecombo == '$31K-$40K' ~ 35,
                                    incomecombo == '$41K-$60K' ~ 50,
                                    incomecombo == '$61K-$80K' ~ 70,
                                    incomecombo == '$81K-100K' ~ 90,
                                    incomecombo == '$100K-150K' ~ 125,
                                    incomecombo == '>150K' ~ 200))

# are older participants more likely to have missing data?
mod.age.loc = lmer(loc.prop.miss ~ relative.age + (1|family), data = dat.dems)
mod.age.pa = lmer(pa.prop.miss ~ relative.age + (1|family), data = dat.dems)
summary(mod.age.loc)
summary(mod.age.pa)
# older participants had significantly less missing PA and loc data

# effect of sex on missing data
mod.sex.loc = lmer(loc.prop.miss ~ sex + (1|family), data = dat.dems)
mod.sex.pa = lmer(pa.prop.miss ~ sex + (1|family), data = dat.dems)
summary(mod.sex.loc)
summary(mod.sex.pa)
# male participants had significantly more missing location and affect data

# effect of OS on missing data
mod.OS.loc = lmer(loc.prop.miss ~ OS + (1|family), data = dat.dems)
mod.OS.pa = lmer(pa.prop.miss ~ OS + (1|family), data = dat.dems)
summary(mod.OS.loc)
summary(mod.OS.pa)
# iOS participants had significantly less missing location and affect data


# effect of OS on missing data
mod.hisp.loc = lmer(loc.prop.miss ~ hispcombo + (1|family), data = dat.dems)
mod.hisp.pa = lmer(pa.prop.miss ~ hispcombo + (1|family), data = dat.dems)
summary(mod.hisp.loc)
summary(mod.hisp.pa)
# hispanic participants had significantly less missing location data


# effect of income on missing data
mod.income.loc = lmer(loc.prop.miss ~ income.cont + (1|family), data = dat.dems)
mod.income.pa = lmer(pa.prop.miss ~ income.cont + (1|family), data = dat.dems)
summary(mod.income.loc)
summary(mod.income.pa)
# every 1000 dollar increase in income is associated with a 0.1% decline in missing loc data and affect data.









