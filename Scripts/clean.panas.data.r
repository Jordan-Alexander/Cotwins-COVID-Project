library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(psych)
library(stringr)
library(readxl)
library(ggplot2)
library(lubridate)
# Phone based checking in data
# Use RDS format, the long user IDs cause issues at numerous steps with CSVs
panas<-readRDS("C:/Users/alexa877/Documents/UMN/Research/Covid Location project/RAship 2022/Datasets/survey_PANAS_responses_20220728.rds")

# SVID and alt ID data 
id_mapping_long <- read_xlsx("C:/Users/alexa877/Documents/UMN/Research/Covid Location project/RAship 2022/Datasets/id_mapping_comp.xlsx")
id_mapping_long$user_id<-as.character(id_mapping_long$ALT_ID)

ct_panas_c_ids<-merge(id_mapping_long, panas, by = "user_id", all = T)
# drop folks without a token
ct_panas_c<-ct_panas_c_ids[complete.cases(ct_panas_c_ids[ , 5]),]
# drop folks without an SVID (haven't come into the lab yet)
ct_panas_c<-ct_panas_c[complete.cases(ct_panas_c[ , 2]),]
# drop rows where the survey wasn't opened yet (total survey time = 0 )
ct_panas_c <- ct_panas_c[ which(ct_panas_c$TOTAL_TIME!=0), ]

# add variable to denote survey method
# phone. 1 = yes, 0 = no (lab based)
#PANAS_lab_2$phone <- 0
ct_panas_c$phone <- 1

# How many obs per person 
table(ct_panas_c$SVID)

# Check for NAs
sapply(ct_panas_c, function(x) sum(is.na(x)))
# 13-18 have more NAs than the other because they were added later

# Cleaning and scoring PANAS
# 1 = very slightly, 2 = a little, 3 = moderately 
# 4 = quite a bit, 5 = extremely
# 6 = Prefer not to answer

## Scoring
# in PANAS, 6 = prefer not to answer, change to NA 
describe(ct_panas_c$PANAS_1)
names(ct_panas_c)


ct_panas_c[, c('PANAS_1','PANAS_2','PANAS_3',
               'PANAS_4','PANAS_5','PANAS_6',
               'PANAS_7','PANAS_8','PANAS_9',
               'PANAS_10','PANAS_11','PANAS_12',
               'PANAS_13','PANAS_14','PANAS_15',
               'PANAS_16','PANAS_17','PANAS_18')][ct_panas_c[, c('PANAS_1','PANAS_2','PANAS_3',
                                                                 'PANAS_4','PANAS_5','PANAS_6',
                                                                 'PANAS_7','PANAS_8','PANAS_9',
                                                                 'PANAS_10','PANAS_11','PANAS_12',
                                                                 'PANAS_13','PANAS_14','PANAS_15',
                                                                 'PANAS_16','PANAS_17','PANAS_18')] == 6] <- NA
describe(ct_panas_c$PANAS_1)



### Reliability, Scoring, & Descriptives
# 1 = very slightly, 2 = a little, 3 = moderately 
# 4 = quite a bit, 5 = extremely
# 6 = Prefer not to answer

# Positive Affect
#  Items: 3, 5, 7, 8, 10
paVARS<-c("PANAS_3",
          "PANAS_5",
          "PANAS_7",
          "PANAS_8",
          "PANAS_10"
          )

# Negative Affect
# Items: 1, 2, 4, 6, 9
naVARS<-c("PANAS_1",
          "PANAS_2",
          "PANAS_4",
          "PANAS_6",
          "PANAS_9"
          )

### Not NA row sums & means

row.sums.n <- function(vars, not.na=0) {
  apply(vars, 1, function(x) ifelse(sum(!is.na(x)) >= not.na, sum(x, na.rm=TRUE), NA))
}

row.means.n <- function(vars, not.na=0) {
  apply(vars, 1, function(x) ifelse(sum(!is.na(x)) >= not.na, mean(x, na.rm=TRUE), NA))
}

## Phone Based
# Phone based time stamp range:
# 02/05/2019 - 06/22/2021

# Count numbers of repeat observations by SVID
phone_count<-as.data.frame((table(ct_panas_c$SVID)))
knitr::kable(describe(phone_count$Freq,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(phone_count, aes(x=Freq))+
  geom_histogram(color="gray", fill="gray66")

# Time spent taking survey, no screening, pre-scoring
knitr::kable(describe(ct_panas_c$TOTAL_TIME,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c, aes(x=TOTAL_TIME))+
  geom_histogram(color="gray", fill="gray66")
# It looks like some people started the survey and picked it back up later or forgot to hit submit
# Potential issue to screen on?
# Time spent taking survey (under 300 seconds, arbitrary 5 minute cut off)
ct_panas_c_time_sub <- ct_panas_c[ which(ct_panas_c$TOTAL_TIME < 300), ]

knitr::kable(describe(ct_panas_c_time_sub$TOTAL_TIME,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c_time_sub, aes(x=TOTAL_TIME))+
  geom_histogram(color="gray", fill="gray66")

### Reliability, Scoring, & Descriptives
# All items for later checks
VARSc<-c("PANAS_1",
          "PANAS_2",
          "PANAS_3",
          "PANAS_4",
          "PANAS_5",
          "PANAS_6",
          "PANAS_7",
          "PANAS_8",
          "PANAS_9",
          "PANAS_10"
          )
ITEMSc<-ct_panas_c[VARSc]

# Positive Affect
#  Items: 3, 5, 7, 8, 10
paVARSc<-c("PANAS_3",
          "PANAS_5",
          "PANAS_7",
          "PANAS_8",
          "PANAS_10"
          )
paITEMSc<-ct_panas_c[paVARSc]
knitr::kable(describe(paITEMSc,type=2,fast=T),booktabs=TRUE,format="markdown")
psych::alpha(paITEMSc)





# Negative Affect
# Items: 1, 2, 4, 6, 9
naVARSc<-c("PANAS_1",
          "PANAS_2",
          "PANAS_4",
          "PANAS_6",
          "PANAS_9"
)
naITEMSc<-ct_panas_c[naVARSc]
knitr::kable(describe(naITEMSc,type=2,fast=T),booktabs=TRUE,format="markdown")
alpha(naITEMSc)



# average alpha by assessment number

ct_panas_c = ct_panas_c %>% group_by(SVID) %>% 
     filter(is.na(date_completed) == F) %>%
     arrange(SVID,date_completed) %>%
     mutate(survey_number = row_number())

ct_panas_c %>% select(c(SVID,date_completed,survey_number)) %>% 
     as.data.frame() %>% head(n=100)
i = 1
alphas = data.frame()
for(i in 1:70){
     tmp = ct_panas_c %>% filter(survey_number == i)
     tmp.pa = tmp[,paVARSc]
     tmp.na = tmp[,naVARSc]
     alpha.pa = psych::alpha(tmp.pa)
     alpha.na = psych::alpha(tmp.na)
     row = data.frame(survey_number = i,
                      n = nrow(tmp),
                      alpha.pa = alpha.pa$total$raw_alpha,
                      alpha.na = alpha.na$total$raw_alpha)
     alphas = rbind(alphas,row)
     print(i)
     }
mean(alphas$alpha.pa)
mean(alphas$alpha.na)

plot(alphas$survey_number,alphas$alpha.pa)
plot(alphas$survey_number,alphas$alpha.na)




### Not NA row sums & means
row.sums.n <- function(vars, not.na=0) {
  apply(vars, 1, function(x) ifelse(sum(!is.na(x)) >= not.na, sum(x, na.rm=TRUE), NA))
}

row.means.n <- function(vars, not.na=0) {
  apply(vars, 1, function(x) ifelse(sum(!is.na(x)) >= not.na, mean(x, na.rm=TRUE), NA))
}
ct_panas_c$pa.sum<-row.sums.n(paITEMSc, not.na = 4)
ct_panas_c$na.sum<-row.sums.n(naITEMSc, not.na = 4)

ct_panas_c$pa.av<-row.means.n(paITEMSc, not.na = 4)
ct_panas_c$na.av<-row.means.n(naITEMSc, not.na = 4)

### Consistency and variability of particpant responses over time
# Looking at subject consistency and variability 
# Library with several useful functions for survey data
library(careless)

ITEMSc <- ITEMSc %>% mutate_if(is.numeric,as.factor)
paITEMSc <- paITEMSc %>% mutate_if(is.numeric,as.factor)
naITEMSc <- naITEMSc %>% mutate_if(is.numeric,as.factor)

#intra-individual response variability (IRV)
#The IRV is the "standard deviation of responses across a set of consecutive 
#item responses for an individual" 
#(Dunn, Heggestad, Shanock, & Theilgard, 2018, p. 108).
#And Marjanovic et al 2015
# low IRV = straight-lining 
# high IRV = highly random responses 
ct_panas_c$irvAll<-irv(ITEMSc, na.rm = TRUE) 
ct_panas_c$irvPA<-irv(paITEMSc, na.rm = TRUE)
ct_panas_c$irvNA<-irv(naITEMSc, na.rm = TRUE)

# Across all items
knitr::kable(describe(ct_panas_c$irvAll,type=2,fast=T),booktabs=TRUE,format="markdown")
hist(ct_panas_c$irvAll)
nrow(ct_panas_c[ct_panas_c$irvAll == 0,])
nrow(ct_panas_c[ct_panas_c$irvAll >= 2.037,])

# Positive Affect - IRV
knitr::kable(describe(ct_panas_c$irvPA,type=2,fast=T),booktabs=TRUE,format="markdown")
hist(ct_panas_c$irvPA)
nrow(ct_panas_c[ct_panas_c$irvPA == 0,])
nrow(ct_panas_c[ct_panas_c$irvPA >= 1.832,])

# Negative Affect - IRV
knitr::kable(describe(ct_panas_c$irvNA,type=2,fast=T),booktabs=TRUE,format="markdown")
hist(ct_panas_c$irvNA)
nrow(ct_panas_c[ct_panas_c$irvNA == 0,])
nrow(ct_panas_c[ct_panas_c$irvNA >= 2.067,])

# Long string index 
# Find the longest string of identical responses per survey
ct_panas_c$lsAll<-longstring(ITEMSc) 
ct_panas_c$lsPA<-longstring(paITEMSc)
ct_panas_c$lsNA<-longstring(naITEMSc)

table(ct_panas_c$lsAll)
table(ct_panas_c$lsPA)
table(ct_panas_c$lsNA)

# Positive Affect - Average
knitr::kable(describe(ct_panas_c$pa.av,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c, aes(x=pa.av))+
  geom_histogram(color="darkblue", fill="lightblue")

# Negative Affect - Average
knitr::kable(describe(ct_panas_c$na.av,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c, aes(x=na.av))+
  geom_histogram(color="tomato4", fill="tomato1")

# Correlation between negative and positive affect
corr.test(ct_panas_c$pa.av, ct_panas_c$na.av)


# data prep 
names(ct_panas_c)
ct_panas_c_sub <- ct_panas_c[c(1,2,4,5,15,18,20:29,59,60)]
ct_panas_c_sub$timestamp <- as.Date(ct_panas_c_sub$SUBMIT_DATE , format = "%y%m/%d/ %H:%M")
ct_panas_c_sub$lastaction <- as.Date(ct_panas_c_sub$LAST_ACTION_DATE , format = "%y%m/%d/ %H:%M")
names(ct_panas_c_sub)

ct_panas_c_sub_pa_wi<-ct_panas_c_sub %>%
  group_by(SVID) %>%
  summarize(pa.av.within = mean(pa.av, na.rm = TRUE))

ct_panas_c_sub_na_wi<-ct_panas_c_sub %>%
  group_by(SVID) %>%
  summarize(na.av.within = mean(na.av, na.rm = TRUE))

# long -> wide
ct_panas_c_sub <- ct_panas_c_sub[order(ct_panas_c_sub$SVID, ct_panas_c_sub$lastaction),]
# create an observation variable based on SVID
ct_panas_c_sub$obs <- with(ct_panas_c_sub, ave(as.character(SVID), SVID, FUN = seq_along))
ct_panas_c_wide <- ct_panas_c_sub %>%
  pivot_wider(id_cols = "SVID", names_from = obs, values_from = c(pa.av, na.av))

ct_panas_c_sub$obs <- as.numeric(ct_panas_c_sub$obs)


names(ct_panas_c_sub)
ct_panas_c_sub_no_na <- ct_panas_c_sub %>% filter_at(vars(pa.av,na.av),any_vars(!is.na(.)))

knitr::kable(describe(ct_panas_c_sub_no_na$TOTAL_TIME,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c_sub_no_na, aes(x=TOTAL_TIME))+
  geom_histogram(color="gray", fill="gray66")

# Time spent taking survey (under 300 seconds, arbitrary 5 minute cut off)
ct_panas_c_time_sub_no_na <- ct_panas_c_sub_no_na[ which(ct_panas_c_sub_no_na$TOTAL_TIME < 300), ]

knitr::kable(describe(ct_panas_c_time_sub$TOTAL_TIME,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c_time_sub, aes(x=TOTAL_TIME))+
  geom_histogram(color="gray", fill="gray66")

# One thing to consider is there are people that completed enough of the survey to get at least one scale score, but they never turned in their survey so they don't have a time stamp

# Summarizing affect within person across time points 
# Positive Affect - Average
knitr::kable(describe(ct_panas_c_sub_pa_wi$pa.av.within,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c_sub_pa_wi, aes(x=pa.av.within))+
  geom_histogram(color="darkblue", fill="lightblue")

# Negative Affect - Average
knitr::kable(describe(ct_panas_c_sub_na_wi$na.av.within,type=2,fast=T),booktabs=TRUE,format="markdown")
ggplot(ct_panas_c_sub_na_wi, aes(x=na.av.within))+
  geom_histogram(color="tomato4", fill="tomato1")

### Affect over time (overall)
#### Positive 
ggplot(ct_panas_c_sub, aes(x = obs, y = pa.av)) +
  geom_point()

#### Negative 
ggplot(ct_panas_c_sub, aes(x = obs, y = na.av)) +
  geom_point()

### Affect over time (within person)
#### Positive

tspag = ggplot(ct_panas_c_sub, aes(x=obs, y=pa.av, group = SVID)) + 
  geom_line() + guides(colour=FALSE) + xlab("Observation") +
  ylab("PA Avg")
spag = tspag + aes(colour = factor(SVID))
spag

# Negative
tspag = ggplot(ct_panas_c_sub, aes(x=obs, y=na.av, group = SVID)) + 
  geom_line() + guides(colour=FALSE) + xlab("Observation") +
  ylab("NA Avg")
spag = tspag + aes(colour = factor(SVID))
spag

# Export data
write_csv(ct_panas_c_sub, 
          "C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Datasets\\PANASphone.07.28.22.long.csv")
write_csv(ct_panas_c_wide, 
          "C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Datasets\\PANASphone.07.28.22.wide.csv")


# append panas data to covid and mobility data

dat = fread('mobility.affect.covid.2.21.csv')
dat = dat %>% select(!c(pa.av,na.av))

panas.bind = ct_panas_c_sub %>% select(SVID, SUBMIT_DATE, pa.av,na.av)
panas.bind$date = date(panas.bind$SUBMIT_DATE)

panas.bind$Year_Week = paste0(year(panas.bind$date),'_',(as.numeric(week(panas.bind$date))))


panas.bind = panas.bind %>% group_by(SVID,Year_Week) %>% summarise(pa.av = mean(pa.av),
                                                              na.av = mean(na.av))


dat$date = ymd(dat$date)
dat = left_join(dat,panas.bind)
nrow(dat)
summary(dat$pa.av)
summary(dat$na.av)

# restrict to below 500km distance to eliminate travel/signal jumps
dat = subset(dat, distance <= 500)

fwrite(dat,'mobility.affect.covid.5.19.23.csv')







