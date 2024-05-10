# heritability of covid cases, mobility, positve and negative affect
setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Datasets")
source("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Scripts\\Jordan.functions.R")

library(data.table)
library(dplyr)

dat = fread('mobility.affect.covid.1.26.22.csv',data.table = F)
dems = read.csv('CoTwinsDemographics.JA.add.dates.081722.csv')


# simple case count heritability model:

ACE.dat = dat %>% dplyr::group_by(SVID) %>%
     dplyr::summarise(cases = mean(daily.cases,na.rm = T))
# removing missing id row
ACE.dat = ACE.dat[2:nrow(ACE.dat),]

# adding family
ACE.dat = left_join(ACE.dat,dems) %>% select(c(SVID,Family,bestzygos,cases))

# converting to wideform

ACE.t1 = ACE.dat[duplicated(ACE.dat$Family) == F,]
ACE.t2 = ACE.dat[duplicated(ACE.dat$Family) == T,]

names(ACE.t1)[c(1,4)] = c('SVID_t1','cases_t1')
names(ACE.t2)[c(1,4)] = c('SVID_t2','cases_t2')

ACE_wide = left_join(ACE.t1,ACE.t2)

MZ_data = subset(ACE_wide, bestzygos == 'MZ')
DZ_data = subset(ACE_wide, bestzygos != 'MZ')
case.ace = fit_ace_univariate(MZ_data,DZ_data,'cases_t1','cases_t2')






# mobility numpts heritability model:

ACE.dat = dat %>% dplyr::group_by(SVID) %>%
     dplyr::summarise(cases = mean(daily.cases,na.rm = T),
                      numpts = mean(numpts,na.rm = T))
# removing missing id row
ACE.dat = ACE.dat[2:nrow(ACE.dat),]

# adding family
ACE.dat = left_join(ACE.dat,dems) %>% select(c(SVID,Family,bestzygos,cases,numpts))

# converting to wideform

ACE.t1 = ACE.dat[duplicated(ACE.dat$Family) == F,]
ACE.t2 = ACE.dat[duplicated(ACE.dat$Family) == T,]

names(ACE.t1)[c(1,4,5)] = c('SVID_t1','cases_t1','numpts_t1')
names(ACE.t2)[c(1,4,5)] = c('SVID_t2','cases_t2','numpts_t2')

ACE_wide = left_join(ACE.t1,ACE.t2)

MZ_data = subset(ACE_wide, bestzygos == 'MZ')
DZ_data = subset(ACE_wide, bestzygos != 'MZ')
case.pts.ace = fit_ace_bivariate(MZ_data,DZ_data,'cases_t1','cases_t2', 
                             'numpts_t1', 'numpts_t2')


