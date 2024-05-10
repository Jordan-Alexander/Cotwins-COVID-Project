setwd("/panfs/jay/groups/1/vrie0006/alexa877/CovidLocationProject/RAship.2022/")
library(dplyr)
library(lubridate)
library(data.table)
library(nlme)

# load and update data ----
# load data
dat = fread('~/CovidLocationProject/RAship.2022/datasets/mobility.affect.covid.missingness.2.15.24.csv', data.table =  F)

# let's create a categorical variable representing different stages of the pandemic

dat <- dat %>% 
  mutate(Covid_Year_Fac = case_when(
    date >   ymd('2019-01-01') &
      date <  ymd('2020-01-20') ~'2019-01-01 to 2020-01-20',
    date >=  ymd('2020-01-20') &
      date < ymd('2021-01-01') ~'2020-01-20 to 2020-12-31',
    date >=  ymd('2021-01-01') &
      date < ymd('2022-01-01') ~'2021-01-01 to 2021-12-31',
    date >=  ymd('2022-01-01') &
      date < ymd('2022-04-19') ~'2022-01-01 to 2022-04-18')) %>% 
  mutate(Covid_Year_Fac = factor(
    Covid_Year_Fac,levels =c('2019-01-01 to 2020-01-19',
                             '2020-01-20 to 2020-12-31',
                             '2021-01-01 to 2021-12-31',
                             '2022-01-01 to 2022-04-18'))) %>%
  filter(distance <= 500)
#       numpts <= 10)


# create relative age variable


# convert character variables back to factor
dat$OS.version[dat$OS.version == ''] = NA
dat$OS.version = factor(dat$OS.version)
print(levels(dat$OS.version))
print(nrow(dat))
print(length(unique(dat$SVID)))

dat$sex[dat$sex == ''] = NA
dat$sex = factor(dat$sex)
print(levels(dat$sex))
# natural log scale population density

dat$pop.density.scaled = log(dat$pop.density)

dat$OS = factor(dat$OS, levels = c('iOS','Android'))


dat.lmer2 = dat %>% filter(date >= ymd('2019-01-20') & date < ymd('2019-05-01')|
                             date >= ymd('2020-01-20') & date < ymd('2020-05-01')|
                             date >= ymd('2021-01-20') & date < ymd('2021-05-01')|
                             date >= ymd('2022-01-20') & date < ymd('2022-05-01')
) %>%
  mutate(Covid_Year_Fac = case_when(date >= ymd('2019-01-20') & date < ymd('2019-05-01')~
                                      'Jan-Apr 2019',
                                    date >= ymd('2020-01-20') & date < ymd('2020-05-01')~
                                      'Jan-Apr 2020 ',
                                    date >= ymd('2021-01-20') & date < ymd('2021-05-01')~
                                      'Jan-Apr 2021',
                                    date >= ymd('2022-01-20') & date < ymd('2022-05-01')~
                                      'Jan-Apr 2022'),
         Covid_Year_Fac = factor(Covid_Year_Fac,
                                 levels = c('Jan-Apr 2019',
                                            'Jan-Apr 2020 ','Jan-Apr 2021',
                                            'Jan-Apr 2022'))) %>%
  filter(date > ymd('2020-01-20'))


# Convert phenotypes and cases into SD units to help with convergence
dat.lmer2 = dat.lmer2 %>%
  mutate(numpts = scale(numpts),
         distance = scale(distance),
         pa.av = scale(pa.av),
         na.av = scale(na.av),
         weekly.cases.per.100k = scale(weekly.cases.per.100k)
  )


iter = commandArgs(trailingOnly = TRUE)
iter = as.integer(iter)
#iter = 1
print(iter)

phenos = c('numpts','distance','pa.av','na.av')
pheno = phenos[iter] # select phenotype to model this iteration
print(pheno)

# rename phenotype "outcome" for convenience
pheno.col = which(names(dat.lmer2) == pheno)

names(dat.lmer2)[pheno.col] = 'outcome'

# lmer model ----

nrow(dat.lmer2)
dat.lmer2.outcome = dat.lmer2 %>% 
  select(c(SVID,family,outcome,sex,relative.age,OS,
           loc.prop.miss,na.prop.miss, Covid_Year_Fac, 
           weekly.cases.per.100k)) %>%
  na.omit()

nrow(dat.lmer2.outcome)

mod = lme(outcome ~ sex + relative.age + OS +
            loc.prop.miss + na.prop.miss + Covid_Year_Fac +
            weekly.cases.per.100k,
          random = ~1+weekly.cases.per.100k|family/SVID,
          method = "ML", 
          control =list(msMaxIter = 1000, msMaxEval = 1000),
          data = dat.lmer2.outcome)


saveRDS(mod, 
        file = paste0('~/CovidLocationProject/RAship.2022/models/',
                      pheno,'_case_count_lme_mod.rds')
)







