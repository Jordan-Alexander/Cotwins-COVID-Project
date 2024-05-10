# create wideform twin data
library(data.table)
library(lubridate)
library(readxl)
library(tidyr)

setwd("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022")

# load and transform location affect covid data ----

dat = fread('Datasets/mobility.affect.covid.5.19.23.csv',data.table = F) %>% 
     select(!c('bestzygos'))
zyg = read_xlsx('Datasets/id_mapping_comp.xlsx') %>% 
     select(c(SVID,bestzygos))
dat = left_join(dat,zyg)

dat$month = month(dat$date) %>% 
     factor(levels = 1:12, labels = c('January','February','March',
                                      'April','May','June',
                                      'July','August','September',
                                      'October','November','December'))
dat$sex[dat$sex == ''] = NA
dat$OS.version[dat$OS.version ==''] = NA

fwrite(dat, 'Datasets/mobility.affect.covid.5.19.23.csv')

# residualize out sex, OS.version, weekend/weekday (mobility only), and population density

pa.mod = lm(pa.av ~ 
                 sex + OS.version + pop.density + month, 
            data = dat)
na.mod = lm(na.av ~ 
                 sex + OS.version + pop.density + month, 
            data = dat)
pts.mod = lm(numpts ~ 
                  sex + OS.version + wknd + pop.density + month, 
             data = dat)
dist.mod = lm(distance ~ 
                  sex + OS.version + wknd + pop.density + month, 
              data = dat)

dat$pa.resid = dat$pa.av - predict(pa.mod,dat)
dat$na.resid = dat$na.av - predict(na.mod,dat)
dat$pts.resid = dat$numpts - predict(pts.mod,dat)
dat$dist.resid = dat$distance - predict(dist.mod,dat)


# convert to wideform

#phenovar = 'na.resid'
#idvar = 'SVID'
#famvar = 'family'
#zygvar = 'bestzygos'
#datevar = 'date'

wideform.twin = function(dat,phenovar,idvar,famvar, zygvar,datevar = 'date'){

     vars = which(names(dat) %in% c(phenovar,idvar,famvar,zygvar,datevar))
     
     tmp = dat[,vars] %>% na.omit()
     tmp = tmp[,c(idvar,famvar,zygvar,datevar,phenovar)]
     names(tmp) = c('ID','family','zyg','date','pheno')
     
     
     # aggregate by week
     tmp$week = paste(year(tmp$date),week(tmp$date),sep = '_')
     
     # add 0s to weeks 1:9 and sort by week
     
     week19 = grep('*_[123456789]$',tmp$week)
     tmp$week[week19] = gsub('_','_0',tmp$week[week19])
     tmp = tmp[order(tmp$week),]
     
     
     
     tmp2 = tmp %>% group_by(ID,week) %>%
          summarise(family = first(family),
                    zyg = first(zyg),
                    pheno = mean(pheno)) %>%
          ungroup()
     
     # convert to wide
     
     
     tmp3 = spread(tmp2,week,pheno)

     # check to see if any cases have been dropped
     cases1 = sum(!is.na(tmp2[,c('pheno')]))
     cases2 = sum(!is.na(tmp3[,4:ncol(tmp3)]))
     
     if(cases1 != cases2){warning(paste0('warning, cases added or dropped when converting to wide!\n',
                                         'ncases before: ',cases1,'\n',
                                         'ncases after: ', cases2))}
     
     mean1 = mean(tmp2$pheno,na.rm = T)
     mean2 = mean(as.matrix(tmp3[,4:ncol(tmp3)]),na.rm = T)     
     
     if(mean1 != mean2){warning(paste0('warning, some values have changed when 
                                       converting to wide! \n',
                                'mean before: ',mean1,'\n',
                                'mean after: ',mean2, '\n'))
          }
     
     # add missing weeks to data
     weeks = names(tmp3)[grep('20[12][1-9]_[0-5][0-9]',
                              names(tmp3))]
     weekstot = c()
     years = unique(year(tmp$date))
     
     for(i in years){
          if(i != 2020){
          weekstot = append(paste0(i,'_',1:52),weekstot)}
          else{weekstot = append(paste0(i,'_',1:53),weekstot)}
          
     }
     week19 = grep('*_[123456789]$',weekstot)
     weekstot[week19] = gsub('_','_0',weekstot[week19])
     weekstot = weekstot[order(weekstot)]
     weekstot = subset(weekstot, weekstot >= first(tmp$week) &
                            weekstot <= last(tmp$week))
     
     weeksadd = setdiff(weekstot, unique(tmp$week))
     
     tmp3[,weeksadd] = NA
     tmp3 = tmp3[,c(names(tmp3)[1:3],weekstot)]
     
     # convert wide data into twin data
     
     t1 = tmp3[!duplicated(tmp3$family),]
     t2 = tmp3[duplicated(tmp3$family),]
     
     if(nrow(t1) + nrow(t2) != nrow(tmp3)){warning(
          paste0('warning, cases added or dropped when splitting into t1 and t2 sets \n',
                 'cases before: ', nrow(t1) + nrow(t2), '\n',
                 'cases after: ', nrow(tmp3)))
          }
     
     names(t1) = c('ID_t1','family','zyg', paste0(names(t1[4:ncol(t1)]),'_t1'))
     names(t2) = c('ID_t2','family','zyg', paste0(names(t2[4:ncol(t2)]),'_t2'))
     
     tmp4 = left_join(t1,t2)
     
     # check that values preserved in tmp4
     cases.t2 = sum(!is.na(t2[,4:ncol(t2)]))
     cases.t1 = sum(!is.na(t1[,4:ncol(t1)]))
     
     index.t1 = names(t1[,4:ncol(t1)])
     index.t2 = names(t2[,4:ncol(t2)])
     
     if(cases.t1 != sum(!is.na(tmp4[,index.t1]))){warning('Warning, t1 cases dropped on merge with t2!')}
     if(cases.t2 != sum(!is.na(tmp4[,index.t2]))){warning('Warning, t2 cases dropped on merge with t1!')}   
     if(mean(as.matrix(t1[,index.t1]),na.rm = T) != mean(as.matrix(tmp4[,index.t1]),na.rm = T)){
          warning('Warning, some t1 values changed on merge with t2!')}
     if(mean(as.matrix(t2[,index.t2]),na.rm = T) != mean(as.matrix(tmp4[,index.t2]),na.rm = T)){
          warning('Warning, some t2 values changed on merge with t1!')}
     
     
     return(tmp4)
     }

# create wideform twin datasets ----
pa.av.twin = wideform.twin(dat,
                           phenovar = 'pa.resid',
                           idvar = 'SVID',
                           famvar = 'family',
                           zygvar = 'bestzygos',
                           datevar = 'date')

pa.av.mz = pa.av.twin[pa.av.twin$zyg == 'MZ',]
pa.av.dz = pa.av.twin[pa.av.twin$zyg != 'MZ',]
fwrite(pa.av.mz, 'Datasets/pa.av.mz.5.23.23.csv')
fwrite(pa.av.dz, 'Datasets/pa.av.dz.5.23.23.csv')

     
na.av.twin = wideform.twin(dat,
                           phenovar = 'na.resid',
                           idvar = 'SVID',
                           famvar = 'family',
                           zygvar = 'bestzygos',
                           datevar = 'date')

na.av.mz = na.av.twin[na.av.twin$zyg == 'MZ',]
na.av.dz = na.av.twin[na.av.twin$zyg != 'MZ',]
fwrite(na.av.mz, 'Datasets/na.av.mz.5.23.23.csv')
fwrite(na.av.dz, 'Datasets/na.av.dz.5.23.23.csv')     


numpts.twin = wideform.twin(dat,
                           phenovar = 'pts.resid',
                           idvar = 'SVID',
                           famvar = 'family',
                           zygvar = 'bestzygos',
                           datevar = 'date')

numpts.mz = numpts.twin[numpts.twin$zyg == 'MZ',]
numpts.dz = numpts.twin[numpts.twin$zyg != 'MZ',]
fwrite(numpts.mz, 'Datasets/numpts.mz.5.23.23.csv')
fwrite(numpts.dz, 'Datasets/numpts.dz.5.23.23.csv')     


#scale distance
dat$distance.scale = scale(dat$dist.resid)

dist.twin = wideform.twin(dat,
                           phenovar = 'distance.scale',
                           idvar = 'SVID',
                           famvar = 'family',
                           zygvar = 'bestzygos',
                           datevar = 'date')

dist.mz = dist.twin[dist.twin$zyg == 'MZ',]
dist.dz = dist.twin[dist.twin$zyg != 'MZ',]
fwrite(dist.mz, 'Datasets/dist.mz.5.23.23.csv')
fwrite(dist.dz, 'Datasets/dist.dz.5.23.23.csv')     
     




