# Create Random intercept Models of Affect and Mobility

setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(data.table)
library(dplyr)
library(lubridate)
library(reshape2)

dat = fread('Datasets/mobility.affect.covid.2.21.csv',data.table = F)
ids = readxl::read_xlsx('Datasets/id_mapping_comp.xlsx')

dat = dat %>% dplyr::select(!c(bestzygos)) %>% 
     dplyr::left_join(ids[,c('SVID','bestzygos')])

head(dat)
# create Affect and mobility wideform datasets

PA.data = dat %>% dplyr::select(c(SVID,pa.av, date))

data = dat
var = 'numpts'

get.wideform.twin.data = function(data, var){
     
     vars = c('SVID','family','bestzygos','date',var)
     df = data[,vars] %>% na.omit()
     names(df)[5] = 'outcome'
     df$year_week = paste(year(df$date),week(df$date),sep = '_')
     
     # add 0s to weeks 1-9 (eg 1 -> 01 etc)
     df$year_week[nchar(df$year_week) < 7] = gsub('_','_0',
                                                  df$year_week[nchar(df$year_week) < 7])
     
     #df = df %>% group_by(SVID, year_week) %>%
     #     dplyr::summarise(outcome = mean(outcome, na.rm = T))

     df_wide = reshape2::dcast(df, SVID + family + bestzygos ~ year_week, value.var = 'outcome',
                               fun.aggregate = mean)
     
     df_wide_t1 = df_wide[!duplicated(df_wide$family),]
     df_wide_t2 = df_wide[duplicated(df_wide$family),]
     names(df_wide_t1)[4:ncol(df_wide_t1)] = paste0('t1_',
                                                    names(df_wide_t1)[4:ncol(df_wide_t1)])
     
     names(df_wide_t1)[1] = 'SVID_t1'
     
     names(df_wide_t2)[4:ncol(df_wide_t2)] = paste0('t2_',
                                                    names(df_wide_t2)[4:ncol(df_wide_t2)])
     
     names(df_wide_t2)[1] = 'SVID_t2'
     
     
     df_wide_twin = left_join(df_wide_t1,df_wide_t2)
     
     MZdata = df_wide_twin[df_wide_twin$bestzygos == 'MZ',]
     DZdata = df_wide_twin[df_wide_twin$bestzygos != 'MZ',]
     
     out = list(MZdata,DZdata)
     return(out)
     }

PA.twin = get.wideform.twin.data(dat, 'pa.av')
PA.mz = PA.twin[[1]]
PA.mz = sapply(PA.mz[,4:ncol(PA.mz),],as.numeric)
fwrite(PA.mz,'PA.mz.3.6.csv')
PA.dz = PA.twin[[2]]
PA.dz = sapply(PA.dz[,4:ncol(PA.dz),],as.numeric)
fwrite(PA.dz,'PA.dz.3.6.csv')

NA.twin = get.wideform.twin.data(dat, 'na.av')
NA.mz = NA.twin[[1]]
NA.mz = sapply(NA.mz[,4:ncol(NA.mz),],as.numeric)
NA.dz = NA.twin[[2]]
NA.dz = sapply(NA.dz[,4:ncol(NA.dz),],as.numeric)
fwrite(NA.mz,'NA.mz.3.6.csv')
fwrite(NA.dz,'NA.dz.3.6.csv')



numpts.twin = get.wideform.twin.data(dat,'numpts')
numpts.mz = numpts.twin[[1]]
numpts.dz = numpts.twin[[2]]
numpts.mz = sapply(numpts.mz[,4:ncol(numpts.mz),],as.numeric)
numpts.dz = sapply(numpts.dz[,4:ncol(numpts.dz),],as.numeric)
fwrite(numpts.mz,'numpts.mz.3.6.csv')
fwrite(numpts.dz,'numpts.dz.3.6.csv')

distance.twin = get.wideform.twin.data(dat,'distance')
distance.mz = distance.twin[[1]]
distance.dz = distance.twin[[2]]
fwrite(distance.mz,'distance.mz.3.6.csv')
fwrite(distance.dz,'distance.dz.3.6.csv')

     
