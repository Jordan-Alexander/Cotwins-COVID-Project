setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")


dems<-read.csv('CoTwinsDemographics061722.csv')
dems2<-read.csv('dems.full.sample.zyg.2.8.csv')
panas.long<-read.csv('PANASphone.long.csv')
ids<-unique(panas.long$SVID)
length(ids)
dems<-subset(dems, SVID %in% ids)
dems2<-subset(dems2, SVID %in% ids)
nrow(dems2)
nrow(dems)

table(dems$bestzygos)

dems$P1race_ethn<-as.factor(dems$P1race_ethn)
levels(dems$P1race_ethn)<-c('American Indian/ Alaskan Native, Hispanic/Latino',
                            'Asian, Hispanic/Latino',
                            'Asian, non Hispanic/Latino',
                            'African American, non Hispanic/Latino',
                            'More than one race','Non-Hispanic White','Hispanic White',
                            'Hispanic White 2?')


setdiff(dems$SVID,dems2$SVID)

table(dems$race_eth.combined)

dems$race_eth.combined = dems$P1race_ethn
dems$race_eth.combined[is.na(dems$race_eth.combined == T)] = dems$P2race_ethn[is.na(dems$race_eth.combined == T)]
dems$race_eth.combined<-factor(dems$race_eth.combined)
levels(dems$race_eth.combined) <- c('American Indian/Alaska Native, Hispanic/Latino',
                                                                  'Asian, Hispanic/Latino',
                                                                  'Asian, non-Hispanic/Latino',
                                                                  'Native Hawaiian or other Pacific Islander, non-Hispanic/Latino',
                                                                  'Black or African American, non-Hispanic/Latino',
                                                                  'White, Hispanic/Latino',
                                                                  'White, non-Hispanic/Latino',
                                                                  'More than one race, Hispanic/Latino',
                                                                  'More than one race, non-Hispanic/Latino')

