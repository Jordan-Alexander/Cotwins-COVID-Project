library(lubridate)
library(data.table)
dat2 = fread('mobility.affect.covid.post.Jan.2020.csv')
dat2$halfyear[year(dat2$date) == '2020' & month(dat2$date) <= 6] = 'Jan-Jun 2020'
dat2$halfyear[year(dat2$date) == '2020' & month(dat2$date) >= 6] = 'Jul-Dec 2020'

dat2$halfyear[year(dat2$date) == '2021' & month(dat2$date) <= 6] = 'Jan-Jun 2021'
dat2$halfyear[year(dat2$date) == '2021' & month(dat2$date) >= 6] = 'Jul-Dec 2021'

dat2$halfyear[year(dat2$date) == '2022' & month(dat2$date) <= 4] = 'Jan-Apr 2022'

dat2$halfyear = factor(dat2$halfyear, levels = c('Jan-Jun 2020','Jul-Dec 2020',
                                                 'Jan-Jun 2021','Jul-Dec 2021',
                                                 'Jan-Apr 2022'))

table(dat2$halfyear)

fwrite(dat2,'mobility.affect.covid.post.Jan.2020.csv')
