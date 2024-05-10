# check if we have all of our ids
setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(data.table)
library(readxl)
library(lubridate)
library(dplyr)

# load ids
idmap<-read_xlsx('ID_mapping_test.xlsx')

idmap$family = substr(idmap$SVID, start = 1, stop = 6)

# are all ids from the SVID file included?
svids = read.csv('CoTwinsIDszygo.csv')
svids$idchar = nchar(svids$SubjNum)
svids$id = paste0(svids$family, substr(svids$SubjNum, start = svids$idchar - 1, stop = svids$idchar))

setdiff(svids$id, idmap$SVID)
# 1 id from the full list isn't included,
# id isn't included in mobility data

# load mobility
move.long<-read.csv('mobility.measures.9.9.22.csv')
move.long$timestamp = as_date(move.long$date, origin = '1970-01-01')
head(move.long)
move.long$distance[move.long$numpts == 1] = 0


#mobility ids got truncated, let's see if we can match them anyway:
ids = as.numeric(idmap$ALT_ID)
ids.mob = move.long$user_id

length(setdiff(ids.mob,ids))

# 26 ids aren't included, what are they?
setdiff(ids.mob,ids)

# let's pull the non-truncated ids and append to move.long file
ids2 = fread('ids.10.19.csv')

move.long$id.merge = move.long$user_id/10^19
ids2$id.merge = as.numeric(ids2$id)/10^19

move.long = left_join(move.long, ids2, by = c('id.merge'))
move.long$user_id = move.long$id 
move.long = move.long %>% select(!c(id, idnum, id.merge))
fwrite(move.long, 'mobility.measures.9.9.22.csv')


# are non-truncated ids all in the mapping file?
length(setdiff(move.long$user_id, idmap$ALT_ID))
# missing 26 ids

# is one the one from the master list?

which(move.long$user_id == "SV049321")

#nope

# how many cases are they contributing?
nrow(move.long[move.long$user_id %in% setdiff(move.long$user_id,idmap$ALT_ID),])
# 4173, 0.6% of cases

# over what timespan?
move.long.no.id = move.long[move.long$user_id %in% 
                                 setdiff(move.long$user_id, idmap$ALT_ID),]
nrow(move.long.no.id)
first(move.long.no.id$timestamp)
last(move.long.no.id$timestamp)


ids.miss = unique(move.long.no.id$user_id)


# bring up eventually, let's stick with what we have so far
names(idmap) = c('SVID','user_id','family')
idmap = idmap[idmap$user_id %in% move.long$user_id,]

move.long.tmp = merge(move.long, idmap, by = c('user_id'), all.x = T)


# who is getting extra cases?
id.case1 = move.long %>% group_by(user_id) %>% summarise(count = n())
id.case2 = move.long.tmp %>% group_by(user_id) %>% summarise(count = n())

which(id.case1$count != id.case2$count)
id.extra = id.case1$user_id[678]

tmp = move.long.tmp[which(move.long.tmp$user_id == id.extra),]
tmp1 = tmp[1:534,]
tmp2 = tmp[535:1068,]

head(tmp1)
head(tmp2)
unique(tmp$user_id)
unique(tmp$SVID)


head(move.long.tmp)

# we have two svids assigned to 625229... 
# for now let's remove both in case they are 
#double counting locations and bring up to 
#Amy/Sam at cotwins meeting


move.long.tmp = subset(move.long.tmp, !(SVID %in% unique(tmp$SVID)))

fwrite(move.long.tmp,'mobility.measures.10.20.22.csv')


