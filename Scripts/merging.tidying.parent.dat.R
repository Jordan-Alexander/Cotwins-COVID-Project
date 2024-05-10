# merging.tidying.parent.dat.R
# author: Sam Freis & Jordan Alexander
# purpose: clean up parent survey data to relevant SES and race/ethnicity vars and bind across waves
# last update - 06/15/22

library(tidyverse)
library(dplyr)
library(psych)
library(haven)
library(readxl)

#### Wave 1 ####
pdat1<-read_csv("rawdata/Robin_parent-Q-monitoring_12-9-16.csv", na = c("", "NA", "888","999", "88","99"))

# subsetting pdat1
# PQA_154A family income, PQA_154B highest edu of parent, PQA01541_1 mother occupation, PQA01541_2 father occupation
pvars<-c("id", "twin1id", "twin2id","PQA0001", "PQA0001_other", "PQA0003", "PQA_154A", "PQA_154B", "PQA01541_1", "PQA01541_2")
p1_items<-pdat1[pvars]

p1_items$PQA_154A<-as.factor(p1_items$PQA_154A)
p1_items$PQA_154B<-as.factor(p1_items$PQA_154B)
p1_items$PQA01541_1<-as.factor(p1_items$PQA01541_1)
p1_items$PQA01541_2<-as.factor(p1_items$PQA01541_2)

p1_items <- p1_items %>% 
  rename(
    rel_twins = PQA0001,
    rel_twins_other = PQA0001_other, 
    race_ethn = PQA0003,
    income = PQA_154A,
    edu =  PQA_154B,
    mom_occupation = PQA01541_1,
    dad_occupation = PQA01541_2,
  )
names(p1_items)


# Create family ID variable in pdat1
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

p1_items$id<-as.character(p1_items$id)
p1_items$id1<-substrRight(p1_items$id,6)

p1_items$Family<-substr(p1_items$id1, 0, 4)
p1_items$Family<-as.factor(p1_items$Family)
print(p1_items$Family)

# filter out empty surveys (more of an issue with wave 2)
p1_items <- p1_items %>% filter(!if_all(c(race_ethn, edu, income), is.na))

#Multiple cases of both parents taking the survey, keep both
#Long -> wide data for parents
#Assigning to parent 1 or parent 2 based on gender 0 and gender 1 part of ID as person part of SVID repeats in families for some reason

p1_items$parent<-substring(p1_items$id1, 6)
table(p1_items$parent)

#Family 305 has same sex parents (2 0 ids), need to recode one parent ID to 1 
names(p1_items)
p1_items <- p1_items[order(p1_items$Family),]
p1_items[358, 13] = "1"

w1fulldat_p1<- p1_items[ which(p1_items$parent=='0'), ]
head(w1fulldat_p1$parent)

w1fulldat_p2<- p1_items[ which(p1_items$parent=='1'), ]
head(w1fulldat_p2$parent)

w1pdf1 <- data.frame(w1fulldat_p1)
w1pdf1<-w1pdf1 %>% dplyr::rename_all(function(x) paste0("P1", x))

w1pdf2 <- data.frame(w1fulldat_p2)
w1pdf2<-w1pdf2 %>% dplyr::rename_all(function(x) paste0("P2", x))

colnames(w1pdf1)[colnames(w1pdf1)=="P1Family"] <- "Family"
colnames(w1pdf2)[colnames(w1pdf2)=="P2Family"] <- "Family"

w1pdf1$Family<-as.factor(w1pdf1$Family)
w1pdf2$Family<-as.factor(w1pdf2$Family)
wide_pdat_w1<-merge(w1pdf1,w1pdf2, by="Family", all = T)

duplicated(wide_pdat_w1$Family)

#### Wave 2 - pre switch to qualtrics ####
pdat2<-read_excel("rawdata/parq1new.xlsx", na = c("", "NA", "888","999", "88","99"))

# Relevant vars
# q154A family income, q154B highest edu of parent, PQA01541_1 mother occupation, PQA01541_2 father occupation 

pvars<-c("id", "twin1id", "twin2id","PQA0001", "PQA0001_other", "PQA0003","q154A", "q154B", "PQA01541_1", "PQA01541_2")
p2_items<-pdat2[pvars]

p2_items$q154A<-as.factor(p2_items$q154A)
p2_items$q154B<-as.factor(p2_items$q154B)
p2_items$PQA01541_1<-as.factor(p2_items$PQA01541_1)
p2_items$PQA01541_2<-as.factor(p2_items$PQA01541_2)

# Re-name vars to match wave 1 

p2_items <- p2_items %>% 
  rename(
    rel_twins = PQA0001,
    rel_twins_other = PQA0001_other, 
    race_ethn = PQA0003,
    income = q154A,
    edu =  q154B,
    mom_occupation = PQA01541_1,
    dad_occupation = PQA01541_2,
  )
names(p2_items)


# Create family ID variable in pdat2
p2_items$id<-as.character(p2_items$id)
p2_items$id1<-substrRight(p2_items$id,6)

p2_items$Family<-substr(p2_items$id1, 0, 4)
p2_items$Family<-as.factor(p2_items$Family)
print(p2_items$Family)
duplicated(p2_items$Family)

# filter out empty surveys (more of an issue with wave 2)
p2_items <- p2_items %>% filter(!if_all(c(race_ethn, edu, income), is.na))

#Multiple cases of both parents taking the survey, keep both
#Long -> wide data for parents
#Assigning to parent 1 or parent 2 based on sex 0 and sex 1 part of ID as person part of SVID repeats in families for some reason

p2_items$parent<-substring(p2_items$id1, 6)
table(p2_items$parent)

w2fulldat_p1<- p2_items[ which(p2_items$parent=='0'), ]
head(w2fulldat_p1$parent)

w2fulldat_p2<- p2_items[ which(p2_items$parent=='1'), ]
head(w2fulldat_p2$parent)

w2pdf1 <- data.frame(w2fulldat_p1)
w2pdf1<-w2pdf1 %>% dplyr::rename_all(function(x) paste0("P1", x))

w2pdf2 <- data.frame(w2fulldat_p2)
w2pdf2<-w2pdf2 %>% dplyr::rename_all(function(x) paste0("P2", x))

colnames(w2pdf1)[colnames(w2pdf1)=="P1Family"] <- "Family"
colnames(w2pdf2)[colnames(w2pdf2)=="P2Family"] <- "Family"

w2pdf1$Family<-as.factor(w2pdf1$Family)
w2pdf2$Family<-as.factor(w2pdf2$Family)
wide_pdat_w2<-merge(w2pdf1,w2pdf2, by="Family", all = T)

duplicated(wide_pdat_w2$Family)

#### Wave 2 - post switch to qualtrics ####
pdat2b<-read_excel("rawdata/parq1qualtrics_codes.xlsx", na = c("", "NA", "888","999", "88","99"))
tokens<-read_excel("rawdata/IndividLinks_COWTWINSParent_Qualtrics_sub.xlsx")

# pull any missing tokens from first name column 
pdat2b<-pdat2b %>% 
  mutate(token = coalesce(token,firstname))

# why 2 people had their tokens in "firstname" but not in the token column, I do not know 

pdat2btok<-merge(tokens, pdat2b, by = "token", all = FALSE)

# find equivalent of these vars from lime survey version: 
# PQA_154A family income, PQA_154B highest edu of parent, PQA01541_1 mother occupation, PQA01541_2 father occupation

pvars<-c("SubID","PQA0001", "PQA0001_other", "PQA0003","q154A", "q154B", "PQA01541_1", "PQA01541_2")
p2b_items<-pdat2btok[pvars]

p2b_items <- p2b_items %>% 
  rename(
    id = SubID,
    rel_twins = PQA0001,
    rel_twins_other = PQA0001_other, 
    race_ethn = PQA0003,
    income = q154A,
    edu =  q154B,
    mom_occupation = PQA01541_1,
    dad_occupation = PQA01541_2,
  )

names(p2b_items)

# add columns to match up with old versions 
p2b_items['twin1id'] <- NA
p2b_items['twin2id'] <- NA

# Create family ID variable in pdat2
p2b_items$id<-as.character(p2b_items$id)
p2b_items$id1<-substrRight(p2b_items$id,6)

p2b_items$Family<-substr(p2b_items$id1, 0, 4)
p2b_items$Family<-as.factor(p2b_items$Family)
print(p2b_items$Family)
duplicated(p2b_items$Family)

# filter out empty surveys (more of an issue with wave 2)
p2b_items <- p2b_items %>% filter(!if_all(c(race_ethn, edu, income), is.na))

#Multiple cases of both parents taking the survey, keep both
#Long -> wide data for parents
#Assigning to parent 1 or parent 2 based on sex 0 and sex 1 part of ID as person part of SVID repeats in families for some reason

p2b_items$parent<-substring(p2b_items$id1, 6)
table(p2b_items$parent)

# Family 0493 has same sex parents (2 0 ids), need to recode one parent ID to 1 
names(p2b_items)
p2b_items$Family

p2b_items <- p2b_items[order(p2b_items$Family),]
p2b_items[19, 13] = "1"

# Family 0499 is a duplicate 
p2b_items<-p2b_items[!duplicated(p2b_items$id), ]

w2bfulldat_p1<- p2b_items[ which(p2b_items$parent=='0'), ]
head(w2bfulldat_p1$parent)

w2bfulldat_p2<- p2b_items[ which(p2b_items$parent=='1'), ]
head(w2bfulldat_p2$parent)

w2bpdf1 <- data.frame(w2bfulldat_p1)
w2bpdf1<-w2bpdf1 %>% dplyr::rename_all(function(x) paste0("P1", x))

w2bpdf2 <- data.frame(w2bfulldat_p2)
w2bpdf2<-w2bpdf2 %>% dplyr::rename_all(function(x) paste0("P2", x))

colnames(w2bpdf1)[colnames(w2bpdf1)=="P1Family"] <- "Family"
colnames(w2bpdf2)[colnames(w2bpdf2)=="P2Family"] <- "Family"

w2bpdf1$Family<-as.factor(w2bpdf1$Family)
w2bpdf2$Family<-as.factor(w2bpdf2$Family)
wide_pdat_w2b<-merge(w2bpdf1,w2bpdf2, by="Family", all = T)

wide_pdat_w2b <- wide_pdat_w2b[order(wide_pdat_w2b$Family),]

duplicated(wide_pdat_w2b$Family)

# Get Wave 1 and 2 into same dataframe
colnames(wide_pdat_w1)
colnames(wide_pdat_w2)
colnames(wide_pdat_w2b)

# double check that responses look similar 
table(wide_pdat_w1$P1income)
table(wide_pdat_w2$P1income)
table(wide_pdat_w2b$P1income)

table(wide_pdat_w1$P1edu)
table(wide_pdat_w2$P1edu)
table(wide_pdat_w2b$P1edu)

str(wide_pdat_w1)
str(wide_pdat_w2)
str(wide_pdat_w2b)

wide_pdat_w1$wave<-1
wide_pdat_w2$wave<-2
wide_pdat_w2b$wave<-3

pdat <- rbind(wide_pdat_w1, wide_pdat_w2, wide_pdat_w2b)

## Sanity checks and lookingfor duplicates
n_occur <- data.frame(table(pdat$Family))
n_occur[n_occur$Freq > 1,]

#### Write clean data ####
write_csv(pdat, "cleandata/CoTwinsParentDemsSESwide_061722.csv")
