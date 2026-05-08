#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd('/Users/ando/Desktop/Econometrics/Econometrics Final Project')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(gdata)
library(dplyr)
library(readr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df1 <- df_filtered

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#vars left to clean:
# RACE ✅
# HISPAN ✅ 
# BPL ✅ 
# NATIVITY ✅ 
# EDUC ✅ 
# REGION ✅ 
# SEX ✅ 
# CITIZEN ✅ 

# STATEFIP - not sure if we are including this var

#~~~~~~~~~~~~
# RACE
df1 <- df1 %>% mutate(race_clean = case_when(
  RACE == 100 ~ 1, #white
  RACE == 200 ~ 2, #black
  RACE == 300 ~ 3, #native american
  RACE == 650 ~ 4, #Asian
  TRUE        ~ 5  # Default "else" case
))

#Omitted cat: all other race combinations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HISPAN
df1 <- df1 %>% mutate(hisp_clean = case_when(
  HISPAN == 000 ~ 0, #non-hispan
  HISPAN == 100 ~ 1, #Mexican
  HISPAN == 200 ~ 2, #Puerto Rican
  HISPAN == 300 ~ 3, #Cuban
  HISPAN == 400 ~ 4, #Dominican
  HISPAN == 500 ~ 5, #salvi
  HISPAN == 611 ~ 6, #central american excluding salvi
  HISPAN == 612 ~ 7, #south american
  HISPAN > 900 ~ 99, # not available no response
  TRUE          ~ 8  #other Hispanic
))

#Omitted cat: all other Hispanic categories

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BPL
df1 <- df1 %>% mutate(bpl_clean = case_when(
  BPL > 13000 ~ 0, #born in US 
  BPL < 13000 & BPL > 99999 ~ 1, #born outside US
  TRUE          ~ 99 #NUI
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NATIVITY
df1 <- df1 %>% mutate(nativity_clean = case_when(
  NATIVITY == 1 ~ 1, #both parents native born
  NATIVITY == 2 ~ 2, #father foreign born
  NATIVITY == 3 ~ 3, #mother foreign born
  NATIVITY == 4 ~ 4, #both foreign born
  NATIVITY == 5 ~ 5,  #foreign born
  TRUE          ~ 99 #Unknown
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDUC
df1 <- df1 %>% mutate(educ_clean = case_when(
  EDUC <= 073 ~ 1, # high school completion or less
  EDUC > 073 & EDUC <= 111 ~ 2, # bachelors or less
  EDUC > 111 & EDUC >= 125 ~ 3,  # more than bach
  TRUE           ~ 99 #NUI
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REGION
df1 <- df1 %>% mutate(region_clean = case_when(
  REGION > 20 ~ 1, #northeast
  REGION < 20 & REGION > 30 ~ 2, #midwest
  REGION < 30 & REGION > 40 ~ 3, #south
  REGION < 40 & REGION > 50 ~ 4, #west
  TRUE ~ 99
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STATEFIP

# see name space

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SEX
df1 <- df1 %>% mutate(sex_clean = case_when(
  SEX == 1 ~ 0, #male
  SEX == 2 ~ 1, #female
  SEX == 9 ~ 99 #NIU
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CITIZEN
df1 <- df1 %>% mutate(citizen_clean = case_when(
  CITIZEN < 4 ~ 1, #born citizen
  CITIZEN == 4 ~ 2, #naturalized citizen
  CITIZEN == 5 ~ 0, #non-citizen
  CITIZEN == 9 ~ 99 #NIU
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# write.csv(df1, "econometrics_v_4.csv", row.names = FALSE)