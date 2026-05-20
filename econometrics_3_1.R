#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ALEJANDRO MELGUIZO
# DATE: 5/20/26
# TOPIC: cleaning CPS data frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd('/Users/ando/Desktop/Econometrics/Econometrics Final Project')
df <- read.csv('cps_00002.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(gdata)
library(dplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#creating the list of vars we want to keep
vars <- c('YEAR', 'AGE', 'SEX', 'RACE', 'BPL', 'YRIMMIG', 'NATIVITY', 'HISPAN', 
          'EDUC', 'REGION', 'STATEFIP', 'CITIZEN', 'HHINCOME', 'FTOTVAL', 'INCTOT')

#filtering the data by those selected vars
df1 <- df[, names(df) %in% vars]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CLEANING VARS - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#further filtering age to be between 25 and 65 years old
df1 <- df1 %>% filter(AGE >= 25 & AGE <=65)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#filtering out INCTOT <= 0
df1 <- df1 %>% filter(INCTOT > 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create a variable for the year of birth to later calculate age at immigration
df1$YEAROFBIRTH <- df1$YEAR - df1$AGE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#creating a var with the actual years of immig
df1 <- df1 %>%
  mutate(yrimmig_clean = case_when(
    YRIMMIG == 0000 ~ NA_real_,        # NIU
    YRIMMIG == 0001 ~ 1949,            # 1949 or earlier
    YRIMMIG == 0002 ~ 1954,            # 1950-1959
    YRIMMIG == 0003 ~ 1962,            # 1960-1964
    YRIMMIG == 0004 ~ 1967,            # 1965-1969
    YRIMMIG == 0005 ~ 1972,            # 1970-1974
    YRIMMIG == 0006 ~ 1977,            # 1975-1979
    YRIMMIG == 0007 ~ 1980,            # 1980-1981
    YRIMMIG == 0008 ~ 1982,            # 1982-1983
    YRIMMIG == 0009 ~ 1984,            # 1984-1985
    YRIMMIG == 0010 ~ 1986,            # 1986-1987
    YRIMMIG == 0011 ~ 1988,            # 1988-1989
    YRIMMIG == 0012 ~ 1990,            # 1990-1991
    YRIMMIG == 0013 ~ 1992,            # 1992-1993
    YRIMMIG == 0014 ~ 1993,            # 1992-1994
    YRIMMIG == 0015 ~ 1993,            # 1992-1995
    YRIMMIG == 0016 ~ 1994,            # 1994-1995
    YRIMMIG == 0017 ~ 1995,            # 1994-1996
    YRIMMIG == 0018 ~ 1995,            # 1994-1997
    YRIMMIG == 0019 ~ 1996,            # 1996-1997
    YRIMMIG == 0020 ~ 1997,            # 1996-1998
    YRIMMIG == 0021 ~ 1997,            # 1996-1999
    YRIMMIG == 0022 ~ 1998,            # 1998
    YRIMMIG == 0023 ~ 1998,            # 1998-1999
    YRIMMIG == 0024 ~ 1999,            # 1998-2000
    YRIMMIG == 0025 ~ 1999,            # 1998-2001
    YRIMMIG == 0026 ~ 2000,            # 2000-2001
    YRIMMIG == 0027 ~ 2001,            # 2000-2002
    YRIMMIG == 0028 ~ 2001,            # 2000-2003
    YRIMMIG == 0029 ~ 2002,            # 2002-2003
    YRIMMIG == 0030 ~ 2003,            # 2002-2004
    YRIMMIG == 0031 ~ 2003,            # 2002-2005
    YRIMMIG == 0032 ~ 2004,            # 2004-2005
    YRIMMIG == 0033 ~ 2005,            # 2004-2006
    YRIMMIG == 0034 ~ 2005,            # 2004-2007
    YRIMMIG == 0035 ~ 2006,            # 2006-2007
    YRIMMIG == 0036 ~ 2007,            # 2006-2008
    YRIMMIG == 0037 ~ 2007,            # 2006-2009
    YRIMMIG == 0038 ~ 2008,            # 2008-2009
    YRIMMIG == 0039 ~ 2009,            # 2008-2010
    YRIMMIG == 0040 ~ 2009,            # 2008-2011
    YRIMMIG == 0041 ~ 2010,            # 2010-2011
    YRIMMIG == 0042 ~ 2011,            # 2010-2012
    YRIMMIG == 0043 ~ 2011,            # 2010-2013
    YRIMMIG == 0044 ~ 2012,            # 2012-2013
    YRIMMIG == 0045 ~ 2013,            # 2012-2014
    YRIMMIG == 0046 ~ 2013,            # 2012-2015
    YRIMMIG == 0047 ~ 2014,            # 2014-2015
    YRIMMIG == 0048 ~ 2015,            # 2014-2016
    YRIMMIG == 0049 ~ 2015,            # 2014-2017
    YRIMMIG == 0050 ~ 2016,            # 2016-2017
    YRIMMIG == 0051 ~ 2017,            # 2016-2018
    YRIMMIG == 0052 ~ 2017,            # 2016-2019
    YRIMMIG == 0053 ~ 2018,            # 2018-2019
    YRIMMIG == 0054 ~ 2019,            # 2018-2020
    YRIMMIG == 0055 ~ 2019,            # 2018-2021
    YRIMMIG == 0056 ~ 2020,            # 2020-2021
    YRIMMIG == 0057 ~ 2021,            # 2020-2022
    YRIMMIG == 0058 ~ 2022,            # 2020-2023
    YRIMMIG == 0059 ~ 2023,            # 2022-2023
    YRIMMIG == 0060 ~ 2024,            # 2022-2024
    YRIMMIG == 0061 ~ 2025,            # 2022-2025
    YRIMMIG == 0062 ~ 2026,            # 2024-2026
    TRUE ~ NA_real_
  ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create an age at immigration var
df1$AGEATIMMIG <- df1$yrimmig_clean - df1$YEAROFBIRTH

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create a years since immigration var
df1$years_since_immig <- df1$AGE - df1$AGEATIMMIG

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RACE
df1 <- df1 %>% 
  mutate(race_clean = case_when(
  RACE == 100  ~ 1, #white
  RACE == 200  ~ 2, #black
  RACE == 300 ~ 3, #native american
  RACE %in% 650:652 ~ 4, #Asian
  TRUE        ~ 5  # Default "else" case
))

#Omitted cat: all other race combinations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HISPAN
df1 <- df1 %>% 
  mutate(hisp_clean = case_when(
  HISPAN == 000 ~ 0, #non-hispan
  HISPAN == 100 ~ 1, #Mexican
  HISPAN == 200 ~ 2, #Puerto Rican
  HISPAN == 300 ~ 3, #Cuban
  HISPAN == 400 ~ 4, #Dominican
  HISPAN == 500 ~ 5, #salvi
  HISPAN == 611 ~ 6, #central american excluding salvi
  HISPAN == 612 ~ 7, #south american
  HISPAN > 900 ~ NA_real_, # not available no response
  TRUE          ~ 8  #other Hispanic
))

#Omitted cat: all other Hispanic categories

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BPL
df1 <- df1 %>% 
  mutate(bpl_clean = case_when(
  BPL < 13000 ~ 0, #born in US 
  BPL > 13000 & BPL < 99999 ~ 1, #born outside US
  TRUE          ~ NA_real_ #NUI
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NATIVITY
df1 <- df1 %>% 
  mutate(nativity_clean = case_when(
  NATIVITY == 1 ~ 1, #both parents native born
  NATIVITY == 2 ~ 2, #father foreign born
  NATIVITY == 3 ~ 3, #mother foreign born
  NATIVITY == 4 ~ 4, #both foreign born
  NATIVITY == 5 ~ 5,  #foreign born
  TRUE          ~ NA_real_ #Unknown
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDUC
df1 <- df1 %>% 
  mutate(educ_clean = case_when(
  EDUC < 073 ~ 0, # less than high school
  EDUC == 073 ~ 1, # high school 
  EDUC >= 080 & EDUC <= 100 ~ 2, # some college
  EDUC == 111 ~ 3, # bachelors
  EDUC >= 123 & EDUC <= 125 ~ 4,  # grad degree
  TRUE           ~ NA_real_ #NUI
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REGION
df1 <- df1 %>% 
  mutate(region_clean = case_when(
  REGION < 20 ~ 1, #northeast
  REGION > 20 & REGION < 30 ~ 2, #midwest
  REGION > 30 & REGION < 40 ~ 3, #south
  REGION > 40 & REGION < 50 ~ 4, #west
  TRUE ~ NA_real_
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SEX
df1 <- df1 %>% 
  mutate(sex_clean = case_when(
  SEX == 1 ~ 0, #male
  SEX == 2 ~ 1, #female
  SEX == 9 ~ NA_real_ #NIU
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CITIZEN
df1 <- df1 %>% 
  mutate(citizen_clean = case_when(
  CITIZEN < 4 ~ 1, #born citizen
  CITIZEN == 4 ~ 2, #naturalized citizen
  CITIZEN == 5 ~ 0, #non-citizen
  CITIZEN == 9 ~ NA_real_ #NIU
))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD NEW CSV - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#write.csv(df1, 'econometrics_v_8.csv', row.names = FALSE)

#v8 : removed inctots <= 0 (5/20/26)
