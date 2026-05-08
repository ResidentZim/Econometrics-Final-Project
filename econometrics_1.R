setwd('/Users/ando/Desktop/Econometrics/Econometrics Final Project')
df <- read.csv('cps_00002.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages('gdata')
library(gdata)
library(dplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#creating the list of vars we want to keep
vars <- c('YEAR', 'AGE', 'SEX', 'RACE', 'BPL', 'YRIMMIG', 'NATIVITY', 'HISPAN', 'EDUC', 'REGION', 'STATEFIP', 'CITIZEN', 'HHINCOME', 'FTOTVAL', 'INCTOT')

#filtering the data by those selected vars
df_filtered <- df[, names(df) %in% vars]

#further filtering age to be between 25 and 65 years old
df_filtered <- df_filtered %>% filter(AGE >= 25 & AGE <=65)

#create a variable for the year of birth to later calculate age at immigration
df_filtered$YEAROFBIRTH <- df_filtered$YEAR - df_filtered$AGE

#creating a var with the actual years of immig
df_filtered <- df_filtered %>%
  mutate(yrimmig_year = case_when(
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

#create an age at immigration var
df_filtered$AGEATIMMIG <- df_filtered$yrimmig_year - df_filtered$YEAROFBIRTH

#!!! FOR SOME REASON I AM GETTING PEOPLE WITH NEGATIVE AGE AT IMMIG, 
#for some reason they are listed as having immigrated in 1949 or before 
#but were not even born at the time

#solved the above issue by using the year in which
#the census was taken to calculate birth year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#write.csv(df_filtered, "econometrics_v_2_5.csv", row.names = FALSE)


