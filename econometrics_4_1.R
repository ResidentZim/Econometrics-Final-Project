#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ALEJANDRO MELGUIZO
# DATE: 5/20/26
# TOPIC: further cleaning, inflation adjusting, and filtering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd('/Users/ando/Desktop/Econometrics/Econometrics Final Project')

library(gdata)
library(dplyr)
library(readr)

df2 <- read_csv('econometrics_v_9.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#NAMESPACE AFTER RUNNING CODE - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# df2 : data frame after econometrics_3_1.R cleaning
# df3 : data frame after inflation adjusting and filtering out NA income values
# df_immig : data frame with non-immigrants taken out
# df_immig_2 : data frame with non-immigrants taken out AND negative ages at immigration excluded

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GETTING RID OF CERTAIN VALUES - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Keep rows where INCOME TOTAL is not NA
df3 <- df2 %>% filter(!is.na(INCTOT))
df3 <- df3 %>% filter(!is.na(INCWAGE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPUTING INCOMES INFLATION ADJUSTED TO 2000 CPI (1999 dollars)- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#CPI values from https://cps.ipums.org/cps/cpi99.shtml 

#INCWAGE INFLATION ADJUSTED
df3 <- df3 %>%
  mutate(hhicome_clean = case_when(
    YEAR == 2001 ~ INCWAGE * 0.967,
    YEAR == 2002 ~ INCWAGE * 0.941,
    YEAR == 2003 ~ INCWAGE * 0.926,
    YEAR == 2004 ~ INCWAGE * 0.905,
    YEAR == 2005 ~ INCWAGE * 0.882,
    YEAR == 2006 ~ INCWAGE * 0.853,
    YEAR == 2007 ~ INCWAGE * 0.826,
    YEAR == 2008 ~ INCWAGE * 0.804,
    YEAR == 2009 ~ INCWAGE * 0.774,
    YEAR == 2010 ~ INCWAGE * 0.777,
    YEAR == 2011 ~ INCWAGE * 0.764,
    YEAR == 2012 ~ INCWAGE * 0.741,
    YEAR == 2013 ~ INCWAGE * 0.726,
    YEAR == 2014 ~ INCWAGE * 0.715,
    YEAR == 2015 ~ INCWAGE * 0.704,
    YEAR == 2016 ~ INCWAGE * 0.703,
    YEAR == 2017 ~ INCWAGE * 0.694,
    YEAR == 2018 ~ INCWAGE * 0.679,
    YEAR == 2019 ~ INCWAGE * 0.663,
    YEAR == 2020 ~ INCWAGE * 0.652,
    YEAR == 2021 ~ INCWAGE * 0.644,
    YEAR == 2022 ~ INCWAGE * 0.615,
    YEAR == 2023 ~ INCWAGE * 0.569,
    YEAR == 2024 ~ INCWAGE * 0.547,
    YEAR == 2025 ~ INCWAGE * 0.531,
    TRUE ~ INCWAGE
  ))

#INCTOT INFLATION ADJUSTED
df3 <- df3 %>%
  mutate(inctot_clean = case_when(
    YEAR == 2001 ~ INCTOT * 0.967,
    YEAR == 2002 ~ INCTOT * 0.941,
    YEAR == 2003 ~ INCTOT * 0.926,
    YEAR == 2004 ~ INCTOT * 0.905,
    YEAR == 2005 ~ INCTOT * 0.882,
    YEAR == 2006 ~ INCTOT * 0.853,
    YEAR == 2007 ~ INCTOT * 0.826,
    YEAR == 2008 ~ INCTOT * 0.804,
    YEAR == 2009 ~ INCTOT * 0.774,
    YEAR == 2010 ~ INCTOT * 0.777,
    YEAR == 2011 ~ INCTOT * 0.764,
    YEAR == 2012 ~ INCTOT * 0.741,
    YEAR == 2013 ~ INCTOT * 0.726,
    YEAR == 2014 ~ INCTOT * 0.715,
    YEAR == 2015 ~ INCTOT * 0.704,
    YEAR == 2016 ~ INCTOT * 0.703,
    YEAR == 2017 ~ INCTOT * 0.694,
    YEAR == 2018 ~ INCTOT * 0.679,
    YEAR == 2019 ~ INCTOT * 0.663,
    YEAR == 2020 ~ INCTOT * 0.652,
    YEAR == 2021 ~ INCTOT * 0.644,
    YEAR == 2022 ~ INCTOT * 0.615,
    YEAR == 2023 ~ INCTOT * 0.569,
    YEAR == 2024 ~ INCTOT * 0.547,
    YEAR == 2025 ~ INCTOT * 0.531,
    TRUE ~ INCTOT
  ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FILTERING FOR IMMIGRANTS, AND FOR ERROR AGES - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#filter a data set that includes only people who have data on immigrating
df_immig <- df3 %>% filter(!is.na(AGEATIMMIG))

#filter immigrants who have negative AGEATIMMIG 
# (probable error in census or error in answering census)
df_immig_2 <- df_immig %>% filter(AGEATIMMIG >= 0)

# only 2% of the 492245 observations where AGEATIMMIG != NA were negative ages. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD NEW CSV - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(df_immig_2, 'econometrics_immigrants_v3.csv', row.names = FALSE)

#v2 : ran with inctots > 0 only
#v3 : removed FTOTVAL and HHINCOME, added vars and cpi adjusted INCWAGE