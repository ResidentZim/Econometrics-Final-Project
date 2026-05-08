setwd('/Users/ando/Desktop/Econometrics/Econometrics Final Project')

library(gdata)
library(dplyr)
library(readr)

df2 <- read_csv('econometrics_v_6.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GETTING RID OF CERTAIN VALUES - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Keep rows where 'var' is NOT "value"
#df3 <- df2[df2$HHINCOME != 99999999 & df2$FTOTVAL != 9999999999 & df2$INCTOT != 999999999, ] 

#im trying to get rid of values that are NUI but the code above is not filtering any obs,
# which is weird. Not sure why.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPUTING INCOMES INFLATION ADJUSTED TO 2000 CPI (1999 dollars)- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#HHINCOME INFLATION ADJUSTED
df3 <- df3 %>%
  mutate(hhicome_clean = case_when(
    YEAR == 2001 ~ HHINCOME * 0.967,
    YEAR == 2002 ~ HHINCOME * 0.941,
    YEAR == 2003 ~ HHINCOME * 0.926,
    YEAR == 2004 ~ HHINCOME * 0.905,
    YEAR == 2005 ~ HHINCOME * 0.882,
    YEAR == 2006 ~ HHINCOME * 0.853,
    YEAR == 2007 ~ HHINCOME * 0.826,
    YEAR == 2008 ~ HHINCOME * 0.804,
    YEAR == 2009 ~ HHINCOME * 0.774,
    YEAR == 2010 ~ HHINCOME * 0.777,
    YEAR == 2011 ~ HHINCOME * 0.764,
    YEAR == 2012 ~ HHINCOME * 0.741,
    YEAR == 2013 ~ HHINCOME * 0.726,
    YEAR == 2014 ~ HHINCOME * 0.715,
    YEAR == 2015 ~ HHINCOME * 0.704,
    YEAR == 2016 ~ HHINCOME * 0.703,
    YEAR == 2017 ~ HHINCOME * 0.694,
    YEAR == 2018 ~ HHINCOME * 0.679,
    YEAR == 2019 ~ HHINCOME * 0.663,
    YEAR == 2020 ~ HHINCOME * 0.652,
    YEAR == 2021 ~ HHINCOME * 0.644,
    YEAR == 2022 ~ HHINCOME * 0.615,
    YEAR == 2023 ~ HHINCOME * 0.569,
    YEAR == 2024 ~ HHINCOME * 0.547,
    YEAR == 2025 ~ HHINCOME * 0.531,
    TRUE ~ HHINCOME
  ))

#FTOTVAL INFLATION ADJUSTED
df3 <- df3 %>%
  mutate(ftotval_clean = case_when(
    YEAR == 2001 ~ FTOTVAL * 0.967,
    YEAR == 2002 ~ FTOTVAL * 0.941,
    YEAR == 2003 ~ FTOTVAL * 0.926,
    YEAR == 2004 ~ FTOTVAL * 0.905,
    YEAR == 2005 ~ FTOTVAL * 0.882,
    YEAR == 2006 ~ FTOTVAL * 0.853,
    YEAR == 2007 ~ FTOTVAL * 0.826,
    YEAR == 2008 ~ FTOTVAL * 0.804,
    YEAR == 2009 ~ FTOTVAL * 0.774,
    YEAR == 2010 ~ FTOTVAL * 0.777,
    YEAR == 2011 ~ FTOTVAL * 0.764,
    YEAR == 2012 ~ FTOTVAL * 0.741,
    YEAR == 2013 ~ FTOTVAL * 0.726,
    YEAR == 2014 ~ FTOTVAL * 0.715,
    YEAR == 2015 ~ FTOTVAL * 0.704,
    YEAR == 2016 ~ FTOTVAL * 0.703,
    YEAR == 2017 ~ FTOTVAL * 0.694,
    YEAR == 2018 ~ FTOTVAL * 0.679,
    YEAR == 2019 ~ FTOTVAL * 0.663,
    YEAR == 2020 ~ FTOTVAL * 0.652,
    YEAR == 2021 ~ FTOTVAL * 0.644,
    YEAR == 2022 ~ FTOTVAL * 0.615,
    YEAR == 2023 ~ FTOTVAL * 0.569,
    YEAR == 2024 ~ FTOTVAL * 0.547,
    YEAR == 2025 ~ FTOTVAL * 0.531,
    TRUE ~ FTOTVAL
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
