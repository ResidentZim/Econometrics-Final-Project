**********************************************************
*Program: project_v1.do
*Author: Alejandro Melguizo
*Date: 5/22/26
*Purpose: final project v1
**********************************************************
clear all
capture log close
cd "C:\Users\A.Melguizo001\Desktop\econometrics project"
log using project_v1.log, replace
capture ssc install estout

import delimited "econometrics_immigrants_v2.csv", clear

//sum age sex_clean inctot_clean hhicome_clean ftotval_clean educ_clean ageatimmig years_since_immig, detail

//add the csv that has years since immig, and think about what other variables to add


// basic regression:
gen ln_inctot = log(inctot_clean)

//eststo r1: reg ln_inctot ageatimmig, r

//cleaning
*getting rid of NA values from hisp_clean
replace hisp_clean = "." if hisp_clean == "NA"
destring hisp_clean, replace

replace nativity_clean = "." if nativity_clean == "NA"
destring nativity_clean, replace

//complex regressions: 

//reg ln_inctot ageatimmig years_since_immig, r

//reg ln_inctot ageatimmig years_since_immig i.hisp_clean i.race_clean i.educ_clean sex_clean i.citizen_clean, r

gen hispXsex = hisp_clean * sex_clean

gen educXsex = sex_clean * educ_clean

gen hispXeduc = hisp_clean * educ_clean


//reg ln_inctot ageatimmig years_since_immig i.hisp_clean i.race_clean i.educ_clean sex_clean i.citizen_clean i.hispXsex i.educXsex i.hispXeduc, r

gen ln_ysi = log(years_since_immig)

gen ln_aai = log(ageatimmig)

//gen sq_aai = ageatimmig^2 //very insignificant

gen sq_ysi = years_since_immig^2

reg ln_inctot age ln_aai ln_ysi sq_ysi i.hisp_clean i.race_clean i.educ_clean sex_clean i.citizen_clean i.hispXsex i.educXsex i.hispXeduc i.region_clean bpl_clean, r

//
esttab







