**********************************************************
*Program: project_v1.do
*Author: Alejandro Melguizo
*Date: 5/22/26
*Purpose: final project v2
**********************************************************

clear all
capture log close
cd "C:\Users\A.Melguizo001\Desktop\econometrics project"
log using project_v1.log, replace
capture ssc install estout

import delimited "econometrics_immigrants_v3.csv", clear



//**********************************************************
// basic regression:
gen ln_inctot = log(inctot_clean)

gen ln_wage = log(incwage_clean)

eststo r1: reg ln_inctot ageatimmig, r

eststo r2: reg ln_wage ageatimmig, r

//
//**********************************************************





//**********************************************************
//var cleaning and generating

replace hisp_clean = "." if hisp_clean == "NA"
destring hisp_clean, replace

replace nativity_clean = "." if nativity_clean == "NA"
destring nativity_clean, replace

gen sq_ysi = years_since_immig^2

gen empXsex = sex_clean * empstat_clean

//
//**********************************************************




//**********************************************************
//more complex regressions: 

eststo r3: reg ln_wage ageatimmig years_since_immig sq_ysi i.hisp_clean i.race_clean i.educ_clean i.citizen_clean i.region_clean bpl_clean sex_clean i.empstat_clean uhrsworkt, r

//
//**********************************************************


esttab
