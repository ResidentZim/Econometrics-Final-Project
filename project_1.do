**********************************************************
*Program: project_v1.do
*Author: Alejandro Melguizo
*Date: 5/22/26
*Purpose: final project v1
**********************************************************
clear all
capture log close
cd "C:\Users\a.melguizo001\Documents\project"
log using project_v1.log, replace
capture ssc install estout

import delimited "econometrics_immigrants_1.csv", clear

eststo t1: sum age sex_clean inctot_clean hhicome_clean ftotval_clean educ_clean ageatimmig years_since_immig, detail

//add the csv that has years since immig, and think about what other variables to add