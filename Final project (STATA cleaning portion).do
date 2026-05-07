**********************************************************
*Program: Final_Project_statacleaning.do
*Author: Justin Espinal, Alejandro Melguizo, Myron Adamson
*Date: May 22, 2026
**********************************************************
clear all
capture log close
cd "C:\Users\LabUser\OneDrive - University of Massachusetts Boston\Econometrics\Final project" 
log using Final_Project_statacleaning.log, replace

import delimited econometrics_v_2, clear
*This is the old uncleaned data so just change this when we get the new data and clean for the rest of the NA's on stata

*taking out the NA for hhincome
replace hhincome = "." if hhincome == "NA"
destring hhincome, replace
drop if missing(hhincome)

*taking out the NA for ftotval
replace ftotval ="." if ftotval =="0"
destring ftotval, replace
drop if missing(ftotval)

*taking out the NA for inctot
replace inctot ="." if inctot =="0"
destring inctot, replace
drop if missing(inctot)

*Changing the NA to numeric variables age at immigration
replace ageatimmig = "." if ageatimmig =="NA"
destring ageatimmig, replace

*Shows the variables that are "." the ones that have values for ageatimmig
misstable summarize

*Basic regression with log personal income and age at immigration
gen ln_pincome = log(inctot)
reg ln_pincome ageatimmig, r

*Same regression with added sex variable
reg ln_pincome ageatimmig sex, r