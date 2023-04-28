capture log close
clear all
set more off

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\BIVAR\NSFG_MALE_1519_BIVAR.log", replace

use "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\data\NSFG\DATA\15-19\MALE\NSFG_MALE_1519_FINAL.dta", clear

*Setup SVY Computation

svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 
svydescribe

*Design-Adjusted Bivariate Results

*Past Month Binge

svy, subpop(if age_eligible == 1): tab pmBINGE IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab pmBINGE IDENTITY_B, col ci obs

*Cigarette Use Past Year

svy, subpop(if age_eligible == 1): tab p12smoking IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab p12smoking IDENTITY_B, col ci obs

*Pack a Day Past Year

svy, subpop(if age_eligible == 1): tab packaday IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab packaday IDENTITY_B, col ci obs

*Marijuana Use Past Year

svy, subpop(if age_eligible == 1): tab p12pot IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab p12pot IDENTITY_B, col ci obs

*Illicit Drug Use Past Year

svy, subpop(if age_eligible == 1): tab p12illicit IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab p12illicit IDENTITY_B, col ci obs

*Marital Status

svy, subpop(if age_eligible == 1): tab marriage IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab marriage IDENTITY_B, col ci obs

*Number of times Married

svy, subpop(if age_eligible == 1): mean numtimesmar, over(IDENTITY_A)
svy, subpop(if age_eligible == 1): mean numtimesmar, over(IDENTITY_B)

svy, subpop(if age_eligible == 1): tab numtimesmar IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab numtimesmar IDENTITY_B, col ci obs


*HH Size

svy, subpop(if age_eligible == 1): mean hhsize, over(IDENTITY_A)
svy, subpop(if age_eligible == 1): mean hhsize, over(IDENTITY_B)

svy, subpop(if age_eligible == 1): tab hhsize IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab hhsize IDENTITY_B, col ci obs

*Child Under 18 in HH

svy, subpop(if age_eligible == 1): tab kidhh IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab kidhh IDENTITY_B, col ci obs

*Life Vasectomy

svy, subpop(if age_eligible == 1): tab vasectomy IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab vasectomy IDENTITY_B, col ci obs

*LAST SEX Condom

svy, subpop(if age_eligible == 1): tab last_sex_cond IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab last_sex_cond IDENTITY_B, col ci obs

*Sex No Contra, Life

svy, subpop(if age_eligible == 1): tab life_sex_nocontra IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab life_sex_nocontra IDENTITY_B, col ci obs

*Sex No Contra, last sex

svy, subpop(if age_eligible == 1): tab no_lastcontra IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab no_lastcontra IDENTITY_B, col ci obs

*Want to have children in Future

svy, subpop(if age_eligible == 1): tab wantfuture IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab wantfuture IDENTITY_B, col ci obs

*Number of Current Sex Partners (LIFE)

svy, subpop(if age_eligible == 1): mean numlifesexpart, over(IDENTITY_A)
svy, subpop(if age_eligible == 1): mean numlifesexpart, over(IDENTITY_B)

svy, subpop(if age_eligible == 1): tab numlifesexpart IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab numlifesexpart IDENTITY_B, col ci obs

*Number of Current Sex Partners (PY)

svy, subpop(if age_eligible == 1): mean numyearsexpart, over(IDENTITY_A)
svy, subpop(if age_eligible == 1): mean numyearsexpart, over(IDENTITY_B)

svy, subpop(if age_eligible == 1): tab numyearsexpart IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab numyearsexpart IDENTITY_B, col ci obs

*Life Anal Sex

svy, subpop(if age_eligible == 1): tab sex_anal IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab sex_anal IDENTITY_B, col ci obs

*STD Life

svy, subpop(if age_eligible == 1): tab lifeSTD IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab lifeSTD IDENTITY_B, col ci obs

*STD P12

svy, subpop(if age_eligible == 1): tab pySTD IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab pySTD IDENTITY_B, col ci obs

*Married
svy, subpop(if age_eligible == 1): tab married IDENTITY_A, col ci obs
svy, subpop(if age_eligible == 1): tab married IDENTITY_B, col ci obs

*Univariate distribution results

svy, subpop(if age_eligible == 1 & IDENTITY_B == .): tab IDENTITY_A, ci obs miss

svy, subpop(if age_eligible == 1 & IDENTITY_A == .): tab IDENTITY_B, ci obs miss

*Race 

svy, subpop(if age_eligible == 1 & race == 0 & IDENTITY_B == . ): tab  IDENTITY_A,  ci obs miss

svy, subpop(if age_eligible == 1 & race == 1 & IDENTITY_B == . ): tab  IDENTITY_A,  ci obs miss

svy, subpop(if age_eligible == 1 & race == 2 & IDENTITY_B == . ): tab  IDENTITY_A,  ci obs miss

svy, subpop(if age_eligible == 1 & race == 0 & IDENTITY_A == .): tab  IDENTITY_B ,  ci obs miss

svy, subpop(if age_eligible == 1 & race == 1 & IDENTITY_A == .): tab  IDENTITY_B ,  ci obs miss

svy, subpop(if age_eligible == 1 & race == 2 & IDENTITY_A == .): tab  IDENTITY_B ,  ci obs miss

svy, subpop(if age_eligible == 1): tab hispanic IDENTITY_A, row ci obs
svy, subpop(if age_eligible == 1): tab hispanic IDENTITY_B, row ci obs

svy, subpop(if age_eligible == 1 & hispanic == 0 & IDENTITY_B == . ): tab  IDENTITY_A,  ci obs miss
svy, subpop(if age_eligible == 1 & hispanic == 1 & IDENTITY_B == . ): tab  IDENTITY_A,  ci obs miss

svy, subpop(if age_eligible == 1 & hispanic == 0 & IDENTITY_A == . ): tab  IDENTITY_B,  ci obs miss
svy, subpop(if age_eligible == 1 & hispanic == 1 & IDENTITY_A == . ): tab  IDENTITY_B,  ci obs miss

svy, subpop(if age_eligible == 1 & hispanic == 0): tab race IDENTITY_A, row ci obs
svy, subpop(if age_eligible == 1 & hispanic == 0): tab race IDENTITY_B, row ci obs

svy, subpop(if age_eligible == 1 & hispanic == 1): tab race IDENTITY_A, row ci obs
svy, subpop(if age_eligible == 1 & hispanic == 1): tab race IDENTITY_B, row ci obs

log close
exit
