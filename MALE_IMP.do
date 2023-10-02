capture log close
clear all
set more off

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\MALE\NSFG_MALE_1519_IMP.log", replace

use "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\data\NSFG\DATA\15-19\MALE\NSFG_MALE_1519_FINAL.dta", clear

*Create a single Identity Variable

gen IDENTITY_COMB = IDENTITY_A
replace IDENTITY_COMB = IDENTITY_B if IDENTITY_COMB == .
recode IDENTITY_COMB (.d .n .r = .)
tab IDENTITY_COMB IDENTITY_A, miss
tab IDENTITY_COMB IDENTITY_B, miss

tab IDENTITY_A IDENTITY_B, miss
gen TG2 = 0 
replace TG2 = 1 if IDENTITY_B !=. 
replace TG2 = 0 if IDENTITY_A == 0 & IDENTITY_B == .n
replace TG2 = . if IDENTITY_A == .n & IDENTITY_B == .n //Missing here is due to not knowing which respondent recieved which version of the question 

*Obtain variable distribution for Identity 

svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy: tab IDENTITY_COMB if TG2 == 0

* Het if u < 0.9544, Lesbian if 0.954 < u < 0.978, Bi if u > 0.978

*Get SMOKE12 mlogit ready 

tab SMOKE12, miss
tab SMOKE12, miss nolab
recode SMOKE12 (. 1 = 0) (2 = 1) (3 = 2) (4 = 3) (5 = 4) (6 = 5) (7 8 = .), gen(r_SMOKE12)
tab r_SMOKE12 SMOKE12, miss

tab TG2 IDENTITY_B, miss

mi set mlong 

mi svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

misstable sum pmBINGE r_SMOKE12 vasectomy last_sex_cond no_lastcontra  p12pot p12illicit married     wantfuture numlifesexpart numyearsexpart sex_anal lifeSTD pySTD  numtimesmar   IDENTITY_COMB no_sex_life

tab last_sex_cond no_sex_life, miss

tab last_sex_cond if  no_sex_life == 0, miss

*numtimesmar has no missing values
*temp remove ever_contraceptive 1 missing value predicting failure perfectly 
*temp remove pycontraceptive and packaday

*UNDER 200, USE MODE	change missing to mode, then impute rest

*Get Rao scott for mode imputation results 

tab pmBINGE, miss
recode pmBINGE (. = 0), gen(imp_pmBINGE)

tab r_SMOKE12, miss
recode r_SMOKE12 (. = 0)

tab vasectomy, miss
recode vasectomy (. = 0), gen(imp_vasectomy)

tab p12pot, miss
recode p12pot (. = 0), gen(imp_p12pot)

tab p12illicit, miss
recode p12illicit (. = 0), gen(imp_p12illicit)

tab married, miss
recode married (. = 0), gen(imp_married)

tab numyearsexpart, miss
recode numyearsexpart (. = 1), gen(imp_numyearsexpart)

tab sex_anal, miss
recode sex_anal (. = 0), gen(imp_sex_anal)

tab lifeSTD, miss
recode lifeSTD (. = 0), gen(imp_lifeSTD)

tab pySTD, miss
recode pySTD (. = 0), gen(imp_pySTD)

tab no_sex_life, miss
recode no_sex_life (. = 0)

tab race, miss
tab hisp, miss
tab cont_age, miss
tab cateduc, miss
tab faminc, miss

misstable sum imp_sex_anal imp_lifeSTD imp_pySTD imp_numyearsexpart imp_pmBINGE r_SMOKE12 imp_p12illicit imp_p12pot imp_married imp_vasectomy numtimesmar race hisp cont_age cateduc faminc last_sex_cond  wantfuture numlifesexpart         IDENTITY_COMB

mi reg imp last_sex_cond age_eligible  wantfuture numlifesexpart         IDENTITY_COMB

mi imp chain ///
(logit)    last_sex_cond  wantfuture age_eligible  ///
(mlogit)  IDENTITY_COMB ///
(poisson) numlifesexpart   ///
 = i.imp_sex_anal i.imp_lifeSTD i.imp_pySTD c.imp_numyearsexpart i.imp_pmBINGE i.r_SMOKE12 i.imp_p12illicit i.imp_p12pot i.imp_married i.imp_vasectomy i.numtimesmar  i.race i.hisp c.cont_age i.cateduc i.faminc i.no_sex_life, add(10) replace noisily rseed(2023) augment force
 
 
 log close 

stop

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\MALE\NSFG_MALE_1519_IMP_PYCONTRA.log", replace

 
 *Fix Last sex_cond
 
 replace last_sex_cond = . if no_sex_life == 1
 

 *fix last sex cond 

{

recode last_sex_cond (0 = 1) (1 = 0), gen(last_sex_nocond)	
replace last_sex_nocond = 1 if imp_vasectomy == 0 & last_sex_cond == 0
replace last_sex_nocond = 0 if imp_vasectomy == 1 | last_sex_cond == 1

tab no_lastcontra imp_vasectomy, miss
tab no_lastcontra last_sex_cond, miss
	
}

*Random Draws 0,1 for TG1 

generate double u = runiform() 

list IDENTITY_COMB u if TG2 == 0 & IDENTITY_COMB == 3

replace IDENTITY_COMB = 0 if IDENTITY_COMB == 3 & u <= 0.954 & TG2 == 0
replace IDENTITY_COMB = 1 if IDENTITY_COMB == 3 & (u > 0.954 & u <= 0.978) & TG2 == 0
replace IDENTITY_COMB = 2 if IDENTITY_COMB == 3 & u >= 0.978 & TG2 == 0

tab IDENTITY_COMB TG2, miss

*Remove SE from TG2 to missing for apples to apples comparison 

recode IDENTITY_COMB (3 = .), gen(r_IDENTITY_COMB)

	mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): tab last_sex_cond r_IDENTITY_COMB, col ci obs
	mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): tab last_sex_cond r_IDENTITY_COMB, col ci obs

		mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): tab last_sex_nocond r_IDENTITY_COMB, col ci obs
	mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): tab last_sex_nocond r_IDENTITY_COMB, col ci obs
	
	preserve
	
	drop if last_sex_cond == .
	drop if last_sex_nocond == .
	drop if r_IDENTITY_COMB == .

	   *mi xeq: svy, subpop(if age_eligible == 1): logit last_sex_cond i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

	   *mi xeq: svy, subpop(if age_eligible == 1): logit last_sex_cond i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race


   mi estimate: svy, subpop(if age_eligible == 1): logit last_sex_cond i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race
   
   	mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

   
      mi estimate: svy, subpop(if age_eligible == 1): logit last_sex_nocond i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race
	  
	  	mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

	  
   restore

log close 

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\MALE\NSFG_MALE_1519_IMP_RESULTS.log", replace

*Gen p12cig packaday ever_sex_no_contra py_sex_no_contra

*Cigarette Smoking and Pack a Day 
{
tab r_SMOKE12, miss
tab r_SMOKE12, nolab miss

gen imp_packaday = (r_SMOKE12 == 4 | r_SMOKE12 == 5)


gen imp_p12smoking = (r_SMOKE12 !=0 )


}

*Ever Sex, No Contra 
{
	
gen imp_life_sex_nocontra = (no_sex_life == 0 & imp_vasectomy == 0)
replace imp_life_sex_nocontra = . if no_sex_life == 1

}



foreach i of varlist imp_packaday imp_p12smoking imp_pmBINGE imp_p12pot imp_p12illicit imp_married imp_vasectomy imp_numyearsexpart imp_sex_anal imp_lifeSTD imp_pySTD  last_sex_cond  wantfuture last_sex_cond imp_life_sex_nocontra kidhh {
	
	mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): tab `i' r_IDENTITY_COMB, col ci obs
	mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): tab `i' r_IDENTITY_COMB, col ci obs

}

mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): regress imp_numyearsexpart i.r_IDENTITY_COMB
mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): regress imp_numyearsexpart i.r_IDENTITY_COMB

mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): regress numlifesexpart i.r_IDENTITY_COMB
mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): regress numlifesexpart i.r_IDENTITY_COMB

mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): regress numtimesmar i.r_IDENTITY_COMB
mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): regress numtimesmar i.r_IDENTITY_COMB
     
mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): regress hhsize i.r_IDENTITY_COMB
mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): regress hhsize i.r_IDENTITY_COMB

log close 

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\MALE\NSFG_MALE_1519_IMP_RESULTS_REG.log", replace

preserve 

drop if r_IDENTITY_COMB == .

mi estimate: svy, subpop(if age_eligible == 1): logit imp_pmBINGE i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

mi estimate: svy, subpop(if age_eligible == 1): logit imp_p12smoking i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

mi estimate: svy, subpop(if age_eligible == 1): logit imp_packaday i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit imp_p12pot i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit imp_p12illicit i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit imp_married i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc i.race
 
mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): regress numtimesmar i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): regress hhsize i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit kidhh i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race
 
mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): logit imp_vasectomy i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit last_sex_nocond i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race
 
mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): logit imp_life_sex_nocontra i.r_IDENTITY_COMB##i.TG2 i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

   mi estimate: svy, subpop(if age_eligible == 1): logit r_no_lastcontra i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit wantfuture i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): regress numlifesexpart i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): regress imp_numyearsexpart i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race
 
mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): logit imp_sex_anal i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

   mi estimate: svy, subpop(if age_eligible == 1): logit imp_lifeSTD i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

    mi estimate: svy, subpop(if age_eligible == 1): logit imp_pySTD i.r_IDENTITY_COMB##i.TG2  i.hisp c.cont_age i.cateduc i.faminc  i.race

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

restore 
 
log close