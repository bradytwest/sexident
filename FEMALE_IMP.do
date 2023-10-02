capture log close
clear all
set more off

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\NSFG_FEMALE_1519_IMP.log", replace

use "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\data\NSFG\DATA\15-19\FEMALE\NSFG_FEMALE_1519_FINAL.dta", clear

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

*Het if u < 0.8897, Lesbian if 0.8898 < u < 0.9166, Bi if u > 0.9167

*Get SMOKE12 mlogit ready 

tab SMOKE12, miss
tab SMOKE12, miss nolab
recode SMOKE12 (. 1 = 0) (2 = 1) (3 = 2) (4 = 3) (5 = 4) (6 = 5) (7 8 = .), gen(r_SMOKE12)
tab r_SMOKE12 SMOKE12, miss

tab TG2 IDENTITY_B, miss

mi set mlong 

mi svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

misstable sum pmBINGE r_SMOKE12  p12pot p12illicit married  ever_contraceptive pycontraceptive  wantfuture numlifesexpart numyearsexpart sex_anal lifeSTD pySTD pregnancy numtimesmar   IDENTITY_COMB

tab pycontraceptive no_sex_py, miss
tab pycontraceptive if ever_contraceptive == 1, miss

*numtimesmar has no missing values

*UNDER 200, USE MODE change missing to mode, then impute rest (EXCEPT FOR SEXUAL IDENTITY)

*Get Rao-scott for mode imputation results 

tab pmBINGE, miss
recode pmBINGE (. = 0), gen(imp_pmBINGE)

tab r_SMOKE12, miss
recode r_SMOKE12 (. = 0)

tab p12pot, miss
recode p12pot (. = 0), gen(imp_p12pot)

tab p12illicit, miss
recode p12illicit (. = 0), gen(imp_p12illicit)

tab married, miss
recode married (. = 0), gen(imp_married)

tab ever_contraceptive, miss
recode ever_contraceptive (. = 1), gen(imp_ever_contraceptive)

tab pregnancy, miss
recode pregnancy (. = 0), gen(imp_pregnancy)

tab numyearsexpart, miss
recode numyearsexpart (. = 1), gen(imp_numyearsexpart)

tab sex_anal, miss
recode sex_anal (. = 0), gen(imp_sex_anal)

tab lifeSTD, miss
recode lifeSTD (. = 0), gen(imp_lifeSTD)

tab pySTD, miss
recode pySTD (. = 0), gen(imp_pySTD)

tab no_sex_py, miss
recode no_sex_py (. = 0)

tab no_sex_life, miss
recode no_sex_life (. = 0)

gen miss_pycontraceptive = (pycontraceptive == .)

mi reg imp pycontraceptive  age_eligible numlifesexpart   wantfuture      IDENTITY_COMB

mi imp chain ///
(logit)  age_eligible  pycontraceptive  wantfuture   ///
(mlogit)  IDENTITY_COMB ///
(poisson) numlifesexpart   ///
 = i.imp_sex_anal i.imp_lifeSTD i.imp_pySTD c.imp_numyearsexpart i.imp_pregnancy i.imp_pmBINGE i.r_SMOKE12 i.imp_p12illicit i.imp_p12pot i.imp_married i.imp_ever_contraceptive i.imp_pregnancy i.race i.hisp c.cont_age i.cateduc i.faminc, add(10) replace noisily rseed(2023) augment force
 
 
*py contra
 
replace pycontraceptive = . if  miss_pycontraceptive == 1
 
*Sex (No Contraceptive Use P12)
{

gen imp_py_sex_nocontra = 1 if no_sex_py == 0 & pycontraceptive == 0 
replace imp_py_sex_nocontra = 0 if no_sex_py == 0 & pycontraceptive == 1
replace imp_py_sex_nocontra = . if no_sex_py == 1

}

*Random Draws 0,1 for TG1 

generate double u = runiform() 

list IDENTITY_COMB u if TG2 == 0 & IDENTITY_COMB == 3

replace IDENTITY_COMB = 0 if IDENTITY_COMB == 3 & u <= 0.8897 & TG2 == 0
replace IDENTITY_COMB = 1 if IDENTITY_COMB == 3 & (u > 0.8897 & u <= 0.9166) & TG2 == 0
replace IDENTITY_COMB = 2 if IDENTITY_COMB == 3 & u >= 0.9167 & TG2 == 0

tab IDENTITY_COMB TG2, miss

*Remove SE from TG2 to missing for apples to apples comparison 

recode IDENTITY_COMB (3 = .), gen(r_IDENTITY_COMB)

mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): tab pycontraceptive r_IDENTITY_COMB, col ci obs
mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): tab pycontraceptive r_IDENTITY_COMB, col ci obs

mi xeq: svy, subpop(if TG2 == 0 & age_eligible == 1): tab imp_py_sex_nocontra r_IDENTITY_COMB, col ci obs
mi xeq: svy, subpop(if TG2 == 1 & age_eligible == 1): tab imp_py_sex_nocontra r_IDENTITY_COMB, col ci obs
	
preserve

drop if r_IDENTITY_COMB == .

mi estimate: svy, subpop(if age_eligible == 1): logit pycontraceptive i.r_IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

mi estimate: svy, subpop(if age_eligible == 1): logit imp_py_sex_nocontra i.r_IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.r_IDENTITY_COMB#1.TG2 2.r_IDENTITY_COMB#1.TG2

restore 

log close 

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\NSFG_FEMALE_1519_IMP_BIVAR.log", replace

*Gen p12cig packaday ever_sex_no_contra py_sex_no_contra

*Cigarette Smoking and Pack a Day 
{
tab r_SMOKE12, miss
tab r_SMOKE12, nolab miss

gen imp_packaday = (r_SMOKE12 == 4 | r_SMOKE12 == 5)

gen imp_p12smoking = (r_SMOKE12 !=0 )
}

foreach i of varlist imp_packaday imp_p12smoking imp_pmBINGE imp_p12pot imp_p12illicit imp_married imp_ever_contraceptive imp_pregnancy imp_sex_anal imp_lifeSTD imp_pySTD  pycontraceptive  wantfuture imp_py_sex_nocontra imp_life_sex_nocontra kidhh {
	
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

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\IMP\NSFG_FEMALE_1519_IMP_REG.log", replace

preserve 

drop if  IDENTITY_COMB == . | IDENTITY_COMB == 3

mi estimate: svy, subpop(if age_eligible == 1): logit imp_pmBINGE i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

mi estimate: svy, subpop(if age_eligible == 1): logit imp_p12smoking i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

mi estimate: svy, subpop(if age_eligible == 1): logit imp_packaday i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit imp_p12pot i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit imp_p12illicit i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit imp_married i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc
 
mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): regress numtimesmar i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): regress hhsize i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit kidhh i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 
 
mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): logit imp_ever_contraceptive i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit pycontraceptive i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 
 
mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): logit imp_life_sex_nocontra i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

   mi estimate: svy, subpop(if age_eligible == 1): logit imp_py_sex_nocontra i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

   mi estimate: svy, subpop(if age_eligible == 1): logit imp_pregnancy i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): logit wantfuture i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): regress numlifesexpart i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

 mi estimate: svy, subpop(if age_eligible == 1): regress imp_numyearsexpart i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 
 
mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

  mi estimate: svy, subpop(if age_eligible == 1): logit imp_sex_anal i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

   mi estimate: svy, subpop(if age_eligible == 1): logit imp_lifeSTD i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

    mi estimate: svy, subpop(if age_eligible == 1): logit imp_pySTD i.IDENTITY_COMB##i.TG2 i.race i.hisp c.cont_age i.cateduc i.faminc 

mi test 1.IDENTITY_COMB#1.TG2 2.IDENTITY_COMB#1.TG2

	restore
 
log close
exit