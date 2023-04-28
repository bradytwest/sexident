capture log close
clear all
set more off

*log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\NSFG_1519_3_4_catCHECK.log", replace

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\NSFG_1519_3_4_catCHECKhisp_FEMALE.log", replace

use "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\data\NSFG\DATA\15-19\COMBINED\NSFG_FULL_1519_FINAL.dta", clear

*Combine sex identity
{
tab IDENTITY_B IDENTITY_A, miss //Need to combine variables
tab IDENTITY_B IDENTITY_A, miss nolab //Need to combine variables

gen IDENTITY_COMB = IDENTITY_A 
replace IDENTITY_COMB = IDENTITY_B if IDENTITY_COMB == . 
recode IDENTITY_COMB (3 = .s )

la de IDENTITY_COMB 0 "Heterosexual" 1 "Homosexual or gay or lesbian" 2 "Bisexual" .s "Something Else"
la val IDENTITY_COMB IDENTITY_COMB

tab IDENTITY_COMB IDENTITY_A ,miss //Combination worked
tab IDENTITY_COMB IDENTITY_B ,miss //Combination worked

gen identity_type = (IDENTITY_B !=.)
replace identity_type =. if IDENTITY_B == . & IDENTITY_A == . 
la de identity_type 0 "TG1" 1 "TG2"
la val identity_type identity_type

tab identity_type IDENTITY_B, miss
tab identity_type IDENTITY_A, miss
}

*Setup the SVYSET

svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

*Female
{
*Run the Regression for PM BINGE
{
svy, subpop(if female == 1 & age_eligible == 1): logit pmBINGE i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit pmBINGE i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit pmBINGE i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit pmBINGE i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for PY CIG
{

svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit p12smoking i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit p12smoking i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit p12smoking i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit p12smoking i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for PY PACK A DAY
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit packaday i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit packaday i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit packaday i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit packaday i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for PY MJ
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit p12pot i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit p12pot i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 1 & age_eligible == 1)

marginsplot, scheme(s1mono) title("") ytitle("Marginal Predicted Probability of" "Past-Year Marijuana Use") xtitle("Sexual Identity") note("TG1 = 3-Category Sexual Identity                 TG2 = 4-Cateogry Sexual Identity") xlabel(,labsize(small))

graph export  "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\Female_MJ.jpg" , replace

svy: logit p12pot i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit p12pot i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for PY OTHER DRUG
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit p12illicit i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit p12illicit i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit p12illicit i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit p12illicit i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for MARITAL STATUS
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit married i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

svy, subpop(if female == 1 & age_eligible == 1): logit married i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit married i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit married i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc



}

*Run the Regression for NUMB TIME MAR
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): regress numtimesmar i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

svy, subpop(if female == 1 & age_eligible == 1): regress numtimesmar i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

}

*Run the Regression for HH Size 
{
svy, subpop(if female == 1 & age_eligible == 1): regress hhsize i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race  i.hispanic

svy, subpop(if female == 1 & age_eligible == 1): regress hhsize i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race  i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB

}

*Run the Regression for Children Under 18 in HH 
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit kidhh i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit kidhh i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit kidhh i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit kidhh i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for Pregnancy
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1 & pregnancy_eligible == 1): logit pregnancy i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1 & pregnancy_eligible == 1): logit pregnancy i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit pregnancy i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1 & pregnancy_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit pregnancy i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for CONTRA LIFE
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit ever_contraceptive i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit ever_contraceptive i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit ever_contraceptive i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit ever_contraceptive i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for CONTRA PY
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit pycontraceptive i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race  i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit pycontraceptive i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit pycontraceptive i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit pycontraceptive i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for SEX NO CONTRA LIFE
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit life_sex_nocontra i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit life_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race  i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit life_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit life_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for SEX NO CONTRA PY
{
	
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit py_sex_nocontra i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit py_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit py_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit py_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for WANT CHILD
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit wantfuture i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit wantfuture i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 1 & age_eligible == 1)

marginsplot, scheme(s1mono) title("") ytitle("Marginal Predicted Probability of" "Wanting a Child/Another Child") xtitle("Sexual Identity") note("TG1 = 3-Category Sexual Identity                 TG2 = 4-Cateogry Sexual Identity")  xlabel(,labsize(small))

graph export  "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\Female_want.jpg" , replace

svy: logit wantfuture i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit wantfuture i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for SEX PARTNERS LIFE
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): regress numlifesexpart i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): regress numlifesexpart i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

}

*Run the Regression for SEX PARTNERS PY
{
svy, subpop(if female == 1 & age_eligible == 1): regress numyearsexpart i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): regress numyearsexpart i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

}

*Run the Regression for LIFE ANAL SEX
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit sex_anal i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit sex_anal i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit sex_anal i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit sex_anal i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for PY STD
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit pySTD i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit pySTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit pySTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit pySTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for LIFE STD
{
	
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit lifeSTD i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 1 & age_eligible == 1): logit lifeSTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 1 & age_eligible == 1)

marginsplot, scheme(s1mono) title("") ytitle("Marginal Predicted Probability of" "Having Herpes, Genital Warts  Syphilis" "in Lifetime") xtitle("Sexual Identity") note("TG1 = 3-Category Sexual Identity                 TG2 = 4-Cateogry Sexual Identity")  xlabel(,labsize(small))

graph export  "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\Female_STDLIFE.jpg" , replace

svy: logit lifeSTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 1 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 1 & age_eligible == 1): logit lifeSTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}
}
log close

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\NSFG_1519_3_4_catCHECKhisp_MALE.log", replace

*MALE
{
	svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

la de male_ident 0 "Heterosexual" 1 "Homosexual or Gay" 2 "Bisexual"
la val IDENTITY_COMB male_ident

*Run the Regression for PM BINGE
{
svy, subpop(if female == 0 & age_eligible == 1): logit pmBINGE i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit pmBINGE i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit pmBINGE i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit pmBINGE i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for PY CIG
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit p12smoking i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit p12smoking i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins  IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 0 & age_eligible == 1)

marginsplot, scheme(s1mono) title("") ytitle("Marginal Predicted Probability of" "Past-Year Cigarette Usage") xtitle("Sexual Identity") note("TG1 = 3-Category Sexual Identity                 TG2 = 4-Cateogry Sexual Identity")  xlabel(,labsize(small))

graph export  "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\Male_CIG.jpg" , replace

svy: logit p12smoking i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit p12smoking i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for PY PACK A DAY
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit packaday i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit packaday i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit packaday i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit packaday i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for PY MJ
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit p12pot i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit p12pot i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit p12pot i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof
svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit p12pot i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for PY OTHER DRUG
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit p12illicit i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit p12illicit i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB

margins IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 0 & age_eligible == 1)

marginsplot, scheme(s1mono) title("") ytitle("Marginal Predicted Probability of" "Past-Year Other Drug Use") xtitle("Sexual Identity") note("TG1 = 3-Category Sexual Identity                 TG2 = 4-Cateogry Sexual Identity")  xlabel(,labsize(small))

graph export  "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\Male_ODU.jpg" , replace

svy: logit p12illicit i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit p12illicit i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for MARITAL STATUS
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit married i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

svy, subpop(if female == 0 & age_eligible == 1): logit married i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit married i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit married i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for NUMB TIME MAR
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): regress numtimesmar i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

svy, subpop(if female == 0 & age_eligible == 1): regress numtimesmar i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

}

*Run the Regression for HH Size 
{
svy, subpop(if female == 0 & age_eligible == 1): regress hhsize i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race  i.hispanic

svy, subpop(if female == 0 & age_eligible == 1): regress hhsize i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race  i.hispanic

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins  IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 0 & age_eligible == 1)

marginsplot, scheme(s1mono) title("") ytitle("Marginal Predicted Size of Household") xtitle("Sexual Identity") note("TG1 = 3-Category Sexual Identity                 TG2 = 4-Cateogry Sexual Identity")  xlabel(,labsize(small))

graph export  "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSFG\ANALYSIS\3v4 SEX\Male_HHSize.jpg" , replace

}

*Run the Regression for Children Under 18 in HH
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit kidhh i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit kidhh i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit kidhh i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit kidhh i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for LIFE CONTRA
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit vasectomy i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit vasectomy i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit vasectomy i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit vasectomy i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for LAST TIME CONDOM
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit last_sex_cond i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit last_sex_cond i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit last_sex_cond i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof
svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit last_sex_cond i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for LIFE NO CONTRA
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit life_sex_nocontra i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit life_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit life_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof
svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit life_sex_nocontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for LAST TIME NO CONTRA
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit no_lastcontra i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit no_lastcontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit no_lastcontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof
svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit no_lastcontra i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for WANT CHILD
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit wantfuture i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit wantfuture i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 0 & age_eligible == 1)


svy, subpop(if female == 0 & age_eligible == 1): logit wantfuture i.identity_type##i.IDENTITY_COMB 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

margins IDENTITY_COMB, by(identity_type) vce(unconditional) subpop(if female == 0 & age_eligible == 1)

svy: logit wantfuture i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit wantfuture i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for SEX PARTNERS LIFE
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): regress numlifesexpart i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): regress numlifesexpart i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

}

*Run the Regression for SEX PARTNERS PY
{
svy, subpop(if female == 0 & age_eligible == 1): regress numyearsexpart i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): regress numyearsexpart i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

}

*Run the Regression for LIFE ANAL SEX
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit sex_anal i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit sex_anal i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB

svy: logit sex_anal i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof 

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit sex_anal i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}

*Run the Regression for PY STD
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit pySTD i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit pySTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit pySTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof 
svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit pySTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc


}

*Run the Regression for LIFE STD
{
		svyset [pweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit lifeSTD i.identity_type i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

svy, subpop(if female == 0 & age_eligible == 1): logit lifeSTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 

svy: logit lifeSTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic if female == 0 & age_eligible == 1

estat gof 

svyset [iweight = WGT2015_2019], strata(sest) psu(secu) 

svy, subpop(if female == 0 & age_eligible == 1): logit lifeSTD i.identity_type##i.IDENTITY_COMB i.cat_age i.cateduc i.faminc i.race i.hispanic 

test 1.identity_type#1.IDENTITY_COMB 1.identity_type#2.IDENTITY_COMB 
lroc

}
}
log close
exit