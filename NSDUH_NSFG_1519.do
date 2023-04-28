capture log close
clear all
set more off

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\Both\NSDUH_NSFG_1519_CHECK.log", replace

*Female

use "\\Client\H$\\Dropbox (University of Michigan)\R03 WEST\data\NSDUH\DATA\COMBINED\NSDUH_FULL_FINAL_1519.dta", clear

gen NSFG = 0

append using  "\\Client\H$\\Dropbox (University of Michigan)\R03 WEST\data\NSFG\DATA\15-19\FEMALE\NSFG_FEMALE_1519_FINAL.dta"

*Combine the complex samping variables
{
replace NSFG = 1 if NSFG == . //Survey Identifier

tab sex NSFG, miss //account for the sex stratification in the NSFG

replace sex = 1 if sex == . & NSFG == 1 //account for the sex stratification in the NSFG

tab sex NSFG, miss //Change Accounted for

replace ANALWT_C = WGT2015_2019 if ANALWT_C == . //Combine Weight Variables

tab secu verep, miss //Need to account for cluster code differences

replace secu = 5 if verep == 1 //Combine cluster code variables
replace secu = 6 if verep == 2 //Combine cluster code variables

tab secu verep, miss //Change Accounted for

tab sest vestr, miss //Need to account for stratum code differences

replace sest = vestr if sest == . //Combine stratum code variables

tab sest vestr, miss //Change Accounted for
}
*Combine 3 cat sex identity
{
tab identity IDENTITY_A, miss //Need to combine variables

gen cat3_sexual_identity = identity 
replace cat3_sexual_identity = IDENTITY_A if cat3_sexual_identity == . 

la de cat3_sexual_identity 0 "Heterosexual" 1 "Homosexual or gay or lesbian" 2 "Bisexual"
la val cat3_sexual_identity cat3_sexual_identity

tab cat3_sexual_identity IDENTITY_A ,miss //Combination worked
tab cat3_sexual_identity identity ,miss //Combination worked
}
*change race
{
tab race hisp, miss //Need to harmonize race variable to match

replace race = 3 if hisp == 1 

tab race hisp, miss //Change Accounted for
}
*Setup the SVYSET

svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PM BINGE
{
svy, subpop(if sex == 1 & age_eligible == 1): logit pmBINGE i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit pmBINGE i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit pmBINGE i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit pmBINGE i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

* Combine Cig varaibles
{
tab p12smoking NSFG, miss
tab p12smoking p12_100cig_smoking, miss

replace p12_100cig_smoking = p12smoking if NSFG == 1

tab p12smoking p12_100cig_smoking, miss
}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY CIG
{
svy, subpop(if sex == 1 & age_eligible == 1): logit p12_100cig_smoking i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit p12_100cig_smoking i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit p12_100cig_smoking i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit p12_100cig_smoking i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc


}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY PACK A DAY
{
svy, subpop(if sex == 1 & age_eligible == 1): logit packaday i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit packaday i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit packaday i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit packaday i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY MJ
{
svy, subpop(if sex == 1 & age_eligible == 1): logit p12pot i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit p12pot i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit p12pot i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit p12pot i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY OTHER DRUG
{
svy, subpop(if sex == 1 & age_eligible == 1): logit p12illicit i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit p12illicit i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit p12illicit i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit p12illicit i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

* Combine HH Variable
{
recode hhsize (7 8 = 6)

tab hhsize NSFG, miss
}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for HH Size 
{
svy, subpop(if sex == 1 & age_eligible == 1): regress hhsize i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race

svy, subpop(if sex == 1 & age_eligible == 1): regress hhsize i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //No SIG INTERACTION

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for Children Under 18 in HH 
{
svy, subpop(if sex == 1 & age_eligible == 1): logit kidhh i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit kidhh i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit kidhh i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit kidhh i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

* Fix Pregnancy Variable
{

gen new_preg = pregnancy if NSFG == 0 
replace new_preg = pregnancy if NSFG  == 1 & pregnancy_eligible == 1	
	
bysort NSFG: tab new_preg pregnancy if sex == 1, miss

tab  cont_age new_preg if NSFG == 1 & sex == 1, miss
tab  cont_age pregnancy if NSFG == 1, miss


}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for Pregnancy
{
svy, subpop(if sex == 1 & age_eligible == 1): logit new_preg i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit new_preg i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit new_preg i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit new_preg i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*New PY STD Variable
{
	
gen new_PY_STD = std if NSFG == 0	
replace new_PY_STD = pySTD if NSFG == 1

tab new_PY_STD std if NSFG == 0, miss
tab new_PY_STD std if NSFG == 1, miss

tab new_PY_STD pySTD if NSFG == 0, miss
tab new_PY_STD pySTD if NSFG == 1, miss

bysort NSFG: tab new_PY_STD pySTD, miss
bysort NSFG: tab new_PY_STD std, miss
	
}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY STD
{
svy, subpop(if sex == 1 & age_eligible == 1): logit new_PY_STD i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 1 & age_eligible == 1): logit new_PY_STD i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit new_PY_STD i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 1 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 1 & age_eligible == 1): logit new_PY_STD i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}

clear all 

*Male

use "\\Client\H$\\Dropbox (University of Michigan)\R03 WEST\data\NSDUH\DATA\COMBINED\NSDUH_FULL_FINAL_1519.dta", clear

gen NSFG = 0

append using  "\\Client\H$\\Dropbox (University of Michigan)\R03 WEST\data\NSFG\DATA\15-19\MALE\NSFG_MALE_1519_FINAL.dta"

*Combine the complex samping variables
{
replace NSFG = 1 if NSFG == . //Survey Identifier

tab sex NSFG, miss //account for the sex stratification in the NSFG

replace sex = 0 if sex == . & NSFG == 1 //account for the sex stratification in the NSFG

tab sex NSFG, miss //Change Accounted for

replace ANALWT_C = WGT2015_2019 if ANALWT_C == . //Combine Weight Variables

tab secu verep, miss //Need to account for cluster code differences

replace secu = 5 if verep == 1 //Combine cluster code variables
replace secu = 6 if verep == 2 //Combine cluster code variables

tab secu verep, miss //Change Accounted for

tab sest vestr, miss //Need to account for stratum code differences

replace sest = vestr if sest == . //Combine stratum code variables

tab sest vestr, miss //Change Accounted for
}
*Combine 3 cat sex identity
{
tab identity IDENTITY_A, miss //Need to combine variables

gen cat3_sexual_identity = identity 
replace cat3_sexual_identity = IDENTITY_A if cat3_sexual_identity == . 

la de cat3_sexual_identity 0 "Heterosexual" 1 "Homosexual or gay" 2 "Bisexual"
la val cat3_sexual_identity cat3_sexual_identity

tab cat3_sexual_identity IDENTITY_A ,miss //Combination worked
tab cat3_sexual_identity identity ,miss  //Combination worked
}
*change race
{
tab race hisp, miss //Need to harmonize race variable to match

replace race = 3 if hisp == 1 

tab race hisp, miss //Change Accounted for
}
*Setup the SVYSET

svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PM BINGE
{
svy, subpop(if sex == 0 & age_eligible == 1): logit pmBINGE i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit pmBINGE i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit pmBINGE i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit pmBINGE i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

* Combine Cig varaibles
{
tab p12smoking NSFG, miss
tab p12smoking p12_100cig_smoking, miss

replace p12_100cig_smoking = p12smoking if NSFG == 1

tab p12smoking p12_100cig_smoking, miss
}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY CIG
{
svy, subpop(if sex == 0 & age_eligible == 1): logit p12_100cig_smoking i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit p12_100cig_smoking i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit p12_100cig_smoking i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit p12_100cig_smoking i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY PACK A DAY
{
svy, subpop(if sex == 0 & age_eligible == 1): logit packaday i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit packaday i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit packaday i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit packaday i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc


}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY MJ
{
svy, subpop(if sex == 0 & age_eligible == 1): logit p12pot i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit p12pot i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit p12pot i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit p12pot i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY OTHER DRUG
{
svy, subpop(if sex == 0 & age_eligible == 1): logit p12illicit i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit p12illicit i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit p12illicit i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit p12illicit i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

* Combine HH Variable
{
recode hhsize (7 8 = 6)

tab hhsize NSFG, miss
}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for HH Size *ISSUE!!!! SIG INTERACTION
{
svy, subpop(if sex == 0 & age_eligible == 1): regress hhsize i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race

svy, subpop(if sex == 0 & age_eligible == 1): regress hhsize i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //SIG INTERACTION

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for Children Under 18 in HH  *ISSUE!!!! SIG INTERACTION
{ 
svy, subpop(if sex == 0 & age_eligible == 1): logit kidhh i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit kidhh i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity // SIG INTERACTION

svy: logit kidhh i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit kidhh i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*New PY STD Variable
{
	
gen new_PY_STD = std if NSFG == 0	
replace new_PY_STD = pySTD if NSFG == 1

tab new_PY_STD std if NSFG == 0, miss
tab new_PY_STD std if NSFG == 1, miss

tab new_PY_STD pySTD if NSFG == 0, miss
tab new_PY_STD pySTD if NSFG == 1, miss

bysort NSFG: tab new_PY_STD pySTD, miss
bysort NSFG: tab new_PY_STD std, miss
	
}
svyset [pweight = ANALWT_C], strata(sest) psu(secu) 

*Run the Regression for PY STD
{
svy, subpop(if sex == 0 & age_eligible == 1): logit new_PY_STD i.NSFG i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

svy, subpop(if sex == 0 & age_eligible == 1): logit new_PY_STD i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION

svy: logit new_PY_STD i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race if sex == 0 & age_eligible == 1

estat gof

svyset [iweight = ANALWT_C], strata(sest) psu(secu) 

svy, subpop(if sex == 0 & age_eligible == 1): logit new_PY_STD i.NSFG##i.cat3_sexual_identity i.cat_age i.cateduc i.faminc i.race 

test 1.NSFG#1.cat3_sexual_identity 1.NSFG#2.cat3_sexual_identity //NO SIG INTERACTION
lroc

}

log close
exit