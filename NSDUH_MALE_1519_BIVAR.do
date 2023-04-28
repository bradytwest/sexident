capture log close
clear all
set more off

log using "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\do\NSDUH\ANALYSIS\BIVAR\NSDUH_MALE_1519_BIVAR.log", replace

use "\\Client\H$\Dropbox (University of Michigan)\R03 WEST\data\NSDUH\DATA\COMBINED\NSDUH_FULL_FINAL_1519.dta", clear

*Setup SVY Computation

svyset [pweight = ANALWT_C], strata(vestr) psu(verep) 
svydescribe

*Design-Adjusted Bivariate Results

*Past Month Binge

svy, subpop(if age_eligible == 1 & sex == 0): tab pmBINGE identity, col ci obs

*Cigarette Use Past Year

svy, subpop(if age_eligible == 1 & sex == 0): tab p12_100cig_smoking identity, col ci obs

*Pack a Day

svy, subpop(if age_eligible == 1 & sex == 0): tab p12_100_packaday identity, col ci obs

*Marijuana Use Past Year

svy, subpop(if age_eligible == 1 & sex == 0): tab p12pot identity, col ci obs

*Illicit Drug Use Past Year

svy, subpop(if age_eligible == 1 & sex == 0): tab p12illicit identity, col ci obs

*HH Size

svy, subpop(if age_eligible == 1 & sex == 0): mean hhsize, over(identity)
svy, subpop(if age_eligible == 1 & sex == 0): tab hhsize identity, col ci obs

*Child Under 18 in HH

svy, subpop(if age_eligible == 1 & sex == 0): tab kidhh identity, col ci obs

*STD PY

svy, subpop(if age_eligible == 1 & sex == 0): tab std identity, col ci obs

*Univaraite

svy, subpop(if age_eligible == 1 & sex == 0 & identity != .): tab identity, ci obs miss

*Race

svy, subpop(if age_eligible == 1 & sex == 0 & identity != .): tab  identity race, col ci obs miss

log close
exit
