

//Changing the directory
cd "C:\Users\aReda\Google Drive\MA Thesis\Indonesia"
capture log close
log using priminf.text, replace
global user "areda"
display "Analysis run by $user at `c(current_time)' `c(current_date)'
version 11.0
*****************************************************
*********** Secondary Infertility research***********
*****************************************************

//Infertility research
//Ayalu Alex Reda

clear *
set memory 3000m, perm
set matsize 2000, perm
set maxvar 10000, perm
set more off

use IDIR61FL_2012.dta, clear

duplicates report caseid
gen id = caseid

******************************************
****complex survey sample variables*******
******************************************
gen sampwt=(v005/1000000)
gen psu = v021
gen stratum = v023
svyset psu [pweight=sampwt], strata(stratum)

*******************descriptive variable******************************
****************common to both primary and secondary infertility*****
*********************************************************************

//RESTRICTING & CREATING VARIABLES one by one
//DESCRIPTIVE statistics
//EDUCATION
codebook v106
gen education = v106
label define educ 0 "no education" 1 "primary education" 2 "secondary education" 3 "Higher education"
label values education educ
tab educ

//WEALTH INDEX
*Wealth index in five quantiles accross years
codebook v190
tab v190
gen wealth = v190
tab wealth

//Residence
//Urban/rural
codebook v025
tab v025
gen reside = v025

//AGE
//current age in discrete years - 
gen age=v012
sum age, det

//FIVE YEAR AGE CATEGORY
//we drop less than 19 year olds since it is less realistic to expect
//infertility in that age group
codebook v013
drop if v013==1
tab v013
gen agecat = v013
recode v013 (2/3=2 "20-29yrs") (4/5=4 "30-39yrs") (6/7=6 "40-49yrs"), gen(agecat2)
tab agecat2

************************************************
*****these will be used to restrict the data****
************************************************

//RETAINING COUPLES
//first we need to drop never married/union groups and those who are widowed
//This is due to lack of or uncertainty in exposure period
//This is because we calculate infertility with in the population of women exposed to the risk of pregnancy
codebook v501
tab v501 
drop if v501==0 | v501==3
tab v501

//dropping sterilized women
//we see below that all sterilzed women had everhad a child
//I will not be dropping them for the primary infertility sample since
//they were exposed to child birth and had it at least once
gen totchild=v201
tab v320
gen sterilized = v320
tab sterilized totchild
keep if sterilized==.

//any CURRENT contraceptive use
codebook v313
gen curuse = v313
recode curuse 0=0 1/3=1
tab curuse //contraceptive
keep if curuse==0

//Ideal number of children
//I code them into zero(no) and yes, wants children
codebook v613, tab(30)
gen wantchild = v613
recode wantchild 1/99=1
label def chilwant 1 "wants children" 0 "does not want children"
label values wantchild chilwant
tab wantchild
drop if wantchild==0

//Desire for more children
//I am declaring sterilized women as missing 
codebook v605
gen morewant=v605
recode morewant 5=0 1/4=1 6=. 7=1 9=.
labe def morewantt 0 "does not want" 1 "wants children"
label values morewant morewantt
tab morewant

*********************************************
*******Event and Event History variables*****
*********Primary Infertility*****************
*********************************************

//working on even history variables.
//time to event is calculated based on the CMC provided in the dhs data
//The waiting time before child birth is calculated from the date of union
//to the date the woman had her first child, if not, it is censored at teh time
//of the survey.
//Date of first marriage/union - cmc
codebook v509
sum v509
gen unioncmc = v509

//DATE of first birth - cmc form - convert into months
codebook v211
sum v211
gen firstbcmc=v211

//date of survey
codebook v008
sum v008
gen survcmc = v008

//fertility history
//total birth history variable
codebook v201
tab v201
gen totbirth = v201

//gen everbirth = 0
//replace everbirth=1 if !missing(b3_01)
//label define everb 0 "no" 1 "yes"
//label values everbirth everb

//gen secondbirth = 0
//replace secondbirth=1 if !missing(b3_02)
//label values secondchild everb

recode totbirth (0=0 "no child yet") (1/18=1 "ever had a child"), gen(everbirth)
recode totbirth (0=.) (1=0 "only first child") (2/18=1 "had second child"), gen(secondchild)

//generating survival times
gen time1 = v211-v509 if everbirth==1
replace time1=v008-v509 if everbirth==0

//browsing just created variables
//browse unioncmc firstbcmc survcmc everbirth time1

//dropping premarital births since we do ...
drop if time1<=0 | time1==.

****Analysis sample***

gen primsample = !missing(time1, everbirth, age, resid, educ, wealth)
tab primsample

***Descriptive statistics***
***============================

tab1 everbirth agecat agecat2 resid educ wealth if primsample==1
histogram time1 if primsample==1&time1<=205, width(1) percent title("Primary infertility") ytitle("Percent") xtitle("Months") graphregion(color(white)) saving(primx, replace)
histogram time1 if primsample==1&time1<=205, width(1) percent ytitle("Percent") xtitle("Months") graphregion(color(white))
graph save prim, replace

//stsetting data
stset time1, fail(everbirth==1) id(id)
stdescribe
sum time1, det

**stset time1, fail(everbirth==1) id(id) enter(time xx) exit(time xx)

//Kaplan-Meier graph
stdescribe
sts graph, graphregion(color(white)) yline(0.5)
graph save survprimgraph, replace

//generating the baseline hazard, for now assigned a value of 0
//I use the baseline hazard from a cox regression below

gen bh = 0
label variable bh "baseline hazard - for a standard mixture model"

//estimating the primary infertility under different distributional assumptions

//strsmix, dist(weibull) link(identity) bhazard(bh) nolog //this failed to converge.

strsmix, dist(lognormal) link(identity) bhazard(bh) nolog 
estat ic


strsmix, dist(gamma) link(identity) bhazard(bh) nolog 
estat ic //this one is the better model

**generating predicted values
predict rs_all, survival
label variable rs_all "Whole sample"
predict rs_uncur, survival uncured
label variable rs_uncur "Fertile women"
predict cure, cure ci
label variable cure "Predicted primary infertility"

eststo
esttab using testt.rtf, se replace label title("Table 1. Over all estimate of primary infertiltiy")
eststo clear

sort time1
gen time1a = time1
replace time1a = 300 if time1>300&time1<.
twoway (line rs_all time1a, lpattern(shortdash)) (line rs_uncur time1a) (line cure time1a), xlabel(0(100)300) legend(cols(1)) xscale(range(1 300)) title("Primary infertility") graphregion(color(white)) saving(primtotal, replace)

//agecat
xi: strsmix i.agecat2, dist(gamma) link(identities) bhazard(bh) nolog 
eststo

predict cure2, cure ci

sort agecat2 cure2
by agecat2: gen first2 = _n==1
tabdisp agecat2 if first2, cellvar(cure2 cure2_lci cure2_uci) 
 
esttab using testt.rtf, se append title("Table 2. Infertility estimate by age category")
eststo clear

**resid
xi: strsmix i.resid, dist(gamma) link(identity) bhazard(bh) nolog
eststo

predict cure3, cure ci

sort resid cure3
by resid: gen first3 = _n==1
tabdisp resid if first3, cellvar(cure3 cure3_lci cure3_uci) 
 
esttab using testt.rtf, se append title("Table 3. Infertility estimate by residence")
eststo clear

**education
xi: strsmix i.educ, dist(gamma) link(identity) bhazard(bh) nolog
eststo

predict cure4, cure ci

sort educ cure4
by educ: gen first4 = _n==1
tabdisp educ if first4, cellvar(cure4 cure4_lci cure4_uci) 
 
esttab using testt.rtf, se append title("Table 4. Infertility estimate by education")
eststo clear

**wealth
xi: strsmix i.wealth, dist(lognormal) link(identity) bhazar(bh) nolog
eststo

predict cure5, cure ci

sort wealth cure5
by wealth: gen first5 = _n==1
tabdisp wealth if first5, cellvar(cure5 cure5_lci cure5_uci) 

esttab using testt.rtf, se append title("Table 5. Infertility estimate by wealth")
eststo clear

***//

log close


* Next I work secondary infertility
//Changing the directory
cd "C:\Users\aReda\Google Drive\MA Thesis\Indonesia"
capture log close
log using secondaryinf.text, replace
global user "areda"
display "Analysis run by $user at `c(current_time)' `c(current_date)'
version 11.0
******************************************
***********Infertility research***********
******************************************

//Infertility research
//Ayalu Alex Reda

clear *
set memory 3000m, perm
set matsize 2000, perm
set maxvar 10000, perm
set more off

use IDIR61FL_2012.dta, clear

duplicates report caseid
gen id = caseid

******************************************
****complex survey sample variables*******
******************************************
gen sampwt=(v005/1000000)
gen psu = v021
gen stratum = v023
svyset psu [pweight=sampwt], strata(stratum)


*******************descriptive variable******************************
****************common to both primary and secondary infertility*****
*********************************************************************

//RESTRICTING & CREATING VARIABLES one by one
//DESCRIPTIVE statistics
//EDUCATION
codebook v106
gen education = v106
label define educ 0 "no education" 1 "primary education" 2 "secondary education" 3 "Higher education"
label values education educ
tab educ

//WEALTH INDEX
*Wealth index in five quantiles accross years
codebook v190
tab v190
gen wealth = v190
tab wealth

//Residence
//Urban/rural
codebook v025
tab v025
gen reside = v025

//AGE
//current age in discrete years - 
gen age=v012
sum age, det

//FIVE YEAR AGE CATEGORY
//we drop less than 19 year olds since it is less realistic to expect
//infertility in that age group
codebook v013
drop if v013==1
tab v013
gen agecat = v013
recode v013 (2/3=2 "20-29yrs") (4/5=4 "30-39yrs") (6/7=6 "40-49yrs"), gen(agecat2)
tab agecat2

************************************************
*****these will be used to restrict the data****
************************************************

//RETAINING COUPLES
//first we need to drop never married/union groups and those who are widowed
//This is due to lack of or uncertainty in exposure period
//This is because we calculate infertility with in the population of women exposed to the risk of pregnancy
codebook v501
tab v501 
drop if v501==0 | v501==3
tab v501

//dropping sterilized women
//we see below that all sterilzed women had everhad a child
//I will not be dropping them for the primary infertility sample since
//they were exposed to child birth and had it at least once
gen totchild=v201
tab v320
gen sterilized = v320
tab sterilized totchild
keep if sterilized==.

//any CURRENT contraceptive use
codebook v313
gen curuse = v313
recode curuse 0=0 1/3=1
tab curuse //contraceptive
keep if curuse==0

//Ideal number of children
//I code them into zero(no) and yes, wants children
codebook v613, tab(30)
gen wantchild = v613
recode wantchild 1/99=1
label def chilwant 1 "wants children" 0 "does not want children"
label values wantchild chilwant
tab wantchild
drop if wantchild==0

//Desire for more children
//I am declaring sterilized women as missing 
codebook v605
gen morewant=v605
recode morewant 5=0 1/4=1 6=. 7=1 9=.
labe def morewantt 0 "does not want" 1 "wants children"
label values morewant morewantt
tab morewant
***********************************************
*************working on the second birth**********************
***********************************************

//working on even history variables.
//time of first birth
codebook v211
sum v211
gen firstbcmc=v211
//date of survey
codebook v008
sum v008
gen survcmc = v008

//fertility history
//total birth history variable
codebook v201
tab v201
gen totbirth = v201

//generating birth histories
recode totbirth (0=0 "no child yet") (1/18=1 "ever had a child"), gen(everbirth)
recode totbirth (0=.) (1=0 "only first child") (2/19=1 "had second child"), gen(secondchild)
recode everbirth 0=.
drop if everbirth==.

//dropping premarital births

drop if v509 >= v211 // this is the final sample on which the analysis starts

//restricting to those with at least a first birth
//i.e dropping those women who never had a child

rename b3_01 b3_1
rename b3_02 b3_2
rename b3_03 b3_3
rename b3_04 b3_4
rename b3_05 b3_5
rename b3_06 b3_6
rename b3_07 b3_7
rename b3_08 b3_8
rename b3_09 b3_9

//Next I am working on date of birth from birth histories
//to identify the time taken to a second birth after the first one
//this is very tricky so a reshape procedure after tweaking the b3_ cmc date series does the job 

preserve

keep b3* v211 caseid

reshape long b3_, i(caseid) j(order)
drop if b3_==.

bysort caseid: gen time2 = b3_- v211 if _n == _N-1

bysort caseid: replace time2 = time2[_N-1] if time2==.
bysort caseid: replace time2=time2[_n]
gen ordmiss = 1 if time2==.

reshape wide b3_, i(caseid) j(order)

sort caseid

save time2data, replace

restore //data restored to long form


merge 1:1 caseid using time2data, keep(match master)

replace time2=v008-v211 if ordmiss==1
drop if time2==0

stset time2, fail(secondchild==1) id(id)

*****************************************************
****Analysis sample***

gen secondsample = !missing(time2, secondchild, agecat, resid, educ, wealth)
tab secondsample

***Descriptive statistics***
***============================

tab1 secondchild agecat2 resid educ wealth if secondsample==1
histogram time2 if secondsample==1&time2<=205, width(1) percent title("Secondary infertility") ytitle("Percent") xtitle("Months") graphregion(color(white)) saving(seco2, replace)
histogram time2 if secondsample==1&time2<=205, width(1) percent ytitle("Percent") xtitle("Months") graphregion(color(white))
graph save seco, replace

sum time2, det

//Kaplan-Meier graph
sts graph, graphregion(color(white)) yline(0.5)
graph save survsecograph, replace
stdescribe

//generating the baseline hazard, for now assigned a value of 0
//I use the baseline hazard from a cox regression below

gen bh = 0
label variable bh "baseline hazard - for a standard mixture model"

//estimating secondary infertility under different assumptions
strsmix, dist(gamma) link(identity) bhazard(bh) nolog
**generating predicted values
predict rs_all, survival ci
label variable rs_all "Whole sample"
predict rs_uncur, survival uncured ci
label variable rs_uncur "Fertile women"
predict cure, cure ci
label variable cure "Predicted secondary infertility"

eststo
esttab using testt2.rtf, se replace label title("Over all estimate of secondary infertiltiy")
eststo clear

sort time2
twoway (line rs_all time2, lpattern(shortdash)) (line rs_uncur time2) (line cure time2), xlabel(0(100)400) legend(cols(1)) title("Secondary infertility") xscale(range(1 400)) graphregion(color(white)) saving(sectotal, replace)

//agecat
xi: strsmix i.agecat2, dist(gamma) link(identity) bhazard(bh) nolog
eststo

predict cure2, cure ci

sort agecat2 cure2
by agecat2: gen first2 = _n==1
tabdisp agecat2 if first2, cellvar(cure2 cure2_lci cure2_uci) 
 
esttab using testt2.rtf, se append title("Table 1. Secondary infertility estimate by age category")
eststo clear

//resid
xi: strsmix i.resid, dist(gamma) link(identity) bhazard(bh) nolog
eststo
predict cure3, cure ci

sort resid cure3
by resid: gen first3 = _n==1
tabdisp resid if first3, cellvar(cure3 cure3_lci cure3_uci) 
 
esttab using testt2.rtf, se append title("Table 2. Secondary infertility estimate by residence")
eststo clear

//educ
xi: strsmix i.educ, dist(lognormal) link(identity) bhazard(bh) nolog

eststo

predict cure4, cure ci

sort educ cure4
by educ: gen first4 = _n==1
tabdisp educ if first4, cellvar(cure4 cure4_lci cure4_uci) 
 
esttab using testt2.rtf, se append title("Table 3. Secondary infertility estimate by education")
eststo clear

//wealth
xi: strsmix i.wealth, dist(lognormal) link(identity) bhazar(bh) nolog
eststo

predict cure5, cure ci

sort wealth cure5
by wealth: gen first5 = _n==1
tabdisp wealth if first5, cellvar(cure5 cure5_lci cure5_uci) 

esttab using testt2.rtf, se append title("Table 4. Secondary infertility estimate by wealth")
eststo clear

//Combining descriptive statistics graphs
graph combine prim.gph seco.gph, col(1) saving(descrcomb, replace)
graph combine prim.gph seco.gph, row(1) saving(descrcomb, replace)

//Combining survival curves
graph combine primtotal.gph sectotal.gph, saving(infertility, replace)

