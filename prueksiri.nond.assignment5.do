clear all
set more off, perm
set scrollbufsize 2000000
set obs 10000
set seed 12345

* REDO: Assignment 2
* Exercise 1 Data Creation

gen x1 = runiform(1,3)
gen x2 = rgamma(3,2)
gen x3 = rnbinomial(10000,0.3)
gen eps = rnormal(2,1)
gen y = 0.5 + 1.2*x1 - 0.9*x2 + 0.1*x3 + eps
gen ydum = 0
egen mean_y = mean(y)
replace ydum = 1 if y > mean_y

* Exercise 2 OLS
corr y x1
reg y x1 x2 x3
bootstrap, reps(49) seed(12345) : reg y x1 x2 x3
bootstrap, reps(499) seed(12345) : reg y x1 x2 x3

* Exercise 3 Probit 
probit ydum x1 x2 x3 

* Exercise 4 Discrete Choice
probit ydum x1 x2 x3 
logit ydum x1 x2 x3 
reg ydum x1 x2 x3 

* Exercise 5 Marginal Effects
probit ydum x1 x2 x3 
margins, dydx(*)
logit ydum x1 x2 x3 
margins, dydx(*)

** Delta Method
probit ydum x1 x2 x3 
margins, dydx(*) vce(delta)

** Bootstrap
bootstrap, reps(49) seed(12345): probit ydum x1 x2 x3 

* REDO: Assignment 3
* Exercise 1 Data Description
clear all
import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A3/product.csv

** Average and dispersion
sum(p*)

g sales =.
replace sales = ppk_stk if choice == 1
replace sales = pbb_stk  if choice == 2
replace sales = pfl_stk if choice == 3
replace sales = phse_stk if choice == 4
replace sales = pgen_stk if choice == 5
replace sales = pimp_stk if choice == 6
replace sales = pss_tub if choice == 7
replace sales = ppk_tub if choice == 8
replace sales = pfl_tub if choice == 9
replace sales = phse_tub if choice == 10

** Market Share
collapse (sum) sales , by(choice)
egen totsales = sum(sales) 
 g mktshare = sales / totsales
list choice mktshare

** Merge Data
clear all
import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A3/demos.csv
save demos.dta

import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A3/product.csv ,clear
merge m:1 hhid using demos.dta

* Exercise 2 + 4 First Model (Conditional Logit) + Marginal Effects

g sales =.
replace sales = ppk_stk if choice == 1
replace sales = pbb_stk  if choice == 2
replace sales = pfl_stk if choice == 3
replace sales = phse_stk if choice == 4
replace sales = pgen_stk if choice == 5
replace sales = pimp_stk if choice == 6
replace sales = pss_tub if choice == 7
replace sales = ppk_tub if choice == 8
replace sales = pfl_tub if choice == 9
replace sales = phse_tub if choice == 10

bysort hhid: gen set = _n

local j = 10
forval j = 1 / 10 {
gen chosen`j' = 0

}

local j = 10
forval j = 1 / 10 {
replace chosen`j' = 1 if choice == `j'
}

reshape long chosen, i(v1) j(c)

g price =.
replace price = ppk_stk if c == 1
replace price = pbb_stk  if c == 2
replace price = pfl_stk if c == 3
replace price = phse_stk if c == 4
replace price = pgen_stk if c == 5
replace price = pimp_stk if c == 6
replace price = pss_tub if c == 7
replace price = ppk_tub if c == 8
replace price = pfl_tub if c == 9
replace price = phse_tub if c == 10

egen gid = group(set hhid)
asclogit chosen price, case(gid) alternatives(c)  nolog
estat mfx

* Exercise 3 + 4 Second Model (Multinomial Logit) + Marginal Effects

mlogit choice income if chosen == 1, nolog

mfx

* Exercise 5 IIA

asmixlogit chosen price, case(gid) alternatives(c) casevars(income) nolog
estimates store bf

drop if c == 1
asmixlogit chosen price, case(gid) alternatives(c) casevars(income) nolog
estimates store br

di "chi2(10) = " 2*( _est_bf - _est_br )
di "Prob > chi2 = "chi2tail(10, 2*( _est_bf - _est_br ))



* REDO: Assignment 4
clear all

*Exercise 1 Data
import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A4/Koop-Tobias.csv
xtset personid timetrnd
reshape wide educ logwage potexper , i(personid) j( timetrnd)
sample 5, count
list logwage*

* Exercise 2 Random Effects
clear all
import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A4/Koop-Tobias.csv
xtset personid timetrnd
xtreg logwage educ potexper, re

* Exercise 3 Fixed Effects Model
** Between Estimator
collapse (mean) logwage potexper educ , by(personid)
reg logwage potexper educ

** Within Estimator
clear all
import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A4/Koop-Tobias.csv
xtset personid timetrnd

egen mean_wage = mean(logwage), by(personid)
egen mean_exper = mean(potexper), by(personid)
egen mean_educ = mean(educ), by(personid)
g fe_wage = logwage - mean_wage
g fe_exper = potexper - mean_exper
g fe_educ = educ - mean_educ
reg fe_wage fe_exper fe_educ, nocon

** First time difference 
g fd_wage = logwage - l.logwage
g fd_exper = potexper - l.potexper
g fd_educ = educ - l.educ
reg fd_wage fd_exper fd_educ, nocon

* Exercise 4 Understanding Fixed Effects
clear all
import delimited https://raw.githubusercontent.com/ms486/Econ613/master/Assignments/A4/Koop-Tobias.csv
xtset personid timetrnd
reshape wide educ logwage potexper , i(personid) j( timetrnd)
sample 100, count
reshape long educ logwage potexper , i(personid) j( timetrnd)

gen alpha = .
qui reg logwage educ potexper ibn.personid, noconst
levelsof personid, local(levels) 
foreach l of local levels {
replace alpha = _b[`l'.personid] if personid == `l'
}

reshape wide alpha educ logwage potexper , i(personid) j( timetrnd)
reg alpha0 ability  mothered fathered brknhome siblings


