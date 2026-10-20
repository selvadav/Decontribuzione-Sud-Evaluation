cd "$DATAin"

use clean.dta, clear
*use clean.dta, clear


*scegli se inserire i pesi per employees al 2019
global weight "[aweight = firm_weight]"
global suffix "_weighted" 
*global weight ""
*global suffix "_unweighted"       

* Weights
gen weight = employees if year == 2019
bys id: egen firm_weight = max(weight)

drop if firm_weight == .

***************************************************************

* problema employees 2023
/*
tab year if missing(employees), row

gen missing_emp = missing(employees)
tabulate year missing_emp, row //28%
*/

**************************************************************


gen totalpersonnelcoststheur_pem = totalpersonnelcoststheur / number_employees //personnel cost (th) per employee
gen added_value_pem = added_value / number_employees //AV per employee
gen fix_asset_pe = total_fixed_assets / employees


*rename variables
rename added_value av
rename added_value_pem av_pe
rename total_assets totasset
rename totalpersonnelcoststheur perscost
rename totalpersonnelcoststheur_pem perscost_pe
rename profit_loss p_l 
rename total_value_production prod_value
rename returnonassetroa roa
rename returnonequityroe roe
rename returnonsalesros ros

rename total_fixed_assets fix_asset
rename plant_machinery plant_mach
rename sales_revenue revenue

*log
gen lemployees = ln(employees)
gen lwages_salaries = ln(wages_salaries)
gen lperscost = ln(perscost)
gen lperscost_pe = ln(perscost_pe)
gen lav = ln(av)
gen lav_pe = ln(av_pe)
gen lprod_value = ln(prod_value)
gen ltotasset = ln(totasset)
gen lfix_asset = ln(fix_asset)
gen lfix_asset_pe = ln(fix_asset_pe)
gen lrevenue = ln(revenue)



/*descriptive statistics in 2019
estpost summarize employees perscost_pe totasset fix_asset tan_asset intan_asset prod_value av av_pe revenue p_l ros roe roa, detail

esttab using "$DATAout\bs_descriptive.tex", replace ///
    cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) count(fmt(0))") ///
    coeflabels(employees "Employees" ///
               perscost_pe "Personnel Costs p.e." ///
               totasset "Total Assets" ///
               fix_asset "Fixed Assets" ///
			   fix_asset_pe "Fixed Assets p.e."
               prod_value "Production Value" ///
			   av_w "Value Added" ///
               av_pe "Value Added p.e." ///
               revenue "Revenue" ///
               p_l "Profit / Loss" ///
               ros "ROS" ///
               roe "ROE" ///
               roa "ROA") ///
    nomtitle nonumber noobs booktabs ///
    collabels("Mean" "Std. Dev." "Median" "N") ///
    title("Descriptive Statistics")
*/


	
* Winsor 
winsor2 employees wages_salaries perscost perscost_pe totasset fix_asset fix_asset_pe prod_value av av_pe revenue p_l ros roe roa, cuts (1, 99)


/*descriptive statistics in 2019
preserve
keep if year == 2019 

estpost summarize employees_w perscost_pe_w fix_asset_w  fix_asset_pe_w prod_value_w av_w av_pe_w revenue_w p_l_w ros_w roe_w roa_w, detail

esttab using "$DATAout\bs_w_descriptive.tex", replace ///
    cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) count(fmt(0))") ///
    coeflabels(employees_w "Employees" ///
               perscost_pe_w "Personnel Costs p.e." ///
               fix_asset_w "Fixed Assets" ///
               fix_asset_pe_w "Fixed Asset p.e." ///
			   prod_value_w "Production Value" ///
			   av_w "Value Added" ///
               av_pe_w "Value Added p.e." ///
               revenue_w "Revenue" ///
               p_l_w "Profit / Loss" ///
               ros_w "ROS" ///
               roe_w "ROE" ///
               roa_w "ROA") ///
    nomtitle nonumber noobs booktabs ///
    collabels("Mean" "Std. Dev." "Median" "N") ///
    title("Descriptive Statistics winsorized in 2019")


restore 

*/


********************************************************
* TEST baseline comparability 2019
********************************************************

/*************OUTCOMES
preserve
keep if year == 2019

local outcomes lemployees lperscost_pe lfix_asset lfix_asset_pe lprod_value lav lav_pe lrevenue p_l_w ros_w roe_w roa_w
local labels `" "ln(Employees)" "ln(Personnel Costs p.e.)" "ln(Fixed Assets)" "ln(Fixed Assets p.e.)" "ln(Production Value)" "ln(Value Added)" "ln(Value Added p.e.)" "ln(Revenue)" "Profit / Loss" "ROS" "ROE" "ROA" "'

tempname results
tempfile resultsfile
postfile `results' str40 varname double mean_t double mean_c double diff str3 stars using "`resultsfile'", replace

local i = 1
foreach v of local outcomes {

    * 1. Pesi identici alla stima DiD (profit/loss non pesato)
    if "`v'" == "p_l_w" {
        local this_weight ""
    }
    else {
        local this_weight "$weight"
    }

    * 2. Confronto tramite regressione pesata e clusterizzata
    quietly regress `v' treated `this_weight', vce(cluster id_municipality)

    * 3. Media controlli (costante), differenza (coefficiente), media trattati
    local mean_c = _b[_cons]
    local diff   = _b[treated]
    local mean_t = `mean_c' + `diff'

    quietly test treated
    local pval = r(p)
	display "`v': diff = " %9.3f `diff' "   p = " %6.4f `pval'

    * 4. Asterischi
    if `pval' < 0.01 {
        local stars "***"
    }
    else if `pval' < 0.05 {
        local stars "**"
    }
    else if `pval' < 0.10 {
        local stars "*"
    }
    else {
        local stars ""
    }

    local lbl : word `i' of `labels'
    post `results' ("`lbl'") (`mean_t') (`mean_c') (`diff') ("`stars'")

    local i = `i' + 1
}

postclose `results'

use "`resultsfile'", clear

* Scrittura diretta del file .tex (stile esttab)
file open texfile using "$DATAout\ttest.tex", write replace

file write texfile "\begin{table}[H]\centering" _n
file write texfile "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write texfile "\caption{Balance 2019}" _n
file write texfile "\label{tab:ttest}" _n
file write texfile "\begin{tabular}{lccc}" _n
file write texfile "\toprule" _n
file write texfile " & Mean Treated & Mean Control & Difference (T-C) \\" _n
file write texfile "\midrule" _n

local N = _N
forvalues i = 1/`N' {
    local vname = varname[`i']
    local st    = stars[`i']

    local mtstr : display %9.2fc mean_t[`i']
    local mcstr : display %9.2fc mean_c[`i']
    local dstr  : display %9.2fc diff[`i']

    local mtstr = strtrim("`mtstr'")
    local mcstr = strtrim("`mcstr'")
    local dstr  = strtrim("`dstr'")

    if "`st'" != "" local dstr "`dstr'\sym{`st'}"

    file write texfile "`vname' & `mtstr' & `mcstr' & `dstr' \\" _n
}

file write texfile "\bottomrule" _n
file write texfile "\end{tabular}" _n
file write texfile "\begin{minipage}{0.95\textwidth}" _n
file write texfile "\footnotesize" _n
file write texfile "\vspace{1mm}" _n
file write texfile "\textit{Notes:} Sample restricted to 2019. Means are weighted by 2019 employment, except for Profit / Loss, which is unweighted. Standard errors are clustered at municipality level. Logarithms are computed on non-winsorised values and differences in logarithms are expressed in log points. Profit / Loss, ROS, ROE and ROA are winsorised at the 1st and 99th percentiles. Significance: \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)." _n
file write texfile "\end{minipage}" _n
file write texfile "\end{table}" _n

file close texfile

restore

*/

/*balance table

preserve
keep if year == 2019

local outcomes employees_w perscost_pe_w fix_asset_w fix_asset_pe_w prod_value_w av_w av_pe_w revenue_w p_l_w ros_w roe_w roa_w
local labels `" "Employees" "Personnel Costs p.e." "Fixed Assets" "Fixed Assets p.e." "Production Value" "Value Added" "Value Added p.e." "Revenue" "Profit / Loss" "ROS" "ROE" "ROA" "'

tempname results
tempfile resultsfile
postfile `results' str40 varname double mean_t double mean_c double diff double reldiff str3 stars long n_t long n_c using "`resultsfile'", replace

local i = 1
foreach v of local outcomes {

    quietly ttest `v', by(treated) unequal

    local mean_c = r(mu_1)   // treated == 0
    local mean_t = r(mu_2)   // treated == 1
    local diff    = `mean_t' - `mean_c'
    local reldiff = (`diff' / `mean_c') * 100
    local pval    = r(p)
    local n_c     = r(N_1)
    local n_t     = r(N_2)

    if `pval' < 0.01 {
        local stars "***"
    }
    else if `pval' < 0.05 {
        local stars "**"
    }
    else if `pval' < 0.10 {
        local stars "*"
    }
    else {
        local stars ""
    }

    local lbl : word `i' of `labels'
    post `results' ("`lbl'") (`mean_t') (`mean_c') (`diff') (`reldiff') ("`stars'") (`n_t') (`n_c')

    local i = `i' + 1
}
postclose `results'

use "`resultsfile'", clear

file open texfile using "$DATAout\balance_2019.tex", write replace

file write texfile "\begin{table}[H]\centering" _n
file write texfile "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file write texfile "\caption{Pre-treatment Summary Statistics and Balance (2019)}" _n
file write texfile "\label{tab:balance2019}" _n
file write texfile "\begin{tabular}{lcccc}" _n
file write texfile "\toprule" _n
file write texfile " & Treated & Control & Difference (T-C) & Relative Diff. (\%) \\" _n
file write texfile "\midrule" _n

local N = _N
forvalues i = 1/`N' {
    local vname = varname[`i']
    local st    = stars[`i']

    local mtstr : display %9.2fc mean_t[`i']
    local mcstr : display %9.2fc mean_c[`i']
    local dstr  : display %9.2fc diff[`i']
    local rdstr : display %9.2f  reldiff[`i']

    local mtstr = strtrim("`mtstr'")
    local mcstr = strtrim("`mcstr'")
    local dstr  = strtrim("`dstr'")
    local rdstr = strtrim("`rdstr'")

    if "`st'" != "" local dstr "`dstr'\sym{`st'}"

    file write texfile "`vname' & `mtstr' & `mcstr' & `dstr' & `rdstr' \\" _n
}


file write texfile "\bottomrule" _n
file write texfile "\end{tabular}" _n
file write texfile "\begin{minipage}{0.95\textwidth}" _n
file write texfile "\raggedright\footnotesize" _n
file write texfile "\vspace{1mm}" _n
file write texfile "\textit{Notes:} Sample restricted to 2019. All variables are winsorised at the 1st and 99th percentiles. Relative difference is the difference as a percentage of the control mean. Two-sample t-test with unequal variances. Significance: \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)." _n
file write texfile "\end{minipage}" _n
file write texfile "\end{table}" _n

file close texfile

restore

*/

***********************************************************
*create the event time dummy
*******************************************************
*event centred
gen event_time = year - 2020
*Dummy per ogni event time 
tabulate event_time, gen(event_time_dummy)
*label
local anno_inizio = 2016
forvalues i = 1/8 {
	local anno_corrente = `anno_inizio' + `i' - 1
	label variable event_time_dummy`i' "year `anno_corrente'"
} 

*Interagire dummy con treatment 
forvalues j = 1 (1) 8 {
	gen event_time_dummy`j'xtreated = event_time_dummy`j'*treated 
}
*label
local anno_inizio = 2016
forvalues i = 1/8 {
	local anno_corrente = `anno_inizio' + `i' - 1
	label variable event_time_dummy`i'xtreated "`anno_corrente' and treated"
} 

drop event_time_dummy4*

********************************************************
* F-test
*******************************************************

/************OUTCOMES

preserve

local outcomes lemployees lperscost_pe lfix_asset lfix_asset_pe lprod_value lav lav_pe lrevenue p_l_w ros_w roe_w roa_w

local labels `" "ln(Employees)" "ln(Personnel Costs p.e.)" "ln(Fixed Assets)" "ln(Fixed Assets p.e.)" "ln(Production Value)" "ln(Value Added)" "ln(Value Added p.e.)" "ln(Revenue)" "Profit / Loss" "ROS" "ROE" "ROA" "'

tempname results1
tempfile resultsfile1
postfile `results1' str40 varname double Fstat double pval double Nobs using "`resultsfile1'", replace

local i = 1
foreach var of local outcomes {

    if "`var'" == "p_l_w" {
        local this_weight ""
    }
    else {
        local this_weight "$weight"
    }

    quietly reghdfe `var' event_time_dummy*xtreated `this_weight', absorb(id event_time) vce(cluster id_municipality)

    quietly test event_time_dummy1xtreated event_time_dummy2xtreated event_time_dummy3xtreated

    local Fstat = r(F)
    local pval  = r(p)
    local Nobs  = e(N)

    local lbl : word `i' of `labels'
    post `results1' ("`lbl'") (`Fstat') (`pval') (`Nobs')

    local i = `i' + 1
}
postclose `results1'

use "`resultsfile1'", clear

file open texfile using "$DATAout\ftest.tex", write replace

file write texfile "\begin{table}[H]\centering" _n
file write texfile "\caption{Parallel Trends Joint F-Tests}" _n
file write texfile "\label{tab:ftest}" _n
file write texfile "\begin{tabular}{lccc}" _n
file write texfile "\toprule" _n
file write texfile "Variable & F-statistic & P-value & Observations \\" _n
file write texfile "\midrule" _n

local N = _N
forvalues i = 1/`N' {
    local vname = varname[`i']
    local Fs : display %9.3f Fstat[`i']
    local ps : display %9.3f pval[`i']
    local ns : display %9.0fc Nobs[`i']

    file write texfile "`vname' & `=strtrim("`Fs'")' & `=strtrim("`ps'")' & `=strtrim("`ns'")' \\" _n
}

file write texfile "\bottomrule" _n
file write texfile "\end{tabular}" _n
file write texfile "\begin{minipage}{0.95\textwidth}" _n
file write texfile "\raggedright\footnotesize" _n
file write texfile "\vspace{1mm}" _n
file write texfile "\textit{Notes:} Joint F-test that the pre-treatment interaction terms (2016, 2017, 2018) of the event-study specification are equal to zero. Estimates are weighted by 2019 employment, except for Profit / Loss, which is unweighted. The specification includes firm and year fixed effects, and standard errors are clustered at municipality level." _n
file write texfile "\end{minipage}" _n 
file write texfile "\end{table}" _n

file close texfile

restore

*/



*******************************************************
/* Parallel trends graphs
*******************************************************



local outcomes lemployees lperscost_pe lfix_asset lfix_asset_pe lprod_value lav lav_pe lrevenue p_l_w ros_w roe_w roa_w
local ytitles `" "ln(Employees)" "ln(Personnel Costs p.e.)" "ln(Fixed Assets)" "ln(Fixed Assets p.e.)" "ln(Production Value)" "ln(Value Added)" "ln(Value Added p.e.)" "ln(Revenue)" "Profit / Loss" "ROS" "ROE" "ROA" "'

cap mkdir "$DATAout/parallel trends"

local i = 1
foreach var of local outcomes {

    local ytitle : word `i' of `ytitles'

    * profit/loss non pesato, come nelle stime
    if "`var'" == "p_l_w" {
        local this_weight ""
    }
    else {
        local this_weight "$weight"
    }

    preserve
    keep if !missing(`var') & !missing(firm_weight)

    * medie (pesate) per anno e gruppo
    collapse (mean) m = `var' `this_weight', by(year treated)
    reshape wide m, i(year) j(treated)

    twoway (line m0 year, lcolor(gs8) lwidth(medium) lpattern(dash)) ///
           (scatter m0 year, mcolor(gs8) msymbol(D) msize(medium)) ///
           (line m1 year, lcolor(black) lwidth(medium)) ///
           (scatter m1 year, mcolor(black) msymbol(O) msize(medium)), ///
           xline(2020, lpattern(dash) lcolor(gs10) lwidth(medium)) ///
           xlabel(2016(1)2023, labsize(large)) ///
           ylabel(, format(%9.2f) angle(horizontal) labsize(large)) ///
           ytitle("`ytitle'", size(large)) xtitle("Year", size(large)) ///
           legend(off) ///
		   /*legend(order(3 "Treated" 1 "Control") rows(1) position(6) size(medsmall))*/ ///
           graphregion(color(white)) bgcolor(white) ///
           note("")

    graph export "$DATAout/parallel trends/trend_`var'$suffix.pdf", replace
    restore

    local i = `i' + 1
}


*/
		
****************************************************************
* GRAPHS - EVENT STUDY
***************************************************************

local outcome "lemployees lperscost_pe lfix_asset lfix_asset_pe lprod_value lav lav_pe lrevenue p_l_w ros_w roe_w roa_w"
local ytitles `" "ln(Employees)" "ln(Personnel Costs p. e.)" "ln(Fixed Asset)" "ln(Fixed Asset p.e.)" "ln(Production Value)" "ln(Value Added)" "ln(Value Added p.e.)" "ln(Revenue)" "Profit / Loss" "ROS" "ROE" "ROA" "'

local i = 1
foreach var of local outcome {
    local ytitle : word `i' of `ytitles'

    * profit/loss non pesato
    if "`var'" == "p_l_w" {
        local this_weight ""
    }
    else {
        local this_weight "$weight"
    }

    reghdfe `var' event_time_dummy*xtreated `this_weight', absorb(id event_time) vce(cluster id_municipality)
    estimates store model`i'
    test event_time_dummy1xtreated event_time_dummy2xtreated event_time_dummy3xtreated
    estadd scalar pval_pt = r(p)

    /* estrazione coefficienti per grafico dalla stessa stima
    scalar c1 = _b[event_time_dummy1xtreated]
    scalar c2 = _b[event_time_dummy2xtreated]
    scalar c3 = _b[event_time_dummy3xtreated]
    scalar c5 = _b[event_time_dummy5xtreated]
    scalar c6 = _b[event_time_dummy6xtreated]
    scalar c7 = _b[event_time_dummy7xtreated]
    scalar c8 = _b[event_time_dummy8xtreated]
    scalar se1 = _se[event_time_dummy1xtreated]
    scalar se2 = _se[event_time_dummy2xtreated]
    scalar se3 = _se[event_time_dummy3xtreated]
    scalar se5 = _se[event_time_dummy5xtreated]
    scalar se6 = _se[event_time_dummy6xtreated]
    scalar se7 = _se[event_time_dummy7xtreated]
    scalar se8 = _se[event_time_dummy8xtreated]

    preserve
    clear
    set obs 8
    gen year = 2016 + _n - 1
    gen coef = .
    replace coef = c1 in 1
    replace coef = c2 in 2
    replace coef = c3 in 3
    replace coef = 0  in 4
    replace coef = c5 in 5
    replace coef = c6 in 6
    replace coef = c7 in 7
    replace coef = c8 in 8
    gen se = .
    replace se = se1 in 1
    replace se = se2 in 2
    replace se = se3 in 3
    replace se = 0  in 4
    replace se = se5 in 5
    replace se = se6 in 6
    replace se = se7 in 7
    replace se = se8 in 8
    gen lb = coef - 1.96*se
    gen ub = coef + 1.96*se

twoway (rcap lb ub year, lcolor(black) lwidth(thin)) ///
       /*(line coef year, lcolor(black) lwidth(medium))*/ ///
       (scatter coef year, mcolor(black) msymbol(O) msize(thin)), ///
       yline(0, lcolor(black) lwidth(medium)) ///
       xline(2020, lpattern(dash) lcolor(black) lwidth(medium)) ///
	   xlabel(2016(1)2023, labsize(large)) ///
       ylabel(, format(%9.2f) angle(horizontal) labsize(large)) ///
       ytitle("`ytitle'", size(large)) xtitle("Year", size(large)) ///
       legend(off) graphregion(color(white)) bgcolor(white)

    graph export "$DATAout/graph event study/event_study_`var'_$suffix.pdf", replace
    restore
*/
    local i = `i' + 1
}




**************************************
/*TAB
*************************************

local title1 "Event Study Excluding the Contruction Sector -- Activity and Input \label{tab:bs1}"
local title2 "Event Study Excluding the Contruction Sector -- Output and Productivity \label{tab:bs2}"
local title4 "Event Study Excluding the Contruction Sector -- Profitability \label{tab:bs3}"

//Tab1: Input
esttab model1 model2 model3 model4 using "$DATAout\construction\bs1$suffix.tex", ///
    replace ///
    b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
	title("`title1'") ///
    keep(event_time_dummy1xtreated ///
	event_time_dummy2xtreated ///
	event_time_dummy3xtreated ///
	event_time_dummy5xtreated ///
	event_time_dummy6xtreated ///
	event_time_dummy7xtreated ///
    event_time_dummy8xtreated) ///
    coeflabels(event_time_dummy1xtreated "\midrule \multicolumn{4}{l}{\textit{Pre-treatment period}} \\ t = -4 (2016)" ///
               event_time_dummy2xtreated "t = -3 (2017)" ///
               event_time_dummy3xtreated "t = -2 (2018)" ///
               event_time_dummy5xtreated "\midrule \multicolumn{4}{l}{\textit{Post-treatment period}} \\ t = 0 (2020)" ///
               event_time_dummy6xtreated "t = +1 (2021)" ///
               event_time_dummy7xtreated "t = +2 (2022)" ///
               event_time_dummy8xtreated "t = +3 (2023)") ///
    mtitles("ln(Employees)" "ln(Personnel Costs p. e.)" "ln(Fixed Asset)" "ln(Fixed Asset p.e.)") ///
    booktabs ///
    alignment(D{.}{.}{-1}) ///
    nonumbers ///
    stats(N r2 pval_pt, fmt(0 3 3) ///
          labels("Observations" "R-squared" "Parallel trends F-test (p-value)")) /*//
    addnotes("Robust standard errors clustered at municipality level in parentheses." ///
             "All specifications include firm and year fixed effects, weighted by 2019 employment.")*/
			 

//Tab2: Output and productivity
esttab model5 model6 model7 using "$DATAout\construction\bs2$suffix.tex", ///
    replace ///
    b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
	title("`title2'") ///
    keep(event_time_dummy1xtreated ///
	event_time_dummy2xtreated ///
	event_time_dummy3xtreated ///
	event_time_dummy5xtreated ///
	event_time_dummy6xtreated ///
	event_time_dummy7xtreated ///
    event_time_dummy8xtreated) ///
    coeflabels(event_time_dummy1xtreated "\midrule \multicolumn{2}{l}{\textit{Pre-treatment period}} \\ t = -4 (2016)" ///
               event_time_dummy2xtreated "t = -3 (2017)" ///
               event_time_dummy3xtreated "t = -2 (2018)" ///
               event_time_dummy5xtreated "\midrule \multicolumn{2}{l}{\textit{Post-treatment period}} \\ t = 0 (2020)" ///
               event_time_dummy6xtreated "t = +1 (2021)" ///
               event_time_dummy7xtreated "t = +2 (2022)" ///
               event_time_dummy8xtreated "t = +3 (2023)") ///
    mtitles("ln(Production Value)" "ln(Value Added)") ///
    booktabs ///
    alignment(D{.}{.}{-1}) ///
    nonumbers ///
    stats(N r2 pval_pt, fmt(0 3 3) ///
          labels("Observations" "R-squared" "Parallel trends F-test (p-value)")) /*//
    addnotes("Robust standard errors clustered at municipality level in parentheses." ///
             "All specifications include firm and year fixed effects, weighted by 2019 employment.")*/

			 

//Tab3: Profitability			 
esttab model8 model9 model10 model11 model12 using "$DATAout\construction\bs3$suffix.tex", ///
    replace ///
    b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
	title("`title4'") ///
    keep(event_time_dummy1xtreated ///
	event_time_dummy2xtreated ///
	event_time_dummy3xtreated ///
	event_time_dummy5xtreated ///
	event_time_dummy6xtreated ///
	event_time_dummy7xtreated ///
    event_time_dummy8xtreated) ///
    coeflabels(event_time_dummy1xtreated "\midrule \multicolumn{5}{l}{\textit{Pre-treatment period}} \\ t = -4 (2016)" ///
               event_time_dummy2xtreated "t = -3 (2017)" ///
               event_time_dummy3xtreated "t = -2 (2018)" ///
               event_time_dummy5xtreated "\midrule \multicolumn{5}{l}{\textit{Post-treatment period}} \\ t = 0 (2020)" ///
               event_time_dummy6xtreated "t = +1 (2021)" ///
               event_time_dummy7xtreated "t = +2 (2022)" ///
               event_time_dummy8xtreated "t = +3 (2023)") ///
    mtitles("ln(Revenue)" "Profit / Loss" "ROS" "ROE" "ROA") ///
    booktabs ///
    alignment(D{.}{.}{-1}) ///
    nonumbers ///
    stats(N r2 pval_pt, fmt(0 3 3) ///
          labels("Observations" "R-squared" "Parallel trends F-test (p-value)")) /*//
    addnotes("Robust standard errors clustered at municipality level in parentheses." ///
             "All specifications include firm and year fixed effects, weighted by 2019 employment.")*/
			 
			 
			 
*****************
