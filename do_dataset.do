
cd "$DATAin"
use AIDA_FULL.dta

//Keep only provinces at the border 
keep if inlist(registeredofficeaddressprovincei, 44, 57, 59, 60, 61, 66, 67, 94) //Ascoli Piceno, Rieti, Latina, Frosinone, Caserta, L'Aquila, Teramo, Isernia

save border.dta, replace

*keep if inlist(registeredofficeaddressprovincei, 109, 43, 54, 55, 56, 61, 66, 67, 94) //Fermo, Macerata, Perugia, Terni, Viterbo, Caserta, L'Aquila, Teramo, Isernia

*save border_donut.dta, replace

*/

cd "$DATAin"
use border.dta, clear

egen id = group(bvd_id_number2)

keep if year >= 2016


*Exclusion of not treated sectors, exept for financial sector
drop if inrange(nacerev2, 100, 322) ///agriculture
| inrange(nacerev2, 8410, 8430) ///public
| inrange(nacerev2, 9700, 9820) //domestic

*Drop firms incorporated after 2020
gen incorporation_date = date(incorporationyear, "DMY")
label variable incorporation_date "incorporation date DMY"
format incorporation_date %td
drop if year(incorporation_date) > 2020


*Only firms alive up to 2020
bys id: egen last_year_alive = max(cond(total_value_production != . & total_value_production != 0, year, .))
label variable last_year_alive "last year with a total production value different from 0 or missing"

keep if last_year_alive >= 2020
drop if last_year_alive == .




*rename
rename registeredofficeaddresscommune municipality
rename registeredofficeaddresscommuneis id_municipality
rename registeredofficeaddressprovincei id_province

*Treatment dummy
gen treated = .
*replace treated = 0 if inlist(id_province, 109, 43, 54, 55, 56) //donut
replace treated = 0 if inlist(id_province, 44, 57, 59, 60)
replace treated = 1 if inlist(id_province, 61, 66, 67, 94)
tab treated
count if treated == . //0
label variable treated "if treated by Decontribuzione Sud"

*Temporal dummy
gen post = (year >= 2020)
label variable post "if year >= 2020"   
tab treated post
sort id year




*save clean_financial.dta, replace


//Exclusion of financial sector
drop if inrange(nacerev2, 6410, 6630) ///financial


save clean.dta, replace

drop if inrange(nacerev2, 4100, 4399) //construction

save clean_construction.dta, replace 

