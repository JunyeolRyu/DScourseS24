clear all
set more off
use "C:\Users\ryujy\OneDrive\바탕 화면\paper\직도입수입\0425\Data_0506.dta", clear



************************************************************1. Structural Break(Bai-Perron)
************************************************************
************************************************************
egen JKM_mean = mean(JKM), by(time)
egen JCC_mean = mean(JCC), by(time)
egen HH_mean = mean(HH), by(time)
collapse JKM_mean JCC_mean HH_mean Year Mon month, by(time)

tsset time


**************Table 2
xtbreak JKM_mean time
xtbreak test JKM_mean time , breaks(1)
xtbreak estimate JKM_mean time , breaks(1)
xtbreak test JKM_mean time , breaks(1) hypothesis(1)
xtbreak test JKM_mean time, breakpoints(78)


**************Figure 4
tsline JKM_mean, ///
ytitle("JKM price ($/MMBTU)") ///
xtitle("time (year/month)") ///
xline(78, lwidth(thin) ///
lpattern(shortdash) lcolor(gs0)) ///
xlabel(1 "201501" 78 "202106" 101 "202305", angle(45))

tsline JCC_mean, ///
ytitle("JCC price") ///
xtitle("time (year/month)") ///
xline(78, lwidth(thin) ///
lpattern(shortdash) lcolor(gs0)) ///
xlabel(1 "201501" 78 "202106" 101 "202305", angle(45))

tsline HH_mean, ///
ytitle("HH price") ///
xtitle("time (year/month)") ///
xline(78, lwidth(thin) ///
lpattern(shortdash) lcolor(gs0)) ///
xlabel(1 "201501" 78 "202106" 101 "202305", angle(45))










clear all
set more off
use "C:\Users\ryujy\OneDrive\바탕 화면\paper\직도입수입\0425\Data_0506.dta", clear


************************************************************2. Summary Stats
**************Table 1

estpost tabstat dum_gen_name1 dum_gen_type dum_gas_type fuelP JKM JCC HH dum_comtype dum_Genconame Capacity, c(stat) stat(mean sd min max n)
esttab using LNG_Summary.tex, replace ///
cells("mean sd min max count") ///
nonumber booktabs noobs ///
title("summary stats") ///
b(3) ///
collabels("Mean" "SD" "Min" "Max" "N")

sum fuelP JKM JCC HH Capacity




************************************************************3. Regression
************************************************************
************************************************************



**************1) Basic
**************Table 3
eststo clear
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC, absorb(i.Year_new_dum_Season i.dum_gen_type) 
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC, absorb(i.Year_new_dum_Season i.dum_gen_type i.Capa_interval) 
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity, absorb(i.Year_new_dum_Season i.dum_gen_type) 
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity, absorb(i.month i.dum_gen_type)

esttab using 0425_LNG_R2.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 2\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입))





**************2) Buyer & Seller Market
**************Table 4


eststo clear
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity, absorb(i.Year_new_dum_Season i.dum_gen_type) 
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 0, absorb(i.Year_new_dum_Season i.dum_gen_type)
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 1, absorb(i.Year_new_dum_Season i.dum_gen_type)

esttab using 0425_LNG_R3.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 3\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입) Capacity 발전기용량)




**************3) Price Ceiling_Seller Market
**************Table 6

eststo clear
eststo: reghdfe fuelP dum_gas_type_1_y_Price_cap dum_gas_type_1_n_Price_cap dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 1, absorb(i.Year_new_dum_Season i.dum_gen_type) 

esttab using 0425_LNG_R6.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 6\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입) Capacity 발전기용량)




**************4) Terminal
**************Table 7

eststo clear
eststo: reghdfe fuelP dum_gas_type_1_y_Termi dum_gas_type_1_n_Termi dum_gas_type_2 JKM JCC Capacity, absorb(i.Year_new_dum_Season i.dum_gen_type) 

eststo: reghdfe fuelP dum_gas_type_1_y_Termi dum_gas_type_1_n_Termi dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 0, absorb(i.Year_new_dum_Season i.dum_gen_type) 

eststo: reghdfe fuelP dum_gas_type_1_y_Termi dum_gas_type_1_n_Termi dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 1, absorb(i.Year_new_dum_Season i.dum_gen_type) 


esttab using 0425_LNG_R5.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 5\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입) Capacity 발전기용량)












**************5) Robust
**************Table 8

***(1) operation start

eststo clear
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity op_year_trun, absorb(i.Year_new_dum_Season i.dum_gen_type) 


***(2) HH가 price
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM HH Capacity, absorb(i.Year_new_dum_Season i.dum_gen_type) 


esttab using 0425_LNG_R8.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 8\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입) Capacity 발전기용량)







**************6) ETC
**************Table 9
eststo clear
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM_Buyer_Seller JKM JCC Capacity, absorb(i.dum_Season i.dum_gen_type) 


esttab using 0425_LNG_R9.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 9\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입) JKM_Buyer_Seller 판매자시장 Capacity 발전기용량)





**************Table 10
eststo clear
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 1, absorb(i.Year_new_dum_Season i.dum_gen_type) 
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 1 & Gasprice_change  == 0, absorb(i.Year_new_dum_Season i.dum_gen_type)  
eststo: reghdfe fuelP dum_gas_type_1 dum_gas_type_2 JKM JCC Capacity if JKM_Buyer_Seller == 1 & Gasprice_change  == 1, absorb(i.Year_new_dum_Season i.dum_gen_type) 


esttab using 0425_LNG_R4.tex, label replace booktabs ///
se(2) b(3) star(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2) drop(_cons) ///
title(회귀분석 4\label{tab1}) ///
rename(dum_gas_type_1 직수입 dum_gas_type_2 혼합(가스공사평균+직수입) Capacity 발전기용량)


















