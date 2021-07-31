/*create dataset for regression*/
/*Author: Masanori Matsuura*/
clear all
set more off
cd "C:\Users\user\Documents\research\AJAD"

import excel using "HS2019_btngs.xlsx", firstrow clear

 ssc install psmatch2
*cleaning
drop if no==.
gen tincome=pamount+Nfarmincome
gen fir=pamount/tincome
label var fir "Farm income ratio"
label var Age "Age of FM"
label var Gen "Gender of FM (=1, if Male)"
label var Edu "Education level"
label var Expe "Experience of farming (year)"
label var member "Family size"
label var Irrigation "Irrigation (=1, if yes)"
label var Extension "Extension service (=1, if yes)"
label var Internet "Using internet (=1, if yes)"
label var Nfarmincome "Non-farm income (peso)"
recode SBF (1=1 "Enrollees") (0=0 "Dis-enrollees"), gen(sbf) 

*cleaning agronomic variables
gen prf=pvalue-VHireLabor-VMachine-Vanimal-VFer-VHer-VPes
destring QPes, gen(qpes)
gen unitv=pvalue/pamount
gen yield=pamount/landsize
gen hiredlabor=VHireLabor/1000 
gen fer=QFer/1000  
label var prf "Profit (peso)"
lavel var pvalue "Production value (kg)"
label var unitv "Unit value (peso/kg)"
label var pamount "Production amount (kg)"
label var landsize "Planted land size (ha)"
label var DFamLabor "Family labor (man-days)"      
label var DHireLabor "Hired labor (man-days)"  
label var hiredlabor "Total cost of Hired labor (1,000 PhP)"     
label var DMachine    "Machine (machine-days)"
label var DAnimal      "Animal (animal-days) "     
label var fer           "Total quantity of Fertilizer (1,000kg) " 
label var QHer   "Total quantity of Herbicide (liter)" 
label var QPes                "Total quantity of Pesticide (liter) "
label var yield "Yield (ton/ha)"
label var sbf "SBF participation"
*summary statistics 
bysort sbf: summarize Age Gen Edu Expe member Irrigation Extension Internet Nfarmincome fir

*t-test pre-matching dataset teffects
*psm estimation
teffects psmatch (prf) (sbf Expe Gen Edu landsize Internet, logit), nn(1) atet 
teffects psmatch (yield) (sbf Expe Gen Edu landsize Internet, logit), nn(1) atet 
teffects psmatch (pvalue) (sbf Expe Gen Edu landsize Internet, logit), nn(1) atet 
teffects psmatch (unitv) (sbf Expe Gen Edu landsize Internet, logit), nn(1) atet
*post matching analysis
tebalance summarize
tebalance density 

*t-test prematching dataset psmatch2
psmatch2 sbf Expe Gen Edu landsize Internet, out(prf) n(1) logit
psmatch2 sbf  Expe Gen Edu landsize Internet, out(prf) n(1) logit noreplace
psmatch2 sbf Expe Gen Edu landsize Internet, out(yield) n(1) logit
psmatch2 sbf  Expe Gen Edu landsize Internet, out(yield) n(1) logit noreplace
psmatch2 sbf  Expe Gen Edu landsize Internet, out(pvalue) n(1) logit 
psmatch2 sbf  Expe Gen Edu landsize Internet, out(pvalue) n(1) logit noreplace
psmatch2 sbf  Expe Gen Edu landsize Internet, out(unitv) n(1) logit noreplace 
psmatch2 sbf  Expe Gen Edu landsize Internet, out(unitv) logit mahalabinos com noreplace
psmatch2 sbf  Expe Gen Edu landsize Internet, out(unitv) logit radius com noreplace
psmatch2 sbf  Expe Gen Edu landsize Internet, out(pvalue) n(1) logit radius com noreplace
psmatch2 sbf  Expe Gen Edu landsize Internet, out(prf) n(1) logit radius com noreplace
psmatch2 sbf  Expe Gen Edu landsize Internet, out(yield) n(1) logit radius com noreplace
*post matching analysis
pstest _pscore, density both
*post matching analysis
tebalance density 


**output
eststo clear // socio economic summary statistics
eststo Enr: quietly estpost summarize Age Gen Edu Expe member Irrigation Extension Internet Nfarmincome fir if sbf == 1
eststo Dis: quietly estpost summarize Age Gen Edu Expe member Irrigation Extension Internet Nfarmincome fir if sbf == 0
eststo diff: quietly estpost ttest Age Gen Edu Expe member Irrigation Extension Internet Nfarmincome fir, by(sbf) unequal
esttab Enr Dis diff using demsoc.tex, cells("mean(pattern(1 1 0) fmt(2)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(2)) t(pattern(0 0 1) par fmt(2))") label replace nodepvar starlevels(* 0.1 ** 0.05 *** 0.01)

eststo clear //agronomic variables
eststo Enr: quietly estpost summarize prf pamount yield unitv landsize DFamLabor DHireLabor DMachine DAnimal fer QHer QPes if sbf == 1
eststo Dis: quietly estpost summarize prf pamount yield unitv landsize DFamLabor DHireLabor DMachine DAnimal fer QHer QPes if sbf == 0
eststo diff: quietly estpost ttest prf pamount yield unitv landsize DFamLabor DHireLabor DMachine DAnimal fer QHer QPes, by(sbf) unequal
esttab Enr Dis diff using agrn.tex, cells("mean(pattern(1 1 0) fmt(2)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(2)) t(pattern(0 0 1) par fmt(2))") label replace nodepvar starlevels(* 0.1 ** 0.05 *** 0.01)
/*esttab using demsoc.tex, cells("mean(fmt(2)) sd(fmt(2))") label nodepvar replace*/

*propensity score matching
psmatch2 sbf  Expe Gen Edu landsize Internet, out(unitv) logit radius com noreplace
psgraph 
graph display, scheme(s1mono) 
graph export prpsty.pdf, replace
*balance
eststo clear
psmatch2 sbf  Expe Gen Edu landsize Internet, out(unitv) logit radius com noreplace
eststo: pstest Expe , both label
eststo: pstest Gen , both label
eststo: pstest  Edu , both label
eststo: pstest landsize , both label
eststo: pstest Internet, both label
esttab using blnc.tex, label replace nocons
*determinants of participation
eststo clear
eststo: logit sbf Expe Gen Edu landsize Internet
esttab using lgt.tex,  b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons  r2 mtitles("SBF participation") addnote("Source: Author's survey")
*ATT
eststo clear
eststo:psmatch2 sbf Expe Gen Edu landsize Internet, out(unitv) logit radius com noreplace
eststo:psmatch2 sbf Expe Gen Edu landsize Internet, out(pvalue) logit radius com noreplace
eststo: psmatch2 sbf Expe Gen Edu landsize Internet, out(prf) logit radius com noreplace
eststo: psmatch2 sbf Expe Gen Edu landsize Internet, out(yield) logit radius com noreplace
esttab using att.tex,  b(%4.3f) se replace nogaps starlevels(*0.05) label nocons  addnote("Source: Author's survey" "1 USD = 51.93 PHP 30 September 2019")