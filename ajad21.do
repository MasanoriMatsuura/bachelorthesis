/*create dataset for regression*/
/*Author: Masanori Matsuura*/
clear all
set more off
cd "C:\Users\user\Documents\research\AJAD"

import excel using "HS2019_btngs.xlsx", firstrow clear

drop if no==.