* ==========================================================================
*
* 	door-to-door
*
* 
* 	Dataset used: Nigeria.dta
*
* 	Version: 5.0 Thu 8th May 2023
*
* 	Author: Sikai Huang
*
* ==========================================================================
**#		1.0 Append seperate country data into a research dataset  
*	Append country data
use "/Users/hskk/Desktop/SMC/Door_to_door/data/NG.dta", clear



**#		2.0 Manage variables for consistency among countries between 2020 and 2022	

*		2.1 Manage the geo-info variables
*	geolevel1: country
* 	geolevel2: state(NG)/ drs(TC, TG and MZ)/ nom_drs(BF) // Province in BF in 2022 between state and district?
*	geolevel3: LGA(NG)/ district(TC, TG and MZ)/ district(BF)  
gen geolevel1 = country

gen geolevel2 = state if geolevel1 == "Nigeria"

gen geolevel3 = lga if country == "Nigeria"

*	Update data as smc.dta
save "/Users/hskk/Desktop/SMC/Door_to_door/data/NG.dta", replace

* 	LGA were coded as numbers. See the LGA_NG_2020 sheet in the supplemental_ref_for_data_cleaning xlsx file. 
	*	make changes to LGA data and save the data in a tempfile
import excel "/Users/hskk/Desktop/Door_to_door/data/supplemental_ref_for_data_cleaning.xlsx", sheet("LGA_NG_2020") cellrange(A1:B142) firstrow allstring clear
tempfile lga
save `lga'
	* 	merge the tempfile to SMC data
use "/Users/hskk/Desktop/SMC/Door_to_door/data/NG.dta", clear

merge m:1 geolevel3 using `lga', nogenerate
replace geolevel3 = geolevel3inlga if geolevel1 == "Nigeria" & svyyear == 2020
replace geolevel3 = "Nongre-Massom" if geolevel3 == "Nongremassom"

*	unify the string names of variables
replace geolevel2 = strproper(geolevel2)
replace geolevel3 = strproper(geolevel3)


order geolevel1 geolevel2 geolevel3, after (svyyear)
order selected_chi_monthe, after(selected_chi_age)

*	Update data as smc.dta

save "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria.dta", replace



* ==========================================================================

* 	why choose Nigeria
use "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria.dta", clear

gen drug_outside_visit = 0 if status_treatment != ""

replace drug_outside_visit = 1 if drug_not_distributor != .

tab drug_outside_visit geolevel1 if svyyear == 2022, column

keep if geolevel1 == "Nigeria" & svyyear != 2020

* 	unify the format of state and lga name 
replace state = strproper(state)

replace lga = strproper(lga)	

replace lga = "Yagba/East" if lga == "Yagba East"

replace lga = "Qua'An Pan" if lga == "Qua _ An Pan"

replace lga = "Barkin Ladi" if lga == "Barkin  Ladi"

tab lga svyyear, mi

save "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria.dta", replace



*	add survey weight
use "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria.dta", clear

tab state svyyear, mi

import excel "/Users/hskk/Desktop/SMC/Door_to_door/data/Supplemental file.xlsx", sheet("Survey Weight") cellrange(A1:G19) firstrow clear

tempfile svyweight

save `svyweight'
	* 	merge the tempfile to SMC data
use "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria.dta", clear

merge m:1 state svyyear using `svyweight', keepusing(svyweight) nogenerate

*	Update data as Nigeria_v1.dta
save "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria_v1.dta", replace

use "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria_v1.dta", clear
**# 	Identify study population
* exclude

gen exclude = 0 if agreed != "No"		// exclude who did not agree with interview 
* "1" refers to exclude them; "0" refers to include them
*
* 	1. family arrived in this location after the time of the last cycle
tab when_arrived geolevel1, mi  // 	The coding varies by both geolevel1 and year
*
replace exclude = 1 if (when_arrived == 3 | when_arrived == 4 | when_arrived == 5) & geolevel1 == "Nigeria" 
*
*
* 	2. reasons for not being treated
*
tab why_not_treated geolevel1, mi
replace exclude = 1 if why_not_treated == 5

replace exclude = 1 if why_not_treated == 4 | why_not_treated == 6
*
*
* 	3. reasons for not being treated: to determine the drop of observation case-by-case (6: child ineligible for another reason; 96: other) 
*	Question relevant when: ${why_not_treated} = '96'
* 	see supplemental_ref_for_data_cleaning/reasons_not_treated excel file
tab not_treated_other
replace not_treated_other = "" if not_treated_other == "."

gen not_treated_other_id = _n if not_treated_other != ""
browse selected_chi_monthe not_treated_other not_treated_other_id if not_treated_other != ""

*	update data as smc_clean.dta
save "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria_v1.dta", replace

*	have identified the reasons for not treated that should be excluded in the supplemental_ref_for_data_cleaning/reasons_not_treated excel file, so the next is to merge back into the smc_clean data
*	id were used because I cannot match or merge with {not_treated_other} directly due to the system error of coding language 
import excel "/Users/hskk/Desktop/SMC/Door_to_door/data/supplemental_ref_for_data_cleaning.xlsx", sheet("exclude_not_treated_other") cellrange(A1:C184) firstrow clear
tempfile exclude_reasons
save `exclude_reasons'
	* 	merge the tempfile to smc_clean data
use "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria_v1.dta", clear
merge m:m not_treated_other_id using `exclude_reasons', keepusing(exclude_add) nogenerate
browse not_treated_other_id not_treated_other exclude exclude_add
replace exclude = exclude_add if not_treated_other != "" & exclude != 1 // 23
drop exclude_add
tab exclude
*
tab selected_chi_ch_selected
keep if selected_chi_ch_selected == 1    // 1 observations deleted

drop if exclude == 1

save "/Users/hskk/Desktop/SMC/Door_to_door/data/Nigeria_v1.dta", replace


**# 	Determine the dependent variable of intertest
* 	Drug_not_distributor:
* 	Group relevant when (selected( ${visited_compound} , 'No') and 
* 	selected( ${status_treatment} , 'received_SMC_drugs')) or 
* 	(selected( ${visited_compound_recieved_SMC} , 'No'))

tab visited_compound status_treatment, mi 	// 238 respodents were treated  
												// but did not being visited by the distributor
											
tab visited_compound_recieved_smc status_treatment      // 76 treated respodents were visited 
												// but did not receive drugs from the distributor 
												
tab drug_not_distributor			// 238 + 76 = 314 


tab drug_not_distributor		


* Create binary variable as households who accessed to medicines outside household visits
drop drug_outside_visit

gen drug_outside_visit = 0 if status_treatment == "received_SMC_drugs"

replace drug_outside_visit = 1 if  drug_not_distributor != .

tab drug_outside_visit, mi    // 314 accessed SMC medicines outside visits

* manage the free text answer "drug_not_distributor_other"

browse svyyear status_treatment drug_outside_visit drug_not_distributor drug_not_distributor_other if drug_not_distributor_other != "" 

replace drug_not_distributor = 5 if drug_not_distributor_other == "From the shop through a friend"

replace drug_not_distributor = 2 if drug_not_distributor_other == "Primary Health care"

replace drug_not_distributor = 3 if drug_not_distributor_other == "Church"

replace drug_not_distributor = 6 if drug_not_distributor_other == "In another state ILORIN" | drug_not_distributor_other == "At the School" | drug_not_distributor_other == "At School" | drug_not_distributor_other == "They didn't enter the house but was given outside the house hold" | drug_not_distributor_other == "From community health workers" | drug_not_distributor_other == "From their school"|drug_not_distributor_other == "CDD gave it to her in her shop" | drug_not_distributor_other == "The drug distributors followed them to their farm at Awe to distribute the drugs" | drug_not_distributor_other == "He was treated in the farm"  | drug_not_distributor_other == "Outside the house not directly to the care giver"

*	Update data as Nigeria_v1.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v1.dta", replace





**# Manage the potential predictors (independent variables) of interest

use "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v1.dta", clear

* 1 Sociodemographics for selected treated child 
tab1 selected_chi_age selected_chi_sex  

* 2 Sociodemographics for caregiver
tab1 caregiver_hh_head caregiver_gender age_caregiver education_caregiver occupation_caregiver married_caregiver

* Group small categories into other categories
gen age_caregiver_orig = age_caregiver

replace age_caregiver = 2 if age_caregiver == 1
replace age_caregiver = 4 if age_caregiver > 4

tab age_caregiver			// Under 29; 30-39; 40 or above 

gen education_caregiver_orig = education_caregiver

replace education_caregiver = 1 if education_caregiver == 2

tab education_caregiver			// No education/informal or religious education; Primary; Secondary; Higher

gen married_caregiver_orig = married_caregiver

replace married_caregiver = 2 if married_caregiver == 3 | married_caregiver == 4			// Married/Partnered; Non-partnered

gen occupation_caregiver_orig  = occupation_caregiver 

replace occupation_caregiver  = 1 if occupation_caregiver  == 2			// Non-employed/Unemployed

replace occupation_caregiver  = 3 if occupation_caregiver  == 4			// Agriculture

replace occupation_caregiver  = 5 if occupation_caregiver  == 6	| occupation_caregiver  == 7	// mannual work

replace occupation_caregiver  = 8 if occupation_caregiver  == 9			// sales/service/professioanl

tab occupation_caregiver


* 3 Sociodemographics for household head
tab caregiver_hh_head	

tab1 age_hh sex_hh education_hh occupation_hh // Question relevant when: selected( ${caregiver_hh_head} , 'No')

* include household heads who were also caregiver
replace age_hh = age_caregiver if caregiver_hh_head == "Yes"

replace sex_hh = caregiver_gender if caregiver_hh_head == "Yes"

replace literacy_hh = literacy_caregiver if caregiver_hh_head == "Yes"

replace education_hh = education_caregiver if caregiver_hh_head == "Yes"

replace occupation_hh = occupation_caregiver if caregiver_hh_head == "Yes"

* Group small categories into other categories
gen age_hh_orig = age_hh

replace age_hh = 2 if age_hh == 1
replace age_hh = 4 if age_hh > 4			// Under 29; 30-39; 40 or above 

gen education_hh_orig = education_hh

replace education_hh = 1 if education_hh == 2			// No education/informal or religious education; Primary; Secondary; Higher

gen occupation_hh_orig  = occupation_hh

replace occupation_hh  = 1 if occupation_hh  == 2			// Non-employed/Unemployed

replace occupation_hh  = 3 if occupation_hh  == 4			// Agriculture

replace occupation_hh  = 5 if occupation_hh  == 6 | occupation_hh  == 7			// Mannual work

replace	occupation_hh  = 8 if occupation_hh  == 9 // Sales/service/professioanl

tab1 born_state religion resident_jul2021

tab religion 

gen religion_orig = religion

replace religion = 3 if religion > 2			// Islam, Christian and others

gen religion_2cat = religion

replace religion_2cat = 2 if religion > 1 		// Isla and Christian and others


* 4 Practice and knowledge towards malaria prevention 
tab1 spray mosquito_nets  spend_mosquito_net 

gen spend_mosquito_net_orig = spend_mosquito_net  // own a mosquito net and spend last night under a mosquito net

replace spend_mosquito_net = "No" if mosquito_nets == "No" & spend_mosquito_net_orig == ""

tab spend_mosquito_net

tab1 heard_smc smc_sources smc_heard_date smc_purpose smc_eligibility smc_age_protection smc_aq_importance smc_adverse_event smc_effective
								

* 5 Housing related factors
* economic status questions were based on https://www.simplepovertyscorecard.com/NGA_2012_ENG.pdf

egen totalpovertyscore = rowtotal (sps_1  sps_2  sps_3 sps_4 sps_5 sps_6 sps_7 sps_8 sps_9 sps_10_b )

* create tertiles for the variable "wealth_index"
xtile poverty = totalpovertyscore, n(3)

* tabulate the distribution of observations across the tertiles
tab poverty

* 	not use this for now:
* 	convert scores to poverty likelihoods using 100% national line
*	gen poverty = 0 
*	replace poverty = 1 if totalpovertyscore > 19 & totalpovertyscore < 75			// interpretation: at least 2.6% likelihood of being poor and at most 75.9% likelihood of being poor.
*	replace poverty = 2 if totalpovertyscore >= 75			// 0% likelihood of being poor)

* Seasonal nomadism/migration is defined as all members of the household not living 
* continually in the same place, but moving cyclically or periodically at least one time per year
tab nomad

*	Update data as Nigeria_v2.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v2.dta", replace
*
* 	generate a unique id for each observation
use "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v2.dta", clear

* 6 Knowledge toward SMC
tab smc_town_announcer svyyear
by svyyear, sort: tab smc_town_announcer heard_smc

tab smc_town_announcer smc_sources_8
replace smc_town_announcer = "No" if heard_smc == "No"
replace smc_town_announcer = "No" if smc_sources_8 == 0 & svyyear == 2021
tab smc_town_announcer svyyear


local heard_smc_vars "smc_heard_date smc_purpose smc_aq_importance smc_eligibility smc_age_protection  smc_adverse_event smc_effective"

foreach var of local heard_smc_vars {
    replace `var' = "No" if heard_smc == "No"
}

local heard_smc_vars "smc_sources_1 smc_sources_2 smc_sources_3 smc_sources_4 smc_sources_5 smc_sources_6 smc_sources_7 smc_sources_8 smc_sources_9 smc_sources_10"

foreach var of local heard_smc_vars {
    replace `var' = 0 if heard_smc == "No"
}

*	Update data as Nigeria_v2.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v2.dta", replace
*



* 	generate a unique id for each observation
use "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v2.dta", clear

gen int uniqueid = _n

gen byte case = drug_outside_visit

gsort - case uniqueid

gen byte case_3cat = case

tab case_3cat

replace case_3cat = 2 if visited_compound == "No" & case == 1

* 	keep variables of interest
keep uniqueid case case_3cat svyyear selected_chi_age selected_chi_sex caregiver_hh_head caregiver_gender caregiver_gender_int age_caregiver education_caregiver occupation_caregiver married_caregiver age_hh sex_hh education_hh occupation_hh heard_smc smc_sources smc_sources_1 smc_sources_2 smc_sources_3 smc_sources_4 smc_sources_5 smc_sources_6 smc_sources_7 smc_sources_8 smc_sources_9 smc_sources_96 smc_sources_other smc_sources_10 smc_town_announcer smc_heard_date smc_purpose smc_eligibility smc_age_protection smc_aq_importance smc_adverse_event smc_effective spray mosquito_nets spend_mosquito_net nomad religion religion_2cat state lga lga_int svyweight born_state drug_not_distributor visited_compound visited_compound_recieved_smc status_treatment totalpovertyscore resident_jul2021 born_state poverty where_smcdrug where_smcdrug_1 where_smcdrug_2 where_smcdrug_3 where_smcdrug_4 where_smcdrug_5 where_smcdrug_6 where_smcdrug_7 dose2 dose2_reason dose3 dose3_reason adverse_reactions adverse_type adverse_type_1 adverse_type_2 adverse_type_3 adverse_type_4 adverse_type_5 adverse_type_6 adverse_type_7 adverse_type_96 adverse_type_oth tell_adverse tell_adversereason tell_type_oth 

rename visited_compound_recieved_smc vistcompod_recieved_smc 

rename resident_jul2021 resident_jul

*	Update data as Nigeria_v3.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v3.dta", replace






*	============================================================================
*	============================================================================






* 	read in data
use "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v3.dta", clear

* 	declare survey design
svyset [pweight=svyweight]

*	distribution of outside housheold visits
tab visited_compound status_treatment, mi 	// 238 respodents were treated  
												// but did not being visited by the distributor
											
tab vistcompod_recieved_smc status_treatment      // 76 treated respodents were visited 
												// but did not receive drugs from the distributor 
												
tab drug_not_distributor			// 238 + 76 = 314 

svy: tab drug_not_distributor, obs  percent cellwidth(12) format(%9.2f) column ci

**# Baseline characteristics according to access to smc drugs

local tab_vars "svyyear state selected_chi_age selected_chi_sex caregiver_hh_head caregiver_gender age_caregiver education_caregiver occupation_caregiver married_caregiver age_hh sex_hh education_hh occupation_hh born_state religion religion_2cat resident_jul nomad poverty spray mosquito_nets heard_smc smc_heard_date smc_purpose smc_aq_importance smc_eligibility smc_age_protection  smc_adverse_event smc_effective smc_sources_1 smc_sources_2 smc_sources_3 smc_sources_4 smc_sources_5 smc_sources_6 smc_sources_7 smc_sources_8 smc_sources_9 smc_sources_10 smc_town_announcer"

foreach var of local tab_vars {
    svy: tab `var' case, obs  percent cellwidth(12) format(%9.2f) column 
}

svy: tab religion_2cat case, obs  percent cellwidth(12) format(%9.2f) column 
 
* Create a list of string variable names
qui ds, has(type string)
local varlist `r(varlist)'

* Loop through the list of variable names and encode them
foreach var of local varlist {
    encoderall `var', setzero
  }
 
*	Update data as Nigeria_v3.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v3.dta", replace


**# Primary regression analysis
use "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v3.dta", clear
*	multicollinearity test
reg case i.selected_chi_age i.selected_chi_sex i.caregiver_hh_head i.caregiver_gender i.age_caregiver i.education_caregiver i.occupation_caregiver i.married_caregiver i.age_hh i.sex_hh i.education_hh i.occupation_hh i.born_state i.religion i.resident_jul i.nomad i.poverty i.spray i.mosquito_nets i.smc_heard_date i.smc_purpose i.smc_aq_importance i.smc_eligibility i.smc_age_protection  i.smc_adverse_event i.smc_effective i.smc_sources_1  i.smc_sources_2  i.smc_sources_3  i.smc_sources_4  i.smc_sources_5 i.smc_sources_6 i.smc_sources_7 i.smc_sources_8 

estat vif		
	

* 	univairte model regression 
local tab_vars "svyyear state selected_chi_age selected_chi_sex caregiver_hh_head caregiver_gender age_caregiver education_caregiver occupation_caregiver married_caregiver age_hh sex_hh education_hh occupation_hh born_state religion resident_jul nomad poverty spray mosquito_nets heard_smc smc_heard_date smc_purpose smc_aq_importance smc_eligibility smc_age_protection  smc_adverse_event smc_effective smc_sources_1 smc_sources_2 smc_sources_3 smc_sources_4 smc_sources_5 smc_sources_6 smc_sources_7 smc_sources_8 smc_sources_9 smc_sources_10 smc_town_announcer"

foreach var of local tab_vars {
    melogit case i.`var' [pweight = svyweight]|| state: || lga: || svyyear: ||  , or nolog
}

melogit case i.smc_sources_9 [pweight = svyweight]|| state: || lga: || svyyear: ||  , or nolog

* 	null regression model
melogit case [pweight = svyweight] || state: || lga: || svyyear:, or nolog



*	forward step-wise var selection

*	full: adding if p<0.05
sw, pe(.05) forward : logit case i.selected_chi_age i.selected_chi_sex i.caregiver_hh_head i.caregiver_gender i.age_caregiver i.education_caregiver i.occupation_caregiver i.married_caregiver i.age_hh i.sex_hh i.education_hh i.occupation_hh i.born_state i.religion_2cat i.resident_jul i.nomad i.poverty i.spray i.mosquito_nets i.smc_heard_date i.smc_purpose i.smc_aq_importance i.smc_eligibility i.smc_age_protection  i.smc_adverse_event i.smc_effective i.smc_sources_1  i.smc_sources_2  i.smc_sources_3  i.smc_sources_4  i.smc_sources_5  i.smc_sources_8 i.smc_sources_9


*	adjusted model using stepwise selected p<0.05 variables 
melogit case i.age_caregiver i.education_caregiver i.smc_heard_date i.mosquito_nets i.born_state i.resident_jul i.poverty i.smc_sources_2 i.age_hh  [pweight = svyweight] || state: || lga: || svyyear:,  or nolog iter(1000)



*	Update data as Nigeria_v4.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v4.dta", replace


**# Secondary analysis: association between knowledge of sources of smc medicines and access to SMC outside household visit

* 	read in data
use "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v4.dta", clear

tab where_smcdrug			//9,316 eligible obs
*	association test
local where_vars "where_smcdrug_1 where_smcdrug_2 where_smcdrug_3 where_smcdrug_4 where_smcdrug_5 where_smcdrug_6 where_smcdrug_7"

foreach var of local where_vars {
    svy: tab `var' case, obs  percent cellwidth(12) format(%9.2f) column
}			// results: 1. the association between knowledge of private purchase and access to SMC medicines outside household visits is statistically significant. p<0.0037;
			// 2. 168(treated by CHW) + 5(treated outside) know the private purchase channel
			
*	cross-tabulation of state
tab state if drug_not_distributor == 1 & case !=.
tab state if where_smcdrug_5 == 1 & case != .

by svyyear, sort: tab state if where_smcdrug_5 == 1 & case != .
tab lga if where_smcdrug_5 == 1 & case != .

*	how many treated people purchased smc?
tab drug_not_distributor			// 8 people

tab visited_compound if drug_not_distributor == 5			// 5 were not visited by drug distributors


**#	Secondary analysis: association between adherence to AQ doses and access to SMC outside household visit

* why not choose spit or vomit? because this variable is conditonal on DOT by CHW 

* 	adherence to AQ doses

svy: tab dose2 case, obs  percent cellwidth(12) format(%9.2f) column

svy: tab dose3 case, obs  percent cellwidth(12) format(%9.2f) column

gen dose23 = 1 if dose2 == 1 & dose3 == 1

replace dose23 = 0 if dose2 == 0 | dose3 == 0

svy: tab dose23 case, obs  percent cellwidth(12) format(%9.2f) column

svy: tab dose2_reason case, obs  percent cellwidth(12) format(%9.2f) column

svy: tab dose3_reason case, obs  percent cellwidth(12) format(%9.2f) column

*	occurrence of adverse events

svy: tab adverse_reactions case, obs  percent cellwidth(12) format(%9.2f) column		// 3,827 obs on adverse events

local where_vars "adverse_type_1 adverse_type_2 adverse_type_3 adverse_type_4 adverse_type_5 adverse_type_6 adverse_type_7 adverse_type_96"

foreach var of local where_vars {
    svy: tab `var' case, obs  percent cellwidth(12) format(%9.2f) column
}	

tab adverse_type_oth

* 	 Parents/caregivers actions to the events
svy: tab tell_adverse case, obs  percent cellwidth(12) format(%9.2f) column	

svy: tab tell_adversereason case, obs  percent cellwidth(12) format(%9.2f) column	

*	Update data as Nigeria_v5.dta
save "/Users/hskk/Desktop/Door_to_door/data/Nigeria_v5.dta", replace

*	end


* Reponses to reviewer
gen non_ddd_hf_cd = 1 if drug_not_distributor == 2 | drug_not_distributor == 3 | drug_not_distributor == 6
replace non_ddd_hf_cd = 0 if drug_not_distributor == 1 | drug_not_distributor == 4 | drug_not_distributor == 5 | drug_not_distributor == 7
tab non_ddd_hf_cd 

svy: tab dose23 non_ddd_hf_cd, obs  percent cellwidth(12) format(%9.2f) column	
svy: tab adverse_reactions non_ddd_hf_cd, obs  percent cellwidth(12) format(%9.2f) column
svy: tab tell_adverse non_ddd_hf_cd, obs  percent cellwidth(12) format(%9.2f) column
svy: tab tell_adversereason non_ddd_hf_cd, obs  percent cellwidth(12) format(%9.2f) column	 //fisher

svy: tab tell_adversereason non_ddd_hf_cd, obs  percent cellwidth(12) format(%9.2f) column	 //fisher


