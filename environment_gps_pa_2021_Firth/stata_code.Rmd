---
title: "Stata Code"
author: "Daniel Fuller"
date: "15/10/2021"
output:
      html_document:
        keep_md: true
---

# Stata Code for Analysis

## Data import, summary by person x time x DA

```{}
import delimited "/Users/caislin/sfuvault/INT_TimexNbhood/person_census_summary_2021_05_19_nogeo.csv", stringcols(3 4)  
save "/Users/caislin/sfuvault/INT_TimexNbhood/person_census_summary_nogeoMay19.dta"

*Drop missing data
drop if geouid == "NA"
drop if date == "NA"
drop if date == "" 
```

### Drop participants with improbable records (> 24 hrs/observation a day)
### Interact IDs 401815437, 401522186, 302955513, 101830987	

```{}
drop if interact_id == 401815437
drop if interact_id == 401522186
drop if interact_id == 302955513
drop if interact_id == 101830987
save "/Users/caislin/sfuvault/INT_TimexNbhood/person_census_summary_nogeoMay19-Drop4.dta"
```

### Add Saskatoon age data
```{}
merge m:1 interact_id using "/Users/caislin/sfuvault/INT_TimexNbhood/skt_age.dta"
```

### Recode and Create variables
### Rename to reflect total minutes in each DA
```{}
rename minutes_id_date_ct minutes_da

*destring
destring ale_tranist, generate(ale_transit_n) ignore(`"NA"') force
destring geouid, generate(geouid_n) ignore(`"NA"') force
destring sprawl, generate(sprawl_n) force
destring max_temp_c, generate(max_temp_c_n) force
destring min_temp_c, generate(min_temp_c_n) force
destring mean_temp_c, generate(mean_temp_c_n) force
destring total_rain_mm, generate(total_rain_mm_n) force
destring total_snow_mm, generate(total_rain_mm_n) force
destring total_precip_mm, generate(total_precip_mm_n) force

destring prox_idx_emp, generate(prox_idx_emp_n) force
destring prox_idx_pharma, generate(prox_idx_pharma_n) force
destring prox_idx_childcare, generate(prox_idx_childcare_n) force
destring prox_idx_health, generate(prox_idx_health_n) force
destring prox_idx_grocery, generate(prox_idx_grocery_n) force
destring prox_idx_educpri, generate(prox_idx_educpri_n) force
destring prox_idx_educsec, generate(prox_idx_educsec_n) force
destring prox_idx_lib, generate(prox_idx_lib_n) force
destring prox_idx_parks, generate(prox_idx_parks_n) force
destring prox_idx_transit, generate(prox_idx_transit_n) force
```

### DA rename
```{}
rename geouid_n geoid_DA
```

###Format date from string to date format
```{}
gen date_n = date(date, "YMD")
format date_n %td
```

### Gender
```{}
gen gender_n = 1 if gender == "Female" | gender == "['Female']"
replace gender_n = 2 if gender == "Male" |  gender== "['Male']" 
replace gender_n = 3 if gender == "['Genderqueer/non-conforming']" | gender == "Genderqueer/non-conforming"
replace gender_n = 4 if gender == "['Transgender']"
replace gender_n = 5 if gender == "['Other']" | gender== "['Male', 'Other']"
label define gender 1 "female" 2 "male" 3 "genderqueer/non-conforming" 4 "transgender" 5 "other"
label values gender_n gender
tab gender gender_n

#### Gender 4 analysis, 3 levels
recode gender_n (3/5=3), gen(genderx3)
label define genderx3 1 "female" 2 "male" 3 "other"
label values genderx3 genderx3
``` 

### Income
```{}
gen income_n = 1 if income == "$1 to $9,999"
replace income_n = 2 if income == "$10,000 to $14,999" | income == "$15,000 to $19,999"
replace income_n = 3 if income == "$20,000 to $29,999"
replace income_n = 4 if income == "$30,000 to $39,999"
replace income_n = 5 if income == "$40,000 to $49,999"
replace income_n = 6 if income == "$50,000 to $99,999"
replace income_n = 7 if income == "$100,000 to $149,999"
replace income_n = 8 if income == "$150,000 to $199,999"
replace income_n = 9 if income == "$200,000 or more"
label define income 1 "<$10k" 2 "10-20k" 3 "$20-30k" 4 "$30-40k" 5 "$40-50k" 6 "$50-100k" 7 "$100-150k" 8 "$150-200k" 9 "$200K+"
label values income_n income
```

### Add in race data from health survey
```{}
use "/Users/caislin/sfuvault/INT_TimexNbhood/person_census_summary_nogeoMay10.dta"
merge m:1 interact_id using "/Users/caislin/sfuvault/_Treksoft_2019_10_10_2851c89/wave1-race.dta"
save "/Users/caislin/sfuvault/INT_TimexNbhood/person_census_summary_nogeoMay10.dta", replace
```

###Low income
```{}
gen lowincome50k = 1 if income_n <= 5 & income_n > 0
replace lowincome50k = 0 if income_n > 5 & income_n <=9
```

### Age groups
```{}
replace age = "." if age == "NA"
replace age = "." if age == "-1"
destring age, replace
replace age == age_ssk if age ==. & city_n ==2
recode age (18/24=1) (25/44=2) (45/64=3) (65/89=4), gen(agegroupx4)
label define agex4 1 "18-24yrs" 2 "25-44yrs" 3 "45-64yrs" 4 "65+ yrs"
label values agegroupx4 agex4
```

### City
```{}
gen city_n = 1 if city == "montreal"
replace city_n = 2 if city== "saskatoon"
replace city_n = 3 if city== "vancouver"
replace city_n = 4 if city== "victoria"
label define city 1 "Montreal" 2 "Saskatoon" 3 "Vancouver" 4 "Victoria" 
label values city_n city
```

### Gentrification 3 levels (using GENUINE data) 
```{}
gen gentrify_Dingx3 = 0
replace gentrify_Dingx3 = 1 if ding_cma_gentrifiable == "1" & ding_cma_gentrified == "0"
replace gentrify_Dingx3 = 2 if ding_cma_gentrifiable == "1" & ding_cma_gentrified == "1"
label define gentrifyx3 0 "High SES" 1 "Low SES" 2 "Gentrified"
label values gentrify_Dingx3 gentrifyx3

gen gentrify_Grubex3 = 0
replace gentrify_Grubex3 = 1 if grube_cma_gentrifiable == "1" & grube_cma_gentrified == "0"
replace gentrify_Grubex3 = 2 if grube_cma_gentrifiable == "1" & grube_cma_gentrified == "1"
label values gentrify_Grubex3 gentrifyx3
```

### CanALE quintiles (national) and city-specific quintiles 

```{}
destring ale_index, generate(ale_index_n) ignore(`"NA"') force
xtile ale_index_q = ale_index_n, nq(5)
xtile Mtl_ale_index_q = ale_index_n if city_n == 1 , nq(5)
xtile Ssk_ale_index_q = ale_index_n if city_n == 2 , nq(5)
xtile Van_ale_index_q = ale_index_n if city_n == 3 , nq(5)
xtile Vic_ale_index_q = ale_index_n if city_n == 4 , nq(5)

gen city_CanAle_q = Mtl_ale_index_q 
replace city_CanAle_q = Ssk_ale_index_q if city_n == 2
replace city_CanAle_q = Van_ale_index_q if city_n == 3
replace city_CanAle_q = Vic_ale_index_q if city_n == 4
```

### Quintiles of proximity measures

```{}
xtile prox_idx_emp_q = prox_idx_emp_n, nq(5)
xtile prox_idx_pharma_q = prox_idx_pharma_n, nq(5)
xtile prox_idx_childcare_q = prox_idx_childcare_n, nq(5)
xtile prox_idx_health_q = prox_idx_health_n, nq(5)
xtile prox_idx_grocery_q = prox_idx_grocery_n, nq(5)
xtile prox_idx_educpri_q = prox_idx_educpri_n, nq(5)
xtile prox_idx_educsec_q = prox_idx_educsec_n, nq(5)
xtile prox_idx_lib_q = prox_idx_lib_n, nq(5)
xtile prox_idx_transit_q = prox_idx_transit_n, nq(5)
xtile prox_idx_parks_q = prox_idx_parks_n, nq(5)
```

###Create flag for unique observations (DA x day x perseon)
```{}
sort interact_id count_days geoid_DA
by interact_id: gen n1 = _n
by interact_id: gen n2 = _N
```

###Create day counter as observation days per participant
```{}
egen startdate = min(date_n), by(interact_id)
egen enddate = max(date_n), by(interact_id)
bysort interact_id (startdate): replace startdate=startdate[1]
bysort interact_id (enddate): replace enddate=enddate[1]
format startdate %td
format enddate %td

gen count_days = (date_n - startdate) + 1
```

### Active minutes in DA as proportion of all active minutes per person 
```{}
destring total_active_minutes, generate(total_active_minutes_n) ignore(`"NA"') force
sort  interact_id
by interact_id : egen sum_activity_mins = total(total_active_minutes_n)
gen pct_activity_mins = total_active_minutes_n/ sum_activity_mins 
```

### MVPA minutes in DA as proportion of all active minutes per person 
```{}
destring mvpa_active_minutes, generate(mvpa_active_minutes_n) ignore(`"NA"') force
sort  interact_id
by interact_id : egen sum_MVactivity_mins = total(mvpa_active_minutes_n)
gen pct_MVactivity_mins = mvpa_active_minutes_n/ sum_MVactivity_mins 
```

### Minutes in DA as proportion of all minutes per person
```{}
sort interact_id 
by interact_id : egen total_minutes = total(minutes_da)
gen pct_mins = minutes_da/total_minutes
``` 

### Create weekend indicator variable (31% of minutes recorded on weekend or 763,862 minutes)
```{}
gen weekend = 0
replace weekend = 1 if weekend_yes == "1"
```

### Creat indicators for count of observations, max count of observations per day and hour/minutes per days
```{}
sort interact_id date geouid 
by interact_id: gen n1 = _n
by interact_id: gen n2 = _N
by interact_id: gen count_daysmax = max(n2)

gen min_per_day = total_minutes/count_daysmax
gen hr_per_day = (total_minutes/60)/count_daysmax
```

## Multilevel negative binomial: active minutes (sedentary + MV) = CanALE (city specific) + e(total minutes) + random intercept(interact_id) + random slope(count_days)

### city specific CanALE measure
```{}
menbreg total_active_minutes_n i.city_CanAle_q i.city_n, exposure(total_minutes) || interact_id: count_days, irr
```

### National CanAlE measure
```{}
menbreg total_active_minutes_n i.ale_index_q i.city_n, exposure(total_minutes) || interact_id: count_days, irr
```

### Adjust for covariates
```{}
menbreg total_active_minutes_n i.ale_index_q i.city_n i.genderx3 i.lowincome50k agegroupx4 home_yes weekend, exposure(total_minutes) || interact_id: count_days, irr

menbreg mvpa_active_minutes_n i.ale_index_q i.city_n i.genderx3 i.lowincome50k agegroupx4 home_yes weekend, exposure(total_minutes) || interact_id: count_days, irr
```

### Model by weekend variable
```{}
menbreg total_active_minutes_n i.ale_index_q i.city_n if weekend_yes ==1, exposure(total_minutes) || interact_id: count_days, irr

menbreg total_active_minutes_n i.ale_index_q i.city_n if weekend_yes ==0, exposure(total_minutes) || interact_id: count_days, irr
```

## Full model, stratified by city FOR ALL PHYSICAL ACTIVITY
### Montreal
```{}
menbreg total_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 1 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store mtl 
```

### Saskatoon
```{}
menbreg total_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 2 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store ssk 
```

### Vancouver
```{}
menbreg total_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 3 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store van 
```

### Victoria
```{}
menbreg total_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 4 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store vic 
```

## Extract Coefficients and VIF
```{}
estimates table mtl ssk van vic, eq (1 1 1)

*Check VIF (CanALE 6.24 prox employment 4.86)
reg total_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n city_n
vif
```

## Full model, stratified by city FOR MV PHYSICAL ACTIVITY
### Montreal
```{}
menbreg mvpa_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 1 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store mtl 
```

### Saskatoon
```{}
menbreg mvpa_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 2 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store ssk 
```

### Vancouver
```{}
menbreg mvpa_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 3 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store van 
```

### Victoria
```{}
menbreg mvpa_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n if city_n == 4 , exposure(total_minutes) || interact_id: count_days, irr nolog
estimates store vic 
```

## Extract Coefficients and VIF
```{}
estimates table mtl ssk van vic, eq (1 1 1)

*Check VIF (CanALE 6.24 prox employment 4.86)
reg mvpa_active_minutes_n ale_index_q i.gentrify_Dingx3 sprawl_n prox_idx_emp_q prox_idx_pharma_q prox_idx_childcare_q prox_idx_health_q prox_idx_grocery_q prox_idx_educpri_q prox_idx_educsec_q prox_idx_lib_q prox_idx_transit_q prox_idx_parks_q i.genderx3 i.incomegrps ib1.white agegroupx4 home_yes weekend total_precip_mm_n mean_temp_c_n city_n
vif
```

