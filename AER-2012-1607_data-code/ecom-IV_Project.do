
/* This is the replication code for the paper "Education, HIV, and Early Fertility". 
The paper based on the early fertility and sexually transmitted infection health issues faced by the teenager girl in sub-Saharan Africa. Aiming to solve this problem, Keneya government issue two policy: education subsidies and HIV education. This research is designed to analyze the efficiency of these two policies in reducing the health risk. */

clear all 

set more off

capture log close

log using ecom-IV_project,replace

cd"\\uofa\users$\users3\a1876083\Desktop\ECOM\112899-V1\AER-2012-1607_data-code"



global treatmentdummies="Uonly Honly UH";
global treatmentdummiesCT2="Uonly HnoCT UHnoCT HwithCT UHwithCT";
global controlsR="yrbirth_all yrbirth_missing date05v3 date07v2 schsize i.stratum";
global controls="yrbirth_all yrbirth_missing schsize i.stratum";
global controlsKAP="age agemissing schsize i.stratum";
global controlsB="yrbirth_all yrbirth_missing";

*********************************************************

*****************Data Introduction***********************

*********************************************************

/* School information dataset first  */
 
use datasets\school_info,replace

// Load the school information dataset, in this dataset we mainly consider the HIV treatment variable, and uniform treatment variable classfied by the shool size and school id. 

keep schoolid HIVtreat Utreat schsize
// HIVtreat and Utreat are all binary variables, HIVtreat takes 1 if school benefitted from teacher training on AIDS curriculum. Utreat equals to 1 if school benefitted from uniform program. School size we take the mean of it. 

gen Honly=(HIVtreat==1) & (Utreat==0)
// HIVtreat only if the school does not benefit from the uniform. 

gen Uonly=(HIVtreat==0) & (Utreat==1)
// Uniform only if the school does not benefir from the HIVtreat. 

gen UH=(HIVtreat==1) & (Utreat==1)
// The school benefit from both. 

sort schoolid
save school_small, replace
des
summarize

// 
use datasets\studysample_allmerged,clear
sort schoolid
merge schoolid using school_small
drop if _merge==2
drop _merge

describe

* Summary Statistics
sum 



				******************;
				* FIX AGE, YR OF BIRTH;	
					* note 1: we need to use yrbirth (collected at baseline), rather than the yr of birth
					* information collected in the survey, because people tend to lie about their year of
					* birth if they have already started a family;

					* note 2: since kids were in grade 6 in 2003, it's not really possible for them to be
					* born after 1992 (they couldn't have been in grade 6 before age 10)
					* or born before 1987 (they couldn't be older than 16 and be in grade 6);

					replace yrbirth=Q_a3_8_date_of_birth  if yrbirth>1992&Q_a3_8_date_of_birth<yrbirth &  LOG_surveyed==1 & Q_a3_8_date_of_birth  !=.
					replace yrbirth=Q_a3_8_date_of_birth  if yrbirth<1987&Q_a3_8_date_of_birth>yrbirth & LOG_surveyed==1 & Q_a3_8_date_of_birth  !=.
					replace yrbirth=Q_a3_8_date_of_birth if yrbirth==.& Q_a3_8_date_of_birth!=.

					replace yrbirth=1985 if yrbirth<1985
					replace yrbirth=1992 if yrbirth>1992 & yrbirth!=.

					replace yrbirth=1985 if yrbirth<1985
					replace yrbirth=1992 if yrbirth>1992 & yrbirth!=.


					**** new age variables;
					gen age2009=2009-yrbirth 

					replace age2009=2009-Q_a3_8_date_of_birth if (age2009<16 | age2009>22)&Q_a3_8_date_of_birth>1986&Q_a3_8_date_of_birth<1993& Q_a3_8_date_of_birth!=.

					gen age2003=age2009-6


					gen age_survey=Q_year-yrbirth
						label var age_survey "age at time of survey"

					gen age_survey_all=age_survey
					gen age_survey_missing=age_survey==. if LOG_surveyed==1 & dead==0
						gen a=age_survey if LOG_surveyed==1 & dead==0
						bys sex: egen r=mean(a)
						replace age_survey_all=r if LOG_surveyed==1 & dead==0 & age_survey==. 
						drop a r
						label var age_survey_all "Age at time of survey, missing replaced with mean by gender"


					gen yrbirth_missing=(yrbirth==.)
					gen yrbirth_all=yrbirth
						bys sex: egen mean=mean(yrbirth)
						replace mean=round(mean,1)
						replace yrbirth_all=mean if yrbirth_missing==1
						drop mean
						label var yrbirth_all "Year of Birth, missing replaced with mean by gender"

			save studysample_allmerged2, replace



********************;
****FIGURE 1************;
********************;


use studysample_allmerged2, clear
collapse evpreg07v2 hsv2_positive, by(sch03v1 Uonly Honly UH sex)

#delimit cr
gen group=1 if Uonly==1
replace group=2 if UH==1

foreach outcome in evpreg07v2 hsv2_positive  {
	local txt "Share of girls who dropped out within 5 years"
	if "`outcome'"=="evpreg07v2" local txt "Share of girls ever pregnant within 5 years"
	if "`outcome'"=="evpreg05v3" local txt "Share of girls ever pregnant within 3 years"
	if "`outcome'"=="child_by_16" local txt "Share of girls who had a child by age 16"
	if "`outcome'"=="hsv2_positive" local txt "Share of girls HSV2 positive after 7 years"
	if "`outcome'"=="reached8" local txt "Share of girls who reached 8th grade"
	twoway (kdensity `outcome' if sex==2 & Uonly==0 & Honly==0 & UH==0, lpat(longdash)) || ///
		(kdensity `outcome' if sex==2 & Uonly==0 & Honly==1 & UH==0, lpat(dash_dot) ) ||  ///
		(kdensity `outcome' if sex==2 & Uonly==0 & Honly==0 & UH==1, lpat(solid) lwidth(medthick)) ||  ///
		(kdensity `outcome' if sex==2 & Uonly==1 & Honly==0 & UH==0, lpat(dash) lwidth(medthick)), ///
		legend(col(2) row(2) label(1 "Control") label(4 "Stand-Alone Education Subsidy") ///
		 label(3 "Joint Program")  label(2 "Stand-Alone HIV education") size(small)) ///
		name(`outcome', replace) graphregion(color(white) fcolor(white)) nodraw /// 
		xtit(`txt', margin(medium)) ytit("Density") tit(" ")
	}	

ksmirnov evpreg07v2  if sex==2, by(group)
local p1=round(r(p_cor),0.001)

ksmirnov hsv2_positive  if sex==2, by(group)
local p2=round(r(p_cor), 0.001)
	
gr combine evpreg07v2 hsv2_positive, col(1) rows(2) xcommon ysize(9) xsize(5.5) graphregion(color(white) fcolor(white)) /// 
	note("Notes: School-level averages." "Two-sample Kolmogorov-Smirnov tests for equality of distribution between Stand-alone Education" "Subsidy and Joint Program:" "   p-value for share ever pregnant (top panel): `p1'**" "   p-value for share HSV2 positive (bottom panel): `p2'**", size(vsmall)) saving("Fig1.gph", replace)

	


**************;
*TABLE 1 PANEL A;
****************;
#delimit;
use datasets\school_info.dta, clear;

gen var="";
for any  mean_U sd_U mean_H sd_H  mean_UH sd_UH  mean_control sd_control p_Uonly p_Honly p_UH p_UUH p_HUH  N: gen X=.;
gen urban=0;
replace urban=1 if situation<3;

gen sexratio_teachers=Nfemale/(TOTteachers-Nfemale);

gen Honly=(HIVtreat==1) & (Utreat==0);
gen Uonly=(HIVtreat==0) & (Utreat==1);
gen UH=(HIVtreat==1) & (Utreat==1);

local vars=10;
for any  kcpe2003  schsize ratio02  latrine_2004 urban total_2km  
TOTteachers  meanage sexratio_teachers HIVtreat

 \ num 1/`vars':
 replace var="X" if _n==Y \
 reg X Uonly \
 replace N=e(N) if _n==Y \
 test Uonly=0 \
 replace p_Uonly=r(p) if _n==Y \
 reg X Honly \
 test Honly=0 \
 replace p_Honly=r(p) if _n==Y \
 reg X UH \
 test UH=0 \
 replace p_UH=r(p) if _n==Y \
 reg X Uonly Honly UH \
 test UH=Uonly \
 replace p_UUH=r(p) if _n==Y \
 test UH=Honly \
 replace p_HUH=r(p) if _n==Y \
 sum X if Utreat==1&HIVtreat==0 \
 replace mean_U=r(mean) if _n==Y \
 replace sd_U=r(sd) if _n==Y \
 sum X if Utreat==0&HIVtreat==1 \
 replace mean_H=r(mean) if _n==Y \
 replace sd_H=r(sd) if _n==Y \
 sum X if Utreat==1&HIVtreat==1 \
 replace mean_UH=r(mean) if _n==Y \
 replace sd_UH=r(sd) if _n==Y \
 sum X if Utreat==0 \
 replace mean_control=r(mean) if _n==Y \
 replace sd_control=r(sd) if _n==Y ;

for var p_Uonly p_Honly p_UH p_UUH p_HUH  mean* sd*: replace X=round(X, 0.001);
outsheet var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control 
p_Uonly p_Honly p_UH p_UUH p_HUH  N if _n<=`vars' using "table1a.xls",replace;



*****************;
*TABLE 1 PANEL B;
******************;
#delimit;
use datasets\sampling_frame.dta, clear
gen age03_female=age03 if female==1&std03v1==6
gen age03_male=age03 if female==0&std03v1==6

gen femalecount=1 if female==1&std03v1==6
gen malecount=1 if female==0&std03v1==6

collapse  Utreat03v1 HIVtreat03v1 age03_female age03_male (sum) femalecount malecount , by(sch03v1)
gen sexratio_grade6=femalecount/malecount

gen Honly=(HIVtreat03v1==1) & (Utreat03v1==0)
gen Uonly=(HIVtreat03v1==0) & (Utreat03v1==1)
gen UH=(HIVtreat03v1==1) & (Utreat03v1==1)

gen var=""
for any  mean_U sd_U mean_H sd_H  mean_UH sd_UH  mean_control sd_control p_Uonly p_Honly p_UH p_UUH p_HUH  N: gen X=.

local vars=5
for any femalecount malecount sexratio_grade6 age03_female age03_male 

 \ num 1/`vars':
 replace var="X" if _n==Y \
 reg X Uonly \
 replace N=e(N) if _n==Y \
 test Uonly=0 \
 replace p_Uonly=r(p) if _n==Y \
 reg X Honly \
 test Honly=0 \
 replace p_Honly=r(p) if _n==Y \
 reg X UH \
 test UH=0 \
 replace p_UH=r(p) if _n==Y \
 reg X Uonly Honly UH \
 test UH=Uonly \
 replace p_UUH=r(p) if _n==Y \
 test UH=Honly \
 replace p_HUH=r(p) if _n==Y \
 sum X if Utreat03v1==1&HIVtreat03v1==0 \
 replace mean_U=r(mean) if _n==Y \
 replace sd_U=r(sd) if _n==Y \
 sum X if Utreat03v1==0&HIVtreat03v1==1 \
 replace mean_H=r(mean) if _n==Y \
 replace sd_H=r(sd) if _n==Y \
 sum X if Utreat03v1==1&HIVtreat03v1==1 \
 replace mean_UH=r(mean) if _n==Y \
 replace sd_UH=r(sd) if _n==Y \
 sum X if Utreat03v1==0 \
 replace mean_control=r(mean) if _n==Y \
 replace sd_control=r(sd) if _n==Y 

for var p_Uonly p_Honly p_UH p_UUH p_HUH mean* sd*: replace X=round(X, 0.001)
outsheet var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control 
p_Uonly p_Honly p_UH p_UUH p_HUH  N if _n<=`vars' using "table1b.xls",replace




log close