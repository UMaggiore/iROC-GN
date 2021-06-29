
////////////////////////////////////////////////////////////////////////////////
**#  START STUDY POPULATION
////////////////////////////////////////////////////////////////////////////////
**# USE WIDE DATSET

use all_vars_iroc_gn_creat_wide, clear
di _newline(3) in ye "---> as of `c(current_date)'  we are using the following number of pts"
tab GNDISEASE
list recordid if missing(HOSPITALIZED)
tab GNDISEASE HOSPITALIZED, row

cap drop _nomiss_current_analyses
egen _nomiss_current_analyses = rowmiss(eGFR_pre eGFR_adm eGFR_recent age GENDER NON_WHITE ACEI )
tab GNDISEASE HOSPITALIZED if _nomiss_current_analyses == 0


*-------------------------------------------------------------------------------
**# Start visualization of data available for longitudinal analyses
*-------------------------------------------------------------------------------
**# USE LONG DATASET

use long_with_time, clear

iis recordid
tis month
xtsum eGFR
bysort GROUP: xtsum eGFR
xtsum month
bysort GROUP: xtsum month

*-------------------------------------------------------------------------------
* Start calculation of median follow-up
*-------------------------------------------------------------------------------
cap drop max_month_post
cap drop max_month_pre
gen max_month_post =.
gen max_month_pre =.  

bysort recordid (TIME): replace max_month_post = month if (_n == _N) & !missing(month) & TIME > 1
bysort recordid (TIME): replace max_month_pre = month if !missing(month) & TIME == 1

summ max_month_post, detail
summ max_month_pre, detail

bysort GROUP: summ max_month_post, detail
bysort GROUP: summ max_month_pre, detail

*-------------------------------------------------------------------------------
* End calculation of median follow-up
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* Start number of longitudinal measurements available
*-------------------------------------------------------------------------------

xtsum eGFR
bysort GROUP: xtsum eGFR

xtsum eGFR if TIME > 1
bysort GROUP: xtsum eGFR if TIME > 1

xtsum eGFR if TIME == 1
bysort GROUP: xtsum eGFR if TIME == 1

xtsum month
bysort GROUP: xtsum month

xtsum month if TIME > 1
bysort GROUP: xtsum month if TIME > 1

xtsum month if TIME == 1
bysort GROUP: xtsum month if TIME == 1

*-------------------------------------------------------------------------------
* Start number of longitudinal measurements available
*-------------------------------------------------------------------------------


////////////////////////////////////////////////////////////////////////////////
**#  END STUDY POPULATION
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
**# //////   START TABLE BASELINE CHARACTERISTIC (3 GROUPS)
////////////////////////////////////////////////////////////////////////////////


use all_vars_iroc_gn_creat_wide, clear


set varabbrev off
cap drop _GROUP
gen _GROUP = GROUP
qui: table (var) (GROUP) , ///
		stat (count _GROUP) ///
		stat(count age) stat(mean age) stat(sd age) ///
		stat(fvfrequency GENDER RACE ETHNICITY) stat(fvpercent GENDER RACE ETHNICITY) ///
        stat(count BMI serumcreatininepriortocovidinfec eGFR_pre serumalbuminpriortoinfection proteinuriaquantifiedpriortocovi ) ///
        stat(mean BMI serumcreatininepriortocovidinfec eGFR_pre serumalbuminpriortoinfection proteinuriaquantifiedpriortocovi ) ///
        stat(sd BMI serumcreatininepriortocovidinfec eGFR_pre serumalbuminpriortoinfection proteinuriaquantifiedpriortocovi ) ///
		stat(fvfrequency HYPERT DIABETES OBESITY CVD COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER COUGH DYSPNEA ///
		FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT) ///
		stat(fvpercent HYPERT DIABETES OBESITY CVD COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER COUGH DYSPNEA ///
		FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT) ///
		stat(count serumcreatinineonadmissionifhosp eGFR_adm serumalbuminduringcovidinfection ///
		peakproteinuriaquantifiedduringc wbc lymphocytecountabsolute ///
		absoluteneutrophilcount hemoglobin platelet ferritin creactiveprotein ddimer) ///
		stat(mean serumcreatinineonadmissionifhosp eGFR_adm serumalbuminduringcovidinfection ///
		peakproteinuriaquantifiedduringc wbc lymphocytecountabsolute ///
		absoluteneutrophilcount hemoglobin platelet ferritin creactiveprotein ddimer) ///
		stat(sd serumcreatinineonadmissionifhosp eGFR_adm serumalbuminduringcovidinfection ///
		peakproteinuriaquantifiedduringc wbc lymphocytecountabsolute ///
		absoluteneutrophilcount hemoglobin platelet ferritin creactiveprotein ddimer) ///
        nformat(%3.0f count) ///
        nformat(%3.1f mean sd) ///
		nformat(%3.1f fvfrequency) ///
		nformat(%3.1f fvpercent)
		
// Change the label of GROUP
collect label dim GROUP "Study Group", modify
		
// don't show stat description along rows
collect style header result, level(hide)
collect preview

// put frequency and percent in the same column as mean and sd, respectively
collect recode result fvfrequency = mean fvpercent = sd
collect layout (var) (GROUP[1 2 3]#result)

// display percent (i.e. sd column) as %
foreach x in GENDER RACE ETHNICITY HYPERT DIABETES OBESITY CVD ///
	COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER COUGH DYSPNEA ///
		FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ///
		ANOSMIA NEUR_SYMPT  {
	collect style cell result[sd]#var[GROUP `x'], sformat("%s%%")
	 }


// display SD within brackets
collect style cell result[sd]#var[age BMI serumcreatininepriortocovidinfec eGFR_pre serumalbuminpriortoinfection proteinuriaquantifiedpriortocovi serumcreatinineonadmissionifhosp eGFR_adm serumalbuminduringcovidinfection peakproteinuriaquantifiedduringc wbc lymphocytecountabsolute absoluteneutrophilcount hemoglobin platelet ferritin creactiveprotein ddimer], sformat("(%s)")
collect preview


// display frequency (i.e. mean column) as integer
collect style cell result[mean]#var[GENDER RACE ETHNICITY HYPERT DIABETES OBESITY CVD COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER  COUGH DYSPNEA FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT], nformat(%4.0f)
collect preview

// calculate-save P values for continuous vars and tag vars

cap program drop pkwallis
    program define pkwallis, rclass
        version 17
        syntax varname [if] [in], BY(varname)
		marksample Touse
        markout `Touse' `by', strok
        qui kwallis `varlist' if `Touse'==1, by(`by')
        return scalar p  = chi2tail(r(df), r(chi2_adj))
    end


foreach x in age BMI serumcreatininepriortocovidinfec eGFR_pre serumalbuminpriortoinfection proteinuriaquantifiedpriortocovi serumcreatinineonadmissionifhosp eGFR_adm serumalbuminduringcovidinfection peakproteinuriaquantifiedduringc wbc lymphocytecountabsolute absoluteneutrophilcount hemoglobin platelet ferritin creactiveprotein ddimer {
	qui: collect r(p), tag(var[`x']): pkwallis `x', by(GROUP)
	 }

// calculate-save P values for categorical vars and tag vars
foreach x in GENDER RACE ETHNICITY HYPERT DIABETES OBESITY CVD COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER COUGH DYSPNEA ///
		FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT {
	qui: collect r(p_exact), tag(var[`x']): tab GROUP `x', exact
	 }	 


// attach columns for result levels p and p_exact
collect layout (var) (GROUP[1 2 3]#result result[p p_exact])

// recode var levels to get P valu in the first row of categorical vars
foreach x in GENDER RACE ETHNICITY HYPERT DIABETES OBESITY CVD ///
	COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER COUGH DYSPNEA ///
		FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ///
		ANOSMIA NEUR_SYMPT  {
	collect recode var `x' = 1.`x', fortags(result[p_exact])
	 }

// display P values with three digits
collect style cell result[p p_exact], nformat(%4.3f)

// label p-values results as "P value"
collect label levels result p "P value", modify
collect label levels result p_exact "P value", modify
collect style header result[p], level(label)
collect style header result[p_exact], level(label)

// display P value from the two type of stat test in a single column
collect recode result p_exact = p 
collect preview


// change the var label of vars
collect label list var, all
collect label levels var _GROUP "Number of patients"
collect label levels var Age "BMI, yrs", modify
collect label levels var BMI "BMI, kg/m2", modify
collect label levels var serumcreatininepriortocovidinfec "sCr, mg/dl ", modify
collect label levels var eGFR_pre "eGFR, ml/min/1.73m2", modify
collect label levels var serumalbuminpriortoinfection "Serum albumin, g/dl", modify
collect label levels var proteinuriaquantifiedpriortocovi "Proteinuria, g/day", modify
collect label levels var proteinuriaquantifiedpriortocovi "Proteinuria, g/day", modify
collect label levels var HYPERT "Hypertension", modify
collect label levels var DIABETES "Diabetes", modify
collect label levels var OBESITY "Obesity", modify
collect label levels var CVD "Cardiovascular disease", modify
collect label levels var COPD "COPD", modify
collect label levels var ASTHMA "Asthma", modify
collect label levels var LIVER_DIS "Liver disease", modify
collect label levels var CANCER "Cancer", modify
collect label levels var HIV "HIV/AIDS", modify
collect label levels var RA "Rheumatoid Arthritis", modify
collect label levels var SLE "SLE", modify
collect label levels var SMOKER "Smoking habit", modify
collect label levels var ACEI "use of RAASi", modify
collect label levels var FEVER "Fever", modify
collect label levels var COUGH "Cough", modify
collect label levels var DYSPNEA "Dyspnea", modify
collect label levels var FATIGUE "Fatigue", modify
collect label levels var MYALGIA "Myalgia", modify
collect label levels var GI_SYMPT "GI symptoms", modify
collect label levels var ANOREXIA "Anorexia", modify
collect label levels var CHILLS "Chills", modify
collect label levels var NASALCONG "Nasal congestion", modify
collect label levels var SORETHROAT "Sore throat", modify
collect label levels var ANOSMIA "Anosmia", modify
collect label levels var NEUR_SYMPT "Neurologic symptoms", modify
collect label levels var serumcreatinineonadmissionifhosp "Admission sCr,  mg/dl", modify
collect label levels var eGFR_adm "Admission eGFR ml/min/1.73m2", modify
collect label levels var serumalbuminduringcovidinfection "During COVID sAlb, g/dl", modify
collect label levels var peakproteinuriaquantifiedduringc "During COVID Proteinuria, g/day", modify
collect label levels var wbc "White blood cells (x1000/uL)", modify
collect label levels var lymphocytecountabsolute "Lymphocytes (x1000/uL)", modify
collect label levels var absoluteneutrophilcount "Neutrophils (x1000/uL)", modify
collect label levels var  hemoglobin "Hemoglobin (g/dL)", modify
collect label levels var  platelet "Platelets (x109/L)", modify
collect label levels var  ferritin "Ferritin (ng/mL)", modify
collect label levels var  creactiveprotein "CRP (mg/L)", modify
collect label levels var  ddimer  "D-Dimer (ng/ml)", modify



// set the layout
collect label list GROUP, all
// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header foreign
collect style cell cell_type[column-header]#GROUP, border(bottom, pattern(single))
collect preview

collect style row stack, nobinder 

// export tables
collect export Table1_3grp.docx, replace
* collect export Table1_3grp.pdf, replace
collect export Table1_3grp.html, replace
* collect export Table1_3grp.xls, replace
* collect export Table1_3grp.md, replace
collect export Table1_3grp.txt, replace



set varabbrev on

*-------------------------------------------------------------------------------
**# Start PAIRWISE COMPARISONS Table 1 3 groups
*-------------------------------------------------------------------------------
/// eg use superscripts a if P<0.05 Ctrl-Hospitalized vs GN-Hospitalized)
///						b if P<0.05 Ctrl-Hospitalized vs GN-Outpatients
///						c if P<0.05 GN-Hospitalized vs GN-Outpatients 

foreach var of varlist age BMI serumcreatininepriortocovidinfec eGFR_pre serumalbuminpriortoinfection proteinuriaquantifiedpriortocovi serumcreatinineonadmissionifhosp eGFR_adm serumalbuminduringcovidinfection peakproteinuriaquantifiedduringc wbc lymphocytecountabsolute absoluteneutrophilcount hemoglobin platelet ferritin creactiveprotein ddimer {
	qui cap ranksum `var' if GROUP == 1 | GROUP ==2, by(GROUP)
	di in gr _newline(3) "----> var `var',  Ctrl-Hospitalized vs GN-Hospitalized, P =" in wh %4.3f r(p)
	qui cap ranksum `var' if GROUP == 1 | GROUP ==3, by(GROUP)
	di in gr _newline(3) "----> var `var',  Ctrl-Hospitalized vs GN-Outpatients, P =" in wh %4.3f r(p)
	qui cap ranksum `var' if GROUP == 2 | GROUP ==3, by(GROUP)
	di in gr _newline(3) "----> var `var',  GN-Hospitalized vs GN-Outpatients, P =" in wh %4.3f r(p)
	 }
	 
	 
foreach var of varlist GENDER RACE ETHNICITY HYPERT DIABETES OBESITY CVD COPD ASTHMA LIVER_DIS CANCER HIV RA SLE SMOKER ACEI FEVER COUGH DYSPNEA ///
		FATIGUE MYALGIA GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT {
	qui cap tab GROUP `var' if GROUP == 1 | GROUP ==2, exact
	di in gr _newline(3) "----> var `var',  Ctrl-Hospitalized vs GN-Hospitalized, P = " in wh %4.3f r(p_exact) 
	qui cap tab GROUP `var' if GROUP == 1 | GROUP ==3, exact
	di in gr _newline(3) "----> var `var',  Ctrl-Hospitalized vs GN-Outpatients, P = " in wh %4.3f r(p_exact)
	qui cap tab GROUP `var' if GROUP == 2 | GROUP ==3, exact
	di in gr _newline(3) "----> var `var',  GN-Hospitalized vs GN-Outpatients, P = " in wh %4.3f r(p_exact)
	 }	 

*-------------------------------------------------------------------------------
**# End PAIRWISE COMPARISONS Table 1 3 groups
*-------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
**# //////   END TABLE BASELINE CHARACTERISTIC (3 GROUPS)
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# //////   START TABLE BASELINE IS AND PRIMARY RENAL DISEASE AND DURATION GN
////////////////////////////////////////////////////////////////////////////////


use all_vars_iroc_gn_creat_wide, clear
collect clear
preserve
keep if GNDISEASE == 2
qui: table (var) (HOSPITALIZED) , ///
		stat(fvfrequency IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE ///
		MPGN PIGN TMA AA_FG ///
		IMMUNOSUPPRESSION STEROIDS MYCOPHENOLATE ///
		AZATHIOPRINE RITUXIMAB ) ///
		stat(fvpercent  IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE /// 
		MPGN PIGN TMA AA_FG ///
		IMMUNOSUPPRESSION STEROIDS MYCOPHENOLATE ///
		AZATHIOPRINE RITUXIMAB) ///
		stat(fvfrequency DURATION_GN) ///
		stat(fvpercent DURATION_GN) ///
		nformat(%4.0f fvfrequency) ///
		nformat(%3.1f fvpercent)
		
// Change the label of HOSPITALIZED
collect label dim HOSPITALIZED "Admission Status", modify
// Change the label of values HOSPITALIZED
collect label levels HOSPITALIZED 1 "Outpatients" 2 "Hospitalized", modify
		
// don't whow stat description in along rows
collect style header result, level(hide)
collect preview

collect layout (var) (HOSPITALIZED[1 2]#result)

// display percent (i.e. sd column) as %
foreach x in  IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE ///
		MPGN PIGN TMA AA_FG ///
		IMMUNOSUPPRESSION STEROIDS MYCOPHENOLATE ///
		AZATHIOPRINE RITUXIMAB DURATION_GN ///
  {
	collect style cell result[fvpercent]#var[HOSPITALIZED `x'], sformat("%s%%")
	 }



// calculate-save P values for categorical vars and tag vars
foreach x in IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE ///
		MPGN PIGN TMA AA_FG ///
		IMMUNOSUPPRESSION STEROIDS MYCOPHENOLATE ///
		AZATHIOPRINE RITUXIMAB {
	qui: collect r(p_exact), tag(var[`x']): tab HOSPITALIZED `x', exact
	 }	
	 
cap drop hospitalized
recode HOSPITALIZED (1 = 0 "Outpatients") (2 = 1 "Hospitalized"), gen(hospitalized) label(hosp_1_0)
set seed 123
collect r(p_exact), tag(var[1.DURATION_GN]):  nptrend hospitalized, group(DURATION_GN) carmitage exact

// attach columns for result levels p and p_exact
collect layout (var) (HOSPITALIZED[1 2]#result result[p p_exact])
// recode var levels to their first level

foreach x in   ///
		IMMUNOSUPPRESSION STEROIDS MYCOPHENOLATE ///
		AZATHIOPRINE RITUXIMAB DURATION_GN ///
			{
	collect recode var `x' = 1.`x', fortags(result[p_exact])
	 }
	 
foreach x in  IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE ///
			MPGN PIGN TMA AA_FG ///
			{
	collect recode var `x' = 0.`x', fortags(result[p_exact])
	 }
	 

// display P values with three digits
collect style cell result[p p_exact], nformat(%4.3f)

// label p-values results as "P value"
collect label levels result p "P value", modify
collect label levels result p_exact "P value", modify
collect style header result[p], level(label)
collect style header result[p_exact], level(label)
// display P value from the two type of stat test in a single column
collect recode result p_exact = p 
collect preview




// change the var label of vars


collect label levels var GN_SLE  "SLE GN diagnosis", modify
collect label levels var VASCULITIS  "Vasculitis", modify
collect label levels var EITHER_VASC_SLE   "SLE GN or Vasculitis", modify
collect label levels var IgAN_HSP  "IgA nephropathy", modify
collect label levels var FSGS_MCD  "FSGS or MCD", modify
collect label levels var MN  "Membranous nephropathy", modify
collect label levels var  MPGN "Membranoproliferative glomerulonephritis", modify
collect label levels var  PIGN "Post infectious Glomerulonephritis", modify
collect label levels var  TMA "Thrombotic microangiopathy", modify
collect label levels var  AA_FG "Amyloidosis/fibrillary glomerunephritis", modify
collect label levels var IMMUNOSUPPRESSION "Immunosuppression", modify 
collect label levels var STEROIDS "Steroids", modify  
collect label levels var MYCOPHENOLATE "MMF", modify
collect label levels var AZATHIOPRINE "AZA", modify 
collect label levels var RITUXIMAB "RTX", modify 
collect label levels var DURATION_GN "Duration of GN disease", modify


collect label levels GN_SLE 0 "No" 1 "Yes", modify
collect label levels VASCULITIS 0 "No" 1 "Yes", modify
collect label levels EITHER_VASC_SLE 0 "No" 1 "Yes", modify
collect label levels IgAN_HSP 0 "No" 1 "Yes", modify
collect label levels FSGS_MCD 0 "No" 1 "Yes", modify
collect label levels MN 0 "No" 1 "Yes", modify
collect label levels MPGN  0 "No" 1 "Yes", modify
collect label levels PIGN 0 "No" 1 "Yes", modify
collect label levels TMA 0 "No" 1 "Yes", modify
collect label levels AA_FG 0 "No" 1 "Yes", modify




// chenage the value label of hiprice
* collect label levels hiprice 0 "No" 1 "Yes", modify


collect label list HOSPITALIZED, all
// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header foreign
collect style cell cell_type[column-header]#HOSPITALIZED, border(bottom, pattern(single))
collect preview

collect style row stack, nobinder 

collect export Table_GN_baseline.docx, replace
collect export Table_GN_baseline.html, replace
collect export Table_GN_baseline.txt, replace

restore



////////////////////////////////////////////////////////////////////////////////
**# //////   END TABLE BASELINE IS AND PRIMARY RENAL DISEASE AND DURATION GN
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
**# ///// START SUPPLEMENTARY TABLE S1 (former Tab 4) in-hospital complications
////////////////////////////////////////////////////////////////////////////////



use all_vars_iroc_gn_creat_wide, clear


preserve
keep if HOSPITALIZED ==2

qui: table (var) (GNDISEASE) , ///
		stat(fvfrequency INTUBATED VASOPRESSORS AKI_STAGE RRT_YESNO) stat(fvpercent INTUBATED VASOPRESSORS AKI_STAGE RRT_YESNO) ///
		stat(count RRT_DURATION) ///
		stat(mean RRT_DURATION) ///
		stat(sd RRT_DURATION) ///
		stat(fvfrequency RRT_AT_DISCHARGE) stat(fvpercent RRT_AT_DISCHARGE) ///		
        stat(count  serumcreatinineonadmissionifhosp peakserumcreatinineduringcovidin serumalbuminduringcovidinfection whatwasthelowestvaluenadirofseru 	peakserumcreatinineduringcovidin) ///
        stat(mean RRT_DURATION serumcreatinineonadmissionifhosp peakserumcreatinineduringcovidin serumalbuminduringcovidinfection whatwasthelowestvaluenadirofseru 	peakserumcreatinineduringcovidin ) ///
        stat(sd RRT_DURATION serumcreatinineonadmissionifhosp peakserumcreatinineduringcovidin serumalbuminduringcovidinfection whatwasthelowestvaluenadirofseru peakproteinuriaquantifiedduringc ) ///
		stat(fvfrequency CARDIAC_COMPL MI ARRHYTHMIA GI CNS_MAIN PE DVT SUPERIMP_INFECTION) ///
		stat(fvpercent CARDIAC_COMPL MI ARRHYTHMIA GI CNS_MAIN PE DVT SUPERIMP_INFECTION) ///
		stat(count LOS) ///
		stat(mean LOS) ///
		stat(sd LOS) ///
		stat(fvfrequency DEATH_YESNO) ///
		stat(fvpercent DEATH_YESNO) ///		
        nformat(%3.0f count) ///
        nformat(%3.1f mean sd) ///
		nformat(%3.1f fvfrequency) ///
		nformat(%3.1f fvpercent)
		
// Change the label of GNDISEASE
collect label dim GNDISEASE "Pre-existing Glomerular Disease", modify
		
// don't whow stat description in along rows
collect style header result, level(hide)
collect preview

// put frequency and percent in the same column as mean and sd, respectively
collect recode result fvfrequency = mean fvpercent = sd
collect layout (var) (GNDISEASE[1 2]#result)

// display percent (i.e. sd column) as %
foreach x in INTUBATED VASOPRESSORS AKI_STAGE RRT_YESNO RRT_AT_DISCHARGE ///
			CARDIAC_COMPL MI ARRHYTHMIA GI CNS_MAIN PE DVT SUPERIMP_INFECTION ///
			DEATH_YESNO ///
  {
	collect style cell result[sd]#var[GNDISEASE `x'], sformat("%s%%")
	 }


// display SD within brackets
collect style cell result[sd]#var[RRT_DURATION serumcreatinineonadmissionifhosp peakserumcreatinineduringcovidin serumalbuminduringcovidinfection whatwasthelowestvaluenadirofseru peakproteinuriaquantifiedduringc  LOS], sformat("(%s)")
collect preview

* collect style header foreign, title(hide)
// display frequency (i.e. mean column) as integer
collect style cell result[mean]#var[GNDISEASE INTUBATED VASOPRESSORS AKI_STAGE RRT_YESNO RRT_AT_DISCHARGE ///
			CARDIAC_COMPL MI ARRHYTHMIA GI CNS_MAIN PE DVT SUPERIMP_INFECTION ///
			DEATH_YESNO], nformat(%4.0f)
collect preview

// calculate-save P values for continuous vars and tag vars
foreach x in RRT_DURATION serumcreatinineonadmissionifhosp peakserumcreatinineduringcovidin serumalbuminduringcovidinfection whatwasthelowestvaluenadirofseru peakproteinuriaquantifiedduringc  LOS {
	qui: collect r(p), tag(var[`x']): ranksum `x', by(GNDISEASE)
	 }

// calculate-save P values for categorical vars and tag vars
foreach x in INTUBATED VASOPRESSORS AKI_STAGE RRT_YESNO RRT_AT_DISCHARGE ///
			CARDIAC_COMPL MI ARRHYTHMIA GI CNS_MAIN PE DVT SUPERIMP_INFECTION ///
			DEATH_YESNO {
	qui: collect r(p_exact), tag(var[`x']): tab GNDISEASE `x', exact
	 }	 


// attach columns for result levels p and p_exact
collect layout (var) (GNDISEASE[1 2]#result result[p p_exact])
// recode var levels to their first level

foreach x in INTUBATED VASOPRESSORS AKI_STAGE  ///
			CARDIAC_COMPL MI ARRHYTHMIA GI CNS_MAIN PE DVT SUPERIMP_INFECTION ///
			{
	collect recode var `x' = 1.`x', fortags(result[p_exact])
	 }
	 
foreach x in RRT_YESNO RRT_AT_DISCHARGE ///
			DEATH_YESNO  {
	collect recode var `x' = 0.`x', fortags(result[p_exact])
	 }

// display P values with three digits
collect style cell result[p p_exact], nformat(%4.3f)

// label p-values results as "P value"
collect label levels result p "P value", modify
collect label levels result p_exact "P value", modify
collect style header result[p], level(label)
collect style header result[p_exact], level(label)
// display P value from the two type of stat test in a single column
collect recode result p_exact = p 
collect preview




// change the var label of vars
collect label list var, all
collect label levels var INTUBATED "Intubated", modify 
collect label levels var VASOPRESSORS "Use of inotropes/vasopressors", modify 
collect label levels var AKI_STAGE "Developed AKI", modify 
collect label levels var RRT_YESNO "RRT new onset required", modify 
collect label levels var RRT_DURATION "Days of RRT", modify
collect label levels var RRT_AT_DISCHARGE "Discharge on RRT", modify
collect label levels var serumcreatinineonadmissionifhosp "Admission serum creatinine, mg/dl", modify 
collect label levels var peakserumcreatinineduringcovidin "Peak serum creatinine, mg/dl", modify
collect label levels var serumalbuminduringcovidinfection "Admission serum albumin, g/dl", modify 
collect label levels var whatwasthelowestvaluenadirofseru "Nadir serum albumin, g/dl", modify	
collect label levels var peakproteinuriaquantifiedduringc "Peak proteinuria, g/day", modify
collect label levels var CARDIAC_COMPL "Cardiac complications", modify 
collect label levels var MI "MI", modify
collect label levels var ARRHYTHMIA "Arrhythmia", modify 
collect label levels var GI "GI complications", modify
collect label levels var CNS_MAIN "CNS complications", modify 
collect label levels var PE "PE", modify
collect label levels var DVT "DVT", modify 
collect label levels var SUPERIMP_INFECTION  "Superimposed bacterial infection", modify 
collect label levels var LOS "Length of hospital stay, days", modify
collect label levels var DEATH_YESNO "Death", modify


// chenage the value label
collect label levels RRT_AT_DISCHARGE 0 "No" 1 "Yes", modify


collect label list GNDISEASE, all
// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header foreign
collect style cell cell_type[column-header]#GNDISEASE, border(bottom, pattern(single))
collect preview

collect style row stack, nobinder 

collect export Table4.docx, replace
* collect export Table4.pdf, replace
collect export Table4.html, replace
* collect export Table4.xls, replace
collect export Table4.md, replace
collect export Table4.txt, replace

restore

tabstat proteinuriaquantifiedpriortocov if GNDISEASE, by(HOSPITALIZED) stat(n mean sd) format(%3.1f)


////////////////////////////////////////////////////////////////////////////////
**# ///// END SUPPLEMENTARY TABLE S1 (Former Tab 4) in-hospital complications
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
**# START TABLE 3 (main) Effect of GN on clinical outcomes AKI RRT Death
////////////////////////////////////////////////////////////////////////////////


use all_vars_iroc_gn_creat_wide, clear
collect clear
collect style clear
collect style use default

keep if HOSPITALIZED == 2
keep if !missing(eGFR_pre)
tab GNDISEASE

cap drop SD_eGFR_pre
qui summ eGFR_pre
gen SD_eGFR_pre  = - (eGFR_pre - 0) / r(sd)

cap drop SD_age
qui summ age
gen SD_age  = (age - 0) / r(sd)

cap collect drop Outcome
cap collect create Outcome, replace

collect _r_b _r_ci _r_p, name(Outcome) tag(AKI["crude"]): ///
        logistic AKI_YESNO GNDISEASE
collect _r_b _r_ci _r_p, name(Outcome) tag(AKI["eGFR-adj"]): ///
        logistic AKI_YESNO GNDISEASE SD_eGFR_pre
collect _r_b _r_ci _r_p, name(Outcome) tag(AKI["fully-adj"]): ///
        logistic AKI_YESNO GNDISEASE SD_eGFR_pre SD_age GENDER NON_WHITE ACEI
collect _r_b _r_ci _r_p, name(Outcome) tag(RRT["crude"]): ///
        logistic RRT_YESNO GNDISEASE
collect _r_b _r_ci _r_p, name(Outcome) tag(RRT["eGFR-adj"]): ///
        logistic RRT_YESNO GNDISEASE SD_eGFR_pre
collect _r_b _r_ci _r_p, name(Outcome) tag(RRT["fully-adj"]): ///
        logistic RRT_YESNO GNDISEASE SD_eGFR_pre SD_age GENDER NON_WHITE ACEI
collect _r_b _r_ci _r_p, name(Outcome) tag(Death["crude"]): ///
        logistic DEATH_YESNO GNDISEASE
collect _r_b _r_ci _r_p, name(Outcome) tag(Death["eGFR-adj"]): ///
        logistic DEATH_YESNO GNDISEASE SD_eGFR_pre
collect _r_b _r_ci _r_p, name(Outcome) tag(Death["fully-adj"]): ///
        logistic DEATH_YESNO GNDISEASE SD_eGFR_pre SD_age GENDER NON_WHITE ACEI
                               
// define number of digits
collect style cell result[_r_b], nformat(%3.2f)
collect style cell result[_r_ci], nformat(%3.2f) sformat("[%s]") cidelimiter(,)
collect style cell result[_p], nformat(%4.3f)

// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header
collect style cell cell_type[column-header], border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)

// do not show result name (OR, 95%CI, P)
collect style header result, level(hide)

// create space between columns
collect style column, extraspace(3)

// stack levels of factor vars
collect style row stack, spacer

// do not show baseline levels
collect style showbase off

// renames names in the first column
collect recode colname GNDISEASE = "Pre-existing GN"
collect recode colname SD_eGFR_pre = "Prior eGFR (per 1 SD unit decrease)"
collect recode colname SD_age = "Age (per 1 SD unit increase)"
collect recode colname GENDER = "Gender"
collect recode colname NON_WHITE = "Non-White Ethnicity"
collect recode colname ACEI = "RAASi use"
collect recode colname _cons = "Intercept"

// specify the three group headers (group- and sub- headings)
collect style header AKI RRT Death, title(name) level(value)

// associate significance stars
collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*", attach(_r_b)

// show variables but not the constant term
collect layout ///
(colname["Pre-existing GN" "Prior eGFR (per 1 SD unit decrease)" ///
"Age (per 1 SD unit increase)" "Gender" "Non-White Ethnicity" ///
"RAASi use"]#result) (AKI RRT Death)

// for repeating column headers, display them once and center them
collect style column, dups(center)

collect preview
collect export Table_All_ORs_CI_p_3models.html, replace
collect export Table_All_ORs_CI_p_3models.docx, replace



////////////////////////////////////////////////////////////////////////////////
**# END TABLE 3 (main) Effect of GN on clinical outcomes AKI RRT DEath
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# START FIGURE 1 PROBABILITY OF AKI GIVEN PRIOR eGFR
////////////////////////////////////////////////////////////////////////////////


use all_vars_iroc_gn_creat_wide, clear

logistic AKI_YESNO age GENDER NON_WHITE ACEI ///
	eGFR_pre ///
	if  HOSPITALIZED == 2 & GNDISEASE == 2


lincom eGFR_pre* - 30, or cformat(%3.2f) pformat(%4.3f) sformat(%3.2f)
return list
local or = r(estimate)
local ub = r(ub)
local lb = r(lb)
local pval = r(p)
local sor  = string( `or', "%3.2f")
local sub  = string( `ub', "%3.2f")
local slb  = string( `lb', "%3.2f")
local spval = string( `pval', "%3.2f")

margins, at(eGFR = (10(5)100))
marginsplot, xdimension(eGFR_pre) ///
 recastci(rarea) ///
 ytitle("Adjusted Probability of AKI (%)") ///
 ylabel(0 "0.0" 0.1 "10" 0.2 "20" 0.3 "30" 0.4 "40" 0.5 "50" 0.6 "60" 0.7 "70" 0.8 "80" 0.9 "90" 1 "100", grid angle(horizontal)) ///
 xlabel(10(10)100, grid) ///
 ysc(titlegap(2)) ///
 xtitle("Pre-COVID eGFR (mL/min/1.73m{sup:2})") ///
 xsc(titlegap(3)) ///
  aspectratio(1) ///
  plotopts(lcolor(navy%30)  lwidth(*1.2)  lpattern(solid)  msymbol(i)  clcolor(navy)) ///
  ciopts(color(navy%30)) ///
  title(" ") ///
  scheme(s1mono)
  graph export aki_probability.png, replace
  
  * note("estimated only in GN-Hospitalized patients; OR per 15ml/min/1.73m2 eGFR:" `sor' "(95%CI: "`slb' " to "`sub'";P= " `spval'")", size(*0.7)) ///
  
 ////////////////////////////////////////////////////////////////////////////////
**# END FIGURE 1 PROBABILITY OF AKI GIVEN PRIOR eGFR
////////////////////////////////////////////////////////////////////////////////


  
////////////////////////////////////////////////////////////////////////////////
**#  START Effect (main) of GN on kidney function recovery (categorical)
////////////////////////////////////////////////////////////////////////////////


use long_with_time, clear

tab TIME


drop _merge
drop base_uprot_
drop base_alb_
drop dup

* keep recordid month TIME eGFR cr alb_ uprot_ HOSPITALIZED GNDISEASE
reshape wide month eGFR cr alb_ uprot_  base_eGFR t, i(recordid) j(TIME)
cap drop rmiss
egen rnomiss =  rownonmiss(eGFR*)

foreach num of numlist 4 5 6 7  {
			cap gen _delta_eGFR_`num' = (eGFR1 - eGFR`num') / eGFR1 * 100
			cap gen byte _recovered_`num' = _delta_eGFR_`num' > -10 & !missing(_delta_eGFR_`num')
			 }
gen RECOVERED = cond( (_recovered_4 == 1 | _recovered_5 == 1 | _recovered_6 == 1 | _recovered_7 == 1), 1, 0)
replace RECOVERED = . if (_recovered_4 == . & _recovered_5 == . & _recovered_6 == . & _recovered_7 == .)

tab GNDISEASE RECOVERED, row exact
tab GNDISEASE RECOVERED if HOSPITALIZED == 2, row exact



cap drop SD_eGFR1
qui summ eGFR1
gen SD_eGFR1  = - (eGFR1 - 0) / r(sd)

cap drop SD_age
qui summ age
gen SD_age  = (age - 0) / r(sd)


cap drop SD_alb_1
qui summ alb_1
gen SD_alb_1  = - (alb_1 - 0) / r(sd)


cap drop SD_uprot_1
qui summ uprot_1
gen SD_uprot_1  = (uprot_1 - 0) / r(sd)

collect clear
collect style clear
collect style use default
cap collect drop Recovery
cap collect create Recovery, replace

collect _r_b _r_ci _r_p, name(Recovery) tag(GNvsCTRL["crude"]): /// 
	logistic RECOVERED GNDISEASE if HOSPITALIZED == 2
collect _r_b _r_ci _r_p, name(Recovery) tag(GNvsCTRL["eGFR-adj."]): /// 
	logistic RECOVERED GNDISEASE SD_eGFR1 if HOSPITALIZED == 2
collect _r_b _r_ci _r_p, name(Recovery) tag(GNvsCTRL["fully-adj."]): ///
	logistic RECOVERED GNDISEASE SD_eGFR1 SD_age GENDER NON_WHITE ACEI if HOSPITALIZED == 2

collect _r_b _r_ci _r_p, name(Recovery) tag(GNonly["eGFR"]): ///
logistic RECOVERED SD_eGFR1 if GNDISEASE == 2
collect _r_b _r_ci _r_p, name(Recovery) tag(GNonly["full model"]): ///
logistic RECOVERED SD_eGFR1 SD_age GENDER NON_WHITE ACEI HOSPITALIZED if GNDISEASE == 2

 // define number of digits
collect style cell result[_r_b], nformat(%3.2f)
collect style cell result[_r_ci], nformat(%3.2f) sformat("[%s]") cidelimiter(,)
collect style cell result[_p], nformat(%4.3f)

// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header
collect style cell cell_type[column-header], border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)

// do not show result name (OR, 95%CI, P)
collect style header result, level(hide)

// create space between columns
collect style column, extraspace(3)

// stack levels of factor vars
collect style row stack, spacer

// do not show baseline levels
collect style showbase off

// renames names in the first column
// renames names in the first column
collect recode colname GNDISEASE = "Pre-existing GN"
collect recode colname SD_eGFR1 = "Prior eGFR (per 1 SD unit decrease)"
collect recode colname SD_age = "Age (per 1 SD unit increase)"
collect recode colname GENDER = "Gender"
collect recode colname NON_WHITE = "Non-White Ethnicity"
collect recode colname ACEI = "RAASi use"
collect recode colname _cons = "Intercept"

// specify the three group headers (group- and sub- headings)
collect style header GNvsCTRL GNonly, title(name) level(value)

// associate significance stars
collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*", attach(_r_b)

// show variables but not the constant term
collect layout ///
(colname["Pre-existing GN" "Prior eGFR (per 1 SD unit decrease)" ///
"Age (per 1 SD unit increase)" "Gender" "Non-White Ethnicity" ///
"RAASi use"]#result) (GNvsCTRL GNonly)

// for repeating column headers, display them once and center them
collect style column, dups(center)

collect preview

collect export recovery_base_model_ORs.html, replace
collect export recovery_base_model_ORs.docx, replace

////////////////////////////////////////////////////////////////////////////////
**#  END Effect (main) of GN on kidney function recovery (categorical)
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# START Effect of GN disease on kidney function recovery (eGFR - mixed model)
////////////////////////////////////////////////////////////////////////////////


*-------------------------------------------------------------------------------
**# START GOF OF THE MIXED LONGITUDINAL MODEL
*-------------------------------------------------------------------------------

use long_with_time, clear
cap drop _no_allgfrmiss
qui mixed eGFR GNDISEASE##(c.month)##AKI_YESNO##c.base_eGFR  ///
c.age i.GENDER i.NON_WHITE i.ACEI ///
if (HOSPITALIZED == 2)  ///
|| recordid: , reml cov(unstr) dfmethod(kroger)

cap drop  yhat res rst
predict  yhat, fitted
predict res, residuals
predict rst, rstandard 
hist res, scheme(s1mono)  ///
note("Residuals should have a normal distributon") 
graph export check_plot_res.png, replace
pnorm res, scheme(s1mono) aspectratio(1) ///
	ylabel(, angle(horizontal) grid) xlabel(, grid) ///
	msymbol(o) mfcolor(white) ///
	note("residuals must overaly along the diagonal line") 
graph export check_normplot_res.png, replace	
	
scatter res yhat, yline(0) msymbol(o) mfcolor(white) ///
	ytitle("Residuals (diff. between predicted and observed eGFR)") ///
	xtitle("Fitted eGFR") ///
    ylabel(, angle(horizontal)) ///
	note("The scatter should form  a uniform strip symmetrical w.r.t the horzontal line") ///
	scheme(s1mono)
graph export check_plot_res_vs_fit.png, replace
tw scatter yhat eGFR, msymbol(o) mfcolor(white)  || ///
   line eGFR eGFR, lpattern(dash) ||,  ///
   aspectratio(1)  ///
   ytitle("Fiited eGFR") ///
   xtitle("Observed eGFR") ///
   ylabel(, angle(horizontal) grid) ///
   xlabel(, grid) ///
   legend(off) ///
   note("Circles should lay close the diagonal line," "the line resembling a regression line") ///
    scheme(s1mono) 
graph export check_plot_fit_vs_obs.png, replace




//  start visualization of data available for longitudinal analyses


local n_pts = el(e(N_g),1,1)
local n_egfr = e(N)
local t_avg = el(e(g_avg),1,1)
local st_avg = string(`t_avg', "%3.1f")
local t_min = el(e(g_min),1,1)
local t_max = el(e(g_max),1,1)

di "No of pts analyzed: `n_pts'; total eGFR measurements: `n_egfr' (average `st_avg' (`t_min' - `t_max') per pt)"

version 16.0
table TIME GNDISEASE if e(sample), c(n eGFR)
table GNDISEASE if e(sample)
version 17.0

cap drop _used
gen byte _used = (e(sample) == 1)

iis recordid 
tis month
preserve
keep if (HOSPITALIZED == 2 & _used == 1)
xtline eGFR,   ///
	connect(L) ///
	ytitle(eGFR (mL/min/1.73m{sup:2})) ///
	ylabel(30 120, angle(horizontal) grid) ///
	xlabel(-12(6)12) ///
	xtitle("Month Since COVID Diagnosis") ///
	byopts(title("Data Used For Longitudinal Analyses", size(*0.7))  note("No of pts analyzed: `n_pts'; total eGFR measurements: `n_egfr' (average `st_avg' (`t_min' - `t_max') per pt)", size(*0.8))) ///
	addplot(scatter eGFR month if GNDISEASE == 1 & DEATH == 1, msymbol(o) mcolor(navy)  || ///
	        scatter eGFR month if GNDISEASE == 2 & DEATH == 1, msymbol(o) mcolor(maroon) || ///
			scatter eGFR month if GNDISEASE == 1 & DEATH == 2, msymbol(X) mcolor(navy)  || ///
	        scatter eGFR month if GNDISEASE == 2 & DEATH == 2, msymbol(X) mcolor(maroon) ///
			)  ///
	legend(rows(1) order(1 "Connected" 2 "Ctrl Alive" 3 "GN Alive" 4 "Ctrl Dead" 5 "GN Dead")) ///
	scheme(s1mono)
graph export visualize_data_used_for_long_anal_death.pdf, replace


xtline eGFR,   ///
	connect(L) ///
	ytitle(eGFR (mL/min/1.73m{sup:2})) ///
	ylabel(30 120, angle(horizontal) grid) ///
	xlabel(-12(6)12) ///
	xtitle("Month Since COVID Diagnosis") ///
	byopts(title("Data Used For Longitudinal Analyses", size(*0.7))  note("No of pts analyzed: `n_pts'; total eGFR measurements: `n_egfr' (average `st_avg' (`t_min' - `t_max') per pt)", size(*0.8))) ///
	addplot(scatter eGFR month if GNDISEASE == 1 & RRT == 1, msymbol(o) mcolor(navy)  || ///
	        scatter eGFR month if GNDISEASE == 2 & RRT == 1, msymbol(o) mcolor(maroon) || ///
			scatter eGFR month if GNDISEASE == 1 & RRT == 2, msymbol(+) mcolor(navy)  || ///
	        scatter eGFR month if GNDISEASE == 2 & RRT == 2, msymbol(+) mcolor(maroon) ///
			)  ///
	legend(rows(1) order(1 "Connected" 2 "Ctrl" 3 "GN" 4 "Ctrl RRT" 5 "GN RRT")) ///
	scheme(s1mono)
graph export visualize_data_used_for_long_anal_rrt.pdf, replace


xtline eGFR,   ///
	connect(L) ///
	ytitle(eGFR (mL/min/1.73m{sup:2})) ///
	ylabel(30 120, angle(horizontal) grid) ///
	xlabel(-12(6)12) ///
	xtitle("Month Since COVID Diagnosis") ///
	byopts(title("Data Used For Longitudinal Analyses", size(*0.7))  note("No of pts analyzed: `n_pts'; total eGFR measurements: `n_egfr' (average `st_avg' (`t_min' - `t_max') per pt)", size(*0.8))) ///
	addplot(scatter eGFR month if GNDISEASE == 1 & RRT == 1, msymbol(o) mcolor(navy)  || ///
	        scatter eGFR month if GNDISEASE == 2 & RRT == 1, msymbol(o) mcolor(maroon) || ///
			scatter eGFR month if GNDISEASE == 1 & RRT == 2, msymbol(+) mcolor(navy)  || ///
	        scatter eGFR month if GNDISEASE == 2 & RRT == 2, msymbol(+) mcolor(maroon) || ///
			line yhat month, lcolor(red) lwidth(*1.5) lpattern(dash) ///
			)  ///
	legend(rows(1) order(1 "Connected" 2 "Ctrl" 3 "GN" 4 "Ctrl RRT" 5 "GN RRT" 6 "fitted eGFR")) ///
	scheme(s1mono)
graph export visualize_data_used_and_fiited_for_long_anal.pdf, replace
graph export visualize_data_used_and_fiited_for_long_anal.png, replace

restore


//  end visualization of data available for longitudinal analyses


*-------------------------------------------------------------------------------
**# END GOF MIXED LONGITUDINAL MODEL
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
**# START FIGURE 2 AND ASSOCIATED P VALUES
*-------------------------------------------------------------------------------

 use long_with_time, clear
* Adjusted Hospitalized patients not developing AKI
mixed eGFR GNDISEASE##(c.month)##AKI_YESNO##c.base_eGFR  ///
c.age i.GENDER  i.NON_WHITE i.ACEI  ///
if (HOSPITALIZED == 2)  ///
|| recordid: , reml cov(unstr) dfmethod(kroger)



test _b[1.AKI_YESNO#c.base_eGFR] = 0, small
test _b[2.GNDISEASE#1.AKI_YESNO#c.base_eGFR] = 0, small
test _b[1.AKI_YESNO#c.month#c.base_eGFR] = 0, small
// four-way interaction term and P value 
test _b[2.GNDISEASE#1.AKI_YESNO#c.month#c.base_eGFR] = 0, 
local pval = r(p)
local spval  = string( `pval', "%4.3f")


* Start longitudinal predicted curves for selected levels of pre-COVID eGFR
* (Supplementary Figure S1)
margins, at(month = (0(1)6) base_eGFR = (22.5 45 75)  AKI_YESNO = (0 1) GNDISEASE = (1 2))
marginsplot, xdimension(month) ///
 by(GNDISEASE)  graphdimension(base_eGFR, ///
 labels("Starting eGFR: 22.5 mL/min/1.73m{sup:2} (Stage 4 CKD)" ///
 "Starting eGFR: 45 mL/min/1.73m{sup:2} (Stage 3 CKD)" ///
 "Starting eGFR: 75 mL/min/1.73m{sup:2} (Stage 2 CKD)")) ///
 byopts( title("Pre-Existing Glomerular Disease")) ///
 recastci(rarea) ///
 ytitle("Adjusted eGFR (mL/min/1.73m{sup:2})") ///
 ylabel(10(10)120, grid angle(horizontal)) ///
 xlabel(0(1)6) ///
 xtitle("Month From COVID-19 Diagnosis") ///
 legend(order (3 "AKI = No" 4 "AKI = Yes")) ///
 aspectratio(1) ///
  plot1opts(lcolor(navy%30)  lwidth(*1.2)  lpattern(solid)  msymbol(i)  clcolor(navy)) ///
  plot2opts(lcolor(maroon%30) lwidth(*1.2)  lpattern(solid) msymbol(i) clcolor(maroon)) ///
  ci1opts(color(navy%30)) ///
  ci2opts(color(maroon%30)) ///
  scheme(s1mono) saving(_mpl, replace)
  graph use _mpl1.gph
  graph export _mpl1.png, as(png) replace
  graph use _mpl2.gph
  graph export _mpl2.png, as(png) replace
  graph use _mpl3.gph
  graph export _mpl3.png, as(png) replace
 * End Supplementary Figure S1


   
*-------------------------------------------------------------------------------
**# Start FIGURE 2: cross-sectional 6-mo evaluation based on the fitted model 
*-------------------------------------------------------------------------------

   use long_with_time, clear
      
mixed eGFR GNDISEASE##(c.month)##AKI_YESNO##c.base_eGFR  ///
c.age i.GENDER i.NON_WHITE i.ACEI  ///
if (HOSPITALIZED == 2)  ///
|| recordid: , reml cov(unstr) dfmethod(kroger)

 margins, at(base_eGFR = (30(10)90)  AKI_YESNO = (0 1) GNDISEASE = (1 2) month = 6)  ///
 saving(margins_6meGFR_adjusted, replace)
 preserve
 use margins_6meGFR_adjusted,clear

 sort _at1 _margin _at4
 range x 30 90 14
 replace x = 30 in 15
 replace x = 90 in 28
 
 
 twoway ///
 line _margin _at4 if _at1==1 & _at3==1, lcolor(maroon)  ||  ///
 line _margin _at4 if _at1==2 & _at3==1, lcolor(maroon)   ||  ///
 line _margin _at4 if _at1==1 & _at3==0, lcolor(navy) || ///
 line _margin _at4 if _at1==2 & _at3==0, lcolor(navy) sort || ///
 rarea _ci_ub _ci_lb _at4 if _at1==1 & _at3==1, color(maroon%30) || ///
 rarea _ci_ub _ci_lb _at4 if _at1==2 & _at3==1,  color(maroon%30) || ///
 rarea _ci_ub _ci_lb _at4 if _at1==1 & _at3==0, color(navy%30) || ///
 rarea _ci_ub _ci_lb _at4 if _at1==2 & _at3==0,   color(navy%30) sort || ///
 line x x, lpattern(dash) lwidth(1.5) lcolor(black%30) || ///
 , by(_at1, note("Hospitalized patients, adjusted analysis") title("Pre-Existing Glomerular Disease")) ///
  ytitle("6-month post-COVID eGFR (mL/min/1.73m{sup:2})") ///
 ylabel(10(10)120, grid angle(horizontal)) ///
 xlabel(30(10)90, grid) ///
 xtitle("Pre-COVID eGFR (mL/min/1.73m{sup:2})") ///
 legend(order (3 "AKI = No" 1 "AKI = Yes")) ///
 aspectratio(1) ///
  scheme(s1mono)
   graph export gndisease_aki_gnprecovid-6_month_ident_line_interaction.png, replace
 restore
 
*-------------------------------------------------------------------------------
**# END FIGURE 2: cross-sectional 6-mo evaluation based on the fitted model 
*------------------------------------------------------------------------------- 

*-----------------------------------------------------------------------------
**# Start Table 5 pairwise testing of beta coefficent eGFR pre vs eGFR post at 6 months
*-----------------------------------------------------------------------------
  

  use long_with_time, clear 
  summ month if month > 0, detail
 
 
	

  mixed eGFR GNDISEASE##(c.month)##c.base_eGFR  ///
	c.age i.GENDER i.NON_WHITE i.ACEI  ///
	if (HOSPITALIZED == 2)  ///
	|| recordid: , reml cov(unstr) dfmethod(kroger)
  margins, at(GNDISEASE = (1 2) month = 6) ///
	dydx(base_eGFR) pwcompare(pveffects groups) mcompare(bonferroni) ///
   cformat(%3.2f) pformat(%4.3f) sformat(%3.2f) 
   
	collect style clear
	collect create pre_vs_post_eGFR, replace
	collect get r(table), name(pre_vs_post_eGFR) tag(model[(1)])
	* collect get r(table_vs), name(pre_vs_post_eGFR) tag(model[(comp)])
	collect label list result , name(pre_vs_post_eGFR) all
	collect layout (result[_r_b _r_ci _r_p]) (colname)
	collect label levels result _r_b "Coeff. pre- vs 6months post-COVID eGFR", modify 
	collect label levels result at "Group", modify 
	collect label values _at 1 "Ctrl" 2 "GN Disease", modify
	collect style cell result[_r_b _r_se _r_ci], nformat(%8.2f)
	collect style cell result[_r_p], nformat(%4.3f) 
	collect style cell result[_r_ci], sformat("[%s]") ///  
                   cidelimiter(,)
	collect style cell border_block[corner row-header],   ///
                   border(right, pattern(nil)) nowarn
	collect style cell border_block,                     ///
                   border(right, pattern(nil))
	collect style cell cell_type[item column-header], halign(center)
	collect style column, extraspace(3)
    collect style header result[at], level(label)
	*collect style header, level(label) title(hide)
	collect style column, dups(center)
	collect preview
	collect export _rho_6mo_2grp.html, replace
  
 

  
  mixed eGFR GNDISEASE##(c.month)##AKI_YESNO##c.base_eGFR  ///
	c.age i.GENDER i.NON_WHITE i.ACEI  ///
	if (HOSPITALIZED == 2)  ///
	|| recordid: , reml cov(unstr) dfmethod(kroger)
  margins, at(AKI_YESNO = (0 1) GNDISEASE = (1 2) month = 6) ///
	dydx(base_eGFR) pwcompare(pveffects groups) mcompare(bonferroni) ///
   cformat(%3.2f) pformat(%4.3f) sformat(%3.2f) 
   
   	collect style clear
	collect create pre_vs_post_eGFR, replace
	collect get r(table), name(pre_vs_post_eGFR) tag(model[(2)])
	* collect get r(table_vs), name(pre_vs_post_eGFR) tag(model[(comp)])
	collect label list result , name(pre_vs_post_eGFR) all
	collect layout (result[_r_b _r_ci _r_p]) (colname)
	collect label levels result _r_b "Coeff. pre- vs 6months post-COVID eGFR", modify 
	collect label levels result at "Group", modify 
	collect label values _at 1 "Ctrl AKI:no" 2 "GN AKI:no" 3 "Ctrl AKI:yes" 4 "GN AKI:yes", modify
	collect style cell result[_r_b _r_se _r_ci], nformat(%8.2f)
	collect style cell result[_r_p], nformat(%4.3f) 
	collect style cell result[_r_ci], sformat("[%s]") ///  
                   cidelimiter(,)
	collect style cell border_block[corner row-header],   ///
                   border(right, pattern(nil)) nowarn
	collect style cell border_block,                     ///
                   border(right, pattern(nil))
	collect style cell cell_type[item column-header], halign(center)
	collect style column, extraspace(3)
    collect style header result[at], level(label)
	*collect style header, level(label) title(hide)
	collect style column, dups(center)
	collect preview
	collect export _rho_6mo_4grp.html, replace
	collect export _rho_6mo_4grp.docx, replace
	
*-----------------------------------------------------------------------------
**# End Table 5 pairwise testing of beta coefficent eGFR pre vs eGFR post at 6 months
*-----------------------------------------------------------------------------



////////////////////////////////////////////////////////////////////////////////
**# END Effect of GN disease on kidney function recovery (eGFR - mixed model)
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
**# Start Analysis on the effect of GN  on POST-COVID GFR
///////////////////////////////////////////////////////////////////////////////





  use long_with_time, clear 
 
	

/*	
cap drop SD_eGFR1
qui summ eGFR1
gen SD_eGFR1  = - (eGFR1 - 0) / r(sd)
*/

cap drop SD_age
qui summ age
gen SD_age  = (age - 0) / r(sd)


cap drop SD_base_alb_
qui summ base_alb_ if TIME == 1
gen SD_base_alb_  = - (base_alb_ - 0) / r(sd)


cap drop SD_base_uprot_
qui summ base_uprot_ if TIME == 1
gen SD_base_uprot_  = (base_uprot_ - 0) / r(sd)


cap drop SLE_recoded
recode SLE (1 = 2) (2 = 1), gen(SLE_recoded)



collect clear
collect style clear
collect style use default
cap collect drop BaseDet_eGFR
cap collect create BaseDet_eGFR, replace

foreach var of varlist  SD_age GENDER NON_WHITE ACEI   {
collect _r_b _r_ci _r_p, name(BaseDet_eGFR) tag(HOSPITALIZED["Base Model"]): /// 
		qui mixed eGFR `var' ///
		c.month##GNDISEASE#AKI_YESNO##c.base_eGFR ///
		if (HOSPITALIZED == 2)  ///
		|| recordid: , reml cov(unstr) dfmethod(kroger)
		
collect _r_b _r_ci _r_p, name(BaseDet_eGFR) tag(GNDISEASE["Base Model"]): /// 
		qui mixed eGFR `var' ///
		c.month##AKI_YESNO##c.base_eGFR i.HOSPITALIZED ///
		if (GNDISEASE == 2)  ///
		|| recordid: , reml cov(unstr) dfmethod(kroger)		
	}
	
collect _r_b _r_ci _r_p, name(BaseDet_eGFR) tag(HOSPITALIZED["Fully-adj."]): /// 
		qui mixed eGFR `var' ///
		c.month##GNDISEASE#AKI_YESNO##c.base_eGFR ///
		SD_age GENDER NON_WHITE ACEI ///
		if (HOSPITALIZED == 2)  ///
		|| recordid: , reml cov(unstr) dfmethod(kroger)
		
collect _r_b _r_ci _r_p, name(BaseDet_eGFR) tag(GNDISEASE["Fully-adj"]): /// 
		qui mixed eGFR `var' ///
		c.month##AKI_YESNO##c.base_eGFR i.HOSPITALIZED ///
		SD_age GENDER NON_WHITE ACEI ///
		if (GNDISEASE == 2)  ///
		|| recordid: , reml cov(unstr) dfmethod(kroger)	
		

 // define number of digits
collect style cell result[_r_b], nformat(%3.1f)
collect style cell result[_r_ci], nformat(%3.1f) sformat("[%s]") cidelimiter(,)
collect style cell result[_p], nformat(%4.3f)

// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header
collect style cell cell_type[column-header], border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)

// do not show result name (OR, 95%CI, P)
collect style header result, level(hide)

// create space between columns
collect style column, extraspace(3)

// stack levels of factor vars
collect style row stack, spacer

// do not show baseline levels
collect style showbase off

// renames names in the first column
// renames names in the first column
collect recode colname SD_age = "Age (per 1 SD unit increase)"
collect recode colname GENDER = "Gender"
collect recode colname NON_WHITE = "Non-White Ethnicity"
collect recode colname ACEI = "RAASi use"
collect recode colname _cons = "Intercept"

// specify the three group headers (group- and sub- headings)
collect style header HOSPITALIZED GNDISEASE, title(name) level(value)

// associate significance stars
collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*", attach(_r_b)

// show variables but not the constant term
collect layout ///
(colname[ ///
"Age (per 1 SD unit increase)" "Gender" "Non-White Ethnicity" ///
"RAASi use"]#result) (HOSPITALIZED GNDISEASE)

// for repeating column headers, display them once and center them
collect style column, dups(center)

collect preview

collect export _basedet_long_average_eGFR.html, replace

////////////////////////////////////////////////////////////////////////////////
**# End Analysis on the effect of GN  on POST-COVID GFR
///////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# START SUPPL. TABLE S2 (Table 3 additional) Effect of GN on clinical outcomes
////////////////////////////////////////////////////////////////////////////////



use all_vars_iroc_gn_creat_wide, clear
collect clear
collect style clear
collect style use default

keep if GNDISEASE == 2
keep if !missing(eGFR_pre)
tab GNDISEASE


rename serumalbuminpriortoinfection alb_1
rename proteinuriaquantifiedpriortocovi  uprot_1

cap drop SD_eGFR_pre
qui summ eGFR_pre
gen SD_eGFR_pre  = - (eGFR_pre - 0) / r(sd)

cap drop SD_age
qui summ age
gen SD_age  = (age - 0) / r(sd)


cap drop SD_alb_1
qui summ alb_1
gen SD_alb_1  = - (alb_1 - 0) / r(sd)


cap drop SD_uprot_1
qui summ uprot_1
gen SD_uprot_1  = (uprot_1 - 0) / r(sd)


cap drop SLE_recoded
recode SLE (1 = 2) (2 = 1), gen(SLE_recoded)


collect style clear
cap collect drop Det_Outcome
cap collect create Det_Outcome, replace
foreach var of varlist SD_alb_1 SD_uprot_1  ///
		AZATHIOPRINE MYCOPHENOLATE RITUXIMAB STEROIDS ///
		SLE_recoded IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE DURATION_GN  {
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(AKI["crude"]): ///
        qui logistic AKI_YESNO `var' HOSPITALIZED
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(AKI["eGFR-adj"]): ///
        qui logistic AKI_YESNO  `var' HOSPITALIZED SD_eGFR_pre
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(AKI["fully-adj"]): ///
        qui logistic AKI_YESNO `var' HOSPITALIZED SD_eGFR_pre SD_age GENDER NON_WHITE ACEI
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(RRT["crude"]): ///
        qui logistic RRT_YESNO `var' HOSPITALIZED
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(RRT["eGFR-adj"]): ///
        qui logistic RRT_YESNO `var' HOSPITALIZED SD_eGFR_pre
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(RRT["fully-adj"]): ///
        qui logistic RRT_YESNO `var' HOSPITALIZED SD_eGFR_pre SD_age GENDER NON_WHITE ACEI
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(Death["crude"]): ///
        qui logistic DEATH_YESNO `var' HOSPITALIZED
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(Death["eGFR-adj"]): ///
        qui logistic DEATH_YESNO  `var' HOSPITALIZED SD_eGFR_pre
collect _r_b _r_ci _r_p, name(Det_Outcome) tag(Death["fully-adj"]): ///
        qui logistic DEATH_YESNO `var' HOSPITALIZED SD_eGFR_pre SD_age GENDER NON_WHITE ACEI
 }
                               
// define number of digits
collect style cell result[_r_b], nformat(%3.2f)
collect style cell result[_r_ci], nformat(%3.2f) sformat("[%s]") cidelimiter(,)
collect style cell result[_p], nformat(%4.3f)

// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header
collect style cell cell_type[column-header], border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)

// do not show result name (OR, 95%CI, P)
collect style header result, level(hide)

// create space between columns
collect style column, extraspace(3)

// stack levels of factor vars
collect style row stack, spacer

// do not show baseline levels
collect style showbase off

// renames names in the first column
collect recode colname SD_alb_1 = "Serum albumin, g/dL (per 1 SD unit decrease)"
collect recode colname SD_uprot_1 = "Urinary Protein, g/day (per 1 SD unit increase)"
collect recode colname AZATHIOPRINE = "Azathioprine"
collect recode colname MYCOPHENOLATE = "Mycophenolate" 
collect recode colname RITUXIMAB = "Rituximab"
collect recode colname STEROIDS = "Steroids"
collect recode colname DURATION_GN = "Duration of GN (trend across categories)"
collect recode colname SLE_recoded = "SLE history"
collect recode colname GN_SLE = "SLE GN diagnosis"
collect recode colname VASCULITIS = "Vasculitis"
collect recode colname EITHER_VASC_SLE  = "SLE GN or Vasculitis" 
collect recode colname IgAN_HSP = "IgA nephropathy" 
collect recode colname FSGS_MCD = "FSGS or MCD"
collect recode colname MN = "Membranous nephropathy"
collect recode colname GNDISEASE = "Pre-existing GN"
collect recode colname SD_eGFR_pre = "Prior eGFR (per 1 SD unit decrease)"
collect recode colname SD_age = "Age (per 1 SD unit increase)"
collect recode colname GENDER = "Gender"
collect recode colname NON_WHITE = "Non-White Ethnicity"
collect recode colname ACEI = "RAASi use"
collect recode colname _cons = "Intercept"

// specify the three group headers (group- and sub- headings)
collect style header AKI RRT Death, title(name) level(value)

// associate significance stars
collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*", attach(_r_b)

// show variables but not the constant term
collect layout ///
(colname["Serum albumin, g/dL (per 1 SD unit decrease)" "Urinary Protein, g/day (per 1 SD unit increase)" "Azathioprine" "Mycophenolate" "Rituximab" "Steroids" "Duration of GN (trend across categories)" "SLE history" "SLE GN diagnosis" "Vasculitis" "SLE GN or Vasculitis" "IgA nephropathy" "FSGS or MCD" "Membranous nephropathy" ]#result) (AKI RRT Death)

// for repeating column headers, display them once and center them
collect style column, dups(center)

collect preview

collect export Table_All_Det_ORs_CI_p_3models.html, replace
collect export Table_All_Det_ORs_CI_p_3models.docx, replace


////////////////////////////////////////////////////////////////////////////////
**# END SUPPL. TABLE S2 (Table 3 additional) Effect of GN on clinical outcomes
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
**# START TABLE 6 LONGITUDINAL VAR WIDE FORMAT
////////////////////////////////////////////////////////////////////////////////



use long_with_time, clear

preserve
keep if GNDISEASE == 2
keep recordid month TIME eGFR cr alb_ uprot_ HOSPITALIZED
reshape wide month eGFR cr alb_ uprot_ , i(recordid) j(TIME)
cap drop rnomiss
egen rnomiss =  rownonmiss(eGFR*)
keep if !missing(eGFR1) & rnomiss > = 2 & !missing(rnomiss)
reshape long month eGFR cr alb_ uprot_, i(recordid)
rename _j TIME
save _long_with_time_at_least_1post, replace
restore

preserve
keep if GNDISEASE == 2
keep recordid month TIME eGFR cr alb_ uprot_ HOSPITALIZED
reshape wide month eGFR cr alb_ uprot_ , i(recordid) j(TIME)
cap drop rmiss
egen rnomiss =  rownonmiss(eGFR*)
keep if !missing(eGFR1) & !missing(eGFR2) & rnomiss > = 3 & !missing(rnomiss)
reshape long month eGFR cr alb_ uprot_, i(recordid)
rename _j TIME
save _long_with_time_at_least_2pre_1post, replace
restore

clear all

cap program drop _widetable
program define _widetable
version 17.0
table  (HOSPITALIZED) (TIME) (), nototals ///
								 stat(median month) stat(min month) stat(max month) ///
								 stat(count eGFR cr alb_ uprot_) stat(mean eGFR cr alb_ uprot_) stat(sd eGFR cr alb_ uprot_) ///
								 nformat(%3.1f median) ///
								 nformat(%3.1f min max) ///
								 nformat(%3.0f count) ///
								 nformat(%3.1f mean sd)
		
		
// don't show stat description 
collect style header result, level(hide)
collect preview

// change labels
collect label list var, all
collect label levels var eGFR "eGFR, ml/min/1.73m2", modify
collect label levels var cr "Serum creatinine, mg/dl", modify
collect label levels var alb_ "Serum albumin, g/dl", modify
collect label levels var uprot_ "Urinary protein, g/day", modify
collect label levels var month "Month since COVID diagnosis", modify
collect label dim TIME "Survey Time Points", modify
collect label levels TIME 1 "Pre-COVID" 2 "At admission" 3 "During COVID" 4 "After COVID" 5 "Second F-Up" 6 "Third F-UP" 7 "Most Recent", modify
// Change the label of HOSPITALIZED
collect label dim HOSPITALIZED "Admission Status", modify
// Change the label of values HOSPITALIZED
collect label levels HOSPITALIZED 1 "Outpatients" 2 "Hospitalized", modify

// do not display vertical lines and draw horzontal line below the title
collect style cell cell_type, border(right, pattern(nil))
collect style cell cell_type[column-header]#TIME, border(bottom, pattern(single))


// put frequency and percent in the same column as mean and sd, respectively
* collect recode result median = count min = mean max = sd
collect recode result median = column1 ///
                      count  = column1 ///
					  min   = column2  ///
					  mean  = column2  ///
					  max   = column3 ///
					  sd    = column3

collect layout (HOSPITALIZED[1 2]#var) (TIME#result[column1 column2 column3])

// display SD within brackets
collect style cell result[column3]#var[eGFR cr alb_ uprot_], sformat("(%s)")
// display min-max with square brackets, separated by comma: [min,max]
collect style cell result[column2]#var[month],  cidelimiter(,) sformat("[%s,")
collect style cell result[column3]#var[month],  cidelimiter(,) sformat("%s]")

collect style cell var[eGFR cr alb_ uprot_]#result[column1], nformat(%3.0f)
collect style cell var[month]#result[column1], nformat(%3.1f)
collect style cell var[eGFR cr alb_ uprot_]#result[column2], nformat(%3.1f)
collect style cell var[month]#result[column2], nformat(%3.1f)
collect style cell var[eGFR cr alb_ uprot_]#result[column3], nformat(%3.1f)
collect style cell var[month]#result[column3], nformat(%3.1f)
end


use _long_with_time_at_least_1post, clear
_widetable
collect export _multiple_wide_var_pval.txt, replace


capture program drop _stattest
program define _stattest
version 17.0
label var eGFR "eGFR (ml/min/1.73m2)"
label var cr "Serum creatinine (mg/dl)"
label var alb_ "Serum albumin (g/dl)"
label var uprot_ "Urinary protein (g/day)"

keep recordid TIME eGFR cr alb_ uprot_ HOSPITALIZED
reshape wide eGFR cr alb_ uprot_ , i(recordid) j(TIME)

local timepoints "Pre_COVID At_admission During_COVID After_COVID Second_FUp Third_F_UP Most_Recent"
local n : word count `timepoint'
local varOrder
foreach name in eGFR cr alb_ uprot_ {
	foreach num of numlist 2 3 4 5 6 7	 {
		foreach status of numlist  1 2		 {
		qui cap signrank `name'1 = `name'`num' if HOSPITALIZED  == `status'
		local t: word `num' of `timepoints'
		di _newline(3) in ye "----> Hospitalized = `: label (HOSPITALIZED) `status'';  Variable = `name' , `t' vs Pre-COVID: P value ="  %4.3f r(p)
		collect get column2=(r(p)), ///
			tag(TIME[`num'] HOSPITALIZED[`status'] var[`name'1])
			}
	 }	
	 local varOrder `varOrder' `name' `name'1
	 local varHide `varHide' `name'1
 }
	collect style autolevels var month `varOrder', clear
	collect style header style var[`varHide'], level(hide)
	collect style cell result[column2]#var[`varHide'], nformat(%6.3f)
	collect layout
end





preserve
use _long_with_time_at_least_1post, clear
_widetable
_stattest

collect export _twide_at_least_1post.html, replace
collect export _twide_at_least_1post.docx, replace
* collect export _twide_at_least_1post.xls, replace
collect export _twide_at_least_1post.txt, replace
restore


preserve 
use _long_with_time_at_least_2pre_1post, clear
_widetable
_stattest
collect export _twide_at_least_2pre_1post.html, replace
collect export _twide_at_least_2pre_1post.docx, replace
* collect export _twide_at_least_2pre_1post.xls, replace
collect export _twide_at_least_2pre_1post.txt, replace
restore






********************************************************************************
*  Without Urinary Protein
********************************************************************************

cap program drop _nouprot_widetable
program define _nouprot_widetable
version 17.0
table  (HOSPITALIZED) (TIME) (), nototals ///
								 stat(median month) stat(min month) stat(max month) ///
								 stat(count eGFR cr alb_) stat(mean eGFR cr alb_) stat(sd eGFR cr alb_) ///
								 nformat(%3.1f median) ///
								 nformat(%3.1f min max) ///
								 nformat(%3.0f count) ///
								 nformat(%3.1f mean sd)
		
		
// don't show stat description 
collect style header result, level(hide)
collect preview

// cahnge labels
collect label list var, all
collect label levels var eGFR "eGFR, ml/min/1.73m2", modify
collect label levels var cr "Serum creatinine, mg/dl", modify
collect label levels var alb_ "Serum albumin, g/dl", modify
collect label levels var month "Month since COVID diagnosis", modify
collect label dim TIME "Survey Time Points", modify
collect label levels TIME 1 "Pre-COVID" 2 "At admission" 3 "During COVID" 4 "After COVID" 5 "Second F-Up" 6 "Third F-UP" 7 "Most Recent", modify
// Change the label of HOSPITALIZED
collect label dim HOSPITALIZED "Admission Status", modify
// Change the label of values HOSPITALIZED
collect label levels HOSPITALIZED 1 "Outpatients" 2 "Hospitalized", modify

// do not display vertical lines and draw horzontal line below the title
collect style cell cell_type, border(right, pattern(nil))
collect style cell cell_type[column-header]#TIME, border(bottom, pattern(single))


// put frequency and percent in the same column as mean and sd, respectively
* collect recode result median = count min = mean max = sd
collect recode result median = column1 ///
                      count  = column1 ///
					  min   = column2  ///
					  mean  = column2  ///
					  max   = column3 ///
					  sd    = column3

collect layout (HOSPITALIZED[1 2]#var) (TIME#result[column1 column2 column3])

// display SD within brackets
collect style cell result[column3]#var[eGFR cr alb_], sformat("(%s)")
// display min-max with square brackets, separated by comma: [min,max]
collect style cell result[column2]#var[month],  cidelimiter(,) sformat("[%s,")
collect style cell result[column3]#var[month],  cidelimiter(,) sformat("%s]")

collect style cell var[eGFR cr alb_]#result[column1], nformat(%3.0f)
collect style cell var[month]#result[column1], nformat(%3.1f)
collect style cell var[eGFR cr alb_]#result[column2], nformat(%3.1f)
collect style cell var[month]#result[column2], nformat(%3.1f)
collect style cell var[eGFR cr alb_]#result[column3], nformat(%3.1f)
collect style cell var[month]#result[column3], nformat(%3.1f)
end



capture program drop _nouprot_stattest
program define _nouprot_stattest
version 17.0
label var eGFR "eGFR (ml/min/1.73m2)"
label var cr "Serum creatinine (mg/dl)"
label var alb_ "Serum albumin (g/dl)"
label var uprot_ "Urinary protein (g/day)"

keep recordid TIME eGFR cr alb_  HOSPITALIZED
reshape wide eGFR cr alb_ , i(recordid) j(TIME)

local timepoints "Pre_COVID At_admission During_COVID After_COVID Second_FUp Third_F_UP Most_Recent"
local n : word count `timepoint'
local varOrder
foreach name in eGFR cr alb_  {
	foreach num of numlist 2 3 4 5 6 7	 {
		foreach status of numlist  1 2		 {
		qui cap signrank `name'1 = `name'`num' if HOSPITALIZED  == `status'
		local t: word `num' of `timepoints'
		di _newline(3) in ye "----> Hospitalized = `: label (HOSPITALIZED) `status'';  Variable = `name' , `t' vs Pre-COVID: P value ="  %4.3f r(p)
		collect get column2=(r(p)), ///
			tag(TIME[`num'] HOSPITALIZED[`status'] var[`name'1])
			}
	 }	
	 local varOrder `varOrder' `name' `name'1
	 local varHide `varHide' `name'1
 }
	collect style autolevels var month `varOrder', clear
	collect style header style var[`varHide'], level(hide)
	collect style cell result[column2]#var[`varHide'], nformat(%6.3f)
	collect layout
end


preserve
use _long_with_time_at_least_1post, clear
_nouprot_widetable
_nouprot_stattest

collect export _twide_no_uprot_at_least_1post.html, replace
collect export _twide_no_uprot_at_least_1post.docx, replace
*collect export _twide_no_uprot_at_least_1post.xls, replace
collect export _twide_no_uprot_at_least_1post.txt, replace
restore

preserve 
use _long_with_time_at_least_2pre_1post, clear
_nouprot_widetable
_nouprot_stattest
collect export _twide_no_uprot_at_least_2pre_1post.html, replace
collect export _twide_no_uprot_at_least_2pre_1post.docx, replace
* collect export _twide_no_uprot_at_least_2pre_1post.xls, replace
collect export _twide_no_uprot_at_least_2pre_1post.txt, replace
restore


////////////////////////////////////////////////////////////////////////////////
**# END TABLE 6 LONGITUDINAL VAR WIDE FORMAT
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**#  START Effect (additional) of GN on kidney function recovery (categorical)
///////////////////////////////////////////////////////////////////////////////


use long_with_time, clear

drop _merge
drop base_uprot_
drop base_alb_
drop dup


* keep recordid month TIME eGFR cr alb_ uprot_ HOSPITALIZED GNDISEASE
reshape wide month eGFR cr alb_ uprot_  base_eGFR t, i(recordid) j(TIME)
cap drop rmiss
egen rnomiss =  rownonmiss(eGFR*)

foreach num of numlist 4 5 6 7  {
			cap gen _delta_eGFR_`num' = (eGFR1 - eGFR`num') / eGFR1 * 100
			cap gen byte _recovered_`num' = _delta_eGFR_`num' > -10 & !missing(_delta_eGFR_`num')
			 }
gen RECOVERED = cond( (_recovered_4 == 1 | _recovered_5 == 1 | _recovered_6 == 1 | _recovered_7 == 1), 1, 0)
replace RECOVERED = . if (_recovered_4 == . & _recovered_5 == . & _recovered_6 == . & _recovered_7 == .)

tab GNDISEASE RECOVERED, row exact
tab GNDISEASE RECOVERED if HOSPITALIZED == 2, row exact



cap drop SD_eGFR1
qui summ eGFR1
gen SD_eGFR1  = - (eGFR1 - 0) / r(sd)

cap drop SD_age
qui summ age
gen SD_age  = (age - 0) / r(sd)


cap drop SD_alb_1
qui summ alb_1
gen SD_alb_1  = - (alb_1 - 0) / r(sd)


cap drop SD_uprot_1
qui summ uprot_1
gen SD_uprot_1  = (uprot_1 - 0) / r(sd)

cap drop SLE_recoded
recode SLE (1 = 2) (2 = 1), gen(SLE_recoded)



collect clear
collect style clear
collect style use default
cap collect drop Recovery
cap collect create Recovery, replace
foreach var of varlist SD_alb_1 SD_uprot_1  ///
		AZATHIOPRINE MYCOPHENOLATE RITUXIMAB STEROIDS ///
		SLE_recoded IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE DURATION_GN  {
	    collect _r_b _r_ci _r_p, name(Recovery) tag(model[Crude]): qui logistic RECOVERED  `var' if GNDISEASE == 2
		collect _r_b _r_ci _r_p, name(Recovery) tag(model[Adjusted]): qui logistic RECOVERED SD_eGFR1 SD_age GENDER NON_WHITE ACEI HOSPITALIZED  `var' if GNDISEASE == 2
		* cap noi lincom `var', cformat(%3.2f) pformat(%4.3f) sformat(%3.2f)
 }
 
 // define number of digits
collect style cell result[_r_b], nformat(%3.2f)
collect style cell result[_r_ci], nformat(%3.2f) sformat("[%s]") cidelimiter(,)
collect style cell result[_p], nformat(%4.3f)

// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header
collect style cell cell_type[column-header], border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)

// do not show result name (OR, 95%CI, P)
collect style header result, level(hide)

// create space between columns
collect style column, extraspace(3)

// stack levels of factor vars
collect style row stack, spacer

// do not show baseline levels
collect style showbase off

// renames names in the first column
collect recode colname SD_alb_1 = "Serum albumin, g/dL (per 1 SD unit decrease)"
collect recode colname SD_uprot_1 = "Urinary Protein, g/day (per 1 SD unit increase)"
collect recode colname AZATHIOPRINE = "Azathioprine"
collect recode colname MYCOPHENOLATE = "Mycophenolate" 
collect recode colname RITUXIMAB = "Rituximab"
collect recode colname STEROIDS = "Steroids"
collect recode colname DURATION_GN = "Duration of GN (trend across categories)"
collect recode colname SLE_recoded = "SLE history"
collect recode colname GN_SLE = "SLE GN diagnosis"
collect recode colname VASCULITIS = "Vasculitis"
collect recode colname EITHER_VASC_SLE  = "SLE GN or Vasculitis" 
collect recode colname IgAN_HSP = "IgA nephropathy" 
collect recode colname FSGS_MCD = "FSGS or MCD"
collect recode colname MN = "Membranous nephropathy"


// associate significance stars
collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*", attach(_r_b)

// show variables but not the constant term
collect layout ///
(colname["Serum albumin, g/dL (per 1 SD unit decrease)" "Urinary Protein, g/day (per 1 SD unit increase)" "Azathioprine" "Mycophenolate" "Rituximab" "Steroids" "Duration of GN (trend across categories)" "SLE history" "SLE GN diagnosis" "Vasculitis" "SLE GN or Vasculitis" "IgA nephropathy" "FSGS or MCD" "Membranous nephropathy" ]#result)  (model)

// for repeating column headers, display them once and center them
collect style column, dups(center)

collect preview

collect export recovery_crude_adj_ORs.html, replace
collect export recovery_crude_adj_ORs.docx, replace


////////////////////////////////////////////////////////////////////////////////
**#  END Effect (additional) of GN on kidney function recovery (categorical)
///////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////
**# Start Analyis of each Additional characteristic on post-COVID GFR (GN pts)
///////////////////////////////////////////////////////////////////////////////

  use long_with_time, clear 
 
	

/*	
cap drop SD_eGFR1
qui summ eGFR1
gen SD_eGFR1  = - (eGFR1 - 0) / r(sd)
*/

cap drop SD_age
qui summ age
gen SD_age  = (age - 0) / r(sd)


cap drop SD_base_alb_
qui summ base_alb_ if TIME == 1
gen SD_base_alb_  = - (base_alb_ - 0) / r(sd)


cap drop SD_base_uprot_
qui summ base_uprot_ if TIME == 1
gen SD_base_uprot_  = (base_uprot_ - 0) / r(sd)


cap drop SLE_recoded
recode SLE (1 = 2) (2 = 1), gen(SLE_recoded)




collect clear
collect style clear
collect style use default
cap collect drop Det_eGFR
cap collect create Det_eGFR, replace
foreach var of varlist  SD_base_alb_ SD_base_uprot_  ///
		AZATHIOPRINE MYCOPHENOLATE RITUXIMAB STEROIDS ///
		SLE_recoded IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE DURATION_GN  {
	    collect _r_b _r_ci _r_p, name(Det_eGFR) tag(model["Base Model"]): qui 	mixed eGFR `var' ///
		c.month##AKI_YESNO##c.base_eGFR i.HOSPITALIZED ///
		if (GNDISEASE == 2)  ///
		|| recordid: , reml cov(unstr) dfmethod(kroger)	
		collect _r_b _r_ci _r_p, name(Det_eGFR) tag(model["Fully Adjusted"]): qui 	mixed eGFR `var' ///
		c.month##AKI_YESNO##c.base_eGFR i.HOSPITALIZED ///
		c.age i.GENDER i.NON_WHITE i.ACEI  ///
		if (GNDISEASE == 2)  ///
		|| recordid: , reml cov(unstr) dfmethod(kroger) 
 }
 
 // define number of digits
collect style cell result[_r_b], nformat(%3.1f)
collect style cell result[_r_ci], nformat(%3.1f) sformat("[%s]") cidelimiter(,)
collect style cell result[_p], nformat(%4.3f)

// do not draw vertical lines
collect style cell cell_type, border(right, pattern(nil))

// draw a lower margin line below the column header
collect style cell cell_type[column-header], border(bottom, pattern(single))
collect style cell cell_type[item column-header], halign(center)

// do not show result name (Beta, 95%CI, P)
collect style header result, level(hide)

// create space between columns
collect style column, extraspace(3)

// stack levels of factor vars
collect style row stack, spacer

// do not show baseline levels
collect style showbase off

// renames names in the first column
collect recode colname SD_base_alb_ = "Serum albumin, g/dL (per 1 SD unit decrease)"
collect recode colname SD_base_uprot_ = "Urinary Protein, g/day (per 1 SD unit increase)"
collect recode colname AZATHIOPRINE = "Azathioprine"
collect recode colname MYCOPHENOLATE = "Mycophenolate" 
collect recode colname RITUXIMAB = "Rituximab"
collect recode colname STEROIDS = "Steroids"
collect recode colname DURATION_GN = "Duration of GN (trend across categories)"
collect recode colname SLE_recoded = "SLE history"
collect recode colname GN_SLE = "SLE GN diagnosis"
collect recode colname VASCULITIS = "Vasculitis"
collect recode colname EITHER_VASC_SLE  = "SLE GN or Vasculitis" 
collect recode colname IgAN_HSP = "IgA nephropathy" 
collect recode colname FSGS_MCD = "FSGS or MCD"
collect recode colname MN = "Membranous nephropathy"


// associate significance stars
collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*", attach(_r_b)

// show variables but not the constant term
collect layout ///
(colname["Serum albumin, g/dL (per 1 SD unit decrease)" "Urinary Protein, g/day (per 1 SD unit increase)" "Azathioprine" "Mycophenolate" "Rituximab" "Steroids" "Duration of GN (trend across categories)" "SLE history" "SLE GN diagnosis" "Vasculitis" "SLE GN or Vasculitis" "IgA nephropathy" "FSGS or MCD" "Membranous nephropathy" ]#result)  (model)

// for repeating column headers, display them once and center them
collect style column, dups(center)

collect preview

collect export _det_long_average_eGFR.html, replace
collect export _det_long_average_eGFR.docx, replace
	
///////////////////////////////////////////////////////////////////////////////
**# End Analyis of each Additional characteristic on post-COVID GFR (GN pts)
///////////////////////////////////////////////////////////////////////////////	
 
