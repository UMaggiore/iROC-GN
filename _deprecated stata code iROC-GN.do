clear
cap log using "C:\Users\Pc\Box\Waldman Projects\analysis iROC-GN `c(current_date)'.smcl", replace
clear
cd "C:\Users\Pc\Box\Waldman Projects"
import delimited "GlomerularDiseaseReg_DATA_LABELS_2021-06-08_0733.csv", encoding(UTF-8) 


////////////////////////////////////////////////////////////////////////////////
**#   START PREPARING DATASET           ///////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
version 17.0
*-------------------------------------------------------------------------------
* START REMOVING RECORDS NOT TO BE INCLUDED
*-------------------------------------------------------------------------------


note recordid: 84 (renal Tx), 165 (Fabry), 191 (unsure whether this is control)
foreach num of numlist 5 22 24 27 32 80 84 85 87 91 100 128 146 151 158 159 165 ///
	174 191 208 {
	drop if recordid ==  `num'
	 }
	 
	 
*-------------------------------------------------------------------------------
* END REMOVING RECORDS NOT TO BE INCLUDED
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START CHECKING PATIENT ID
*-------------------------------------------------------------------------------

inspect subjectid
inspect recordid
inspect subjectid if eventname == "initial contact"
inspect subjectid if eventname == "follow up 1"
inspect recordid if eventname == "initial contact"
inspect recordid if eventname == "follow up 1"
gen row = 0
replace row = 1 if eventname == "follow up 1"
replace row = 2 if eventname == "follow up 2"
label define row 0 "initial contact" 1 "follow up 1" 2 "follow up 2"
label values row row
sort recordid row

*-------------------------------------------------------------------------------
* END CHECKING PATIENT ID
*-------------------------------------------------------------------------------
	 
*-------------------------------------------------------------------------------
* START FILLING  IN EMPTY VALUES FOR GNDISEASE VAR
*-------------------------------------------------------------------------------

replace doesthepatienthaveglomerulardise = trim(doesthepatienthaveglomerulardise)
replace doesthepatienthaveglomerulardise = "Yes" ///
	if  (doesthepatienthaveglomerulardise == "" & eventname=="initial contact")
	
*-------------------------------------------------------------------------------
* END FILLING  IN EMPTY VALUES FOR GNDISEASE VAR
*-------------------------------------------------------------------------------


*-------------------------------------------------------------------------------
* START CORRECTING STRING VARIABLES THAT SHOULD BE NUMERIC
*-------------------------------------------------------------------------------
replace serumcreatininepriortocovidinfec = serumcreatininepriortocovidinfec / 88.42 if specifyserumcreatininescrunitsus == "umol/L"
destring hiddenorigquestionestimatedgfrpr, replace force ignore(">") 
destring proteinuriaquantifiedpriortocovi, replace force ignore("<" "mg/dl" "trace" "negative") 
replace proteinuriaquantifiedpriortocovi = proteinuriaquantifiedpriortocovi * 0.00884 if proteinuriaspecifyunitsofmeasure == "mg/mmol"
replace proteinuriaquantifiedpriortocovi = proteinuriaquantifiedpriortocovi * 1.0 if proteinuriaspecifyunitsofmeasure == "mg/mg"
replace proteinuriaquantifiedpriortocovi = proteinuriaquantifiedpriortocovi / 1000  if (proteinuriaspecifyunitsofmeasure == "mg/mg" & proteinuriaquantifiedpriortocovi > 40 & !missing(proteinuriaquantifiedpriortocovi))
replace proteinuriaquantifiedpriortocovi = proteinuriaquantifiedpriortocovi / 1000  if (proteinuriaspecifyunitsofmeasure == "g/24 hr" & proteinuriaquantifiedpriortocovi > 40 & !missing(proteinuriaquantifiedpriortocovi))
replace proteinuriaquantifiedpriortocovi = . if proteinuriaspecifyunitsofmeasure =="NA or unknown"

destring oralpredisonedoseifapplicable, replace force ignore("mg")
destring hiddenserumcreatininenormalizedt, replace force
destring serumalbuminpriortoinfection, replace force
replace serumalbuminpriortoinfection = serumalbuminpriortoinfection /10 if specifyserumalbuminunitstobeused == "g/L"
destring elapsedtimefromprecovidserologie, replace force
destring whatwasthemostrecentvalueofc3pri, replace force ignore("normal")
destring whatwasthemostrecentvalueofc4pri, replace force ignore("normal")
destring specifyapproximatenumberofdaysof, replace force ignore("prior to diagnosis" "symptoms started end of March 2020 - .." "unknown")
destring durationoftimeonventilator, force replace
destring wbc, replace force
replace wbc = wbc * 1000 if wb < 100
destring hemoglobin, replace force
replace hemoglobin = hemoglobin / 10 if hemoglobin > 30
destring platelet, replace force
replace platelet = platelet * 1000 if platelet < 1000
destring lymphocytecountabsolute, force replace ignore("% (no absolute calculated)")
replace lymphocytecountabsolute = lymphocytecountabsolute * wbc if lymphocytecountabsolute < 1
replace lymphocytecountabsolute = lymphocytecountabsolute/100 * wbc if lymphocytecountabsolute == 23.0
replace lymphocytecountabsolute = lymphocytecountabsolute * 100 if lymphocytecountabsolute >= 1 & lymphocytecountabsolute < 10
destring absoluteneutrophilcount, force replace ignore("% (no absolute calculated)")
replace absoluteneutrophilcount = absoluteneutrophilcount * 1000 if absoluteneutrophilcount >= 1 & absoluteneutrophilcount < 50
destring cd4count, force replace
replace cd4count = cd4count * 100 if cd4count < 1
destring cd8count, force replace
replace cd8count = cd8count * 100 if cd8count < 1
destring ddimer, force replace ignore("<")
replace ddimer = ddimer * 1000 if ddimerunits == "ng/ml"
replace ddimer = ddimer * 1    if ddimerunits == "mg/l"
replace ddimer = ddimer / 1000 if ddimer > 50000 & !missing(ddimer)
replace ddimer = ddimer * 1000 if ddimer < 50
destring creactiveprotein, force replace
replace creactiveprotein = creactiveprotein * 10 if whataretheunitsforcrp == "mg/dl"
destring esr, replace force ignore(">")
destring il6, replace force ignore("pg/mL" "pg/ml")
destring fibrinogen, replace force ignore(">")
replace fibrinogen = fibrinogen * 100 if fibrinogen < 10
destring ferritin, replace force
replace ferritin = ferritin * 100 if ferritin < 1
destring lactatedehydrogenaseldh, replace force ignore("U/L")
replace lactatedehydrogenaseldh = lactatedehydrogenaseldh * 1000 if lactatedehydrogenaseldh < 1
destring cpk, replace force
destring lactate, replace force 
replace lactate = lactate / 1000 if lactate > 100
destring whatwastotalcumulativedosageofst, replace force
destring equivalentdoseinprednisone, replace force
destring serumcreatinineonadmissionifhosp, replace force ignore("Died within 48 hours of admission")
replace serumcreatinineonadmissionifhosp = serumcreatinineonadmissionifhosp / 88.42 if serumcreatinineonadmissionifhosp > 20 & !missing(serumcreatinineonadmissionifhosp)
destring peakserumcreatinineduringcovidin, replace force ignore("Died within 48 hours of admission")
replace peakserumcreatinineduringcovidin = peakserumcreatinineduringcovidin / 88.42 if peakserumcreatinineduringcovidin > 30 & !missing(peakserumcreatinineduringcovidin)
destring timebetweencoviddiagnosisandpeak, replace force
destring numberofdaystopeakserumcreatinin, replace force ignore("-Feb" "NA" "Died within 48 hours of admission")
destring serumalbuminduringcovidinfection, replace force ignore("Died within 48 hours of admission")
replace serumalbuminduringcovidinfection = serumalbuminduringcovidinfection / 10 if serumalbuminduringcovidinfection > 10
destring whatwasthelowestvaluenadirofseru, replace force
destring peakproteinuriaquantifiedduringc, replace force ignore("+" "mg/dL" "negative" "no measure" "Died within 48 hours of admission")
replace peakproteinuriaquantifiedduringc = peakproteinuriaquantifiedduringc * 0.00884 if v397 == "mg/mmol"
replace peakproteinuriaquantifiedduringc = peakproteinuriaquantifiedduringc * 1.0 if v397 == "mg/mg"
replace peakproteinuriaquantifiedduringc = peakproteinuriaquantifiedduringc / 1000  if (v397 == "mg/mg" & peakproteinuriaquantifiedduringc > 40 & !missing(peakproteinuriaquantifiedduringc))
replace peakproteinuriaquantifiedduringc = peakproteinuriaquantifiedduringc / 1000  if (v397 == "g/24 hr" & peakproteinuriaquantifiedduringc > 40 & !missing(peakproteinuriaquantifiedduringc))
replace v397 = trim(v397)
replace peakproteinuriaquantifiedduringc =. if (v397 == "NA or unknown" | v397 == "dipstick 1+" | v397 == "dipstick 2+" | v397 == "dipstick 3+" | v397 == "dipstick 4+")

destring ifdeathdayssincediagnosis, replace force
destring timeelapsedfromcoviddiagnosistod, replace force
destring ifhospitalizedlengthofhospitalst, replace force ignore("and ongoing")
destring dayssincecoviddiagnosis, replace force ignore("Died within 48 hours of admission" "No renal follow up since illness" "no follow up data yet" "unknown")
destring timesincecoviddiagnosis, replace force
destring serumcreatinineaftercovidinfecti, replace force ignore("NA")
replace serumcreatinineaftercovidinfecti = serumcreatinineaftercovidinfecti / 88.42 if serumcreatinineaftercovidinfecti > 20 & !missing(serumcreatinineaftercovidinfecti)
destring hiddenorigquestionnowcalcestimat, replace force ignore(">" "NA")
destring timeelapsedfromcoviddiagnosistof, replace force
destring hiddennormalizedserumcreatininet, replace force
destring estimatedgfraftercovidinfection, replace force
destring proteinuriaquantifiedaftercovidi, replace force ignore("mg/dL" "NA" "negative")
replace proteinuriaquantifiedaftercovidi = proteinuriaquantifiedaftercovidi * 0.00884 if proteinuriaspecifyunits == "mg/mmol"
replace proteinuriaquantifiedaftercovidi = proteinuriaquantifiedaftercovidi * 1.0 if proteinuriaspecifyunits == "mg/mg"
replace proteinuriaquantifiedaftercovidi = proteinuriaquantifiedaftercovidi / 1000  if (proteinuriaspecifyunits == "mg/mg" & proteinuriaquantifiedaftercovidi > 40 & !missing(proteinuriaquantifiedaftercovidi))
replace proteinuriaquantifiedaftercovidi = proteinuriaquantifiedaftercovidi / 1000  if (proteinuriaspecifyunits == "g/24 hr" & proteinuriaquantifiedaftercovidi > 40 & !missing(proteinuriaquantifiedaftercovidi))

destring serumalbuminaftercovidinfection, replace force ignore("NA")
destring timefromcoviddiagnosistodeath, replace force
destring followuptimepointsincecovid19dia, replace force
destring timesincedischargefromhospitaliz, replace force
destring whatiscurrentweight, replace force
destring mostrecentserumcreatinine, replace force
replace mostrecentserumcreatinine = mostrecentserumcreatinine / 88.42 if mostrecentserumcreatinine > 20 & !missing(mostrecentserumcreatinine)
destring hiddencreatininenormalizedvaluet, replace force
destring estimatedgfr, replace force
destring timingofserumcreatininerelativet, replace force
destring mostrecentserumalbuminfromsameda, replace force
replace mostrecentserumalbuminfromsameda = mostrecentserumalbuminfromsameda / 10 if mostrecentserumalbuminfromsameda > 10
destring mostrecentproteinuriavaluefromsa, replace force 
replace mostrecentproteinuriavaluefromsa = mostrecentproteinuriavaluefromsa * 0.00884 if specifyunitsofmeasurementofprote == "mg/mmol"
replace mostrecentproteinuriavaluefromsa = mostrecentproteinuriavaluefromsa * 1.0 if specifyunitsofmeasurementofprote == "mg/mg"
replace mostrecentproteinuriavaluefromsa = mostrecentproteinuriavaluefromsa / 1000  if (specifyunitsofmeasurementofprote == "mg/mg" & mostrecentproteinuriavaluefromsa > 40 & !missing(mostrecentproteinuriavaluefromsa))
replace mostrecentproteinuriavaluefromsa = mostrecentproteinuriavaluefromsa / 1000  if (specifyunitsofmeasurementofprote == "g/24 hr" & mostrecentproteinuriavaluefromsa > 40 & !missing(mostrecentproteinuriavaluefromsa))

destring timingoflabs2ndsetrelativetocovi, replace force
destring serumcreatininefromadifferent2nd, replace force
replace serumcreatininefromadifferent2nd = serumcreatininefromadifferent2nd / 88.42 if serumcreatininefromadifferent2nd > 20
destring v466, replace force
destring estimatedgfr2ndset, replace force
destring serumalbuminfromdifferent2ndtime, replace force
replace serumalbuminfromdifferent2ndtime = serumalbuminfromdifferent2ndtime / 10 if serumalbuminfromdifferent2ndtime > 10
destring proteinuriaquantificationfromdif, replace force
replace proteinuriaquantificationfromdif = proteinuriaquantificationfromdif * 0.00884 if specifyunitsofmeasurementforprot == "mg/mmol"
replace proteinuriaquantificationfromdif = proteinuriaquantificationfromdif * 1.0 if specifyunitsofmeasurementforprot == "mg/mg"
replace proteinuriaquantificationfromdif = proteinuriaquantificationfromdif / 1000  if (specifyunitsofmeasurementforprot == "mg/mg" & proteinuriaquantificationfromdif > 40 & !missing(proteinuriaquantificationfromdif))
replace proteinuriaquantificationfromdif = proteinuriaquantificationfromdif / 1000  if (specifyunitsofmeasurementforprot == "g/24 hr" & proteinuriaquantificationfromdif > 40 & !missing(proteinuriaquantificationfromdif))


summ mostrecentproteinuriabydipsticki proteinuriabydipstickfromdiffere proteinuriabydipstick3rdsetoflab

foreach var of varlist ///
	mostrecentproteinuriabydipsticki ///
	proteinuriabydipstickfromdiffere ///
	proteinuriabydipstick3rdsetoflab  {
    // remove the last character as var name may have max name length (32)
	local new = substr("`var'", 1, length("`var'")-1)
	// store variable label
	local x : variable label `var'	 
	gen  `new' = .
	// attach variable label
    label variable `new' "`x'"
	qui replace `new' = 0 if `var' == "negative"
	qui replace `new' = 1 if `var' == "1+"
	qui replace `new' = 2 if `var' == "2+"
	qui replace `new' = 3 if `var' == "3+"
	qui replace `new' = . if `var' == "Not available"
	drop `var'
		}



destring timingoflabs3rdsetrelativetocovi, replace force
destring serumcreatininevalue3rdsetoflabs, replace force
replace serumcreatininevalue3rdsetoflabs = serumcreatininevalue3rdsetoflabs / 88.42 if serumcreatininevalue3rdsetoflabs > 20 & !missing(serumcreatininevalue3rdsetoflabs)
destring v476, replace force
destring estimatedgfr3rdset, replace force
replace serumalbuminduringcovidinfection = serumalbuminduringcovidinfection / 10 if serumalbuminduringcovidinfection > 10
destring serumalbuminvalue3rdsetoflabs, replace force 
replace serumalbuminvalue3rdsetoflabs = serumalbuminvalue3rdsetoflabs / 10 if serumalbuminvalue3rdsetoflabs > 10
destring proteinuriaquantification3rdseto, replace force
replace proteinuriaquantification3rdseto = proteinuriaquantification3rdseto if v480 == "mg/mmol"
replace proteinuriaquantification3rdseto = proteinuriaquantification3rdseto if v480 == "mg/mg"
destring whatisthec3labvalue, replace force
replace whatisthec3labvalue = whatisthec3labvalue * 100 if whatisthec3labvalue < 2
destring whatisthec4labvalue, replace force
replace whatisthec4labvalue = whatisthec4labvalue * 100 if whatisthec4labvalue < 1

destring lactatedehydrogenaseldh, replace force

encode doyouhaveadditionallabvaluesfors, gen(doyouhaveadditionallabvaluesfor)
drop doyouhaveadditionallabvaluesfors
encode doyouhavemorelabvaluesforserumcr, gen(doyouhavemorelabvaluesforserumc)
drop doyouhavemorelabvaluesforserumcr
encode livergiissueschoicepancreatitis, gen(livergiissueschoicepancreatiti)
drop livergiissueschoicepancreatitis
encode specifyserumcreatinineunitsusedi, gen(specifyserumcreatinineunitsused)
drop specifyserumcreatinineunitsusedi
encode specifyserumalbuminunitstobeused, gen(specifyserumalbuminunitstobeuse)
drop specifyserumalbuminunitstobeused
encode specifyserumalbuminunitsusedinyo, gen(specifyserumalbuminunitsusediny)
drop specifyserumalbuminunitsusediny
encode didyourepeatarenalbiopsyonthispa, gen(didyourepeatarenalbiopsyonthisp)
drop didyourepeatarenalbiopsyonthisp

replace patientheightcm = patientheightcm * 100 if patientheightcm < 50
replace patientheightcm = 175 if patientheightcm == 75
gen BMI = patientweightkg  / (patientheightcm/ 100) ^2


rename v819 _alb_pre
rename v978 _alb_during
rename v993 _alb_after

rename v984 diag_to_death_days

label define yesno 0 "No" 1 "Yes"
label define yes2no1 1 "No" 2 "Yes"

* convert categorical strings into labeled numeric
encode gender, gen(GENDER)
encode race, gen(RACE)
gen NON_WHITE = (RACE != 6) & !missing(RACE) 
encode doesthepatienthaveglomerulardise, gen(GNDISEASE)
encode approximatedurationofkidneydisea, gen(_DURATION_GN)
recode _DURATION_GN (1= 1 "1-6  months") (2 = 3 "12-24 month") (3 = 4 "2-5 years") (4 = 2 "6-12 months") (5 = 5 ">5 years"), gen(DURATION_GN)

encode wasthepatienthospitalized, gen(HOSPITALIZED)

replace HOSPITALIZED = 2 if recordid == 111

encode v184, gen(TRANSPLANT)
encode v177, gen(ESKD)
encode didthepatienthavepreexistingglo, gen(PRE_EX_GN)

encode didthepatientdie, gen(DEATH)
tab didthepatientdie DEATH

replace DEATH = 1 if missing(DEATH)
tab DEATH
replace DEATH = 2 if isthepatientalive == "No"
tab DEATH
recode DEATH (1 = 0 "No") (2 = 1 "Yes"), gen(DEATH_YESNO) label(deathyesno)
tab DEATH_YESNO

egen _sumdeath = sum(DEATH_YESNO), by(recordid)
replace DEATH_YESNO = 1 if _sumdeath > 0 & DEATH_YESNO == 0

encode ethnicity, gen(ETHNICITY)
encode glomerulardiseasediagnosis, gen(GLOM_DIS_DIAG)


encode hematuriaondipstick, gen(HEMAT_PRE)
encode hematuriaurinalysisdipstick, gen(HEMAT_ADM)
encode v419, gen(HEMAT_POST)


*-------------------------------------------------------------------------------
* END CORRECTING STRING VARIABLES THAT SHOULD BE NUMERIC
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START COMPLETE DROPPING NON-ELIGIBLE PATIENTS
*-------------------------------------------------------------------------------

drop if PRE_EX_GN == 2
drop if TRANSPLANT == 1
* there are no patients with ESKD left


*-------------------------------------------------------------------------------
* END COMPLETE DROPPING NON-ELIGIBLE PATIENTS
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START CORRECTING STRING DATES INTO STATA DATES
*-------------------------------------------------------------------------------

*---- Start converting string dates into Stata dates

foreach var of varlist ///
dateofcoviddiagnosis whenwasthisserumcreatininelastch ///
whenwasthelasttimeyoucheckedrele whenwasthepatienthospitalized ///
dateofinitialsetoflabsafterdiagn whatisthedateofthispeakserumcrea ///
dateoffollowupclosetohospitaldis dateserumcreatininemeasured ///
dateofserumcreatininelabaftercov whatwasthedateoftheoriginaldiagn ///
dateofadditional2ndsetlabs dateofadditional3rdsetoflabs whendidthepatientdie ///
specifyserumcreatininescrunitsus dateofdeath ///
whenwasivcorticosteroidslastadmi ///
	{
    // remove the last character as var name may have max name length (32)
	local new = substr("`var'", 1, length("`var'")-1)
	// store variable label
	local x : variable label `var'
	// convert string into dates
	gen double `new'  = date(`var',"MD20Y")
	format `new' %td
	drop `var' 
	// attach variable label
    label variable `new' "`x'"
		}
	
	// convert string date that includes clock
	gen double _v426 = clock(v426,"MD20Y hm")
	format _v426 %tc
	gen double __v426 = dofc(_v426)
	format __v426 %td
	drop v426 _v426
	rename __v426  v426
	
list dateofadditional2ndsetlab ///
	dateofadditional3rdsetoflab if (eventname != "initial contact"), ///
	sepby(recordid) noobs	
	

* Start display name of date vars
ds, has(format %t*)
describe `r(varlist)', fullnames
qui ds, has(format %t*)
tabstat `r(varlist)', stat(n mean min max) col(stat) varwidth(32)
findname, format(%t*) varwidth(31)

* double check label and dates
lookfor "When was this serum creatinine last checked"
di r(varlist)
lookfor "Date of COVID diagnosis"
di r(varlist)
lookfor "What was the date of the original diagnosis of COVID?"
di r(varlist)
lookfor "When was the patient hospitalized?"
di r(varlist)
lookfor "Date of initial set of labs after diagnosis of COVID"
di r(varlist)
lookfor "What is the date of this peak serum creatinine?"
di r(varlist)
lookfor "Date serum creatinine measured"
di r(varlist)
lookfor "Date of serum creatinine lab after covid infection"
di r(varlist)
lookfor "Date of additional"
di r(varlist)



*-------------------------------------------------------------------------------
* END CORRECTING STRING DATES INTO STATA DATES
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START CHECKING VAR NAMES WITH CREATININE
*-------------------------------------------------------------------------------

* Start display name of date vars
ds, has(format %t*)
describe `r(varlist)', fullnames
qui ds, has(format %t*)
tabstat `r(varlist)', stat(n mean min max) col(stat) varwidth(32)
findname, format(%t*) varwidth(31)

* double check label and dates
lookfor "When was this serum creatinine last checked"
di r(varlist)
lookfor "Date of COVID diagnosis"
di r(varlist)
lookfor "What was the date of the original diagnosis of COVID?"
di r(varlist)
lookfor "When was the patient hospitalized?"
di r(varlist)
lookfor "Date of initial set of labs after diagnosis of COVID"
di r(varlist)
lookfor "What is the date of this peak serum creatinine?"
di r(varlist)
lookfor "Date serum creatinine measured"
di r(varlist)
lookfor "Date of serum creatinine lab after covid infection"
di r(varlist)
lookfor "Date of additional"
di r(varlist)

* Variables with label including serum creatinine
qui lookfor "creat"
di r(varlist)
describe `r(varlist)', fullnames
qui lookfor "creat"
tabstat `r(varlist)', stat(n mean min max) col(stat) varwidth(32)
* Variables with varname including serum creatinine
findname, chartext(*creat*) varwidth(32)

*-------------------------------------------------------------------------------
* END CHECKING VAR NAMES WITH CREATININE
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START CHECKING VAR NAMES WITH GFR
*-------------------------------------------------------------------------------

* Variables with label including GFR
qui lookfor "GFR"
di r(varlist)
describe `r(varlist)', fullnames
qui lookfor "GFR"
tabstat `r(varlist)', stat(n mean min max) col(stat) varwidth(32)

* Variables with varname including gfr
findname, chartext(*gfr*) varwidth(32)

*-------------------------------------------------------------------------------
* END CHECKING VAR NAMES WITH GFR
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START CHECKING VAR NAMES WITH ALB
*-------------------------------------------------------------------------------

drop didyourepeatarenalbiopsyonthispa
drop specifyserumalbuminunitsusedinyo
* Variables with label including serum albumin
qui lookfor "alb"
di r(varlist)
describe `r(varlist)', fullnames
qui lookfor "alb"
tabstat `r(varlist)', stat(n mean min max) col(stat) varwidth(32)
* Variables with varname including serum albumin
findname, chartext(*alb*) varwidth(32)

*-------------------------------------------------------------------------------
* END CHECKING VAR NAMES WITH ALB
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
*  START ALLIGNING ROW DATASET BY SPLITTING AND MERGING BY RECORD ID
*-------------------------------------------------------------------------------


**# ALLIGN RECORDS
*-------------------------------------------------------------------------------
* START DATASET WITH "initial contact" 1 "follow up 1" "follow up 1" ALLIGNED
*-------------------------------------------------------------------------------

qui ds, has(format %t*)
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)

qui qui lookfor "creat"
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)

qui qui lookfor "gfr"
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)


preserve

qui ds, has(format %t*)
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
qui ds, has(format %t*)
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)

qui lookfor "timing of"
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
	 
qui lookfor "elapsed"
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }

qui lookfor "creat"
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
qui lookfor "creat"
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)

qui lookfor "gfr"
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
qui lookfor "gfr"
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)

qui lookfor "alb"
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
qui lookfor "alb"
tabstat `r(varlist)', by(row) stat(n mean) col(stat) varwidth(32)

qui lookfor "prot"
foreach var of varlist `r(varlist)'  {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
qui lookfor "prot"


foreach var of varlist wasthepatientdischargedfromhospi isthepatientstillrequiringrenalr {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
	 }
	 
foreach var of varlist cardiacissueschoicechestpain cardiacissueschoicecongestivehea ///
	cardiacissueschoicepericarditis cardiacissueschoicemyocarditis cardiacissueschoicevalvularprobl ///
	cardiacissueschoicearrhythmia cardiacissueschoicehypertension cardiacissueschoicehypotension ///
	cardiacissueschoiceother v944 v945 v948  myocardialinfarction v946 /// 
	arrhythmia wasonsetofthisarrhythmiatemporal v360 cardiacissueschoicearrhythmia v947 /// 
	centralnervoussystem v952 othercns v955 ///
	gastrointestinalandhepatobiliary v949  ///
	pulmonaryembolismpe venousthromboembolismbesidespe v958 v959 ///
	superimposedinfection v962 {
	qui bysort recordid (row): replace `var' = `var'[_n-1] if missing(`var')
	qui bysort recordid (row): replace `var' = `var'[_n+1] if missing(`var')
		 }

bysort recordid (row): egen __followuptimepointsincecovid19 = max(followuptimepointsincecovid19dia)
drop followuptimepointsincecovid19dia
rename __followuptimepointsincecovid19 followuptimepointsincecovid19dia
label var followuptimepointsincecovid19dia "maximum follow-up time (days)"
		 
drop if row !=0

*-------------------------------------------------------------------------------
* END DATASET WITH "initial contact" 1 "follow up 1" "follow up 1" ALLIGNED
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START ENCODING CATEGORICAL STRING VARIATES AND GENERATING INDICATOR VARS
*-------------------------------------------------------------------------------

encode didthepatienthaveanyofthesecomor, gen(HYPERT)
encode v175, gen(DIABETES)
encode v186, gen(OBESITY)
encode v178, gen(CVD)
encode v180, gen(COPD)
encode v179, gen(ASTHMA)
encode v189, gen(LIVER_DIS)
encode v182, gen(CANCER)
encode v185, gen(HIV)
encode v187, gen(SLE)
encode v188, gen(RA)
encode v191, gen(SMOKER)
encode wasthepatientonaaceinhibitoratti, gen(ACEI)
replace ACEI = 1 if missing(ACEI)
encode symptomsofcovid19infectionchoice, gen(FEVER)
encode v205, gen(COUGH)
encode v206, gen(DYSPNEA)
encode v212, gen(FATIGUE)
encode v208, gen(MYALGIA)
encode v209, gen(GI_SYMPT)
encode v213, gen(ANOREXIA)
encode v204, gen(CHILLS)
encode v211, gen(NASALCONG)
encode v214, gen(SORETHROAT)
encode v207, gen(ANOSMIA)
encode v215, gen(NEUR_SYMPT)

gen byte IgAN_HSP = (GLOM_DIS_DIAG == 2)
gen byte VASCULITIS =  (GLOM_DIS_DIAG == 9)
gen byte FSGS_MCD = (GLOM_DIS_DIAG == 1 | GLOM_DIS_DIAG == 6)
gen byte MN = (GLOM_DIS_DIAG == 5)
gen byte GN_SLE = (GLOM_DIS_DIAG == 3)
gen byte EITHER_VASC_SLE = (VASCULITIS == 1 | GN_SLE == 1)

gen byte MPGN = (GLOM_DIS_DIAG == 4)
gen byte PIGN = (GLOM_DIS_DIAG == 7)
gen byte TMA = (GLOM_DIS_DIAG == 8)
gen byte AA_FG = (GLOM_DIS_DIAG == 10 | GLOM_DIS_DIAG == 11 | GLOM_DIS_DIAG == 12)

gen STEROIDS =  cond(steroids =="", 1, 2)
encode wasthepatienttakingimmunosuppres, gen(IMMUNOSUPPRESSION)
replace IMMUNOSUPPRESSION = 1 if (missing(IMMUNOSUPPRESSION) & GNDISEASE == 2)

encode v94, gen(AZATHIOPRINE)
encode v93, gen(MYCOPHENOLATE)
encode v98, gen(RITUXIMAB)
encode specifynumberofmonthsthatrituxim, gen(RTX_GRP_MONTHS_BEFORE_COVID)
gen RTX_MONTHS_BEFORE_COVID = timesincelastrituximabdose / (365.25 / 12)
encode v95, gen(CNI)
 

label values STEROIDS yes2no1
encode v307, gen(TX_STEROID_TYPE)
encode whatrouteweresteroidsadministere, gen(TX_STEROID_IVOS)
replace steroid = "" if steroid =="NA"
encode steroid, gen(TX_STEROID_CONTINUED)
encode basedontheintialsurveythispatien, gen(TX_STEROID_v2_CONTINUED)

gen TX_HYDROXYCHLO = 0
replace TX_HYDROXYCHLO  = 1 if v563 != ""
label values TX_HYDROXYCHLO  yesno

encode wasanticoagulationthromboprophyl, gen(TX_ANTICO_PROPH_YESNO)
encode otheranticoagulationtherapy, gen(TX_ANTICO_OTHER)
encode wasanticoagulationgiveninstandar, gen(TX_ANTICO_PROPH_INTENSITY)
encode didpatientdevelopathromboticeven, gen(COMPL_THROMB_UNDERPRO)
encode ifpatientwasnotdialysisdependent, gen(RRT)
recode RRT (1 = 0 "No") (2 = 1 "Yes"), gen(RRT_YESNO) label(rrtyesno)
encode ifpatientdevelopedakipleaseindic, gen(AKI_STAGE)
gen byte AKI_YESNO = cond((AKI_STAGE ==1 | AKI_STAGE == 2 | AKI_STAGE ==3), 1, 0)

label values AKI_YESNO yesno
gen byte AKI_3 = cond((AKI_STAGE ==3), 1, 0)
label values AKI_3 yesno

label var RRT_YESNO "Developed RRT During Hospital Admission"
label var AKI_YESNO "Developed AKI During Hospital Admission"
label var AKI_3 "Developed AKI Stage 3 During Hospital Admission"
label var AKI_STAGE "AKI Stage Developed During Hospital Admission"


replace AKI_YESNO = 0 if missing(AKI_STAGE)
replace AKI_YESNO = . if HOSPITALIZED == 1
replace AKI_STAGE = . if HOSPITALIZED == 1
replace RRT_YESNO = 0 if missing(ifpatientwasnotdialysisdependent) 
replace RRT = . if HOSPITALIZED == 1

// correction error Jun 12 2021
replace AKI_YESNO = 1 if RRT_YESNO == 1 & HOSPITALIZED == 2

encode howwouldyoucharacterizetherenald, gen(_howwouldyoucharacterizetherenal)
gen ACTIVE_GN = 0
replace ACTIVE_GN = 1 if _howwouldyoucharacterizetherenal == 4 | _howwouldyoucharacterizetherenal == 5 | _howwouldyoucharacterizetherenal == 6
label var ACTIVE_GN "Active glomerulonephritis"
label values ACTIVE_GN yesno
gen UPROT500 = (proteinuriaquantifiedpriortocovi > 500) & !missing(proteinuriaquantifiedpriortocovi)
label var UPROT500 "Pre-COVID PROTEINURIA > 500mg/day"
label values UPROT500 yesno


encode wasthepatientadmittedtotheintens, gen(ICU)
replace ICU = 1 if missing(wasthepatientadmittedtotheintens)

encode v227, gen(_PNEUMONIA_WO) 
encode v228, gen(_PNEUMONIA_WITH)
encode wasthepatientintubated, gen(INTUBATED)

replace INTUBATED = 1 if missing(wasthepatientintubated)
replace INTUBATED = . if HOSPITALIZED == 1

encode v230, gen(ARDS)
encode v231, gen(SEPTIC_SHOCK)
encode didthepatientrequirepressors, gen(VASOPRESSORS)

replace VASOPRESSORS = 1 if missing(didthepatientrequirepressors)
replace VASOPRESSORS = . if HOSPITALIZED == 1

encode cardiac, gen(CARDIAC_COMPL) 
encode cardiacarrest, gen(CARDIAC_ARREST) 
replace CARDIAC_ARREST = 1 if missing(CARDIAC_ARREST) 
replace CARDIAC_ARREST = . if missing(CARDIAC_COMPL) & CARDIAC_ARREST == 1
gen CARDIAC_OTHER = cond(!missing(othercardiaccomplication), 2, 1)
replace CARDIAC_OTHER = . if missing(CARDIAC_COMPL) & CARDIAC_OTHER == 1
label values CARDIAC_OTHER yes2no1
encode arrhythmia, gen(ARRHYTHMIA)
replace ARRHYTHMIA = 1 if missing(ARRHYTHMIA) & HOSPITALIZED == 2
encode myocardialinfarction, gen(MI)
replace MI = 1 if missing(MI)

encode centralnervoussystem, gen(CNS_MAIN)
gen CNS_OTHER = cond(!missing(othercns), 2, 1)
replace CNS_OTHER = . if missing(CNS_MAIN) & CNS_OTHER == 1
label values CNS_OTHER yes2no1

encode gastrointestinalandhepatobiliary, gen(GI)

encode pulmonaryembolismpe, gen(PE)
replace PE = 1 if missing(PE) & HOSPITALIZED == 2
encode venousthromboembolismbesidespe, gen(DVT)
replace DVT = 1 if missing(DVT) & HOSPITALIZED == 2

encode superimposedinfection, gen(SUPERIMP_INFECTION)
 

// change value label of the variables
foreach var of varlist TRANSPLANT HYPERT DIABETES OBESITY CVD /// 
	COPD ASTHMA LIVER_DIS CANCER HIV SLE RA ///
	SMOKER ///
	FEVER COUGH DYSPNEA FATIGUE MYALGIA GI_SYMPT ANOREXIA ///
	CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT ///
	AZATHIOPRINE MYCOPHENOLATE RITUXIMAB CNI ///
	_PNEUMONIA_WO _PNEUMONIA_WITH ARDS ///
	SEPTIC_SHOCK ///
	{
	* label list HYPERT
	label define `var' `="Checked":`var'' "Yes", modify
	label define `var' `="Unchecked":`var'' "No", modify
	 }

gen PNEUMONIA = cond((_PNEUMONIA_WO == 2 | _PNEUMONIA_WITH == 2), 1, 0)
label value PNEUMONIA yesno
replace PNEUMONIA = . if (missing(_PNEUMONIA_WO) & missing(_PNEUMONIA_WITH))

*-------------------------------------------------------------------------------
* END ENCODING CATEGORICAL STRING VARIATES AND GENERATING INDICATOR VARS
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
*  START CHECKING DUPLICATES AFTER ALLIGNMENT
*-------------------------------------------------------------------------------
cap drop dup
duplicates tag age GENDER RACE, gen(dup)
tab dup

duplicates tag recordid, gen(dup_id)
drop if dup_id !=0


drop if missing(age) & missing(GENDER)

*-------------------------------------------------------------------------------
*  END CHECKING DUPLICATES AFTER ALLIGNMENT
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* START CALCULATING CKD-EPI eGFR
*------------------------------------------------------------------------------- 

	gen eGFR_pre = .
	replace eGFR_pre=141*(serumcreatininepriortocovidinfec/0.9)^(cond(serumcreatininepriortocovidinfec<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_pre=141*1.018*(serumcreatininepriortocovidinfec/0.7)^(cond(serumcreatininepriortocovidinfec<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_pre=141*1.159*(serumcreatininepriortocovidinfec/0.9)^(cond(serumcreatininepriortocovidinfec<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_pre=141*1.018*1.159*(serumcreatininepriortocovidinfec/0.7)^(cond(serumcreatininepriortocovidinfec<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_pre "eGFR (mL/min/1.73m2) by CKD-EPI before COVID-19"
	
	gen time_eGFR_pre = - amountoftimeelapsedfromserumcrea

	
	gen eGFR_adm = .	
		replace eGFR_adm=141*(serumcreatinineonadmissionifhosp/0.9)^(cond(serumcreatinineonadmissionifhosp<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_adm=141*1.018*(serumcreatinineonadmissionifhosp/0.7)^(cond(serumcreatinineonadmissionifhosp<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_adm=141*1.159*(serumcreatinineonadmissionifhosp/0.9)^(cond(serumcreatinineonadmissionifhosp<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_adm=141*1.018*1.159*(serumcreatinineonadmissionifhosp/0.7)^(cond(serumcreatinineonadmissionifhosp<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_adm "eGFR (mL/min/1.73m2) by CKD-EPI on admission"
	
	gen time_eGFR_adm = 0
	
	
	gen eGFR_peak = .
		replace eGFR_peak=141*(peakserumcreatinineduringcovidin/0.9)^(cond(peakserumcreatinineduringcovidin<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_peak=141*1.018*(peakserumcreatinineduringcovidin/0.7)^(cond(peakserumcreatinineduringcovidin<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_peak=141*1.159*(peakserumcreatinineduringcovidin/0.9)^(cond(peakserumcreatinineduringcovidin<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_peak=141*1.018*1.159*(peakserumcreatinineduringcovidin/0.7)^(cond(peakserumcreatinineduringcovidin<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_peak "eGFR (mL/min/1.73m2) by CKD-EPI at peak SCr"
	
	gen time_eGFR_peak = timebetweencoviddiagnosisandpeak
	
	gen eGFR_post = .
	replace eGFR_post=141*(serumcreatinineaftercovidinfecti/0.9)^(cond(serumcreatinineaftercovidinfecti<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_post=141*1.018*(serumcreatinineaftercovidinfecti/0.7)^(cond(serumcreatinineaftercovidinfecti<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_post=141*1.159*(serumcreatinineaftercovidinfecti/0.9)^(cond(serumcreatinineaftercovidinfecti<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_post=141*1.018*1.159*(serumcreatinineaftercovidinfecti/0.7)^(cond(serumcreatinineaftercovidinfecti<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_post "eGFR (mL/min/1.73m2) by CKD-EPI after COVID-19"
	
	gen time_eGFR_post = timeelapsedfromcoviddiagnosistof
	
	
	
	
	gen eGFR_2ndfup = .
			replace eGFR_2ndfup=141*(serumcreatininefromadifferent2nd/0.9)^(cond(serumcreatininefromadifferent2nd<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_2ndfup=141*1.018*(serumcreatininefromadifferent2nd/0.7)^(cond(serumcreatininefromadifferent2nd<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_2ndfup=141*1.159*(serumcreatininefromadifferent2nd/0.9)^(cond(serumcreatininefromadifferent2nd<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_2ndfup=141*1.018*1.159*(serumcreatininefromadifferent2nd/0.7)^(cond(serumcreatininefromadifferent2nd<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_2ndfup "eGFR (mL/min/1.73m2) by CKD-EPI at 2nd Follow-up"
	
	gen time_eGFR_2ndfup = timingoflabs2ndsetrelativetocovi
	
	gen eGFR_3rdfup = .
			replace eGFR_3rdfup=141*(serumcreatininevalue3rdsetoflabs/0.9)^(cond(serumcreatininevalue3rdsetoflabs<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_3rdfup=141*1.018*(serumcreatininevalue3rdsetoflabs/0.7)^(cond(serumcreatininevalue3rdsetoflabs<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_3rdfup=141*1.159*(serumcreatininevalue3rdsetoflabs/0.9)^(cond(serumcreatininevalue3rdsetoflabs<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_3rdfup=141*1.018*1.159*(serumcreatininevalue3rdsetoflabs/0.7)^(cond(serumcreatininevalue3rdsetoflabs<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_3rdfup "eGFR (mL/min/1.73m2) by CKD-EPI at 3rd Follow-up"
	
	gen time_eGFR_3rdfup = timingoflabs3rdsetrelativetocovi
	

	
	gen eGFR_recent = .
			replace eGFR_recent=141*(mostrecentserumcreatinine/0.9)^(cond(mostrecentserumcreatinine<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race!=("Black or African American")
	replace eGFR_recent=141*1.018*(mostrecentserumcreatinine/0.7)^(cond(mostrecentserumcreatinine<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race!=("Black or African American")
	replace eGFR_recent=141*1.159*(mostrecentserumcreatinine/0.9)^(cond(mostrecentserumcreatinine<=0.9,-0.411, -1.209))*0.993^age if gender=="Male" & race==("Black or African American")
	replace eGFR_recent=141*1.018*1.159*(mostrecentserumcreatinine/0.7)^(cond(mostrecentserumcreatinine<=0.7,-0.329, -1.209))*0.993^age if gender=="Female" & race==("Black or African American")
	label var eGFR_recent "eGFR (mL/min/1.73m2) by CKD-EPI at most recent SCr"
	
	gen time_eGFR_recent = timingofserumcreatininerelativet
	

*-------------------------------------------------------------------------------
* END CALCULATING CKD-EPI eGFR
*------------------------------------------------------------------------------- 


*-------------------------------------------------------------------------------
* START CORRECTIONS OF ADDITIONAL NUMERICAL VARS
*-------------------------------------------------------------------------------


// added Nadir Serum Alb (whatwasthelowestvaluenadirofseru)- May 16, 2021
foreach var of varlist serumalbuminpriortoinfection ///
			serumalbuminduringcovidinfection ///
			whatwasthelowestvaluenadirofseru ///
			serumalbuminaftercovidinfection ///
			serumalbuminfromdifferent2ndtime ///
			serumalbuminvalue3rdsetoflabs ///
			mostrecentserumalbuminfromsameda  {
				replace  `var' =  `var' / 10 if  `var' >= 7 & !missing(`var')
				 }


gen RRT_DURATION = durationofrrt
label var RRT_DURATION "Number of days on RRT during COVID-19 infection"

gen byte RRT_AT_DISCHARGE = (isthepatientstillrequiringrenalr == "Yes")
// check if reasonable (May 16, 2021)
replace RRT_AT_DISCHARGE = . if RRT_YESNO == 0

gen TIME_TO_DEATH = ifdeathdayssincediagnosis
label var TIME_TO_DEATH "Number of days from admission to death"
gen LOS = ifhospitalizedlengthofhospitalst
label var LOS "Length of Stay (days)"

*-------------------------------------------------------------------------------
* END CORRECTIONS OF ADDITIONAL NUMERICAL VARS
*-------------------------------------------------------------------------------


* preliminary saving of wide dataset
cap save all_vars_iroc_gn_creat_wide, replace



*-------------------------------------------------------------------------------
* START CREATING LONGITUDINAL DATASET
*-------------------------------------------------------------------------------
keep recordid doesthepatienthaveglomerulardise ///
	age gender race ethnicity ///
	estimatedgfrpriortocovidinfectio estimatedgfraftercovidinfection ///
	serumcreatininepriortocovidinfec amountoftimeelapsedfromserumcrea ///
	hiddenserumcreatininenormalizedt serumcreatinineaftercovidinfecti ///
	timeelapsedfromcoviddiagnosistof hiddennormalizedserumcreatininet ///
	whenwasthisserumcreatininelastc dateofserumcreatininelabafterco ///
	dateofcoviddiagnosi  whenwasthisserumcreatininelastc ///
	whenwasthelasttimeyoucheckedrel dateofinitialsetoflabsafterdiag ///
	dateoffollowupclosetohospitaldi dateofserumcreatininelabafterco ///
	serumcreatinineonadmissionifhosp peakserumcreatinineduringcovidin /// 
	timebetweencoviddiagnosisandpeak  numberofdaystopeakserumcreatinin ///
	mostrecentserumcreatinine timingofserumcreatininerelativet ///
	serumcreatininefromadifferent2nd serumcreatininevalue3rdsetoflabs ///
	serumalbuminpriortoinfection serumalbuminduringcovidinfection ///
	serumalbuminaftercovidinfection mostrecentserumalbuminfromsameda ///
	serumalbuminfromdifferent2ndtime serumalbuminvalue3rdsetoflabs ///
	eGFR_* age GENDER RACE ETHNICITY NON_WHITE GNDISEASE PRE_EX_GN GLOM_DIS_DIAG DURATION_GN ///
	HEMAT_PRE HEMAT_ADM HEMAT_POST ///
	TRANSPLANT HYPERT DIABETES OBESITY CVD ASTHMA LIVER_DIS CANCER HIV ///
	SLE RA SMOKER ACEI ///
	IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE ///
	FEVER COUGH DYSPNEA FATIGUE MYALGIA GI_SYMPT ANOREXIA ///
	CHILLS NASALCONG SORETHROAT ANOSMIA NEUR_SYMPT ///
	HOSPITALIZED  DEATH
cap save _iroc_gn_creat_wide, replace 
restore


preserve
use all_vars_iroc_gn_creat_wide, clear
rename eGFR_pre	g1
rename serumcreatininepriortocovidinfec cr1
rename time_eGFR_pre	t1

rename eGFR_adm	g2
rename serumcreatinineonadmissionifhosp cr2
rename time_eGFR_adm	t2

rename eGFR_peak	g3
rename peakserumcreatinineduringcovidin cr3
rename time_eGFR_peak	t3

rename eGFR_post	g4
rename serumcreatinineaftercovidinfecti cr4
rename time_eGFR_post	t4

rename eGFR_2ndfup	g5
rename serumcreatininefromadifferent2nd cr5
rename time_eGFR_2ndfup	t5

rename eGFR_3rdfup	g6
rename serumcreatininevalue3rdsetoflabs cr6
rename time_eGFR_3rdfup	t6

rename eGFR_recent	g7
rename mostrecentserumcreatinine cr7
rename time_eGFR_recent	t7



rename serumalbuminpriortoinfection alb_1
rename serumalbuminduringcovidinfection alb_2 
rename whatwasthelowestvaluenadirofseru alb_3
rename serumalbuminaftercovidinfection alb_4
rename serumalbuminfromdifferent2ndtime alb_5
rename serumalbuminvalue3rdsetoflabs alb_6
rename mostrecentserumalbuminfromsameda alb_7


rename proteinuriaquantifiedpriortocovi  uprot_1
rename peakproteinuriaquantifiedduringc  uprot_2
rename proteinuriaquantifiedaftercovidi  uprot_4 


order recordid g* cr* alb_* uprot_* t*  
keep recordid g* cr* alb_* uprot_* t* GNDISEASE DEATH HOSPITALIZED AKI* ACTIVE_GN RRT age GENDER RACE ETHNICITY NON_WHITE GLOM_DIS_DIAG DURATION_GN /// 
	HEMAT_PRE HYPERT DIABETES OBESITY CVD COPD ASTHMA LIVER_DIS ///
	CANCER HIV RA SLE SMOKER ACEI ///
	IgAN_HSP VASCULITIS FSGS_MCD MN GN_SLE EITHER_VASC_SLE ///
	FEVER COUGH DYSPNEA FATIGUE MYALGIA ///
	GI_SYMPT ANOREXIA CHILLS NASALCONG SORETHROAT ANOSMIA  NEUR_SYMPT ///
	wbc lymphocytecountabsolute absoluteneutrophilcount hemoglobin ///
	platelet ferritin creactiveprotein ddimer  ///
	followuptimepointsincecovid19dia ///
	MYCOPHENOLATE RITUXIMAB CNI AZATHIOPRINE STEROIDS
reshape long g cr alb_ uprot_ t, i(recordid)
rename _j TIME
rename g eGFR
sort recordid TIME
cap save long_with_time, replace

*--------------------------------------------------------------------------------
* START Major addition June 21, 2021: add missing time points and drop duplicates
*--------------------------------------------------------------------------------
clear
import excel "C:\Users\Pc\Box\Waldman Projects\missing_or_duplicated_timepoints_PC.xls", sheet("Sheet1") firstrow
drop PRECOVID
sort recordid TIME
cap save missing_or_duplicated_timepoints_PC, replace
merge 1:1 recordid TIME using long_with_time
replace t = day if missing(t) & !missing(day)
drop day


bysort recordid (t): gen     base_eGFR =  eGFR[1]
bysort recordid (t): replace base_eGFR = eGFR[_n-1] if missing(eGFR)

bysort recordid (t): gen     base_uprot_ =  uprot_[1]
bysort recordid (t): replace base_uprot_ = uprot_[_n-1] if missing(uprot_)

bysort recordid (t): gen     base_alb_ =  alb_[1]
bysort recordid (t): replace base_alb_ = alb_[_n-1] if missing(alb_)


label define TIME 1 "PRE-COVID" 2 "ADMISSION" 3 "PEAK" 4 "AFTER" 5 "2nd F-up" 6 "3rd F-up" 7 "MOST RECENT"
label values TIME TIME
gen month =  t / 30.4375
sort recordid month

// Major Change June 21, 2021
cap drop dup
duplicates tag recordid month eGFR if (!missing(eGFR) & !missing(month)), gen(dup)
sort recordid TIME
list recordid TIME month eGFR if (dup!=0 &(!missing(eGFR) & !missing(month))) , sepby(recordid) noobs nolab
duplicates drop recordid month eGFR if (!missing(eGFR) & !missing(month)), force

* preliminary saving of longitudinal dataset
cap save long_with_time, replace
restore

*--------------------------------------------------------------------------------
* END Major addition June 21, 2021: add missing time points and drop duplicates
*-------------------------------------------------------------------------------- 


*-------------------------------------------------------------------------------
* END CREATING LONGITUDINAL DATASET
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
*  start checking deaths (after allignment issues) 
*-------------------------------------------------------------------------------
cd "C:\Users\Pc\Box\Waldman Projects"
use all_vars_iroc_gn_creat_wide, clear
foreach num of numlist 8 9 12 19 20 28 34 42 169 171 176 179 181 206 ///
	50 65 75 85 98 102 111 128 195 235  {
        di in ye "-----------> recordid: `num'"
		list DEATH_YESNO if recordid ==  `num', noobs ab(12)
		 }
		
*-------------------------------------------------------------------------------
*  end checking deaths (after allignment issues) 
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
* Start additional changes on the long. dataset (includes drop duplicates)
*-------------------------------------------------------------------------------
use long_with_time, clear
drop if GNDISEASE == 1 & HOSPITALIZED ==1
gen GROUP = .
replace GROUP = 1 if GNDISEASE == 1
replace GROUP = 2 if GNDISEASE == 2 & HOSPITALIZED == 2
replace GROUP = 3 if GNDISEASE == 2 & HOSPITALIZED == 1
label define GROUP 1 "Ctrl - Hospitalized" 2 "GN - Hospitalized" 3 "GN - Outpatients"
label values GROUP GROUP
*-------------------------------------------------------------------------------
* End additional changes on the long. dataset (includes drop duplicates)
*-------------------------------------------------------------------------------
**# SAVE LONGITUDINAL DATASET
save long_with_time, replace


* check duplicates in an external file 
use long_with_time, clear
sort recordid TIME
cap drop dup
duplicates tag recordid month, gen(dup)
preserve 
drop if missing(eGFR)
keep if dup != 0
rename month day
drop dup
export excel recordid TIME using "C:\Users\Pc\Box\Waldman Projects\missing_or_duplicated_month.xls", firstrow(variables) nolabel replace
restore




*-------------------------------------------------------------------------------
* Start additional changes on the wide  dataset 
*-------------------------------------------------------------------------------
**# second preliminary saving of the wide dataset before additional changes
use all_vars_iroc_gn_creat_wide, clear
drop if GNDISEASE == 1 & HOSPITALIZED ==1
gen GROUP = .
replace GROUP = 1 if GNDISEASE == 1
replace GROUP = 2 if GNDISEASE == 2 & HOSPITALIZED == 2
replace GROUP = 3 if GNDISEASE == 2 & HOSPITALIZED == 1
label define GROUP 1 "Ctrl - Hospitalized" 2 "GN - Hospitalized" 3 "GN - Outpatients"
label values GROUP GROUP
*-------------------------------------------------------------------------------
* End additional changes on the wide  dataset
*-------------------------------------------------------------------------------	 
**# SAVE WIDE SET
save all_vars_iroc_gn_creat_wide, replace



// Major changed June 21, 2021
// still to remove the following duplicates in the wide file for consistency 
// with the long file (in which the duplicates were remoned on June 21, 2021)
/*

  +---------------------------------------+
  | recordid   TIME      month       eGFR |
  |---------------------------------------|
  |      112      2          0   105.4586 |
  |      112      7          0   105.4586 |
  |---------------------------------------|
  |      143      2          0   10.87033 |
  |      143      3          0   10.87033 |
  |      143      4   .9856263   17.03693 |
  |      143      6   .9856263   17.03693 |
  |---------------------------------------|
  |      160      3   .6570842   7.848947 |
  |      160      4   .6570842   7.848947 |
  |---------------------------------------|
  |      173      2          0   28.29119 |
  |      173      3          0   28.29119 |
  |---------------------------------------|
  |      175      4   1.051335   121.7706 |
  |      175      7   1.051335   121.7706 |
  |---------------------------------------|
  |      176      3     .62423   24.40716 |
  |      176      7     .62423   24.40716 |
  |---------------------------------------|
  |      178      2          0   57.66981 |
  |      178      3          0   57.66981 |
  |---------------------------------------|
  |      179      3   2.529774   92.85821 |
  |      179      4   2.529774   92.85821 |
  |---------------------------------------|
  |      181      3   .3285421   4.840813 |
  |      181      7   .3285421   4.840813 |
  |---------------------------------------|
  |      184      4   4.829569   94.46082 |
  |      184      7   4.829569   94.46082 |
  |---------------------------------------|
  |      188      4   4.829569   81.64633 |
  |      188      5   4.829569   81.64633 |
  |---------------------------------------|
  |      189      4   3.449692   95.05386 |
  |      189      7   3.449692   95.05386 |
  |---------------------------------------|
  |      190      2          0     67.414 |
  |      190      3          0     67.414 |
  |---------------------------------------|
  |      198      2          0   34.44392 |
  |      198      3          0   34.44392 |
  |---------------------------------------|
  |      199      2          0   42.82951 |
  |      199      3          0   42.82951 |
  |---------------------------------------|
  |      200      4   1.379877    74.9191 |
  |      200      5   1.379877    74.9191 |
  |---------------------------------------|
  |      202      4   1.445585   78.35364 |
  |      202      5   1.445585   78.35364 |
  |---------------------------------------|
  |      209      4   3.383984   64.98684 |
  |      209      7   3.383984   64.98684 |
  |---------------------------------------|
  |      212      2          0   86.75809 |
  |      212      3          0   86.75809 |
  |      212      4   1.215606    109.267 |
  |      212      5   1.215606    109.267 |
  |---------------------------------------|
  |      223      4   .6899384   93.35253 |
  |      223      7   .6899384   93.35253 |
  |---------------------------------------|
  |      230      2          0   43.09606 |
  |      230      3          0   43.09606 |
  |---------------------------------------|
  |      232      4   1.905544   120.9182 |
  |      232      7   1.905544   120.9182 |
  |---------------------------------------|
  |      233      3   1.018481   31.77046 |
  |      233      4   1.018481   31.77046 |
  |---------------------------------------|
  |      236      3   1.117043   52.69367 |
  |      236      4   1.117043   52.69367 |
  |---------------------------------------|
  |      245      2          0   35.54935 |
  |      245      3          0   35.54935 |
  |---------------------------------------|
  |      246      3   .1314168   23.87157 |
  |      246      4   .1314168   23.87157 |
  +---------------------------------------+
*/



////////////////////////////////////////////////////////////////////////////////
**#                END PREPARING DATASET ///////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**#  START STUDY POPULATION
////////////////////////////////////////////////////////////////////////////////
**# USE WIDE DATSET
cd "C:\Users\Pc\Box\Waldman Projects"
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
cd "C:\Users\Pc\Box\Waldman Projects"
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

cd "C:\Users\Pc\Box\Waldman Projects"
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

cd "C:\Users\Pc\Box\Waldman Projects"
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


cd "C:\Users\Pc\Box\Waldman Projects"
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

cd "C:\Users\Pc\Box\Waldman Projects"
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

cd "C:\Users\Pc\Box\Waldman Projects"
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

cd "C:\Users\Pc\Box\Waldman Projects"
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
cd "C:\Users\Pc\Box\Waldman Projects"
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
 cd "C:\Users\Pc\Box\Waldman Projects"
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
  cd "C:\Users\Pc\Box\Waldman Projects" 
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
  
  cd "C:\Users\Pc\Box\Waldman Projects"
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




  cd "C:\Users\Pc\Box\Waldman Projects"
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


cd "C:\Users\Pc\Box\Waldman Projects"
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


cd "C:\Users\Pc\Box\Waldman Projects"
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

cd "C:\Users\Pc\Box\Waldman Projects"
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

  cd "C:\Users\Pc\Box\Waldman Projects"
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
 
log close
cap translate "C:\Users\Pc\Box\Waldman Projects\analysis iROC-GN `c(current_date)'.smcl" "C:\Users\Pc\Box\Waldman Projects\analysis iROC-GN `c(current_date)'.pdf", replace
exit