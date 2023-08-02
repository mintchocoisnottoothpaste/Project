libname raw "/userdata20/rroom029/data_source/user_data";/*raw data*/
libname db "/userdata20/rroom029/data_out/data_store"; 
libname db_out "/userdata20/rroom029/data_out/data_out";


/*2008-2020년 진단받은 폐암환자의 진단 이후 장기요양보험 신청(2008-2020년) 및 인정율 */
PROC SQL; CREATE TABLE TOTAL AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0
GROUP BY SEX_TYPE ; QUIT; 

PROC SQL; CREATE TABLE APLY_AFTER AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND C_DATE<=APLY_YMD AND PRE_C=0
GROUP BY SEX_TYPE ; QUIT; 


PROC SQL; CREATE TABLE APLY_AFTER_GRADE AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND C_DATE<=APLY_YMD AND PRE_C=0 AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6')
GROUP BY SEX_TYPE ; QUIT; 



/*2008-2020년 진단받은 폐암환자의 장기요양보험 신청(2008-2020년) 및 인정율 */
PROC SQL; CREATE TABLE NOT_APLY AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND INDI_SEQ IS  NULL AND PRE_C=0
GROUP BY SEX_TYPE ; QUIT; 

PROC SQL; CREATE TABLE APLY AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND INDI_SEQ IS NOT NULL AND PRE_C=0
GROUP BY SEX_TYPE ; QUIT; 

PROC SQL; CREATE TABLE APLY_GRADE AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND INDI_SEQ IS NOT NULL AND PRE_C=0 AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6')
GROUP BY SEX_TYPE ; QUIT; 



/*암 진단 시 연령 별 진단 당시(해당 월) 노인장기요양 신청율(%) */
PROC SQL; CREATE TABLE C_AGEGRP AS
SELECT DISTINCT SEX_TYPE, CASE WHEN 65<=C_AGE<70 THEN '65-69'
												  WHEN 70<=C_AGE<75 THEN '70-74'
												  WHEN 75<=C_AGE<80 THEN '75-79'
												  WHEN 80<=C_AGE<85 THEN '80-84'
												  WHEN 85<=C_AGE THEN '85+' END AS AGE_GRP,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 
GROUP BY SEX_TYPE, AGE_GRP
; QUIT; 

PROC SQL; CREATE TABLE MONTHAPLY_C_AGEGRP AS
SELECT DISTINCT SEX_TYPE, CASE WHEN 65<=C_AGE<70 THEN '65-69'
												  WHEN 70<=C_AGE<75 THEN '70-74'
												  WHEN 75<=C_AGE<80 THEN '75-79'
												  WHEN 80<=C_AGE<85 THEN '80-84'
												  WHEN 85<=C_AGE THEN '85+' END AS AGE_GRP,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND APLY_YMD2<= C_DATE<=APLY_YMD AND PRE_C=0 
GROUP BY SEX_TYPE, AGE_GRP
; QUIT; 


/*2008-2019년 진단년도 별 고령암환자의 노인장기요양보험 신청율*/
/*암 진단년도 별 고령암환자의 암 진단 당시(해당 월) 노인장기요양 신청율(%)*/
PROC SQL; CREATE TABLE C_YEAR AS
SELECT DISTINCT SEX_TYPE, C_YEAR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 
GROUP BY SEX_TYPE, C_YEAR
; QUIT; 
PROC SQL; CREATE TABLE MONTHAPLY_C_YEAR AS
SELECT DISTINCT SEX_TYPE, C_YEAR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND APLY_YMD2<= C_DATE<=APLY_YMD AND PRE_C=0 
GROUP BY SEX_TYPE, C_YEAR
; QUIT; 


/*암진단 이후 생존기간 별 고령암환자의 노인장기요양 신청율(%) */
PROC SQL; CREATE TABLE SURVYR AS
SELECT DISTINCT SEX_TYPE, CASE WHEN C_DATE<=DTH_DATE<INTNX('YEAR',C_DATE,1,'S') THEN '0-1년'
						  WHEN INTNX('YEAR',C_DATE,1,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,2,'S') THEN '1-2년'
						  WHEN INTNX('YEAR',C_DATE,2,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,3,'S') THEN '2-3년'
						  WHEN INTNX('YEAR',C_DATE,3,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,4,'S') THEN '3-4년'
						  WHEN INTNX('YEAR',C_DATE,4,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,5,'S') THEN '4-5년'
						  WHEN INTNX('YEAR',C_DATE,5,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,6,'S') THEN '5-6년'
						  WHEN INTNX('YEAR',C_DATE,6,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,7,'S') THEN '6-7년'
						  WHEN INTNX('YEAR',C_DATE,7,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,8,'S') THEN '7-8년'
						  WHEN INTNX('YEAR',C_DATE,8,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,9,'S') THEN '8-9년'
						  WHEN INTNX('YEAR',C_DATE,9,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,10,'S') THEN '9-10년'
						  WHEN INTNX('YEAR',C_DATE,10,'S')<=DTH_DATE THEN '10+년' END AS SURVYR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND DTH_DATE IS NOT NULL AND C_DATE<=DTH_DATE
GROUP BY SEX_TYPE, SURVYR
; QUIT; 


/*진단이후 신청률*/
PROC SQL; CREATE TABLE APLYAFTER_SURVYR AS
SELECT DISTINCT SEX_TYPE, CASE WHEN C_DATE<=DTH_DATE<INTNX('YEAR',C_DATE,1,'S') THEN '0-1년'
						  WHEN INTNX('YEAR',C_DATE,1,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,2,'S') THEN '1-2년'
						  WHEN INTNX('YEAR',C_DATE,2,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,3,'S') THEN '2-3년'
						  WHEN INTNX('YEAR',C_DATE,3,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,4,'S') THEN '3-4년'
						  WHEN INTNX('YEAR',C_DATE,4,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,5,'S') THEN '4-5년'
						  WHEN INTNX('YEAR',C_DATE,5,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,6,'S') THEN '5-6년'
						  WHEN INTNX('YEAR',C_DATE,6,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,7,'S') THEN '6-7년'
						  WHEN INTNX('YEAR',C_DATE,7,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,8,'S') THEN '7-8년'
						  WHEN INTNX('YEAR',C_DATE,8,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,9,'S') THEN '8-9년'
						  WHEN INTNX('YEAR',C_DATE,9,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,10,'S') THEN '9-10년'
						  WHEN INTNX('YEAR',C_DATE,10,'S')<=DTH_DATE THEN '10+년' END AS SURVYR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND DTH_DATE IS NOT NULL AND C_DATE<=APLY_YMD AND C_DATE<=DTH_DATE
GROUP BY SEX_TYPE, SURVYR
; QUIT; 

/*암진단년도 별 진단 당시(해당월) 신청된 노인장기요양 서비스 인정자의 인정등급 분포 */
PROC SQL; CREATE TABLE MONTHAPLY_C_YEAR_GRADE AS
SELECT DISTINCT SEX_TYPE, C_YEAR, GJU_RCGT_GRADE_CD, 
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND APLY_YMD2<= C_DATE<=APLY_YMD AND PRE_C=0 AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6')
GROUP BY SEX_TYPE, C_YEAR, GJU_RCGT_GRADE_CD
ORDER BY SEX_TYPE, GJU_RCGT_GRADE_CD
; QUIT; 

/*암진단 이후 신청년도 별 고령암환자의 노인장기요양 인정자의 인정등급 분포*/
PROC SQL; CREATE TABLE APLYYEAR_GRADE AS
SELECT DISTINCT SEX_TYPE, GJU_RCGT_GRADE_CD,
CASE WHEN C_DATE<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,1,'S') THEN '0-1년'
WHEN INTNX('YEAR',C_DATE,1,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,2,'S') THEN '1-2년'
WHEN INTNX('YEAR',C_DATE,2,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,3,'S') THEN '2-3년'
WHEN INTNX('YEAR',C_DATE,3,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,4,'S') THEN '3-4년'
WHEN INTNX('YEAR',C_DATE,4,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,5,'S') THEN '4-5년'
WHEN INTNX('YEAR',C_DATE,5,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,6,'S') THEN '5-6년'
WHEN INTNX('YEAR',C_DATE,6,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,7,'S') THEN '6-7년'
WHEN INTNX('YEAR',C_DATE,7,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,8,'S') THEN '7-8년'
WHEN INTNX('YEAR',C_DATE,8,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,9,'S') THEN '8-9년'
WHEN INTNX('YEAR',C_DATE,9,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,10,'S') THEN '9-10년'
WHEN INTNX('YEAR',C_DATE,10,'S')<=APLY_YMD THEN '10+년' END AS APLYYR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND DTH_DATE IS NOT NULL AND C_DATE<=APLY_YMD AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6')
GROUP BY SEX_TYPE, APLYYR,GJU_RCGT_GRADE_CD
ORDER BY SEX_TYPE, GJU_RCGT_GRADE_CD
; QUIT; 


/*2010년 이후 조건 추가*/

/*2010-2020년 진단받은 폐암환자의 진단 이후 장기요양보험 신청(2010-2020년) 및 인정율 */
PROC SQL; CREATE TABLE TOTAL AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND C_YEAR>=2010
GROUP BY SEX_TYPE ; QUIT; 

PROC SQL; CREATE TABLE APLY_AFTER AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND C_DATE<=APLY_YMD AND PRE_C=0 AND C_YEAR>=2010
GROUP BY SEX_TYPE ; QUIT; 


PROC SQL; CREATE TABLE APLY_AFTER_GRADE AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND C_DATE<=APLY_YMD AND PRE_C=0 AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6') AND C_YEAR>=2010
GROUP BY SEX_TYPE ; QUIT; 



/*2010-2020년 진단받은 폐암환자의 장기요양보험 신청(2010-2020년) 및 인정율 */
PROC SQL; CREATE TABLE NOT_APLY AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND INDI_SEQ IS  NULL AND PRE_C=0 AND C_YEAR>=2010
GROUP BY SEX_TYPE ; QUIT; 

PROC SQL; CREATE TABLE APLY AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND INDI_SEQ IS NOT NULL AND PRE_C=0 AND C_YEAR>=2010
GROUP BY SEX_TYPE ; QUIT; 

PROC SQL; CREATE TABLE APLY_GRADE AS
SELECT DISTINCT SEX_TYPE, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND INDI_SEQ IS NOT NULL AND PRE_C=0 AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6') AND C_YEAR>=2010
GROUP BY SEX_TYPE ; QUIT; 



/*암 진단 시 연령 별 진단 당시(해당 월) 노인장기요양 신청율(%) */
PROC SQL; CREATE TABLE C_AGEGRP AS
SELECT DISTINCT SEX_TYPE, CASE WHEN 65<=C_AGE<70 THEN '65-69'
												  WHEN 70<=C_AGE<75 THEN '70-74'
												  WHEN 75<=C_AGE<80 THEN '75-79'
												  WHEN 80<=C_AGE<85 THEN '80-84'
												  WHEN 85<=C_AGE THEN '85+' END AS AGE_GRP,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0  AND C_YEAR>=2010
GROUP BY SEX_TYPE, AGE_GRP
; QUIT; 

PROC SQL; CREATE TABLE MONTHAPLY_C_AGEGRP AS
SELECT DISTINCT SEX_TYPE, CASE WHEN 65<=C_AGE<70 THEN '65-69'
												  WHEN 70<=C_AGE<75 THEN '70-74'
												  WHEN 75<=C_AGE<80 THEN '75-79'
												  WHEN 80<=C_AGE<85 THEN '80-84'
												  WHEN 85<=C_AGE THEN '85+' END AS AGE_GRP,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND APLY_YMD2<= C_DATE<=APLY_YMD AND PRE_C=0  AND C_YEAR>=2010
GROUP BY SEX_TYPE, AGE_GRP
; QUIT; 


/*2010-2019년 진단년도 별 고령암환자의 노인장기요양보험 신청율*/
/*암 진단년도 별 고령암환자의 암 진단 당시(해당 월) 노인장기요양 신청율(%)*/
PROC SQL; CREATE TABLE C_YEAR AS
SELECT DISTINCT SEX_TYPE, C_YEAR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0  AND C_YEAR>=2010
GROUP BY SEX_TYPE, C_YEAR
; QUIT; 
PROC SQL; CREATE TABLE MONTHAPLY_C_YEAR AS
SELECT DISTINCT SEX_TYPE, C_YEAR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND APLY_YMD2<= C_DATE<=APLY_YMD AND PRE_C=0  AND C_YEAR>=2010
GROUP BY SEX_TYPE, C_YEAR
; QUIT; 


/*암진단 이후 생존기간 별 고령암환자의 노인장기요양 신청율(%) */
PROC SQL; CREATE TABLE SURVYR AS
SELECT DISTINCT SEX_TYPE, CASE WHEN C_DATE<=DTH_DATE<INTNX('YEAR',C_DATE,1,'S') THEN '0-1년'
						  WHEN INTNX('YEAR',C_DATE,1,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,2,'S') THEN '1-2년'
						  WHEN INTNX('YEAR',C_DATE,2,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,3,'S') THEN '2-3년'
						  WHEN INTNX('YEAR',C_DATE,3,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,4,'S') THEN '3-4년'
						  WHEN INTNX('YEAR',C_DATE,4,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,5,'S') THEN '4-5년'
						  WHEN INTNX('YEAR',C_DATE,5,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,6,'S') THEN '5-6년'
						  WHEN INTNX('YEAR',C_DATE,6,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,7,'S') THEN '6-7년'
						  WHEN INTNX('YEAR',C_DATE,7,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,8,'S') THEN '7-8년'
						  WHEN INTNX('YEAR',C_DATE,8,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,9,'S') THEN '8-9년'
						  WHEN INTNX('YEAR',C_DATE,9,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,10,'S') THEN '9-10년'
						  WHEN INTNX('YEAR',C_DATE,10,'S')<=DTH_DATE THEN '10+년' END AS SURVYR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND DTH_DATE IS NOT NULL AND C_YEAR>=2010 AND C_DATE<=DTH_DATE
GROUP BY SEX_TYPE, SURVYR
; QUIT; 
/*진단이후 신청률*/
PROC SQL; CREATE TABLE APLYAFTER_SURVYR AS
SELECT DISTINCT SEX_TYPE, CASE WHEN C_DATE<=DTH_DATE<INTNX('YEAR',C_DATE,1,'S') THEN '0-1년'
						  WHEN INTNX('YEAR',C_DATE,1,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,2,'S') THEN '1-2년'
						  WHEN INTNX('YEAR',C_DATE,2,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,3,'S') THEN '2-3년'
						  WHEN INTNX('YEAR',C_DATE,3,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,4,'S') THEN '3-4년'
						  WHEN INTNX('YEAR',C_DATE,4,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,5,'S') THEN '4-5년'
						  WHEN INTNX('YEAR',C_DATE,5,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,6,'S') THEN '5-6년'
						  WHEN INTNX('YEAR',C_DATE,6,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,7,'S') THEN '6-7년'
						  WHEN INTNX('YEAR',C_DATE,7,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,8,'S') THEN '7-8년'
						  WHEN INTNX('YEAR',C_DATE,8,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,9,'S') THEN '8-9년'
						  WHEN INTNX('YEAR',C_DATE,9,'S')<=DTH_DATE<INTNX('YEAR',C_DATE,10,'S') THEN '9-10년'
						  WHEN INTNX('YEAR',C_DATE,10,'S')<=DTH_DATE THEN '10+년' END AS SURVYR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND DTH_DATE IS NOT NULL AND C_DATE<=APLY_YMD AND C_YEAR>=2010 AND C_DATE<=DTH_DATE
GROUP BY SEX_TYPE, SURVYR
; QUIT; 

/*암진단년도 별 진단 당시(해당월) 신청된 노인장기요양 서비스 인정자의 인정등급 분포 */
PROC SQL; CREATE TABLE MONTHAPLY_C_YEAR_GRADE AS
SELECT DISTINCT SEX_TYPE, C_YEAR, GJU_RCGT_GRADE_CD, 
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND APLY_YMD2<= C_DATE<=APLY_YMD AND PRE_C=0 AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6') AND C_YEAR>=2010
GROUP BY SEX_TYPE, C_YEAR, GJU_RCGT_GRADE_CD
ORDER BY SEX_TYPE, GJU_RCGT_GRADE_CD
; QUIT; 

/*암진단 이후 신청년도 별 고령암환자의 노인장기요양 인정자의 인정등급 분포*/
PROC SQL; CREATE TABLE APLYYEAR_GRADE AS
SELECT DISTINCT SEX_TYPE, GJU_RCGT_GRADE_CD,
CASE WHEN C_DATE<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,1,'S') THEN '0-1년'
WHEN INTNX('YEAR',C_DATE,1,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,2,'S') THEN '1-2년'
WHEN INTNX('YEAR',C_DATE,2,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,3,'S') THEN '2-3년'
WHEN INTNX('YEAR',C_DATE,3,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,4,'S') THEN '3-4년'
WHEN INTNX('YEAR',C_DATE,4,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,5,'S') THEN '4-5년'
WHEN INTNX('YEAR',C_DATE,5,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,6,'S') THEN '5-6년'
WHEN INTNX('YEAR',C_DATE,6,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,7,'S') THEN '6-7년'
WHEN INTNX('YEAR',C_DATE,7,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,8,'S') THEN '7-8년'
WHEN INTNX('YEAR',C_DATE,8,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,9,'S') THEN '8-9년'
WHEN INTNX('YEAR',C_DATE,9,'S')<=APLY_YMD AND APLY_YMD2<INTNX('YEAR',C_DATE,10,'S') THEN '9-10년'
WHEN INTNX('YEAR',C_DATE,10,'S')<=APLY_YMD THEN '_10+년' END AS APLYYR,
			COUNT(DISTINCT INDI_DSCM_NO) AS CNT 
FROM DB.TARGET_LTC_TBNYBASE
WHERE C_AGE>=65 AND PRE_C=0 AND DTH_DATE IS NOT NULL AND C_DATE<=APLY_YMD AND GJU_RCGT_GRADE_CD IN ('1','2','3','4','5','6') AND C_YEAR>=2010 AND C_DATE<=DTH_DATE
GROUP BY SEX_TYPE, APLYYR,GJU_RCGT_GRADE_CD
ORDER BY SEX_TYPE, GJU_RCGT_GRADE_CD
; QUIT; 
