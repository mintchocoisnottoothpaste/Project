libname raw "/userdata20/rroom029/data_source/user_data";/*raw data*/
libname db "/userdata20/rroom029/data_out/data_store"; 
libname db_out "/userdata20/rroom029/data_out/data_out";

/**************요양병원 이용************/
%MACRO YEARLY; %DO YEAR=2008 %TO 2020;
PROC SQL; CREATE TABLE CMN_ADJ_&YEAR. AS SELECT CMN_KEY, T30_DATE, LTC_ADJ FROM DB.TARGET_T30_LTC2YR_&YEAR.
WHERE LTC_ADJ^=''; QUIT;
PROC SQL; CREATE TABLE TARGET_ADJ_&YEAR. AS SELECT A.INDI_DSCM_NO, A.C_DATE, A.CMN_KEY, B.T30_DATE, A.MDCARE_DD_CNT, B.LTC_ADJ
FROM DB.TARGET_T20_&YEAR. AS A INNER JOIN CMN_ADJ_&YEAR. AS B ON A.CMN_KEY=B.CMN_KEY; QUIT;
%END; %MEND YEARLY; %YEARLY;

DATA DB.TARGET_ADJ_2YR; SET TARGET_ADJ_2008-TARGET_ADJ_2020; RUN;

/*치료 종료일 진단 2년 이후면 진단 2년째 날을 종료일로*/
DATA TARGET_ADJ; SET DB.TARGET_ADJ_2YR; 
FORMAT T30_END_DT yymmdd10. ADJ2YR_END_DT yymmdd10. ;
T30_END_DT = T30_DATE+MDCARE_DD_CNT;
IF T30_END_DT >= C_DATE+(365*2) THEN ADJ2YR_END_DT = C_DATE+(365*2);
ELSE ADJ2YR_END_DT = T30_END_DT;RUN;


/****** 시설 및 재가 이용 ******/
PROC SQL; CREATE TABLE DB.TARGET_LTC_2YR_TBNYPAY AS
SELECT DISTINCT A.*, B.INDI_SEQ, B.LTCP_TYPE_CD, INPUT(B.LTCP_FR_DT,YYMMDD10.) AS LTC_STRT_DT, B.EADJ_TOTAL_PAY_DCNT
FROM DB.TARGET_IDBFCDTH AS A
LEFT JOIN RAW.RVSN_HFVT_TBNYPAY AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
WHERE B.LTCP_TYPE_CD IN ('1','2') AND C_DATE <= INPUT(B.LTCP_FR_DT,YYMMDD10.)  <= C_DATE+(365*2)
; QUIT;

DATA TARGET_LTC; SET DB.TARGET_LTC_2YR_TBNYPAY;
FORMAT LTC_STRT_DT yymmdd10. LTC_END_DT yymmdd10. LTC2YR_END_DT yymmdd10. ;
LTC_END_DT = LTC_STRT_DT + EADJ_TOTAL_PAY_DCNT;
IF LTC_END_DT >= C_DATE+(365*2) THEN LTC2YR_END_DT = C_DATE+(365*2);
ELSE LTC2YR_END_DT = LTC_END_DT; RUN;


/*인당 이용기간(MAX-MIN) 구하기*/
PROC SQL; CREATE TABLE ADJ_INDI AS SELECT INDI_DSCM_NO, MIN(T30_DATE) AS MIN_DT FORMAT=yymmdd10., 
MAX(ADJ2YR_END_DT) AS MAX_DT FORMAT=yymmdd10., MAX(ADJ2YR_END_DT)- MIN(T30_DATE) AS ADJ_DD
FROM TARGET_ADJ GROUP BY INDI_DSCM_NO ORDER BY INDI_DSCM_NO; QUIT;

PROC SQL; CREATE TABLE INS_INDI AS SELECT INDI_DSCM_NO, MIN(LTC_STRT_DT) AS MIN_DT FORMAT=yymmdd10., 
MAX(LTC_END_DT) AS MAX_DT FORMAT=yymmdd10., MAX(LTC_END_DT)- MIN(LTC_STRT_DT) AS INS_DD
FROM TARGET_LTC WHERE LTCP_TYPE_CD='1' GROUP BY INDI_DSCM_NO ORDER BY INDI_DSCM_NO; QUIT;

PROC SQL; CREATE TABLE HOME_INDI AS SELECT INDI_DSCM_NO, MIN(LTC_STRT_DT) AS MIN_DT FORMAT=yymmdd10., 
MAX(LTC_END_DT) AS MAX_DT FORMAT=yymmdd10., MAX(LTC_END_DT)- MIN(LTC_STRT_DT) AS HOME_DD
FROM TARGET_LTC WHERE LTCP_TYPE_CD='2' GROUP BY INDI_DSCM_NO ORDER BY INDI_DSCM_NO; QUIT;


/*각 서비스 이용일 수 MERGE*/
DATA LTC_USE_DD(KEEP=INDI_DSCM_NO ADJ_DD INS_DD HOME_DD); 
MERGE DB.TARGET_ID(IN=A) ADJ_INDI INS_INDI HOME_INDI; BY INDI_DSCM_NO; IF A=1; RUN;


/*암 진단이후 2년 내 최다 이용일 수, 최다일 이용 기관*/
DATA LTC_USE_MAX; SET LTC_USE_DD;
N_SEVICE = N(OF ADJ_DD, INS_DD, HOME_DD); /*이용 기관 수*/
MAX_DD = MAX(ADJ_DD, INS_DD, HOME_DD); /*최다 이용일 수*/
IF N_SEVICE^=0 AND MAX_DD = ADJ_DD THEN LTC_MAIN_USE = '01. 요양병원';
ELSE IF N_SEVICE^=0 AND MAX_DD = INS_DD THEN LTC_MAIN_USE = '02. 시설';
ELSE IF N_SEVICE^=0 AND MAX_DD = HOME_DD THEN LTC_MAIN_USE = '03. 재가';
ELSE IF N_SEVICE=0 THEN LTC_MAIN_USE = '04. 이용안함';
; RUN;


PROC FREQ DATA=LTC_USE_MAX; TABLES N_SEVICE; RUN;
PROC UNIVARIATE DATA=LTC_USE_MAX; VAR ADJ_DD ; HISTOGRAM; WHERE ADJ_DD^=.;RUN;
PROC UNIVARIATE DATA=LTC_USE_MAX; VAR INS_DD ; HISTOGRAM; WHERE INS_DD^=.;RUN;
PROC UNIVARIATE DATA=LTC_USE_MAX; VAR HOME_DD ; HISTOGRAM; WHERE HOME_DD^=.;RUN;
PROC FREQ DATA=LTC_USE_MAX; TABLES LTC_MAIN_USE;RUN;


/*여러개 서비스를 이용하는 사람 수*/
DATA DB.TARGET_LTC_USE; SET LTC_USE_MAX;
FORMAT LTC_USE_GRP $20.;
IF ADJ_DD^=. THEN ADJ=1 ;ELSE ADJ=0;
IF INS_DD^=. THEN INS=1 ;ELSE INS=0;
IF HOME_DD^=. THEN HOME=1; ELSE HOME=0;
IF ADJ=0 AND INS=0 AND HOME=0 THEN LTC_USE_GRP='00. NONE';
IF ADJ=1 AND INS=0 AND HOME=0 THEN LTC_USE_GRP='01. ONLY ADJ';
IF ADJ=0 AND INS=1 AND HOME=0 THEN LTC_USE_GRP='02. ONLY INS';
IF ADJ=0 AND INS=0 AND HOME=1 THEN LTC_USE_GRP='03. ONLY HOME';
IF ADJ=1 AND INS=1 AND HOME=0 THEN LTC_USE_GRP='04. ADJ+INS';
IF ADJ=1 AND INS=0 AND HOME=1 THEN LTC_USE_GRP='05. ADJ+HOME';
IF ADJ=0 AND INS=1 AND HOME=1 THEN LTC_USE_GRP='06. INS+HOME';
IF ADJ=1 AND INS=1 AND HOME=1 THEN LTC_USE_GRP='07. ADJ+INS_HOME';
RUN;

PROC FREQ DATA=DB.TARGET_LTC_USE;TABLES LTC_USE_GRP;RUN;


/*2개 서비스를 이용하는 경우 서비스 간 이용일 수 차이는?*/
DATA LTC_USE_2SERV; SET DB.TARGET_LTC_USE; 
IF LTC_USE_GRP='04. ADJ+INS' THEN DIFF=ABS(ADJ_DD-INS_DD);
IF LTC_USE_GRP='05. ADJ+HOME' THEN DIFF=ABS(ADJ_DD-HOME_DD);
IF LTC_USE_GRP='06. INS+HOME' THEN DIFF=ABS(HOME_DD-INS_DD);
WHERE LTC_USE_GRP IN ('04. ADJ+INS','05. ADJ+HOME','06. INS+HOME');
RUN;

PROC UNIVARIATE DATA=LTC_USE_2SERV; VAR DIFF; HISTOGRAM; RUN;
PROC UNIVARIATE DATA=LTC_USE_2SERV; VAR DIFF; HISTOGRAM; WHERE DIFF<90;RUN;

PROC FREQ DATA=LTC_USE_2SERV;TABLES LTC_USE_GRP; WHERE DIFF<30; RUN;
PROC FREQ DATA=LTC_USE_2SERV;TABLES LTC_USE_GRP; WHERE DIFF<15; RUN;

PROC SGPLOT DATA=LTC_USE_2SERV; VBAR DIFF; RUN; /*JVM 오류*/



/****** 암 진단 이전 이용자 시설 및 재가 이용 최초 시점******/
PROC SQL; CREATE TABLE TARGET_LTC_TBNYPAY_noaplygrp AS
SELECT DISTINCT A.INDI_DSCM_NO, A.C_DATE, B.INDI_SEQ, B.LTCP_TYPE_CD, 
INPUT(B.LTCP_FR_DT,YYMMDD10.) AS LTC_STRT_DT FORMAT=YYMMDD10., B.EADJ_TOTAL_PAY_DCNT
FROM NOAPLYGRP AS A
LEFT JOIN RAW.RVSN_HFVT_TBNYPAY AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
WHERE B.LTCP_TYPE_CD IN ('1','2') 
; QUIT;

PROC SQL; CREATE TABLE NOAPLYGRP_FIRST_USE_DT AS SELECT DISTINCT INDI_DSCM_NO, C_DATE, MIN(LTC_STRT_DT) AS FIRST_DT FORMAT=YYMMDD10.,
C_DATE - MIN(LTC_STRT_DT)  AS USE_DD_BEF_C
FROM TARGET_LTC_TBNYPAY_noaplygrp GROUP BY INDI_DSCM_NO; QUIT;












/**************************참고***************************/


/*2010년 이후 환자 대상 TABLES*/
/*환자 수 TABLES*/
libname TB "/userdata20/rroom029/data_out/data_out/TABLES";

PROC SQL; CREATE TABLE TB.TRT_SEXAGE_2010 AS
SELECT TRT_GRP_2YR, SEX_TYPE, AGE_GRP, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND C_YEAR>=2010
GROUP BY TRT_GRP_2YR, SEX_TYPE, AGE_GRP 
;QUIT;

PROC SQL; CREATE TABLE TB.TRT_CYEAR_2010 AS
SELECT TRT_GRP_2YR, C_YEAR, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND C_YEAR>=2010
GROUP BY TRT_GRP_2YR, C_YEAR
;QUIT;

/*PROC SQL; CREATE TABLE TRT_CYEARGRP AS
SELECT TRT_GRP_2YR, C_YEAR_GRP, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0  AND C_YEAR>=2010
GROUP BY TRT_GRP_2YR, C_YEAR_GRP
;QUIT;*/


PROC SQL; CREATE TABLE TB.LTC_SEXAGE_2010 AS
SELECT MAIN_USE_2YR, SEX_TYPE, E_AGE_GRP, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND E_AGE_GRP^='' AND C_YEAR>=2010
GROUP BY MAIN_USE_2YR, SEX_TYPE, E_AGE_GRP 
;QUIT;

PROC SQL; CREATE TABLE TB.LTC_CYEAR_2010 AS
SELECT MAIN_USE_2YR, C_YEAR, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND E_AGE_GRP^='' AND C_YEAR>=2010
GROUP BY MAIN_USE_2YR, C_YEAR
;QUIT;

PROC SQL; CREATE TABLE TB.LTC_TRT_2010 AS
SELECT SEX_TYPE, E_AGE_GRP, TRT_GRP_2YR, MAIN_USE_2YR, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND E_AGE_GRP^='' AND C_YEAR>=2010
GROUP BY SEX_TYPE, E_AGE_GRP, TRT_GRP_2YR, MAIN_USE_2YR
;QUIT;


/*진단이후 경과연별로 이용한 사람 수, 성별 연령별, 복수이용자 중복 카운트*/
%MACRO SERV_YN_YR;
%DO YR=1 %TO 10;
/*비누적  YR*/
PROC SQL; CREATE TABLE ADJ_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, 1 AS ADJ_YN_&YR.
FROM DB.TARGET_ADJ
WHERE INTNX('YEAR',C_DATE,%EVAL(&YR.-1),'S')<=INPUT(MDCARE_STRT_DT,YYMMDD10.) <INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0
AND C_YEAR>=2010
;QUIT;
PROC SQL; CREATE TABLE INS_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, 1 AS INS_YN_&YR.
FROM DB.TARGET_LTCP
WHERE INTNX('YEAR',C_DATE,%EVAL(&YR.-1),'S')<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '1'
AND C_YEAR>=2010
;QUIT;
PROC SQL; CREATE TABLE HOME_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, 1 AS HOME_YN_&YR.
FROM DB.TARGET_LTCP
WHERE INTNX('YEAR',C_DATE,%EVAL(&YR.-1),'S')<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '2'
AND C_YEAR>=2010
;QUIT;

/*누적 YR*/
PROC SQL; CREATE TABLE CUM_ADJ_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, 1 AS ADJ_YN_&YR.
FROM DB.TARGET_ADJ
WHERE C_DATE<=INPUT(MDCARE_STRT_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0
AND C_YEAR>=2010
;QUIT;
PROC SQL; CREATE TABLE CUM_INS_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, 1 AS INS_YN_&YR.
FROM DB.TARGET_LTCP
WHERE C_DATE<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '1'
AND C_YEAR>=2010
;QUIT;
PROC SQL; CREATE TABLE CUM_HOME_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, 1 AS HOME_YN_&YR.
FROM DB.TARGET_LTCP
WHERE C_DATE<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '2'
AND C_YEAR>=2010
;QUIT;
%END;
%MEND; %SERV_YN_YR;

/*비누적이용자 수 테이블*/
DATA SERV_YN_1TO10YR; MERGE ADJ_YN_1-ADJ_YN_10 INS_YN_1-INS_YN_10 HOME_YN_1-HOME_YN_10; BY INDI_DSCM_NO;RUN;

DATA TARGET_E_AGEGRP; SET DB.TARGET_ID;
IF 65<= C_AGE<75 THEN E_AGE_GRP='01. 65-74'; /*ELDERLY AGE GRP*/
ELSE IF 75 <= C_AGE <85 THEN E_AGE_GRP='02. 75-84';
ELSE IF 78 <= C_AGE THEN E_AGE_GRP='03. 85+';
WHERE C_AGE>=65;
RUN;

PROC SQL; CREATE TABLE SURV_YN_DAT AS
SELECT A.SEX_TYPE, A.E_AGE_GRP, A.C_YEAR, B.* /*혹시몰라서 C_YEAR도 넣음*/
FROM TARGET_E_AGEGRP AS A
INNER JOIN SERV_YN_1TO10YR AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
;QUIT;

PROC SQL; CREATE TABLE TB.SERV_USE_CNT AS
SELECT SEX_TYPE, E_AGE_GRP, 
SUM(ADJ_YN_1) AS ADJ1_CNT, SUM(ADJ_YN_2) AS ADJ2_CNT, SUM(ADJ_YN_3) AS ADJ3_CNT, SUM(ADJ_YN_4) AS ADJ4_CNT, SUM(ADJ_YN_5) AS ADJ5_CNT,  
SUM(ADJ_YN_6) AS ADJ6_CNT, SUM(ADJ_YN_7) AS ADJ7_CNT, SUM(ADJ_YN_8) AS ADJ8_CNT, SUM(ADJ_YN_9) AS ADJ9_CNT, SUM(ADJ_YN_10) AS ADJ10_CNT,
SUM(INS_YN_1) AS INS1_CNT, SUM(INS_YN_2) AS INS2_CNT, SUM(INS_YN_3) AS INS3_CNT, SUM(INS_YN_4) AS INS4_CNT, SUM(INS_YN_5) AS INS5_CNT, 
SUM(INS_YN_6) AS INS6_CNT, SUM(INS_YN_7) AS INS7_CNT, SUM(INS_YN_8) AS INS8_CNT, SUM(INS_YN_9) AS INS9_CNT, SUM(INS_YN_10) AS INS10_CNT,
SUM(HOME_YN_1) AS HOME1_CNT, SUM(HOME_YN_2) AS HOME2_CNT, SUM(HOME_YN_3) AS HOME3_CNT, SUM(HOME_YN_4) AS HOME4_CNT, SUM(HOME_YN_5) AS HOME5_CNT, 
SUM(HOME_YN_6) AS HOME6_CNT, SUM(HOME_YN_7) AS HOME7_CNT, SUM(HOME_YN_8) AS HOME8_CNT, SUM(HOME_YN_9) AS HOME9_CNT, SUM(HOME_YN_10) AS HOME10_CNT
FROM SURV_YN_DAT
GROUP BY SEX_TYPE, E_AGE_GRP
;QUIT;

/*누적이용자 수 테이블*/
DATA CUM_SERV_YN_1TO10YR; MERGE CUM_ADJ_YN_1-CUM_ADJ_YN_10 CUM_INS_YN_1-CUM_INS_YN_10 CUM_HOME_YN_1-CUM_HOME_YN_10; BY INDI_DSCM_NO;RUN;

PROC SQL; CREATE TABLE CUM_SURV_YN_DAT AS
SELECT A.SEX_TYPE, A.E_AGE_GRP, A.C_YEAR, B.* /*혹시몰라서 C_YEAR도 넣음*/
FROM TARGET_E_AGEGRP AS A
INNER JOIN CUM_SERV_YN_1TO10YR AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
;QUIT;

PROC SQL; CREATE TABLE TB.CUM_SERV_USE_CNT AS
SELECT SEX_TYPE, E_AGE_GRP, 
SUM(ADJ_YN_1) AS ADJ1_CNT, SUM(ADJ_YN_2) AS ADJ2_CNT, SUM(ADJ_YN_3) AS ADJ3_CNT, SUM(ADJ_YN_4) AS ADJ4_CNT, SUM(ADJ_YN_5) AS ADJ5_CNT,  
SUM(ADJ_YN_6) AS ADJ6_CNT, SUM(ADJ_YN_7) AS ADJ7_CNT, SUM(ADJ_YN_8) AS ADJ8_CNT, SUM(ADJ_YN_9) AS ADJ9_CNT, SUM(ADJ_YN_10) AS ADJ10_CNT,
SUM(INS_YN_1) AS INS1_CNT, SUM(INS_YN_2) AS INS2_CNT, SUM(INS_YN_3) AS INS3_CNT, SUM(INS_YN_4) AS INS4_CNT, SUM(INS_YN_5) AS INS5_CNT, 
SUM(INS_YN_6) AS INS6_CNT, SUM(INS_YN_7) AS INS7_CNT, SUM(INS_YN_8) AS INS8_CNT, SUM(INS_YN_9) AS INS9_CNT, SUM(INS_YN_10) AS INS10_CNT,
SUM(HOME_YN_1) AS HOME1_CNT, SUM(HOME_YN_2) AS HOME2_CNT, SUM(HOME_YN_3) AS HOME3_CNT, SUM(HOME_YN_4) AS HOME4_CNT, SUM(HOME_YN_5) AS HOME5_CNT, 
SUM(HOME_YN_6) AS HOME6_CNT, SUM(HOME_YN_7) AS HOME7_CNT, SUM(HOME_YN_8) AS HOME8_CNT, SUM(HOME_YN_9) AS HOME9_CNT, SUM(HOME_YN_10) AS HOME10_CNT
FROM CUM_SURV_YN_DAT
GROUP BY SEX_TYPE, E_AGE_GRP
;QUIT;







/*휴지통*/
/*이용일 수 기준*/
%MACRO DD_CNT(WI_YR);
/*ADJ  암진단이후 &WI_YR.년 내 요양일수 합*/
PROC SQL; CREATE TABLE COUNT_DD_ADJ_&WI_YR.YR AS
SELECT INDI_DSCM_NO, SUM(MDCARE_DD_CNT) AS DD_&WI_YR.YR_ADJ
FROM DB.TARGET_ADJ
WHERE C_DATE < INPUT(MDCARE_STRT_DT,YYMMDD10.) < INTNX('YEAR',C_DATE,&WI_YR.,'S')
GROUP BY INDI_DSCM_NO
ORDER BY INDI_DSCM_NO
;QUIT;
/*시설 암 진단이후 &WI_YR.년 내 심사조정총급여일수 합*/
PROC SQL; CREATE TABLE COUNT_DD_INS_&WI_YR.YR AS
SELECT INDI_DSCM_NO, SUM(EADJ_TOTAL_PAY_DCNT) AS DD_&WI_YR.YR_INS
FROM DB.TARGET_LTCP
WHERE C_DATE < INPUT(LTCP_FR_DT,YYMMDD10.) < INTNX('YEAR',C_DATE,&WI_YR.,'S')
AND LTCP_TYPE_CD = '1'
GROUP BY INDI_DSCM_NO
ORDER BY INDI_DSCM_NO
;QUIT;
/*재가 암 진단이후 &WI_YR.년 내 심사조정총급여일수 합*/
PROC SQL; CREATE TABLE COUNT_DD_HOME_&WI_YR.YR AS
SELECT INDI_DSCM_NO, SUM(EADJ_TOTAL_PAY_DCNT) AS DD_&WI_YR.YR_HOME
FROM DB.TARGET_LTCP
WHERE C_DATE < INPUT(LTCP_FR_DT,YYMMDD10.) < INTNX('YEAR',C_DATE,&WI_YR.,'S')
AND LTCP_TYPE_CD = '2'
GROUP BY INDI_DSCM_NO
ORDER BY INDI_DSCM_NO
;QUIT;

DATA CNT_DD_&WI_YR.YR; MERGE COUNT_DD_ADJ_&WI_YR.YR COUNT_DD_INS_&WI_YR.YR COUNT_DD_HOME_&WI_YR.YR; BY INDI_DSCM_NO; RUN;

/*암 진단이후 &WI_YR.년 내 최다 이용일 수, 최다일 이용 기관*/
DATA MAX_DD_&WI_YR.YR; SET CNT_DD_&WI_YR.YR;
N_&WI_YR.YR = N(OF DD_&WI_YR.YR_ADJ, DD_&WI_YR.YR_INS, DD_&WI_YR.YR_HOME);
MAX_DD_&WI_YR.YR = MAX(DD_&WI_YR.YR_ADJ, DD_&WI_YR.YR_INS, DD_&WI_YR.YR_HOME);
IF MAX_DD_&WI_YR.YR = DD_&WI_YR.YR_ADJ THEN MAIN_USE_&WI_YR. = '03. 요양병원';
ELSE IF MAX_DD_&WI_YR.YR = DD_&WI_YR.YR_INS THEN MAIN_USE_&WI_YR. = '01. 시설';
ELSE IF MAX_DD_&WI_YR.YR = DD_&WI_YR.YR_HOME THEN MAIN_USE_&WI_YR. = '02. 재가'
; RUN;

%MEND;

%DD_CNT(2);/*34874*/
%DD_CNT(5);/*44484*/
%DD_CNT(10);/*49242*/

/*DATA MAX_2510(KEEP=INDI_DSCM_NO MAIN_USE_2 MAIN_USE_5 MAIN_USE_10); 
MERGE MAX_DD_2YR MAX_DD_5YR MAX_DD_10YR; BY INDI_DSCM_NO; RUN;

DATA MAX_2510_VAR; SET MAX_2510;
IF MAIN_USE_2 ^='' AND MAIN_USE_5 ^='' AND MAIN_USE_2 = MAIN_USE_5 = MAIN_USE_10 THEN VAR = 0;
ELSE IF MAIN_USE_2='' AND MAIN_USE_5 ^='' AND MAIN_USE_5 = MAIN_USE_10 THEN VAR = 0;
ELSE IF MAIN_USE_2='' AND MAIN_USE_5 ='' THEN VAR = 0;
ELSE VAR=1;
RUN;

PROC SQL; CREATE TABLE VAR1 AS SELECT * FROM MAX_2510_VAR WHERE VAR=1; QUIT; 
*/
/*주이용서비스가 변동되는 사람 578명, 전체10년내이용자의 11%*/


/*복수의 요양기관을 이용하면 요양일수가 각각 카운트 됨 */
DATA TEST; SET USE2_2YR; WHERE MAX_DD_2YR>700; RUN;

PROC SQL; CREATE TABLE TEST AS SELECT * FROM RAW.RVSN_HFVT_TBNYPAY
WHERE INDI_DSCM_NO=29715982 ORDER BY LTC_OR_SYM, LTCP_FR_DT; QUIT;

%LET WI_YR=2;
DATA TEST; SET  COUNT_DD_HOME_2YR; WHERE INDI_DSCM_NO=22824188; RUN;

PROC SQL; CREATE TABLE COUNT_DD_HOME_&WI_YR.YR AS
SELECT *
FROM DB.TARGET_LTCP
WHERE C_DATE < INPUT(LTCP_FR_DT,YYMMDD10.) < INTNX('YEAR',C_DATE,&WI_YR.,'S') AND INDI_DSCM_NO=22824188
AND LTCP_TYPE_CD = '2'
ORDER BY INDI_DSCM_NO
;QUIT;

/*2008년 이후*/
/*환자 수 TABLES*/
libname TB "/userdata20/rroom029/data_out/data_out/TABLES";

PROC SQL; CREATE TABLE TB.TRT_SEXAGE AS
SELECT TRT_GRP_2YR, SEX_TYPE, AGE_GRP, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0
GROUP BY TRT_GRP_2YR, SEX_TYPE, AGE_GRP 
;QUIT;

PROC SQL; CREATE TABLE TB.TRT_CYEAR AS
SELECT TRT_GRP_2YR, C_YEAR, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0
GROUP BY TRT_GRP_2YR, C_YEAR
;QUIT;

/*PROC SQL; CREATE TABLE TRT_CYEARGRP AS
SELECT TRT_GRP_2YR, C_YEAR_GRP, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0
GROUP BY TRT_GRP_2YR, C_YEAR_GRP
;QUIT;*/


PROC SQL; CREATE TABLE TB.LTC_SEXAGE AS
SELECT MAIN_USE_2YR, SEX_TYPE, E_AGE_GRP, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND E_AGE_GRP^=''
GROUP BY MAIN_USE_2YR, SEX_TYPE, E_AGE_GRP 
;QUIT;

PROC SQL; CREATE TABLE TB.LTC_CYEAR AS
SELECT MAIN_USE_2YR, C_YEAR, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND E_AGE_GRP^=''
GROUP BY MAIN_USE_2YR, C_YEAR
;QUIT;

PROC SQL; CREATE TABLE TB.LTC_TRT AS
SELECT SEX_TYPE, E_AGE_GRP, TRT_GRP_2YR, MAIN_USE_2YR, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.LTC_FULLDAT
WHERE PRE_C=0 AND E_AGE_GRP^=''
GROUP BY SEX_TYPE, E_AGE_GRP, TRT_GRP_2YR, MAIN_USE_2YR
;QUIT;


/*0523추가*/

/*진단이후 경과연별로 장기요양서비스를 이용한 사람 수, 성별 연령별, 복수이용자 중복 카운트*/
%MACRO SERV_YN_YR;
%DO YR=1 %TO 10;
/*비누적 YR*/
PROC SQL; CREATE TABLE ADJ_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, C_YEAR, 1 AS ADJ_YN_&YR.
FROM DB.TARGET_ADJ
WHERE INTNX('YEAR',C_DATE,%EVAL(&YR.-1),'S')<=INPUT(MDCARE_STRT_DT,YYMMDD10.) <INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0
;QUIT;
PROC SQL; CREATE TABLE INS_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, C_YEAR, 1 AS INS_YN_&YR.
FROM DB.TARGET_LTCP
WHERE INTNX('YEAR',C_DATE,%EVAL(&YR.-1),'S')<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '1'
;QUIT;
PROC SQL; CREATE TABLE HOME_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, C_YEAR, 1 AS HOME_YN_&YR.
FROM DB.TARGET_LTCP
WHERE INTNX('YEAR',C_DATE,%EVAL(&YR.-1),'S')<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '2'
;QUIT;

/*누적 YR*/
PROC SQL; CREATE TABLE CUM_ADJ_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, C_YEAR, 1 AS ADJ_YN_&YR.
FROM DB.TARGET_ADJ
WHERE C_DATE<=INPUT(MDCARE_STRT_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0
;QUIT;
PROC SQL; CREATE TABLE CUM_INS_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, C_YEAR, 1 AS INS_YN_&YR.
FROM DB.TARGET_LTCP
WHERE C_DATE<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '1'
;QUIT;
PROC SQL; CREATE TABLE CUM_HOME_YN_&YR. AS
SELECT DISTINCT INDI_DSCM_NO, C_YEAR, 1 AS HOME_YN_&YR.
FROM DB.TARGET_LTCP
WHERE C_DATE<=INPUT(LTCP_FR_DT,YYMMDD10.)<INTNX('YEAR',C_DATE,&YR.,'S') AND PRE_C=0 AND LTCP_TYPE_CD = '2'
;QUIT;
%END;
%MEND; %SERV_YN_YR;

/*비누적*/
DATA SERV_YN_1TO10YR; MERGE ADJ_YN_1-ADJ_YN_10 INS_YN_1-INS_YN_10 HOME_YN_1-HOME_YN_10; BY INDI_DSCM_NO;RUN;

DATA TARGET_E_AGEGRP; SET DB.TARGET_ID;
IF 65<= C_AGE<75 THEN E_AGE_GRP='01. 65-74'; /*ELDERLY AGE GRP*/
ELSE IF 75 <= C_AGE <85 THEN E_AGE_GRP='02. 75-84';
ELSE IF 78 <= C_AGE THEN E_AGE_GRP='03. 85+';
WHERE C_AGE>=65;
RUN;

PROC SQL; CREATE TABLE SURV_YN_DAT AS
SELECT A.SEX_TYPE, A.E_AGE_GRP, A.C_YEAR, B.* /*혹시몰라서 C_YEAR도 넣음*/
FROM TARGET_E_AGEGRP AS A
INNER JOIN SERV_YN_1TO10YR AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
;QUIT;

PROC SQL; CREATE TABLE SERV_USE_CNT AS
SELECT SEX_TYPE, E_AGE_GRP, 
SUM(ADJ_YN_1) AS ADJ1_CNT, SUM(ADJ_YN_2) AS ADJ2_CNT, SUM(ADJ_YN_3) AS ADJ3_CNT, SUM(ADJ_YN_4) AS ADJ4_CNT, SUM(ADJ_YN_5) AS ADJ5_CNT,  
SUM(ADJ_YN_6) AS ADJ6_CNT, SUM(ADJ_YN_7) AS ADJ7_CNT, SUM(ADJ_YN_8) AS ADJ8_CNT, SUM(ADJ_YN_9) AS ADJ9_CNT, SUM(ADJ_YN_10) AS ADJ10_CNT,
SUM(INS_YN_1) AS INS1_CNT, SUM(INS_YN_2) AS INS2_CNT, SUM(INS_YN_3) AS INS3_CNT, SUM(INS_YN_4) AS INS4_CNT, SUM(INS_YN_5) AS INS5_CNT, 
SUM(INS_YN_6) AS INS6_CNT, SUM(INS_YN_7) AS INS7_CNT, SUM(INS_YN_8) AS INS8_CNT, SUM(INS_YN_9) AS INS9_CNT, SUM(INS_YN_10) AS INS10_CNT,
SUM(HOME_YN_1) AS HOME1_CNT, SUM(HOME_YN_2) AS HOME2_CNT, SUM(HOME_YN_3) AS HOME3_CNT, SUM(HOME_YN_4) AS HOME4_CNT, SUM(HOME_YN_5) AS HOME5_CNT, 
SUM(HOME_YN_6) AS HOME6_CNT, SUM(HOME_YN_7) AS HOME7_CNT, SUM(HOME_YN_8) AS HOME8_CNT, SUM(HOME_YN_9) AS HOME9_CNT, SUM(HOME_YN_10) AS HOME10_CNT
FROM SURV_YN_DAT
GROUP BY SEX_TYPE, E_AGE_GRP
;QUIT;

PROC SQL; CREATE TABLE SERV_USE_CNT_CYEAR AS
SELECT SEX_TYPE, E_AGE_GRP, C_YEAR,
SUM(ADJ_YN_1) AS ADJ1_CNT, SUM(ADJ_YN_2) AS ADJ2_CNT, SUM(ADJ_YN_3) AS ADJ3_CNT, SUM(ADJ_YN_4) AS ADJ4_CNT, SUM(ADJ_YN_5) AS ADJ5_CNT,  
SUM(ADJ_YN_6) AS ADJ6_CNT, SUM(ADJ_YN_7) AS ADJ7_CNT, SUM(ADJ_YN_8) AS ADJ8_CNT, SUM(ADJ_YN_9) AS ADJ9_CNT, SUM(ADJ_YN_10) AS ADJ10_CNT,
SUM(INS_YN_1) AS INS1_CNT, SUM(INS_YN_2) AS INS2_CNT, SUM(INS_YN_3) AS INS3_CNT, SUM(INS_YN_4) AS INS4_CNT, SUM(INS_YN_5) AS INS5_CNT, 
SUM(INS_YN_6) AS INS6_CNT, SUM(INS_YN_7) AS INS7_CNT, SUM(INS_YN_8) AS INS8_CNT, SUM(INS_YN_9) AS INS9_CNT, SUM(INS_YN_10) AS INS10_CNT,
SUM(HOME_YN_1) AS HOME1_CNT, SUM(HOME_YN_2) AS HOME2_CNT, SUM(HOME_YN_3) AS HOME3_CNT, SUM(HOME_YN_4) AS HOME4_CNT, SUM(HOME_YN_5) AS HOME5_CNT, 
SUM(HOME_YN_6) AS HOME6_CNT, SUM(HOME_YN_7) AS HOME7_CNT, SUM(HOME_YN_8) AS HOME8_CNT, SUM(HOME_YN_9) AS HOME9_CNT, SUM(HOME_YN_10) AS HOME10_CNT
FROM SURV_YN_DAT
GROUP BY SEX_TYPE, E_AGE_GRP, C_YEAR
;QUIT;


/*누적*/
DATA CUM_SERV_YN_1TO10YR; MERGE CUM_ADJ_YN_1-CUM_ADJ_YN_10 CUM_INS_YN_1-CUM_INS_YN_10 CUM_HOME_YN_1-CUM_HOME_YN_10; BY INDI_DSCM_NO;RUN;

PROC SQL; CREATE TABLE CUM_SURV_YN_DAT AS
SELECT A.SEX_TYPE, A.E_AGE_GRP, A.C_YEAR, B.* /*혹시몰라서 C_YEAR도 넣음*/
FROM TARGET_E_AGEGRP AS A
INNER JOIN CUM_SERV_YN_1TO10YR AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
;QUIT;

PROC SQL; CREATE TABLE CUM_SERV_USE_CNT AS
SELECT SEX_TYPE, E_AGE_GRP, 
SUM(ADJ_YN_1) AS ADJ1_CNT, SUM(ADJ_YN_2) AS ADJ2_CNT, SUM(ADJ_YN_3) AS ADJ3_CNT, SUM(ADJ_YN_4) AS ADJ4_CNT, SUM(ADJ_YN_5) AS ADJ5_CNT,  
SUM(ADJ_YN_6) AS ADJ6_CNT, SUM(ADJ_YN_7) AS ADJ7_CNT, SUM(ADJ_YN_8) AS ADJ8_CNT, SUM(ADJ_YN_9) AS ADJ9_CNT, SUM(ADJ_YN_10) AS ADJ10_CNT,
SUM(INS_YN_1) AS INS1_CNT, SUM(INS_YN_2) AS INS2_CNT, SUM(INS_YN_3) AS INS3_CNT, SUM(INS_YN_4) AS INS4_CNT, SUM(INS_YN_5) AS INS5_CNT, 
SUM(INS_YN_6) AS INS6_CNT, SUM(INS_YN_7) AS INS7_CNT, SUM(INS_YN_8) AS INS8_CNT, SUM(INS_YN_9) AS INS9_CNT, SUM(INS_YN_10) AS INS10_CNT,
SUM(HOME_YN_1) AS HOME1_CNT, SUM(HOME_YN_2) AS HOME2_CNT, SUM(HOME_YN_3) AS HOME3_CNT, SUM(HOME_YN_4) AS HOME4_CNT, SUM(HOME_YN_5) AS HOME5_CNT, 
SUM(HOME_YN_6) AS HOME6_CNT, SUM(HOME_YN_7) AS HOME7_CNT, SUM(HOME_YN_8) AS HOME8_CNT, SUM(HOME_YN_9) AS HOME9_CNT, SUM(HOME_YN_10) AS HOME10_CNT
FROM CUM_SURV_YN_DAT
GROUP BY SEX_TYPE, E_AGE_GRP
;QUIT;

PROC SQL; CREATE TABLE CUM_SERV_USE_CNT_CYEAR AS
SELECT SEX_TYPE, E_AGE_GRP, C_YEAR,
SUM(ADJ_YN_1) AS ADJ1_CNT, SUM(ADJ_YN_2) AS ADJ2_CNT, SUM(ADJ_YN_3) AS ADJ3_CNT, SUM(ADJ_YN_4) AS ADJ4_CNT, SUM(ADJ_YN_5) AS ADJ5_CNT,  
SUM(ADJ_YN_6) AS ADJ6_CNT, SUM(ADJ_YN_7) AS ADJ7_CNT, SUM(ADJ_YN_8) AS ADJ8_CNT, SUM(ADJ_YN_9) AS ADJ9_CNT, SUM(ADJ_YN_10) AS ADJ10_CNT,
SUM(INS_YN_1) AS INS1_CNT, SUM(INS_YN_2) AS INS2_CNT, SUM(INS_YN_3) AS INS3_CNT, SUM(INS_YN_4) AS INS4_CNT, SUM(INS_YN_5) AS INS5_CNT, 
SUM(INS_YN_6) AS INS6_CNT, SUM(INS_YN_7) AS INS7_CNT, SUM(INS_YN_8) AS INS8_CNT, SUM(INS_YN_9) AS INS9_CNT, SUM(INS_YN_10) AS INS10_CNT,
SUM(HOME_YN_1) AS HOME1_CNT, SUM(HOME_YN_2) AS HOME2_CNT, SUM(HOME_YN_3) AS HOME3_CNT, SUM(HOME_YN_4) AS HOME4_CNT, SUM(HOME_YN_5) AS HOME5_CNT, 
SUM(HOME_YN_6) AS HOME6_CNT, SUM(HOME_YN_7) AS HOME7_CNT, SUM(HOME_YN_8) AS HOME8_CNT, SUM(HOME_YN_9) AS HOME9_CNT, SUM(HOME_YN_10) AS HOME10_CNT
FROM CUM_SURV_YN_DAT
GROUP BY SEX_TYPE, E_AGE_GRP, C_YEAR
;QUIT;
