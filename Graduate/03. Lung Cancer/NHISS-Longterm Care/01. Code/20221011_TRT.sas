libname raw "/userdata20/rroom029/data_source/user_data";/*raw data*/
libname db "/userdata20/rroom029/data_out/data_store"; 
libname db_out "/userdata20/rroom029/data_out/data_out";


/* [1]  DB.TARGET_T20_&YEAR, DB.TARGET_T30_&YEAR*/
%MACRO YEARLY; %DO YEAR=2008 %TO 2020;
%MACRO MONTHLY(STRT, STP); 

%DO YM=&STRT. %TO &STP.;

/*대상자 20테이블DB*/
PROC SQL; CREATE TABLE TARGET_T20_&YM. AS
SELECT DISTINCT A.C_DATE, B.*
FROM DB.TARGET_IDBFCDTH AS A
INNER JOIN RAW.T20_&YM. AS B
ON A.INDI_DSCM_NO=B.INDI_DSCM_NO
; QUIT;

PROC SQL; CREATE TABLE TARGET_CMN_&YM. AS SELECT DISTINCT INDI_DSCM_NO, C_DATE, CMN_KEY, MDCARE_STRT_DT FROM TARGET_T20_&YM.; QUIT;

/*대상자 30테이블DB*/
PROC SQL; CREATE TABLE TARGET_T30_&YM. AS
SELECT DISTINCT A.*, B.*
FROM TARGET_CMN_&YM. AS A
INNER JOIN RAW.T30_&YM. AS B
ON A.CMN_KEY=B.CMN_KEY
; QUIT;

%END; 
%MEND MONTHLY;  

%MONTHLY(STRT=&YEAR.01, STP=&YEAR.12);

DATA DB.TARGET_T20_&YEAR.; SET TARGET_T20_&YEAR.01-TARGET_T20_&YEAR.12; RUN;
DATA DB.TARGET_T30_&YEAR.; SET TARGET_T30_&YEAR.01-TARGET_T30_&YEAR.12; RUN;

%END; 
%MEND YEARLY;%YEARLY;


/*수술 및 항암 업데이트 후*/

/* [2] DB.TARGET_T30_LTC_&YEAR*/
%MACRO YEARLY; %DO YEAR=2015 %TO 2020;
DATA DB.TARGET_T30_LTC_&YEAR.; SET DB.TARGET_T30_&YEAR.;

/*******************************************************************************************************************************/
FORMAT CTx $30.;

/*1. Cytotoxic Chemotherapy*/
IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4528") THEN DO; CTx="Belotecan" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1227") THEN DO; CTx="Capecitabine";CT_CYTYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1237") THEN DO; CTx="Carboplatin" ;CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1345") THEN DO; CTx="Cisplatin" ;CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1390") THEN DO; CTx="Cyclophosphamide" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1483") THEN DO; CTx="Docetaxel" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1494") THEN DO; CTx="Doxorubicin" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1571") THEN DO; CTx="Etoposide" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1649") THEN DO; CTx="Gemcitabine" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1733") THEN DO; CTx="Ifosfamide" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1774", "6660") THEN DO; CTx="Irinotecan" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1921") THEN DO; CTx="Methotrexate" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2078","5037") THEN DO; CTx="Paclitaxel" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4812") THEN DO; CTx="Pemetrexed" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4524","4525") THEN DO; CTx="Tegafur/Gemeracil/Oteracil" ; CT_CYTYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2419") THEN DO; CTx="Topotecan" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2480") THEN DO; CTx="Vincristine" ; CT_CYTYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2482") THEN DO; CTx="Vinorelbine" ; CT_CYTYN=1; END;
/*2. Targeted Therapy*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6261") THEN DO; CTx="Afatinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6562") THEN DO; CTx="Alectinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("5543") THEN DO; CTx="Bevacizumab"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4528") THEN DO; CTx="Belotecan"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4633") THEN DO; CTx="Bortezomib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6757") THEN DO; CTx="Brigatinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("5564") THEN DO; CTx="Cetuximab"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6344") THEN DO; CTx="Ceritinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6175") THEN DO; CTx="Crizotinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6631") THEN DO; CTx="Dabrafenib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6916") THEN DO; CTx="Dacomitinib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4774") THEN DO; CTx="Erlotinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4530") THEN DO; CTx="Gefitinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6951") THEN DO; CTx="Lazertinib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6997") THEN DO; CTx="Lorlatinib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6525") THEN DO; CTx="Osimertinib"; CT_TARYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6524") THEN DO; CTx="Olmutinib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4880") THEN DO; CTx="Sorafenib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4877") THEN DO; CTx="Sunitinib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6454") THEN DO; CTx="Trametinib"; CT_TARYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2428") THEN DO; CTx="Trastuzumab"; CT_TARYN=1; END;/*신규*/
/*3. Immunotherapy*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6577") THEN DO; CTx="Atezolizumab" ; CT_IMUYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6788") THEN DO; CTx="Avelumab" ; CT_IMUYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6769") THEN DO; CTx="Durvalumab" ; CT_IMUYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6384") THEN DO; CTx="Nivolumab" ; CT_IMUYN=1; END;
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6390") THEN DO; CTx="Pembrolizumab" ; CT_IMUYN=1; END;       /*"Tislelizumab"신규*/
/*4. Other(Unknown)*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6866") THEN DO; CTx="Abemaciclib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6239") THEN DO; CTx="ABN401" ; CT_OTHYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6588") THEN DO; CTx="Aflibercept" ; CT_OTHYN=1; END;/*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1067") THEN DO; CTx="Amifostine" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("7074") THEN DO; CTx="Amivantamab" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6210") THEN DO; CTx="Axitinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4843") THEN DO; CTx="Azacitidine" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6146") THEN DO; CTx="Bendamustine" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1172") THEN DO; CTx="Bicalutamide" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1180") THEN DO; CTx="Bleomycin" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4633") THEN DO; CTx="Bortezomib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1206") THEN DO; CTx="Busulfan" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6664") THEN DO; CTx="Cabozantinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6484") THEN DO; CTx="Canakinumab" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("7038") THEN DO; CTx="Capmatinib" ; CT_OTHYN=1; END; /*이동*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6478") THEN DO; CTx="Carfilzomib" ; CT_OTHYN=1; END; /*이동*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1396") THEN DO; CTx="Cytarabine" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1399") THEN DO; CTx="Dacarbazine" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1401") THEN DO; CTx="Dactinomycin" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4933") THEN DO; CTx="Dasatinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1406") THEN DO; CTx="Daunorubicin" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4956") THEN DO; CTx="Decitabine" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1527") THEN DO; CTx="Epirubicin" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1614","3821","6373") THEN DO; CTx="Fluorouracil" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1672") THEN DO; CTx="Goserelin" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6281") THEN DO; CTx="Ibrutinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4127") THEN DO; CTx="Imatinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6452") THEN DO; CTx="Lenvatinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1964") THEN DO; CTx="Mitomycin" ; CT_OTHYN=1; END; /*이동 C*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("5626") THEN DO; CTx="Nilotinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6561") THEN DO; CTx="Nintedanib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6589") THEN DO; CTx="Ociperlimab" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6435") THEN DO; CTx="Olaparib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("1143") THEN DO; CTx="Oncotice" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2058") THEN DO; CTx="Oxaliplain" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6552") THEN DO; CTx="Palbociclib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6118") THEN DO; CTx="Pazopanib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6246") THEN DO; CTx="Pertuzumab" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6248") THEN DO; CTx="Regorafenib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("4226") THEN DO; CTx="Rituximab" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("7079") THEN DO; CTx="Selpercatinib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2345") THEN DO; CTx="Tamoxifen" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6241") THEN DO; CTx="Vandetanib" ; CT_OTHYN=1; END; /*신규*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("2478") THEN DO; CTx="Vinblastine" ; CT_OTHYN=1; END;/*이동*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,4) IN ("6181") THEN DO; CTx="Vorinostat" ; CT_OTHYN=1; END;/*이동*/

FORMAT CTx_CODE $10.;
IF CTx^=" " THEN CTx_CODE=SUBSTR(MCARE_DIV_CD_ADJ,1,4);

/*******************************************************************************************************************************/
FORMAT OP $200.;

/*(1) 기관, 기관지 및 폐*/
IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1300","O1301") THEN OP="01. Tracheostomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1303","O1306") THEN OP="02. Cricothyroidotomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1305") THEN OP="03. Mediastinal Tracheostomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1311","O1312","O1313","O1314","O1317") THEN OP="04. Exicision of Tracheal or Bronchial Tumor";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1315","O1316") THEN OP="05. Endoscopic Excision of Tracheal or Bronchial Tumor";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1318") THEN OP="06. Endoscopic Cryotherapy (Tracheal, Bronchial, Lung Tomor)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1321","O1326") THEN OP="07. Tracheal or Bronchial Repair";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1332","O1333") THEN OP="08. Endoscopic Removal of Tracheal or Broncial Foreign Body";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1336") THEN OP="09. Removal of Tracheal or Bronchial Foreign Body by Thoracotomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1341","O1342","O1343","O1344","O1345") THEN OP="10. Surgery fro Tracheal or Bronchial Stenosis";

ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1346","O1347","O1348") THEN OP="11. Endoscopic Dilatation of Tracheal or Broncial Stenosis";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1349") THEN OP="12. Tracheoesophageal Shunt Operation (Amatsu Method)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1351","O1352","O1353","1354") THEN OP="13. Repair of Bronchial Fistula";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1360") THEN OP="14. Exploratory Thoracotomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1371","O1372") THEN OP="15. Revision of Tracheostoma";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1380") THEN OP="16. Incision of lUng Abscess";
/*폐쐐기절제술*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1401","O1402","O1403","O1404","O1405") THEN OP="17. Wedge Resection of Lung (+O1402)";
/*폐구역절제술*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1410") THEN OP="18. Segmentectomy of Lung";
/*폐엽절제술*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1421","O1422","O1423","O1424") THEN OP="19. Lobectomy of Lung";
/*폐전적출술*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1431","O1432") THEN OP="20. Pneumonectomy";

ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1440") THEN OP="21. Repair of Lung";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1450") THEN OP="22. Pleural Decortication";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1460") THEN OP="23. Apocolysis, Pleurolysis";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1471") THEN OP="24. Cryosurgical Ablation of Lung Cancer";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("OZ201") THEN OP="25. Bronchoscopic Lung Volume Reduction-Insection of Unilateral Endobronchial Valve";

/*(2) 흉곽*/
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1480") THEN OP="26. Pleurodesis";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1481","O1482","O1483","O1484","O1485","O1486") THEN OP="27. Resection of Chest Wall Tumor (+O1481,O1482)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1491","O1492") THEN OP="28. Thoracoplasty";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1502") THEN OP="29. Irrigation of Empyema Cavity";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1501") THEN OP="30. Thorachoperitoneal Shunt";

ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1510","O1520") THEN OP="31. Thoracostomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1521") THEN OP="32. Pericardiaolysis with Redo-sternotomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1530") THEN OP="33. Reconstructive Repair of Pectus Excavatum (Ravitch Procedure), Carinatum";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1531","O1532") THEN OP="34. Removal of Chest Wall Foreign Body";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1540") THEN OP="35. Pleurectomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1541") THEN OP="36. Open Reduction of Sternum Facture";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1550") THEN OP="37. Schiodeia Operation";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1561") THEN OP="38. Ostectomy of Sternum";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1562") THEN OP="39. Sternum Resection and Reconstruction";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1570") THEN OP="40. Resection of Bullae";

ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1571") THEN OP="41. Closure of Sternotomy Separation";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1572") THEN OP="42. Radical Resection of Malignant Sternal Tumor";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1581","O1586") THEN OP="43. Mediastinostomy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1591","O1592","O1593") THEN OP="44. Excision of Mediastinal Tumor";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1596","O1597") THEN OP="45. Mediasternal Lymph Node Dissection";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1600") THEN OP="46. Repair od Diaphragm";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1605") THEN OP="47. Excision of Diaphragmatic Tumor and Reconstruction";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1610") THEN OP="48. Repair of Diaphragmatic Hernia";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("O1621") THEN OP="49. Suture and Ligation of Thoracic Duct";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("OX181") THEN OP="50. Cryoanalgesia in Thoracotomy";

FORMAT OP_CODE $10.;
IF OP^=" " THEN OP_CODE=SUBSTR(MCARE_DIV_CD_ADJ,1,5);

/*******************************************************************************************************************************/

/*(1) 방사선 모의치료 및 치료계획*/
FORMAT RT $200.;

IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD010","HD410","HD011","HD411","HD012","HD412","HD013","HD413","HD014","HD414",
														"HD015","HD415","HD016","HD416","HD017","HD018","HD418","HD019","HD419","HD020","HD420","HD041","HD441") 
														THEN RT="01. For Teletherapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD021","HD022","HD023") THEN RT="02. For Brachytherapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD031","HD032","HD033") THEN RT="03. Design and Construction of Therapeutic Devices";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD040") THEN RT="04. Iodine-125 Permanent Implant for Prostate Cancer";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HX401") THEN RT="05. Dosimetry";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HY402","HY404","HY405") THEN RT="06. Verification and Correction";

/*(2) 방사선 치료*/

IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD051","HD054","HD052","HD055","HD053","HD056") THEN RT="07. Teletherapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD057","HD058","HD059") THEN RT="08. Rotational Irradiation";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD061") THEN RT="09. 3-Dimensional Conformal Therapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD071","HD072","HD073") THEN RT="10. Unsealed Source";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD080","HD081","HD082","HD083","HD084","HD085","HD086","HD087","HD088","HD089") THEN RT="11. Brachytherapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD091","HD092") THEN RT="12. Total Body Inrradiation";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD093") THEN RT="13. Total Skin Electron Beam Therapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD110") THEN RT="14. Fractionated Stereotactic Radiotherapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD111","HD112","HD211","HD212") THEN RT="15. Body Stereotactic Radiosurgery";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD113","HD114","HD115","S4751") THEN RT="16. Cranical Sterotactic Radiosurgery (+S4751)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD121") THEN RT="17. Proton Therapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HZ271") THEN RT="18. Intensity Modulated Radiation Therapy";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HZ272") THEN RT="19. 방사선 온열치료 및 온열치료 계획";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HZ273") THEN RT="20. 중성자선 치료";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HZ274") THEN RT="21. 안구 내 종양 근접 방사선치료 및 계획";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HZ275") THEN RT="22. 양성자 치료 및 양성자 치료계획";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HD150") THEN RT="23. Iodine-125 Permanent Implant for Prostate Cancer";

ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HH121") THEN RT="24. 치료촬영1차";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HH122") THEN RT="25. 치료촬영2차";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("HH137") THEN RT="26. 원격치료(4MV)";

FORMAT RT_CODE $10.;
IF RT^=" " THEN RT_CODE=SUBSTR(MCARE_DIV_CD_ADJ,1,4);

/*******************************************************************************************************************************/
/*요양병원 환자군 분류체계 및 일당정액 수가*/
FORMAT  LTC_ADJ $50.;
IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A1100") THEN LTC_ADJ="01. 의료최고도 (입원)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A1900") THEN LTC_ADJ="02. 의료최고도 (외박)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A2100") THEN LTC_ADJ="03. 의료고도 (입원)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A2900") THEN LTC_ADJ="04. 의료고도 (외박)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A3000") THEN LTC_ADJ="05. 의료중도";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A3100") THEN LTC_ADJ="06. 의료중도 (입원)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A3900") THEN LTC_ADJ="07. 의료중도 (외박)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A6100") THEN LTC_ADJ="08. 의료경도 (입원)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A6900") THEN LTC_ADJ="09. 의료경도 (외박)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A7100") THEN LTC_ADJ="10. 선택입원 (입원)";
ELSE IF SUBSTR(MCARE_DIV_CD_ADJ,1,5) IN ("A7900") THEN LTC_ADJ="11. 선택입원 (외박)";

FORMAT T30_DATE yymmdd10.;
T30_DATE=INPUT(PUT(MDCARE_STRT_DT,8.),yymmdd8.);

;RUN;
%END; 
%MEND YEARLY;%YEARLY;


/*진단 이후 2년내 치료 자료만 사용*/
/* [3] : DB.TARGET_T30_LTC2YR_&YEAR. */
%MACRO YEARLY; %DO YEAR=2008 %TO 2020;
DATA DB.TARGET_T30_LTC2YR_&YEAR.; SET DB.TARGET_T30_LTC_&YEAR.;
WHERE C_DATE <= T30_DATE <= C_DATE+(365*2); RUN;
%END; 
%MEND YEARLY;%YEARLY;

DATA DB.TARGET_T30_LTC2YR; SET DB.TARGET_T30_LTC2YR_2008-DB.TARGET_T30_LTC2YR_2020;RUN;

/*********수술***************/
/*2년 이내 전체 수술*/
PROC SQL; CREATE TABLE DB.T30_LTC_OP_2YR AS SELECT *
FROM DB.TARGET_T30_LTC2YR WHERE OP^=" "  
ORDER BY INDI_DSCM_NO, CMN_KEY; QUIT;

/*첫 수술 CMN_KEY*/
DATA DB.FIRST_OP_CMN (KEEP=/*INDI_DSCM_NO*/ CMN_KEY); 
SET DB.T30_LTC_OP_2YR; IF FIRST.INDI_DSCM_NO; BY INDI_DSCM_NO; RUN;

/*최초 수술 명세서 내역 전체*/
PROC SQL;
CREATE TABLE FIRST_OP_T30 AS
SELECT A.*
FROM DB.T30_LTC_OP_2YR AS A
INNER JOIN DB.FIRST_OP_CMN AS B
ON A.CMN_KEY=B.CMN_KEY
;QUIT;

DATA FIRST_OP; SET FIRST_OP_T30; 
IF OP="17. Wedge Resection of Lung (+O1402)"  THEN OP_WEDYN=1;
ELSE IF OP="18. Segmentectomy of Lung" THEN OP_SEGYN=1;
ELSE IF OP="19. Lobectomy of Lung" THEN OP_LOBYN=1;
ELSE IF OP="20. Pneumonectomy" THEN OP_PNEYN=1; 
IF OP_WEDYN=. THEN OP_WEDYN=0;
IF OP_SEGYN=. THEN OP_SEGYN=0;
IF OP_LOBYN=. THEN OP_LOBYN=0;
IF OP_PNEYN=. THEN OP_PNEYN=0;
RUN;

PROC SQL; CREATE TABLE INDIOP AS SELECT DISTINCT INDI_DSCM_NO, 
SUM(OP_WEDYN) AS OP_WED, SUM(OP_SEGYN) AS OP_SEG, SUM(OP_LOBYN) AS OP_LOB, SUM(OP_PNEYN) AS OP_PNE
FROM FIRST_OP GROUP BY INDI_DSCM_NO; QUIT;

DATA DB.TARGET_OP; SET INDIOP;
FORMAT OP_CTG $30.;
IF OP_PNE>=1 THEN OP_CTG="01. Pneumonectomy";
ELSE IF OP_LOB>=1 THEN OP_CTG="02. Lobectomy";
ELSE IF OP_SEG>=1 THEN OP_CTG="03. Segmentectomy";
ELSE IF OP_WED>=1 THEN OP_CTG="04. Wedge";
ELSE OP_CTG="05. None";
IF OP_PNE=1 OR OP_LOB=1 OR OP_SEG=1 OR OP_WED=1 THEN OP_YN=1; ELSE OP_YN=0; RUN;


/***************항암**************/
PROC SQL; CREATE TABLE INDICT AS SELECT DISTINCT INDI_DSCM_NO, 
SUM(CT_CYTYN) AS CT_CYT, SUM(CT_TARYN) AS CT_TAR, SUM(CT_IMUYN) AS CT_IMU, SUM(CT_OTHYN) AS CT_OTH
FROM DB.TARGET_T30_LTC2YR GROUP BY INDI_DSCM_NO; QUIT;
DATA DB.TARGET_CT; SET INDICT;
IF CT_CYT>=1 OR CT_TAR>=1 OR CT_IMU>=1 OR CT_OTH>=1 THEN CT_YN=1; ELSE CT_YN=0; RUN;

/****************방사선****************/
PROC SQL; CREATE TABLE DB.TARGET_RT AS SELECT DISTINCT INDI_DSCM_NO, 1 AS RT_YN
FROM DB.TARGET_T30_LTC2YR WHERE RT^=" ";QUIT;

/*DB.TARGET_ID에 치료정보 붙이기*/

DATA TG_TRT; 
MERGE DB.TARGET_ID(IN=A) DB.TARGET_OP DB.TARGET_CT DB.TARGET_RT ; 
BY INDI_DSCM_NO; IF A=1 ; 
RUN;

DATA TARGET_TRT; SET TG_TRT ;
ARRAY CHANGE _NUMERIC_;
DO OVER CHANGE;
IF CHANGE=. THEN CHANGE=0;
END;
RUN;

DATA DB.TARGET_TRT; SET TARGET_TRT;
IF OP_YN=1 OR CT_YN=1 OR RT_YN=1 THEN TRT_YN=1; ELSE TRT_YN=0; 
FORMAT TRT_GRP $20.;
IF OP_YN=0 THEN OP_CTG="05. None";
IF OP_YN=1 AND CT_YN=0 AND RT_YN=0 THEN TRT_GRP="01. SURGERY ONLY";
IF OP_YN=1 AND CT_YN=1 AND RT_YN=0 THEN TRT_GRP="02. SURGERY + CTx";
IF OP_YN=1 AND CT_YN=1 AND RT_YN=1 THEN TRT_GRP="03. SURGERY + CTx + RT";
IF OP_YN=1 AND CT_YN=0 AND RT_YN=1 THEN TRT_GRP="04. SURGERY + RT";
IF OP_YN=0 AND CT_YN=1 AND RT_YN=0 THEN TRT_GRP="05. CTx";
IF OP_YN=0 AND CT_YN=1 AND RT_YN=1 THEN TRT_GRP="06. CTx + RT";
IF OP_YN=0 AND CT_YN=0 AND RT_YN=1 THEN TRT_GRP="07. RT";
IF CT_CYT>=1 THEN CT_CYT_YN=1 ; ELSE CT_CYT_YN=0;
IF CT_TAR>=1 THEN CT_TAR_YN=1; ELSE CT_TAR_YN=0;
IF CT_IMU>=1 THEN CT_IMU_YN=1; ELSE CT_IMU_YN=0;
IF CT_OTH>=1 THEN CT_OTH_YN=1; ELSE CT_OTH_YN=0;
RUN;

/*******************************************************/


/*항암제별 빈도*/
DATA CT_DIST; SET DB.TG_TRT; 
IF CT_CYT>=1 THEN CT_CYTYN=1;
IF CT_TAR>=1 THEN CT_TARYN=1;
IF CT_IMU>=1 THEN CT_IMUYN=1;
IF CT_OTH>=1 THEN CT_OTHYN=1; RUN;


PROC SQL; CREATE TABLE CTFREQ AS SELECT CT_CYTYN, CT_TARYN, CT_IMUYN, CT_OTHYN, COUNT(DISTINCT INDI_DSCM_NO) AS CNT
FROM DB.TG_TRT GROUP BY CT_CYT, CT_TAR, CT_IMU, CT_IMU; QUIT;


%MACRO CTFREQ(COND, CONDNAME);
PROC SQL; CREATE TABLE CTFREQ&CONDNAME. AS SELECT CT_CYTYN, CT_TARYN, CT_IMUYN, CT_OTHYN, COUNT(DISTINCT INDI_DSCM_NO) AS CNT&CONDNAME. 
FROM CT_DIST WHERE PRE_C=0 AND C_AGE>=65 AND C_YEAR>=2009  &COND.  GROUP BY CT_CYTYN, CT_TARYN, CT_IMUYN, CT_OTHYN; QUIT;
%MEND;

 
%CTFREQ();
%CTFREQ(COND= AND C_AGE_GRP2='01. 65-74', CONDNAME=_6574);
%CTFREQ(COND= AND C_AGE_GRP2='02. 75-84', CONDNAME=_7584);
%CTFREQ(COND= AND C_AGE_GRP2='03. 85+', CONDNAME=_85U);


DATA CTFQ; MERGE CTFREQ(IN=A) CTFREQ_6574 CTFREQ_7584 CTFREQ_85U ; 
BY CT_CYTYN CT_TARYN CT_IMUYN CT_OTHYN; IF A=1;RUN;
