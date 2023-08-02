proc sql;
 create table drug_selection as
 select * from temp_drugtotal
 where 
 /*Domin : Cytotoxic Agents*/
/*Capecitabine*/
 COMPRESS(ORD_CD,'') like '%CPC%' or 
/*Cisplatin*/
 COMPRESS(ORD_CD,'') like '%CDDP%' or
/*Docetaxel*/
 COMPRESS(ORD_CD,'') like '%DCT%' or
/*Doxifluridine*/
 COMPRESS(ORD_CD,'') like '%DFUR%' or
/*Fluorouracil*/
 COMPRESS(ORD_CD,'') like '%FU%' or
/*Oxaliplatin*/
 COMPRESS(ORD_CD,'') like '%OXLP%' or
/*Paclitaxel*/
 COMPRESS(ORD_CD,'') like '%TAXO%' or
 /*Tegafur/uracil*/
 COMPRESS(ORD_CD,'') like '%TUE%' or
 /*Trifluridine/Tipiracil*/
 COMPRESS(ORD_CD,'') like '%TRFTP%' or
 
/*CKD-516*/
 COMPRESS(ORD_CD,'') like '%CKD%' or
/*DHP107*/
 COMPRESS(ORD_CD,'') like '%DHP%' or
/*PEP02*/
 COMPRESS(ORD_CD,'') like '%PEP02%' or
/*Tegafur/Gemeracil/Oteracil*/
COMPRESS(ORD_CD,'') like '%TSO%' or '%166TS%' or '%190TS%' or '%264TS%' or
'%265TS%' or '%296TS%' or '%586TS%' or
/*Vinflunine*/
COMPRESS(ORD_CD,'') like '%VLFN%' or 


/*Domin : Targeted Therapies*/
/*Ramucirumab*/
COMPRESS(ORD_CD,'') like '%RMCR%' or
/*Trastuzumab*/
COMPRESS(ORD_CD,'') like '%HER%' or
/*AMG337*/
COMPRESS(ORD_CD,'') like '%715AMG%' or
/*BGB-3111*/
COMPRESS(ORD_CD,'') like '%15286BGB8%' or
/*CWP232291*/
COMPRESS(ORD_CD,'') like '%17170CWP%' or
/*GC1118*/
COMPRESS(ORD_CD,'') like '%GC100%' or
/*HM781-36B*/
COMPRESS(ORD_CD,'') like '%655HM%' or
/*IDX-1197*/
COMPRESS(ORD_CD,'') like '%20046IDX40%' or
/*Nimotuzumab*/
COMPRESS(ORD_CD,'') like '%0167NMTZ' or
/*Trastuzumab Emtansine*/
COMPRESS(ORD_CD,'') like '%TDM110%' or
/*Vactosertib*/
COMPRESS(ORD_CD,'') like '%19042VTSTB%' or
/*Zanubrutinib*/
COMPRESS(ORD_CD,'') like '%20238ZNB64%' or
/*Zolbetuximab or placebo*/
COMPRESS(ORD_CD,'') like '%18288ZBTXM%' or


/*Domin : Cancer Immunotherapy*/
/*Nivolumab*/
COMPRESS(ORD_CD,'') like '%NVLM%' or
/*Pembrolizumab*/
COMPRESS(ORD_CD,'') like '%15277MK%' or '%PBRL%' or
/*Nivolumab or Placebo / Ono-4538 or Placebo*/
COMPRESS(ORD_CD,'') like '%ONO%' or
/*Tislelizumab or placebo*/
COMPRESS(ORD_CD,'') like '%19163TSLZM%' or
/*Tremelimumab*/
COMPRESS(ORD_CD,'') like '%17169TMLM%' or
/*TTX-030*/
COMPRESS(ORD_CD,'') like '%21007TTX03%' or

/*Domin : 기타 항암제*/
/*Irinotecan - 세포독성항암제로도 분류가능*/
COMPRESS(ORD_CD,'') like '%IRNT%' or

/*Domin : 임상시험*/
/*WT1 -T Cell*/
COMPRESS(ORD_CD,'') like '%629WT1T%'
 ;

 quit;