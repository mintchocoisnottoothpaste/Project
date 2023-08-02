proc sql; 
create table surgery_selection as 
select * from temp_surgery
where
/*Esophagojejunostomy*/
COMPRESS(ECD_CD,'') like '%Q2601%' or 

/*Extirpation of Adnexal Tumor(Benign)*/
COMPRESS(ECD_CD,'') like '%R4421%' or 

/*Extirpation of Adnexal Tumor(Malignant) Simple*/
COMPRESS(ECD_CD,'') like '%R4423%' or 

/*Extirpation of Adnexal Tumor(Malignant with Hystrectomy) Simple*/
COMPRESS(ECD_CD,'') like '%R4427%' or 

/*Gastrojejunostomy*/
COMPRESS(ECD_CD,'') like '%Q2572%' or 

/*Hepatectomy(Lobectomy)*/
COMPRESS(ECD_CD,'') like '%Q7223%' or 

/*Hepatectomy(Segmentectomy)*/
COMPRESS(ECD_CD,'') like '%Q7222%' or 

/*Hepatectomy(Wedge Resection)*/
COMPRESS(ECD_CD,'') like '%Q7221%' or 

/*Operation of Liver Abscess(Hepatotomy for Drainage)*/
COMPRESS(ECD_CD,'') like '%Q7212%' or 

/*Wedge Resection of Lung(2-3개)*/
COMPRESS(ECD_CD,'') like '%O1402%' or 

/*Wedge Resection of Lung(Single)*/
COMPRESS(ECD_CD,'') like '%%O1401' or 

/*Pelvic and para-aortic Lymphadenectomy*/
COMPRESS(ECD_CD,'') like '%R4157%' or 

/*Ommaya Reservoir Insert*/
COMPRESS(ECD_CD,'') like '%S4712%' or 

/*Pancreatectomy(Distal Pancreatectomy)*/
COMPRESS(ECD_CD,'') like '%Q7565%' or 

/*Pancreaticoduodenectomy(Whipple's Op)*/
COMPRESS(ECD_CD,'') like '%Q7571' or 

/*Pancreaticoduodenectomy(Pylorus-Preserving Op)*/
COMPRESS(ECD_CD,'') like '%Q7572%' or 

/*Pancreaticoenterostomy(End-to-End Anastomosis)*/
COMPRESS(ECD_CD,'') like '%Q7592%' or 

/*Primary Repair of Bowel and Mesenteric Injury(Serosal or Perforated Intestine)*/
COMPRESS(ECD_CD,'') like '%Q2773%' or 

/*Primary Repair of Bowel and Mesenteric Injury(Serosa,Intestine,Mesentery)*/
COMPRESS(ECD_CD,'') like '%Q2775%' or 

/*Robot Assisted Gastrectomy A,B,C(위암센터),Robot Assisted Laparascopic Gastrectomy(위암센터)*/
COMPRESS(ECD_CD,'') like '%QZ961%' or 

/*Simple Closure of Perforated Stomach or Duodenum*/
COMPRESS(ECD_CD,'') like '%Q2540%' or 

/*Subtotal Gastrectomy(Distal) with Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q0252%' or 

/*Subtotal Gastrectomy(Others),Wedge Resection or Partial*/
COMPRESS(ECD_CD,'') like '%Q2597%' or 

/*Subtotal Gastrectomy(Proximal resection) with Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q0258%' or 

/*Subtotal Gastrectomy(Proximal resection) without Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q2598%' or 

/*Subtotal Gastrectomy(Pylous preserving) with Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q0254%' or 

/*Subtotal Gastrectomy(Pylous preserving) without Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q0255%' or 

/*Subtotal Gastrectomy(Wedge Resection) with Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q0256%' or 

/*Subtotal Gastrectomy(Wedge Resection) without Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q0257%' or 

/*Subtotal Gastrectomy(with Lymphatic Dissection) Partial*/
COMPRESS(ECD_CD,'') like '%Q2594%' or 

/*Subtotal Gastrectomy(with Lymphatic Dissection),Subtotal*/
COMPRESS(ECD_CD,'') like '%Q2595%' or 

/*Total Gastrectomy(Abdominal Approach) Including Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q2533%' or 

/*Total Gastrectomy(Thora and Abd. Approach) Including Lymphatic Dissection*/
COMPRESS(ECD_CD,'') like '%Q2534%' or 

/*Total Gastrectomy-Interposition of Jejunum(Thoracic and Abdominal Approach)*/
COMPRESS(ECD_CD,'') like '%QA534%' 
 ;
 quit;


 /* 
 S4712 의 경우 Shunt removal은 Ommaya reservoir insertion과 동일한 
 EDI code임에도 불구하고 관련의견 X 동일하게 EDI code로 잡혀있으면 이부분도 고려가 가능한지?
 */