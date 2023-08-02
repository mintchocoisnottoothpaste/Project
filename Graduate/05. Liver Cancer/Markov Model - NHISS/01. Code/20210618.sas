/*
1. time
(1) LC=0 and death=0 : end of study (2017.12.31) - start date (2009.01.01)
(2) LC=1 and death=0 : LC diagnosis date (----.--.--) - start date (2009.01.01)
(3) LC=0 and death=1 : death date (----.--.--) - start date (2009.01.01)
(4) LC=1 and death=1 : LC diagnosis date(----.--.--) - start date (2009.01.01)

2. status
1=LC diagnosis
0=no LC diagnosis 
*/

proc phreg data=base;
class sex (ref="f")  age (ref="40-49") family_history  (ref="0")
		  DM (ref="0") FLD (ref="0") cirrhosis  (ref="0")
		  HBV  (ref="0")  HCV (ref="0")  others (ref="0")
		  Drink (ref="0") Smok (ref="0") Obese (ref="0");
model time*status(0)= sex age family_history DM FLD cirrhosis HBV HCV others Drink Smok Obese;run;
