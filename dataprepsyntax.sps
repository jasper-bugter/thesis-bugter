* Encoding: UTF-8.

FILTER OFF.
USE ALL.
SELECT IF (Q3 > 0).
EXECUTE.

VARSTOCASES
  /ID=id
  /MAKE choice FROM DCE_Random1 DCE_Random2 DCE_Random3 DCE_Random4 DCE_Random5 DCE_Random6 
    DCE_Random7 DCE_Random8 DCE_Random9 DCE_Random10
  /INDEX=Index1(10) 
  /KEEP=Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q9bis Q9tris Q9quat Hidden_Price Hidden2_PriceRange Hidden3_Doel 
    Hidden4_Class Q14 Q15 Q16 Q17 Q18 Q19 Q21_r1 Q21_r2 Q21_r3 Q21_r4 Q21_r5 Q21_r6 Q21_r7 Q21_r8 
    Q23_r1 Q23_r2 Q23_r3 Q23_r4 Q23_r5 Q25_r1 Q25_r2 Q26remarks sys_RespNum sys_SumPageTimes sys_CBCVersion_DCE
  /NULL=KEEP.

RENAME VARIABLES (Index1 sys_CBCVersion_DCE =  Task Version). 
Execute.

SORT CASES BY Version Task.
DATASET ACTIVATE DataSet2.
SORT CASES BY Version Task.
DATASET ACTIVATE DataSet1.
MATCH FILES /FILE=*
  /TABLE='DataSet2'
  /BY Version Task.
EXECUTE.

SORT CASES BY id(A).

IF  (Concept = choice) Biofuel=1.
EXECUTE.

DO IF (choice > 0).
RECODE Biofuel (1=1) (ELSE=0).
END IF.
EXECUTE.

Recode Att2Emissiedekking (1=10) (2=25) (3=50) (4=75) (5=100) (6=125) (7=150) (8=200).
EXECUTE.

Recode Att4Tijdtotcompensatie (1=0.5) (2=1) (3=2) (4=4) (5=10) (6=20). 
EXECUTE. 

Recode Att6Extrakosten (1=5) (2=10) (3=15) (4=20) (5=25) (6=30).
EXECUTE.

COMPUTE displayprice=Att6Extrakosten * Hidden_Price /100.
EXECUTE.

COXREG Task
  /STATUS=Biofuel(1)
  /STRATA=id
  /METHOD=ENTER Att2Emissiedekking Att4Tijdtotcompensatie Att6Extrakosten Concept
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG Task
  /STATUS=Biofuel(1)
  /STRATA=id
  /CONTRAST (Att2Emissiedekking)=Indicator(1)
  /CONTRAST (Att6Extrakosten)=Indicator(1)
  /CONTRAST (Att4Tijdtotcompensatie)=Indicator(1)
  /METHOD=ENTER Att2Emissiedekking Att4Tijdtotcompensatie Att6Extrakosten Concept
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG Task
  /STATUS=Biofuel(1)
  /STRATA=id
  /CONTRAST (Att2Emissiedekking)=Indicator(1)
  /CONTRAST (Att4Tijdtotcompensatie)=Indicator(1)
  /METHOD=ENTER Att2Emissiedekking Att4Tijdtotcompensatie displayprice Hidden_Price Concept
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).


