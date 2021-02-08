MODULE MODI_SHUMAN_MF

INTERFACE

      SUBROUTINE MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PA,PMZM)

INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KIDIA
INTEGER,              INTENT(IN)       :: KFDIA
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU 
INTEGER,              INTENT(IN)       :: KKL    
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)      :: PMZM   

END SUBROUTINE MZM_MF

      SUBROUTINE DZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PA,PDZM)

INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KIDIA
INTEGER,              INTENT(IN)       :: KFDIA
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU 
INTEGER,              INTENT(IN)       :: KKL    
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     
                                                 
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)      :: PDZM   

END SUBROUTINE DZM_MF

END INTERFACE

END MODULE
