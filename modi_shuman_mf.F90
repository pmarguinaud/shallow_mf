MODULE MODI_SHUMAN_MF

INTERFACE

      SUBROUTINE MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PA,PMZM,KSTPT,KSTSZ,PSTACK)

INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KIDIA
INTEGER,              INTENT(IN)       :: KFDIA
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU 
INTEGER,              INTENT(IN)       :: KKL    
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)      :: PMZM   
INTEGER,              INTENT(IN)       :: KSTSZ
INTEGER,              INTENT(IN)       :: KSTPT
REAL   ,              INTENT(INOUT)    :: PSTACK (KSTSZ)

END SUBROUTINE MZM_MF

      SUBROUTINE DZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PA,PDZM,KSTPT,KSTSZ,PSTACK)

INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KIDIA
INTEGER,              INTENT(IN)       :: KFDIA
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU 
INTEGER,              INTENT(IN)       :: KKL    
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     
                                                 
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)      :: PDZM   
                                                 INTEGER,              INTENT(IN)       :: KSTSZ
INTEGER,              INTENT(IN)       :: KSTPT
REAL   ,              INTENT(INOUT)    :: PSTACK (KSTSZ)

END SUBROUTINE DZM_MF

END INTERFACE

END MODULE
