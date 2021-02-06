MODULE MODI_THL_RT_FROM_TH_R_MF

INTERFACE
SUBROUTINE THL_RT_FROM_TH_R_MF( KLON,KLEV,KRR,KRRL,KRRI,       &
                                      PTH, PR, PEXN, &
                                      PTHL, PRT                      )

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KRR           
INTEGER,                INTENT(IN)   :: KRRL          
INTEGER,                INTENT(IN)   :: KRRI          

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PTH      
REAL, DIMENSION(KLON,KLEV,KRR), INTENT(IN) :: PR       
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PEXN    

REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  :: PTHL     
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  :: PRT      

END SUBROUTINE THL_RT_FROM_TH_R_MF
END INTERFACE

END MODULE
