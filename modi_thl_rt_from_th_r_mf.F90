MODULE MODI_THL_RT_FROM_TH_R_MF

INTERFACE

#ifdef USE_ACC
!$acc routine (THL_RT_FROM_TH_R_MF)
#endif
      SUBROUTINE THL_RT_FROM_TH_R_MF( KLON,KIDIA,KFDIA,KLEV,KRR,KRRL,KRRI,       &
                                      PTH, PR, PEXN, &
                                      PTHL, PRT,KSTPT,KSTSZ,PSTACK                      )

#include "temp.h"

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KRR           
INTEGER,                INTENT(IN)   :: KRRL          
INTEGER,                INTENT(IN)   :: KRRI          

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PTH      
REAL, DIMENSION(KLON,KLEV,KRR), INTENT(IN) :: PR       
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PEXN    

REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  :: PTHL     
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  :: PRT      
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

END SUBROUTINE THL_RT_FROM_TH_R_MF

END INTERFACE

END MODULE
