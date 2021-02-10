MODULE MODI_COMPUTE_MF_CLOUD_DIRECT

INTERFACE

#ifdef USE_ACC
!$acc routine (COMPUTE_MF_CLOUD_DIRECT)
#endif
      SUBROUTINE COMPUTE_MF_CLOUD_DIRECT(KLON,KIDIA,KFDIA, KLEV, KKB, KKE, KKL, &
                                        &KKLCL, PFRAC_UP, PRC_UP, PRI_UP,&
                                        &PRC_MF, PRI_MF, PCF_MF,KSTPT,KSTSZ,PSTACK)

#include "temp.h"

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KKB            
INTEGER,                INTENT(IN)   :: KKE            
INTEGER,                INTENT(IN)   :: KKL            
INTEGER, DIMENSION(KLON),  INTENT(IN)   :: KKLCL          
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   :: PFRAC_UP       
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   :: PRC_UP,PRI_UP  
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  :: PRC_MF, PRI_MF 
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  :: PCF_MF         
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

END SUBROUTINE COMPUTE_MF_CLOUD_DIRECT

END INTERFACE

END MODULE
