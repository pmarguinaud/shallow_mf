MODULE MODI_COMPUTE_BL89_ML

INTERFACE

#ifdef USE_ACC
!$acc routine (COMPUTE_BL89_ML)
#endif
      SUBROUTINE COMPUTE_BL89_ML(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PDZZ2D, &
             PTKEM_DEP,PG_O_THVREF,PVPT,KK,OUPORDN,OFLUX,PLWORK,KSTPT,KSTSZ,PSTACK)

      
#include "temp.h"

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KKA          
INTEGER,                INTENT(IN)   :: KKB          
INTEGER,                INTENT(IN)   :: KKE          
INTEGER,                INTENT(IN)   :: KKU          
INTEGER,                INTENT(IN)   :: KKL          
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)  :: PDZZ2D        
REAL, DIMENSION(KLON),     INTENT(IN)  :: PTKEM_DEP     
REAL, DIMENSION(KLON),     INTENT(IN)  :: PG_O_THVREF   
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)  :: PVPT          
INTEGER,                INTENT(IN)  :: KK            
LOGICAL,                INTENT(IN)  :: OUPORDN       
                                                     
LOGICAL,                INTENT(IN)  :: OFLUX         
REAL, DIMENSION(KLON),     INTENT(OUT) :: PLWORK        

INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

  
END SUBROUTINE COMPUTE_BL89_ML

END INTERFACE

END MODULE
