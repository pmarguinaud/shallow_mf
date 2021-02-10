MODULE MODI_TRIDIAG_MASSFLUX

INTERFACE

#ifdef USE_ACC
!$acc routine (TRIDIAG_MASSFLUX)
#endif
       SUBROUTINE TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PVARM,PF,PDFDT,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,PVARP,KSTPT,KSTSZ,PSTACK             )

       
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
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PVARM   
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PF      
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PDFDT   
REAL,                   INTENT(IN) :: PTSTEP  
REAL,                   INTENT(IN) :: PIMPL   
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PDZZ    
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PRHODJ  

REAL, DIMENSION(KLON,KLEV), INTENT(OUT):: PVARP   
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

END SUBROUTINE TRIDIAG_MASSFLUX

END INTERFACE

END MODULE
