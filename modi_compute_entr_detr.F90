MODULE MODI_COMPUTE_ENTR_DETR

INTERFACE

#ifdef USE_ACC
!$acc routine (COMPUTE_ENTR_DETR)
#endif
          SUBROUTINE COMPUTE_ENTR_DETR(KLON,KIDIA,KFDIA,KLEV,KK,KKB,KKE,KKL,OTEST,OTESTLCL,&
                            HFRAC_ICE,PFRAC_ICE,PRHODREF,&
                            PPRE_MINUS_HALF,&
                            PPRE_PLUS_HALF,PZZ,PDZZ,&
                            PTHVM,PTHLM,PRTM,PW_UP2,PTH_UP,&
                            PTHL_UP,PRT_UP,PLUP,&
                            PRC_UP,PRI_UP,PTHV_UP,&
                            PRSAT_UP,PRC_MIX,PRI_MIX,      &
                            PENTR,PDETR,PENTR_CLD,PDETR_CLD,&
                            PBUO_INTEG_DRY,PBUO_INTEG_CLD,&
                            PPART_DRY,KSTPT,KSTSZ,PSTACK)

          
#include "temp.h"

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KK
INTEGER,                INTENT(IN)   :: KKB          
INTEGER,                INTENT(IN)   :: KKE          
INTEGER,                INTENT(IN)   :: KKL          
LOGICAL,DIMENSION(KLON),   INTENT(IN)   :: OTEST 
LOGICAL,DIMENSION(KLON),   INTENT(IN)   :: OTESTLCL 
CHARACTER*1,            INTENT(IN)   :: HFRAC_ICE 
                                              
                                              
REAL, DIMENSION(KLON), INTENT(IN)      :: PFRAC_ICE 

REAL, DIMENSION(KLON),     INTENT(IN) ::  PRHODREF  
REAL, DIMENSION(KLON),     INTENT(IN) ::  PPRE_MINUS_HALF 
REAL, DIMENSION(KLON),     INTENT(IN) ::  PPRE_PLUS_HALF 
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PZZ       
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PDZZ       
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PTHVM      

REAL, DIMENSION(KLON,KLEV), INTENT(IN)     ::  PTHLM     
REAL, DIMENSION(KLON,KLEV), INTENT(IN)     ::  PRTM      
REAL, DIMENSION(KLON,KLEV), INTENT(IN)     ::  PW_UP2    
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PTH_UP,PTHL_UP,PRT_UP  
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PLUP      
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PRC_UP,PRI_UP   
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PTHV_UP 
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PRSAT_UP 
REAL, DIMENSION(KLON),   INTENT(INOUT)  ::  PRC_MIX, PRI_MIX      
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PENTR     
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PDETR     
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PENTR_CLD 
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PDETR_CLD 
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PBUO_INTEG_DRY, PBUO_INTEG_CLD
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PPART_DRY 
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

END SUBROUTINE COMPUTE_ENTR_DETR

END INTERFACE

END MODULE
