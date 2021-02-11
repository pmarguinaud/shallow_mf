MODULE MODI_MF_TURB

INTERFACE

#ifdef USE_ACC
!$acc routine (MF_TURB) seq
#endif
      SUBROUTINE MF_TURB(KLON,KIDIA,KFDIA,KLEV,KSV,KKA,KKB,KKE,KKU,KKL,OMIXUV,    &
                ONOMIXLG,KSV_LGBEG,KSV_LGEND,                         &
                PIMPL, PTSTEP, PTSTEP_MET, PTSTEP_SV,                 &
                PDZZ,                                                 &
                PRHODJ,                                               &
                PTHLM,PTHVM,PRTM,PUM,PVM,PSVM,                        &
                PTHLDT,PRTDT,PUDT,PVDT,PSVDT,                         &
                PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP,PSV_UP,       &
                PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF,      &
                PFLXZSVMF,KSTPT,KSTSZ,PSTACK                                             )

      
#include "temp.h"

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KSV
INTEGER,                INTENT(IN)   :: KKA          
INTEGER,                INTENT(IN)   :: KKB          
INTEGER,                INTENT(IN)   :: KKE          
INTEGER,                INTENT(IN)   :: KKU          
INTEGER,                INTENT(IN)   :: KKL          
LOGICAL,                INTENT(IN)   :: OMIXUV      
LOGICAL,                INTENT(IN)   :: ONOMIXLG  
INTEGER,                INTENT(IN)   :: KSV_LGBEG 
INTEGER,                INTENT(IN)   :: KSV_LGEND 
REAL,                   INTENT(IN)   :: PIMPL       
REAL,                 INTENT(IN)     ::  PTSTEP   
REAL,                 INTENT(IN)     ::  PTSTEP_MET
REAL,                 INTENT(IN)     ::  PTSTEP_SV

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PDZZ        

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PRHODJ      

REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PTHLM        
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PRTM         

REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PTHVM 

REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PUM
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PVM

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN) ::  PSVM

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PTHLDT

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PRTDT 

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PUDT
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PVDT

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT) ::  PSVDT

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   ::  PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP
REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN) ::  PSV_UP

REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  ::  PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT)::  PFLXZSVMF
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

END SUBROUTINE MF_TURB    

END INTERFACE

END MODULE
