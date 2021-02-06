MODULE MODI_COMPUTE_UPDRAFT

INTERFACE
SUBROUTINE COMPUTE_UPDRAFT(KLON,KLEV,KSV,KKA,KKB,KKE,KKU,KKL,HFRAC_ICE, &
                                 OENTR_DETR,OMIXUV,               &
                                 ONOMIXLG,KSV_LGBEG,KSV_LGEND,    &
                                 PZZ,PDZZ,                        &
                                 PSFTH,PSFRV,                     &
                                 PPABSM,PRHODREF,PUM,PVM, PTKEM,  &
                                 PTHM,PRVM,PTHLM,PRTM,            &
                                 PSVM,PTHL_UP,PRT_UP,             &
                                 PRV_UP,PRC_UP,PRI_UP,PTHV_UP,    &
                                 PW_UP,PU_UP, PV_UP, PSV_UP,      &
                                 PFRAC_UP,PFRAC_ICE_UP,PRSAT_UP,  &
                                 PEMF,PDETR,PENTR,                &
                                 PBUO_INTEG,KKLCL,KKETL,KKCTL,    &
                                 PDEPTH     )

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KSV
INTEGER,                INTENT(IN)   :: KKA          
INTEGER,                INTENT(IN)   :: KKB          
INTEGER,                INTENT(IN)   :: KKE          
INTEGER,                INTENT(IN)   :: KKU          
INTEGER,                INTENT(IN)   :: KKL          
CHARACTER*1,            INTENT(IN)   :: HFRAC_ICE    
LOGICAL,                INTENT(IN) :: OENTR_DETR
LOGICAL,                INTENT(IN) :: OMIXUV    
LOGICAL,                INTENT(IN)   :: ONOMIXLG  
INTEGER,                INTENT(IN)   :: KSV_LGBEG 
INTEGER,                INTENT(IN)   :: KSV_LGEND 
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PZZ       
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PDZZ      
 
REAL, DIMENSION(KLON),   INTENT(IN)   ::  PSFTH,PSFRV

REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PPABSM     
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PRHODREF   
                                                  
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PUM        
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PVM        
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PTKEM      

REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHM           
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PRVM           
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHLM,PRTM     

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN)   ::  PSVM           

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PTHL_UP,PRT_UP   
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PU_UP, PV_UP     
REAL, DIMENSION(KLON,KLEV),   INTENT(INOUT)::  PRV_UP,PRC_UP, & 
                                         PRI_UP,PTHV_UP,& 
                                         PW_UP,PFRAC_UP,& 
                                         PFRAC_ICE_UP,&   
                                         PRSAT_UP         

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT)  ::  PSV_UP           
                                         
REAL, DIMENSION(KLON,KLEV),   INTENT(INOUT)::  PEMF,PDETR,PENTR 
                                                          
REAL, DIMENSION(KLON,KLEV),   INTENT(INOUT) :: PBUO_INTEG       
INTEGER, DIMENSION(KLON),  INTENT(INOUT) :: KKLCL,KKETL,KKCTL
REAL, DIMENSION(KLON),     INTENT(OUT)   :: PDEPTH           

END SUBROUTINE COMPUTE_UPDRAFT
END INTERFACE

END MODULE
