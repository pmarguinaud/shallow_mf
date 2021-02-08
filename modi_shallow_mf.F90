MODULE MODI_SHALLOW_MF

INTERFACE

      SUBROUTINE SHALLOW_MF(KLON,KIDIA,KFDIA,KLEV,KSV,KKA,KKU,KKL,KRR,KRRL,KRRI,  &
                HMF_UPDRAFT, HMF_CLOUD, HFRAC_ICE, OMIXUV,            &
                ONOMIXLG,KSV_LGBEG,KSV_LGEND,                         &
                PIMPL_MF, PTSTEP, PTSTEP_MET, PTSTEP_SV,              &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXNM,                                        &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,PSVM,                          &
                PDUDT_MF,PDVDT_MF,                                    &
                PDTHLDT_MF,PDRTDT_MF,PDSVDT_MF,                       &
                PSIGMF,PRC_MF,PRI_MF,PCF_MF,PFLXZTHVMF,               &
                PFLXZTHMF,PFLXZRMF,PFLXZUMF,PFLXZVMF,                 &
                PTHL_UP,PRT_UP,PRV_UP,PRC_UP,PRI_UP,                  &
                PU_UP, PV_UP, PTHV_UP, PW_UP,                         &
                PFRAC_UP,PEMF,PDETR,PENTR,                            &
                KKLCL,KKETL,KKCTL                                     )

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KSV
INTEGER,                INTENT(IN)   :: KKA          
INTEGER,                INTENT(IN)   :: KKU          
INTEGER,                INTENT(IN)   :: KKL          
INTEGER,                INTENT(IN)   :: KRR          
INTEGER,                INTENT(IN)   :: KRRL         
INTEGER,                INTENT(IN)   :: KRRI         
CHARACTER (LEN=4),      INTENT(IN)   :: HMF_UPDRAFT  
                                     
CHARACTER (LEN=4),      INTENT(IN)   :: HMF_CLOUD    
                                                     
CHARACTER*1,            INTENT(IN)   :: HFRAC_ICE    
LOGICAL,                INTENT(IN)   :: OMIXUV    
LOGICAL,                INTENT(IN)   :: ONOMIXLG  
INTEGER,                INTENT(IN)   :: KSV_LGBEG 
INTEGER,                INTENT(IN)   :: KSV_LGEND 
REAL,                   INTENT(IN)   :: PIMPL_MF     
REAL,              INTENT(IN)     ::  PTSTEP   
REAL,              INTENT(IN)     ::  PTSTEP_MET
REAL,              INTENT(IN)     ::  PTSTEP_SV

REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PZZ         
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PDZZ        
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PRHODJ      
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PRHODREF    
                                                           
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PPABSM      
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PEXNM       

REAL, DIMENSION(KLON),   INTENT(IN)   ::  PSFTH,PSFRV 
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   ::  PTHM        
REAL, DIMENSION(KLON,KLEV,KRR), INTENT(IN) ::  PRM         
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PUM,PVM     
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PTKEM       

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN) ::  PSVM        

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)::  PDUDT_MF     
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)::  PDVDT_MF     
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)::  PDTHLDT_MF   
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)::  PDRTDT_MF    
REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT)::  PDSVDT_MF    

REAL, DIMENSION(KLON,KLEV), INTENT(OUT)     ::  PSIGMF,PRC_MF,PRI_MF,PCF_MF 
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)     ::  PFLXZTHVMF           
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)     ::  PFLXZTHMF
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)     ::  PFLXZRMF
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)     ::  PFLXZUMF
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)     ::  PFLXZVMF
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PTHL_UP   
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PRT_UP    
REAL, DIMENSION(KLON,KLEV), INTENT(OUT) ::  PRV_UP    
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PU_UP     
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PV_UP     
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PRC_UP    
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PRI_UP    
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PTHV_UP   
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PW_UP     
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PFRAC_UP  
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) ::  PEMF      
REAL, DIMENSION(KLON,KLEV), INTENT(OUT) ::  PDETR     
REAL, DIMENSION(KLON,KLEV), INTENT(OUT) ::  PENTR     
INTEGER,DIMENSION(KLON), INTENT(OUT) :: KKLCL,KKETL,KKCTL 

END SUBROUTINE SHALLOW_MF

END INTERFACE

END MODULE
