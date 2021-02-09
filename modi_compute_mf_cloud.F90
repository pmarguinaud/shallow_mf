MODULE MODI_COMPUTE_MF_CLOUD

INTERFACE

      SUBROUTINE COMPUTE_MF_CLOUD(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,KRR,KRRL,KRRI,HMF_CLOUD,&
                                  PFRAC_ICE,                                            &
                                  PRC_UP,PRI_UP,PEMF,                                   &
                                  PTHL_UP, PRT_UP, PFRAC_UP,                            &
                                  PTHV_UP, PFRAC_ICE_UP, PRSAT_UP,                      &
                                  PEXNM, PTHLM, PRTM, PTHM, PTHVM, PRM,                 &
                                  PDZZ, PZZ, KKLCL,                                     &
                                  PPABSM, PRHODREF,                                     &
                                  PRC_MF, PRI_MF, PCF_MF, PSIGMF, PDEPTH,KSTPT,KSTSZ,PSTACK    )

INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KKA          
INTEGER,                INTENT(IN)   :: KKB          
INTEGER,                INTENT(IN)   :: KKE          
INTEGER,                INTENT(IN)   :: KKU          
INTEGER,                INTENT(IN)   ::  KKL          
INTEGER,                INTENT(IN)   ::  KRR          
INTEGER,                INTENT(IN)   ::  KRRL         
INTEGER,                INTENT(IN)   ::  KRRI         
CHARACTER (LEN=4),      INTENT(IN)   ::  HMF_CLOUD    
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PFRAC_ICE    
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PRC_UP,PRI_UP,PEMF
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHL_UP, PRT_UP   
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PFRAC_UP          
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHV_UP           
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PFRAC_ICE_UP      
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PRSAT_UP          
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PEXNM             
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHLM, PRTM       
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHM, PTHVM       
REAL, DIMENSION(KLON,KLEV,KRR), INTENT(IN)   ::  PRM               
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PDZZ, PZZ
INTEGER, DIMENSION(KLON),  INTENT(IN)   ::  KKLCL             
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PPABSM, PRHODREF  
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PRC_MF, PRI_MF    
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PCF_MF            
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PSIGMF            
REAL, DIMENSION(KLON),     INTENT(IN)   ::  PDEPTH            

INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

END SUBROUTINE COMPUTE_MF_CLOUD

END INTERFACE

END MODULE
