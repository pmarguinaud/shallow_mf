!     ######spl
#ifdef USE_ACC
!$acc routine (COMPUTE_UPDRAFT) seq
#endif
      SUBROUTINE COMPUTE_UPDRAFT(KLON,KIDIA,KFDIA,KLEV,KSV,KKA,KKB,KKE,KKU,KKL,HFRAC_ICE, &
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
                                 PDEPTH,KSTPT,KSTSZ,PSTACK     )

      
#include "temp.h"

USE PARKIND1, ONLY : JPRB
!     #################################################################
!!
!!****  *COMPUTE_UPDRAFT* - calculates caracteristics of the updraft 
!!                         
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to build the updraft model 
!!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      !!     REFERENCE
!!     ---------
!!       Book 1 of Meso-NH documentation (chapter Turbulence)
!!       Soares et al. 2004 QJ
!!
!!     AUTHOR
!!     ------
!!     J.Pergaud
!!     V.Masson : Optimization 07/2010
!!     S. Riette : 07/2010 : modification for reproducibility  
!!     S. Riette may 2011: ice added, interface modified
!!     S. Riette Jan 2012: support for both order of vertical levels
!!     V.Masson, C.Lac : 02/2011 : SV_UP initialized by a non-zero value
!!     S. Riette Apr 2013: improvement of continuity at the condensation level
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CST
USE MODD_CMFSHALL

USE MODI_COMPUTE_ENTR_DETR
USE MODI_TH_R_FROM_THL_RT_1D
USE MODI_SHUMAN_MF

USE MODI_COMPUTE_BL89_ML


IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
!
INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KSV
INTEGER,                INTENT(IN)   :: KKA          ! near ground array index
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKU          ! uppest atmosphere array index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
CHARACTER*1,            INTENT(IN)   :: HFRAC_ICE    ! partition liquid/ice scheme
LOGICAL,                INTENT(IN) :: OENTR_DETR! flag to recompute entrainment, detrainment and mass flux
LOGICAL,                INTENT(IN) :: OMIXUV    ! True if mixing of momentum
LOGICAL,                INTENT(IN)   :: ONOMIXLG  ! False if mixing of lagrangian tracer
INTEGER,                INTENT(IN)   :: KSV_LGBEG ! first index of lag. tracer
INTEGER,                INTENT(IN)   :: KSV_LGEND ! last  index of lag. tracer
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PZZ       !  Height at the flux point
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PDZZ      !  Metrics coefficient
 
REAL, DIMENSION(KLON),   INTENT(IN)   ::  PSFTH,PSFRV
! normal surface fluxes of theta,rv,(u,v) parallel to the orography
!
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PPABSM     ! Pressure at t-dt
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PRHODREF   ! dry density of the
                                                  ! reference state
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PUM        ! u mean wind
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PVM        ! v mean wind
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PTKEM      ! TKE at t-dt
!
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHM           ! liquid pot. temp. at t-dt
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PRVM           ! vapor mixing ratio at t-dt
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHLM,PRTM     ! cons. var. at t-dt

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN)   ::  PSVM           ! scalar var. at t-dt

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PTHL_UP,PRT_UP   ! updraft properties
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PU_UP, PV_UP     ! updraft wind components
REAL, DIMENSION(KLON,KLEV),   INTENT(INOUT)::  PRV_UP,PRC_UP, & ! updraft rv, rc
                                         PRI_UP,PTHV_UP,& ! updraft ri, THv
                                         PW_UP,PFRAC_UP,& ! updraft w, fraction
                                         PFRAC_ICE_UP,&   ! liquid/solid fraction in updraft
                                         PRSAT_UP         ! Rsat

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT)  ::  PSV_UP           ! updraft scalar var. 
                                         
REAL, DIMENSION(KLON,KLEV),   INTENT(INOUT)::  PEMF,PDETR,PENTR ! Mass_flux,
                                                          ! detrainment,entrainment
REAL, DIMENSION(KLON,KLEV),   INTENT(INOUT) :: PBUO_INTEG       ! Integrated Buoyancy 
INTEGER, DIMENSION(KLON),  INTENT(INOUT) :: KKLCL,KKETL,KKCTL! LCL, ETL, CTL
REAL, DIMENSION(KLON),     INTENT(OUT)   :: PDEPTH           ! Deepness of cloud
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!                       1.2  Declaration of local variables
!
!
! Mean environment variables at t-dt at flux point
! Theta,rv of
temp (REAL, ZRVM_F, (KLON,KLEV))
temp (REAL, ZTHM_F, (KLON,KLEV))
! updraft environnement
! wet entrainment and detrainment
temp (REAL, ZDETR_CLD     , (KLON,KLEV))
temp (REAL, ZENTR_CLD     , (KLON,KLEV))
temp (REAL, ZBUO_INTEG_CLD, (KLON,KLEV))
temp (REAL, ZBUO_INTEG_DRY, (KLON,KLEV))
temp (REAL, ZW_UP2        , (KLON,KLEV))
temp (REAL, ZG_O_THVREF   , (KLON,KLEV))
temp (REAL, ZTHVM         , (KLON,KLEV))
temp (REAL, ZTHVM_F       , (KLON,KLEV))
temp (REAL, ZPRES_F       , (KLON,KLEV))
temp (REAL, ZRHO_F        , (KLON,KLEV))
temp (REAL, ZVM_F         , (KLON,KLEV))
temp (REAL, ZUM_F         , (KLON,KLEV))
temp (REAL, ZTKEM_F       , (KLON,KLEV))
temp (REAL, ZTHLM_F       , (KLON,KLEV))
temp (REAL, ZRTM_F        , (KLON,KLEV))
! scalar variables 
temp (REAL, ZSVM_F        , (KLON,KLEV,KSV))
! guess of Rc and Ri for KF mixture
temp (REAL, ZRI_MIX       , (KLON,KLEV))
temp (REAL, ZRC_MIX       , (KLON,KLEV))
temp (REAL, ZTH_UP        , (KLON,KLEV))
! diminution coefficient for too high clouds 
temp (REAL, ZCOEF         , (KLON,KLEV))
! Surface w'thetav'
temp (REAL, ZWTHVSURF     , (KLON ))
REAL  :: ZRDORV       ! RD/RV
REAL  :: ZRVORD       ! RV/RD

temp (REAL, ZMIX2_CLD, (KLON))
temp (REAL, ZMIX3_CLD, (KLON))
temp (REAL, ZMIX2    , (KLON))
temp (REAL, ZMIX1    , (KLON))
! Upward Mixing length from the ground
temp (REAL, ZLUP     , (KLON))
INTEGER  :: ISV                ! Number of scalar variables                               
INTEGER  :: JK,JI,JSV          ! loop counters


temp (LOGICAL, GTESTETL, (KLON))
temp (LOGICAL, GTESTLCL, (KLON))
temp (LOGICAL, GTEST   , (KLON))
! Test if the ascent continue, if LCL or ETL is reached
LOGICAL                          ::  GLMIX 
                               ! To choose upward or downward mixing length

temp (LOGICAL, GWORK1, (KLON))
temp (LOGICAL, GWORK2, (KLON,KLEV))
INTEGER  :: ITEST

temp (REAL, ZPART_DRY, (KLON))
temp (REAL, ZRSATI   , (KLON))
temp (REAL, ZRSATW   , (KLON))
temp (REAL, ZRV_UP   , (KLON))
temp (REAL, ZRI_UP   , (KLON))
temp (REAL, ZRC_UP   , (KLON))

REAL  :: ZDEPTH_MAX1, ZDEPTH_MAX2 ! control auto-extinction process

REAL  :: ZTMAX,ZRMAX  ! control value

init_stack ()

alloc (ZPART_DRY)
alloc (ZRSATI)
alloc (ZRSATW)
alloc (ZRV_UP)
alloc (ZRI_UP)
alloc (ZRC_UP)
alloc (GWORK2)
alloc (GWORK1)
alloc (GTESTETL)
alloc (GTESTLCL)
alloc (GTEST)
alloc (ZLUP)
alloc (ZMIX2_CLD)
alloc (ZMIX3_CLD)
alloc (ZMIX2)
alloc (ZMIX1)
alloc (ZWTHVSURF)
alloc (ZCOEF)
alloc (ZRI_MIX)
alloc (ZRC_MIX)
alloc (ZTH_UP)
alloc (ZSVM_F)
alloc (ZDETR_CLD)
alloc (ZENTR_CLD)
alloc (ZBUO_INTEG_CLD)
alloc (ZBUO_INTEG_DRY)
alloc (ZW_UP2)
alloc (ZG_O_THVREF)
alloc (ZTHVM)
alloc (ZTHVM_F)
alloc (ZPRES_F)
alloc (ZRHO_F)
alloc (ZVM_F)
alloc (ZUM_F)
alloc (ZTKEM_F)
alloc (ZTHLM_F)
alloc (ZRTM_F)
alloc (ZRVM_F)
alloc (ZTHM_F)

! Thresholds for the  perturbation of
! theta_l and r_t at the first level of the updraft
ZTMAX=2.0
ZRMAX=1.E-3
!------------------------------------------------------------------------

!                     INITIALISATION

! Initialisation of the constants   
ZRDORV   = XRD / XRV   !=0.622
ZRVORD   = (XRV / XRD) 

ZDEPTH_MAX1=3000. ! clouds with depth inferior to this value are keeped untouched
ZDEPTH_MAX2=4000. ! clouds with depth superior to this value are suppressed

!                 Local variables, internal domain
!number of scalar variables
ISV=KSV

IF (OENTR_DETR) THEN
  ! Initialisation of intersesting Level :LCL,ETL,CTL
  KKLCL(KIDIA:KFDIA)=KKE
  KKETL(KIDIA:KFDIA)=KKE
  KKCTL(KIDIA:KFDIA)=KKE

  !
  ! Initialisation
  !* udraft governing variables
  PEMF(KIDIA:KFDIA,:)=0.
  PDETR(KIDIA:KFDIA,:)=0.
  PENTR(KIDIA:KFDIA,:)=0.

  ! Initialisation
  !* updraft core variables
  PRV_UP(KIDIA:KFDIA,:)=0.
  PRC_UP(KIDIA:KFDIA,:)=0.
  PRI_UP(KIDIA:KFDIA,:)=0.
  PW_UP(KIDIA:KFDIA,:)=0.
  ZTH_UP(KIDIA:KFDIA,:)=0.
  PFRAC_UP(KIDIA:KFDIA,:)=0.
  PTHV_UP(KIDIA:KFDIA,:)=0.

  PBUO_INTEG(KIDIA:KFDIA,:)=0.

  PFRAC_ICE_UP(KIDIA:KFDIA,:)=0.
  PRSAT_UP(KIDIA:KFDIA,:)=PRVM(KIDIA:KFDIA,:) ! should be initialised correctly but is (normaly) not used

  !cloud/dry air mixture cloud content
  ZRC_MIX(KIDIA:KFDIA,:) = 0.
  ZRI_MIX(KIDIA:KFDIA,:) = 0.

END IF

! Initialisation of environment variables at t-dt
! variables at flux level
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PTHLM,ZTHLM_F,ISTPT,KSTSZ,PSTACK)
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PRTM, ZRTM_F,ISTPT,KSTSZ,PSTACK )
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PUM,  ZUM_F,ISTPT,KSTSZ,PSTACK  )
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PVM,  ZVM_F,ISTPT,KSTSZ,PSTACK  )
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PTKEM,ZTKEM_F,ISTPT,KSTSZ,PSTACK)

DO JSV=1,ISV
  IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PSVM(:,:,JSV), ZSVM_F(:,:,JSV),ISTPT,KSTSZ,PSTACK)
END DO
!                     
!          Initialisation of updraft characteristics 
PTHL_UP(KIDIA:KFDIA,:)=ZTHLM_F(KIDIA:KFDIA,:)
PRT_UP(KIDIA:KFDIA,:)=ZRTM_F(KIDIA:KFDIA,:)
PU_UP(KIDIA:KFDIA,:)=ZUM_F(KIDIA:KFDIA,:)
PV_UP(KIDIA:KFDIA,:)=ZVM_F(KIDIA:KFDIA,:)
PSV_UP(KIDIA:KFDIA,:,:)=ZSVM_F(KIDIA:KFDIA,:,:)


! Computation or initialisation of updraft characteristics at the KKB level
! thetal_up,rt_up,thetaV_up, w2,Buoyancy term and mass flux (PEMF)

PTHL_UP(KIDIA:KFDIA,KKB)= ZTHLM_F(KIDIA:KFDIA,KKB)+MAX(0.,MIN(ZTMAX,(PSFTH(KIDIA:KFDIA)/SQRT(ZTKEM_F(KIDIA:KFDIA,KKB)))*XALP_PERT))
PRT_UP(KIDIA:KFDIA,KKB) = ZRTM_F(KIDIA:KFDIA,KKB)+MAX(0.,MIN(ZRMAX,(PSFRV(KIDIA:KFDIA)/SQRT(ZTKEM_F(KIDIA:KFDIA,KKB)))*XALP_PERT)) 


IF (OENTR_DETR) THEN
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PTHM ,     ZTHM_F,ISTPT,KSTSZ,PSTACK )
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PPABSM,    ZPRES_F,ISTPT,KSTSZ,PSTACK) 
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PRHODREF,  ZRHO_F,ISTPT,KSTSZ,PSTACK )   
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PRVM,      ZRVM_F,ISTPT,KSTSZ,PSTACK )

  ! thetav at mass and flux levels
  ZTHVM_F(KIDIA:KFDIA,:)=ZTHM_F(KIDIA:KFDIA,:)*((1.+ZRVORD*ZRVM_F(KIDIA:KFDIA,:))/(1.+ZRTM_F(KIDIA:KFDIA,:)))
  ZTHVM(KIDIA:KFDIA,:)=PTHM(KIDIA:KFDIA,:)*((1.+ZRVORD*PRVM(KIDIA:KFDIA,:))/(1.+PRTM(KIDIA:KFDIA,:)))

  PTHV_UP(KIDIA:KFDIA,:)=ZTHVM_F(KIDIA:KFDIA,:)

  ZW_UP2(KIDIA:KFDIA,:)=0.
  ZW_UP2(KIDIA:KFDIA,KKB) = MAX(0.0001,(2./3.)*ZTKEM_F(KIDIA:KFDIA,KKB))


  ! Computation of non conservative variable for the KKB level of the updraft
  ! (all or nothing ajustement)
  PRC_UP(KIDIA:KFDIA,KKB)=0.
  PRI_UP(KIDIA:KFDIA,KKB)=0.
  CALL TH_R_FROM_THL_RT_1D(KLON,KIDIA,KFDIA,HFRAC_ICE,PFRAC_ICE_UP(:,KKB),ZPRES_F(:,KKB), &
             PTHL_UP(:,KKB),PRT_UP(:,KKB),ZTH_UP(:,KKB), &
             PRV_UP(:,KKB),PRC_UP(:,KKB),PRI_UP(:,KKB),ZRSATW,ZRSATI,ISTPT,KSTSZ,PSTACK)

  ! compute updraft thevav and buoyancy term at KKB level
  PTHV_UP(KIDIA:KFDIA,KKB) = ZTH_UP(KIDIA:KFDIA,KKB)*((1+ZRVORD*PRV_UP(KIDIA:KFDIA,KKB))/(1+PRT_UP(KIDIA:KFDIA,KKB)))
  ! compute mean rsat in updraft
  PRSAT_UP(KIDIA:KFDIA,KKB) = ZRSATW(KIDIA:KFDIA)*(1-PFRAC_ICE_UP(KIDIA:KFDIA,KKB)) &
                            + ZRSATI(KIDIA:KFDIA)*PFRAC_ICE_UP(KIDIA:KFDIA,KKB)
                                                            
  ! Closure assumption for mass flux at KKB level
  !

  ZG_O_THVREF(KIDIA:KFDIA,:)=XG/ZTHVM_F(KIDIA:KFDIA,:)

  ! compute L_up
  GLMIX=.TRUE.
  ZTKEM_F(KIDIA:KFDIA,KKB)=0.

  CALL COMPUTE_BL89_ML(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PDZZ,ZTKEM_F(:,KKB),&
                       ZG_O_THVREF(:,KKB),ZTHVM,KKB,GLMIX,.TRUE.,ZLUP,ISTPT,KSTSZ,PSTACK)
  ZLUP(KIDIA:KFDIA)=MAX(ZLUP(KIDIA:KFDIA),1.E-10)

  ! Compute Buoyancy flux at the ground
  ZWTHVSURF(KIDIA:KFDIA) = (ZTHVM_F(KIDIA:KFDIA,KKB)/ZTHM_F(KIDIA:KFDIA,KKB))*PSFTH(KIDIA:KFDIA)+     &
                (0.61*ZTHM_F(KIDIA:KFDIA,KKB))*PSFRV(KIDIA:KFDIA)

  ! Mass flux at KKB level (updraft triggered if PSFTH>0.)
  WHERE (ZWTHVSURF(KIDIA:KFDIA)>0.)
    PEMF(KIDIA:KFDIA,KKB) = XCMF * ZRHO_F(KIDIA:KFDIA,KKB) * ((ZG_O_THVREF(KIDIA:KFDIA,KKB))&
                            *ZWTHVSURF(KIDIA:KFDIA)*ZLUP(KIDIA:KFDIA))**(1./3.)
    PFRAC_UP(KIDIA:KFDIA,KKB)=MIN(PEMF(KIDIA:KFDIA,KKB)/(SQRT(ZW_UP2(KIDIA:KFDIA,KKB))*ZRHO_F(KIDIA:KFDIA,KKB)),XFRAC_UP_MAX)
    ZW_UP2(KIDIA:KFDIA,KKB)=(PEMF(KIDIA:KFDIA,KKB)/(PFRAC_UP(KIDIA:KFDIA,KKB)*ZRHO_F(KIDIA:KFDIA,KKB)))**2
    GTEST(KIDIA:KFDIA)=.TRUE.
  ELSEWHERE
    PEMF(KIDIA:KFDIA,KKB) =0.
    GTEST(KIDIA:KFDIA)=.FALSE.
  ENDWHERE
ELSE
  GTEST(KIDIA:KFDIA)=PEMF(KIDIA:KFDIA,KKB+KKL)>0.
END IF

!--------------------------------------------------------------------------

!                        3. Vertical ascending loop
!                           -----------------------
!
! If GTEST = T the updraft starts from the KKB level and stops when GTEST becomes F
!
!
GTESTLCL(KIDIA:KFDIA)=.FALSE.
GTESTETL(KIDIA:KFDIA)=.FALSE.

!       Loop on vertical level



DO JK=KKB,KKE-KKL,KKL

! IF the updraft top is reached for all column, stop the loop on levels
  ITEST=COUNT(GTEST(KIDIA:KFDIA))
  IF (ITEST==0) CYCLE

!       Computation of entrainment and detrainment with KF90
!       parameterization in clouds and LR01 in subcloud layer


! to find the LCL (check if JK is LCL or not)

  WHERE ((PRC_UP(KIDIA:KFDIA,JK)+PRI_UP(KIDIA:KFDIA,JK)>0.).AND.(.NOT.(GTESTLCL(KIDIA:KFDIA))))
      KKLCL(KIDIA:KFDIA) = JK           
      GTESTLCL(KIDIA:KFDIA)=.TRUE.
  ENDWHERE

! COMPUTE PENTR and PDETR at mass level JK
  IF (OENTR_DETR) THEN
    IF(JK/=KKB) THEN
      ZRC_MIX(KIDIA:KFDIA,JK) = ZRC_MIX(KIDIA:KFDIA,JK-KKL) ! guess of Rc of mixture
      ZRI_MIX(KIDIA:KFDIA,JK) = ZRI_MIX(KIDIA:KFDIA,JK-KKL) ! guess of Ri of mixture
    ENDIF
    CALL COMPUTE_ENTR_DETR(KLON,KIDIA,KFDIA,KLEV,JK,KKB,KKE,KKL,GTEST,GTESTLCL,HFRAC_ICE,PFRAC_ICE_UP(:,JK),&
                           PRHODREF(:,JK),ZPRES_F(:,JK),ZPRES_F(:,JK+KKL),&
                           PZZ,PDZZ,ZTHVM,  &
                           PTHLM,PRTM,ZW_UP2,ZTH_UP(:,JK),   &
                           PTHL_UP(:,JK),PRT_UP(:,JK),ZLUP,         &
                           PRC_UP(:,JK),PRI_UP(:,JK),PTHV_UP(:,JK),&
                           PRSAT_UP(:,JK),ZRC_MIX(:,JK),ZRI_MIX(:,JK),                 &
                           PENTR(:,JK),PDETR(:,JK),ZENTR_CLD(:,JK),ZDETR_CLD(:,JK),&
                           ZBUO_INTEG_DRY(:,JK), ZBUO_INTEG_CLD(:,JK), &
                           ZPART_DRY,ISTPT,KSTSZ,PSTACK   )
    PBUO_INTEG(KIDIA:KFDIA,JK)=ZBUO_INTEG_DRY(KIDIA:KFDIA,JK)+ZBUO_INTEG_CLD(KIDIA:KFDIA,JK)

    IF (JK==KKB) THEN
       PDETR(KIDIA:KFDIA,JK)=0.
       ZDETR_CLD(KIDIA:KFDIA,JK)=0.
    ENDIF   
 
!       Computation of updraft characteristics at level JK+KKL
    WHERE(GTEST(KIDIA:KFDIA))
      ZMIX1(KIDIA:KFDIA)=0.5*(PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*(PENTR(KIDIA:KFDIA,JK)-PDETR(KIDIA:KFDIA,JK))
      PEMF(KIDIA:KFDIA,JK+KKL)=PEMF(KIDIA:KFDIA,JK)*EXP(2*ZMIX1(KIDIA:KFDIA))
    ENDWHERE
  ELSE
    GTEST(KIDIA:KFDIA) = (PEMF(KIDIA:KFDIA,JK+KKL)>0.)
  END IF 
  

! stop the updraft if MF becomes negative
  WHERE (GTEST(KIDIA:KFDIA).AND.(PEMF(KIDIA:KFDIA,JK+KKL)<=0.))
    PEMF(KIDIA:KFDIA,JK+KKL)=0.
    KKCTL(KIDIA:KFDIA) = JK+KKL
    GTEST(KIDIA:KFDIA)=.FALSE.
    PFRAC_ICE_UP(KIDIA:KFDIA,JK+KKL)=PFRAC_ICE_UP(KIDIA:KFDIA,JK)
    PRSAT_UP(KIDIA:KFDIA,JK+KKL)=PRSAT_UP(KIDIA:KFDIA,JK)
  ENDWHERE


! If the updraft did not stop, compute cons updraft characteritics at jk+KKL
  WHERE(GTEST(KIDIA:KFDIA))     
    ZMIX2(KIDIA:KFDIA) = (PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*PENTR(KIDIA:KFDIA,JK) !&
    ZMIX3_CLD(KIDIA:KFDIA) = (PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*(1.-ZPART_DRY(KIDIA:KFDIA))*ZDETR_CLD(KIDIA:KFDIA,JK) !&                   
    ZMIX2_CLD(KIDIA:KFDIA) = (PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*(1.-ZPART_DRY(KIDIA:KFDIA))*ZENTR_CLD(KIDIA:KFDIA,JK)
                
    PTHL_UP(KIDIA:KFDIA,JK+KKL)=(PTHL_UP(KIDIA:KFDIA,JK)*(1.-0.5*ZMIX2(KIDIA:KFDIA)) + PTHLM(KIDIA:KFDIA,JK)*ZMIX2(KIDIA:KFDIA)) &
                          /(1.+0.5*ZMIX2(KIDIA:KFDIA))   
    PRT_UP(KIDIA:KFDIA,JK+KKL) =(PRT_UP (KIDIA:KFDIA,JK)*(1.-0.5*ZMIX2(KIDIA:KFDIA)) + PRTM(KIDIA:KFDIA,JK)*ZMIX2(KIDIA:KFDIA))  &
                          /(1.+0.5*ZMIX2(KIDIA:KFDIA))
  ENDWHERE
  

  IF(OMIXUV) THEN
    IF(JK/=KKB) THEN
      WHERE(GTEST(KIDIA:KFDIA))
        PU_UP(KIDIA:KFDIA,JK+KKL) = (PU_UP (KIDIA:KFDIA,JK)*(1-0.5*ZMIX2(KIDIA:KFDIA)) + PUM(KIDIA:KFDIA,JK)*ZMIX2(KIDIA:KFDIA)+ &
                          0.5*XPRES_UV*(PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*&
                          ((PUM(KIDIA:KFDIA,JK+KKL)-PUM(KIDIA:KFDIA,JK))/PDZZ(KIDIA:KFDIA,JK+KKL)+&
                           (PUM(KIDIA:KFDIA,JK)-PUM(KIDIA:KFDIA,JK-KKL))/PDZZ(KIDIA:KFDIA,JK))        )   &
                          /(1+0.5*ZMIX2(KIDIA:KFDIA))
        PV_UP(KIDIA:KFDIA,JK+KKL) = (PV_UP (KIDIA:KFDIA,JK)*(1-0.5*ZMIX2(KIDIA:KFDIA)) + PVM(KIDIA:KFDIA,JK)*ZMIX2(KIDIA:KFDIA)+ &
                          0.5*XPRES_UV*(PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*&
                          ((PVM(KIDIA:KFDIA,JK+KKL)-PVM(KIDIA:KFDIA,JK))/PDZZ(KIDIA:KFDIA,JK+KKL)+&
                           (PVM(KIDIA:KFDIA,JK)-PVM(KIDIA:KFDIA,JK-KKL))/PDZZ(KIDIA:KFDIA,JK))    )   &
                          /(1+0.5*ZMIX2(KIDIA:KFDIA))
      ENDWHERE
    ELSE
      WHERE(GTEST(KIDIA:KFDIA))
        PU_UP(KIDIA:KFDIA,JK+KKL) = (PU_UP (KIDIA:KFDIA,JK)*(1-0.5*ZMIX2(KIDIA:KFDIA)) + PUM(KIDIA:KFDIA,JK)*ZMIX2(KIDIA:KFDIA)+ &
                          0.5*XPRES_UV*(PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*&
                          ((PUM(KIDIA:KFDIA,JK+KKL)-PUM(KIDIA:KFDIA,JK))/PDZZ(KIDIA:KFDIA,JK+KKL))        )   &
                          /(1+0.5*ZMIX2(KIDIA:KFDIA))
        PV_UP(KIDIA:KFDIA,JK+KKL) = (PV_UP (KIDIA:KFDIA,JK)*(1-0.5*ZMIX2(KIDIA:KFDIA)) + PVM(KIDIA:KFDIA,JK)*ZMIX2(KIDIA:KFDIA)+ &
                          0.5*XPRES_UV*(PZZ(KIDIA:KFDIA,JK+KKL)-PZZ(KIDIA:KFDIA,JK))*&
                          ((PVM(KIDIA:KFDIA,JK+KKL)-PVM(KIDIA:KFDIA,JK))/PDZZ(KIDIA:KFDIA,JK+KKL))    )   &
                          /(1+0.5*ZMIX2(KIDIA:KFDIA))
      ENDWHERE

    ENDIF
  ENDIF
  DO JSV=1,ISV 
     IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
      WHERE(GTEST(KIDIA:KFDIA)) 
           PSV_UP(KIDIA:KFDIA,JK+KKL,JSV) = (PSV_UP (KIDIA:KFDIA,JK,JSV)*(1-0.5*ZMIX2(KIDIA:KFDIA)) + &
                        PSVM(KIDIA:KFDIA,JK,JSV)*ZMIX2(KIDIA:KFDIA))  /(1+0.5*ZMIX2(KIDIA:KFDIA))
      ENDWHERE                        
  END DO  
  
 IF (OENTR_DETR) THEN

! Compute non cons. var. at level JK+KKL
  ZRC_UP(KIDIA:KFDIA)=PRC_UP(KIDIA:KFDIA,JK) ! guess = level just below
  ZRI_UP(KIDIA:KFDIA)=PRI_UP(KIDIA:KFDIA,JK) ! guess = level just below
  CALL TH_R_FROM_THL_RT_1D(KLON,KIDIA,KFDIA,HFRAC_ICE,PFRAC_ICE_UP(:,JK+KKL),ZPRES_F(:,JK+KKL), &
          PTHL_UP(:,JK+KKL),PRT_UP(:,JK+KKL),ZTH_UP(:,JK+KKL),              &
          ZRV_UP,ZRC_UP,ZRI_UP,ZRSATW,ZRSATI,ISTPT,KSTSZ,PSTACK)
  WHERE(GTEST(KIDIA:KFDIA))
    PRC_UP(KIDIA:KFDIA,JK+KKL)=ZRC_UP(KIDIA:KFDIA)
    PRV_UP(KIDIA:KFDIA,JK+KKL)=ZRV_UP(KIDIA:KFDIA)
    PRI_UP(KIDIA:KFDIA,JK+KKL)=ZRI_UP(KIDIA:KFDIA)
    PRSAT_UP(KIDIA:KFDIA,JK+KKL) = ZRSATW(KIDIA:KFDIA)*(1-PFRAC_ICE_UP(KIDIA:KFDIA,JK+KKL)) &
                                 + ZRSATI(KIDIA:KFDIA)*PFRAC_ICE_UP(KIDIA:KFDIA,JK+KKL)
  ENDWHERE
  

! Compute the updraft theta_v, buoyancy and w**2 for level JK+KKL
  WHERE(GTEST(KIDIA:KFDIA))
    PTHV_UP(KIDIA:KFDIA,JK+KKL) = ZTH_UP(KIDIA:KFDIA,JK+KKL)*((1+ZRVORD*PRV_UP(KIDIA:KFDIA,JK+KKL))/(1+PRT_UP(KIDIA:KFDIA,JK+KKL)))
    WHERE (ZBUO_INTEG_DRY(KIDIA:KFDIA,JK)>0.)
      ZW_UP2(KIDIA:KFDIA,JK+KKL)  = ZW_UP2(KIDIA:KFDIA,JK) + 2.*(XABUO-XBENTR*XENTR_DRY)* ZBUO_INTEG_DRY(KIDIA:KFDIA,JK)
    ELSEWHERE
      ZW_UP2(KIDIA:KFDIA,JK+KKL)  = ZW_UP2(KIDIA:KFDIA,JK) + 2.*XABUO* ZBUO_INTEG_DRY(KIDIA:KFDIA,JK)
    ENDWHERE
    ZW_UP2(KIDIA:KFDIA,JK+KKL)  = ZW_UP2(KIDIA:KFDIA,JK+KKL)*(1.-(XBDETR*ZMIX3_CLD(KIDIA:KFDIA)+XBENTR*ZMIX2_CLD(KIDIA:KFDIA)))&
            /(1.+(XBDETR*ZMIX3_CLD(KIDIA:KFDIA)+XBENTR*ZMIX2_CLD(KIDIA:KFDIA))) &
            +2.*(XABUO)*ZBUO_INTEG_CLD(KIDIA:KFDIA,JK)/(1.+(XBDETR*ZMIX3_CLD(KIDIA:KFDIA)+XBENTR*ZMIX2_CLD(KIDIA:KFDIA)))
 ENDWHERE


  ! Test if the updraft has reach the ETL
  GTESTETL(KIDIA:KFDIA)=.FALSE.
  WHERE (GTEST(KIDIA:KFDIA).AND.(PBUO_INTEG(KIDIA:KFDIA,JK)<=0.))
      KKETL(KIDIA:KFDIA) = JK+KKL
      GTESTETL(KIDIA:KFDIA)=.TRUE.
  ENDWHERE

  ! Test is we have reached the top of the updraft
  WHERE (GTEST(KIDIA:KFDIA).AND.((ZW_UP2(KIDIA:KFDIA,JK+KKL)<=0.).OR.(PEMF(KIDIA:KFDIA,JK+KKL)<=0.)))
      ZW_UP2(KIDIA:KFDIA,JK+KKL)=0.
      PEMF(KIDIA:KFDIA,JK+KKL)=0.
      GTEST(KIDIA:KFDIA)=.FALSE.
      PTHL_UP(KIDIA:KFDIA,JK+KKL)=ZTHLM_F(KIDIA:KFDIA,JK+KKL)
      PRT_UP(KIDIA:KFDIA,JK+KKL)=ZRTM_F(KIDIA:KFDIA,JK+KKL)
      PRC_UP(KIDIA:KFDIA,JK+KKL)=0.
      PRI_UP(KIDIA:KFDIA,JK+KKL)=0.
      PRV_UP(KIDIA:KFDIA,JK+KKL)=0.
      PTHV_UP(KIDIA:KFDIA,JK+KKL)=ZTHVM_F(KIDIA:KFDIA,JK+KKL)
      PFRAC_UP(KIDIA:KFDIA,JK+KKL)=0.
      KKCTL(KIDIA:KFDIA)=JK+KKL
  ENDWHERE
 
  ! compute frac_up at JK+KKL
  WHERE (GTEST(KIDIA:KFDIA))
    PFRAC_UP(KIDIA:KFDIA,JK+KKL)=PEMF(KIDIA:KFDIA,JK+KKL)/(SQRT(ZW_UP2(KIDIA:KFDIA,JK+KKL))*ZRHO_F(KIDIA:KFDIA,JK+KKL))
  ENDWHERE

  ! Updraft fraction must be smaller than XFRAC_UP_MAX
  WHERE (GTEST(KIDIA:KFDIA))
    PFRAC_UP(KIDIA:KFDIA,JK+KKL)=MIN(XFRAC_UP_MAX,PFRAC_UP(KIDIA:KFDIA,JK+KKL))
  ENDWHERE

  ! When cloudy and non-buoyant, updraft fraction must decrease
  WHERE ((GTEST(KIDIA:KFDIA).AND.GTESTETL(KIDIA:KFDIA)).AND.GTESTLCL(KIDIA:KFDIA))
    PFRAC_UP(KIDIA:KFDIA,JK+KKL)=MIN(PFRAC_UP(KIDIA:KFDIA,JK+KKL),PFRAC_UP(KIDIA:KFDIA,JK))
  ENDWHERE

  ! Mass flux is updated with the new updraft fraction
  IF (OENTR_DETR) PEMF(KIDIA:KFDIA,JK+KKL)=PFRAC_UP(KIDIA:KFDIA,JK+KKL)*SQRT(ZW_UP2(KIDIA:KFDIA,JK+KKL))*ZRHO_F(KIDIA:KFDIA,JK+KKL)

 END IF

ENDDO

IF(OENTR_DETR) THEN

  PW_UP(KIDIA:KFDIA,:)=SQRT(ZW_UP2(KIDIA:KFDIA,:))

  PEMF(KIDIA:KFDIA,KKB) =0.

! Limits the shallow convection scheme when cloud heigth is higher than 3000m.
! To do this, mass flux is multiplied by a coefficient decreasing linearly
! from 1 (for clouds of ZDEPTH_MAX1 m of depth) to 0 (for clouds of ZDEPTH_MAX2 m of depth).
! This way, all MF fluxes are diminished by this amount.
! Diagnosed cloud fraction is also multiplied by the same coefficient.
!
  DO JI=KIDIA,KFDIA 
     PDEPTH(JI) = MAX(0., PZZ(JI,KKCTL(JI)) -  PZZ(JI,KKLCL(JI)) )
  END DO

  GWORK1(KIDIA:KFDIA)= (GTESTLCL(KIDIA:KFDIA) .AND. (PDEPTH(KIDIA:KFDIA) > ZDEPTH_MAX1) )
  GWORK2(KIDIA:KFDIA,:) = SPREAD( GWORK1(KIDIA:KFDIA), DIM=2, NCOPIES=MAX(KKU,KKA) )
  ZCOEF(KIDIA:KFDIA,:) = SPREAD( (1.-(PDEPTH(KIDIA:KFDIA)-ZDEPTH_MAX1)/(ZDEPTH_MAX2-ZDEPTH_MAX1)), DIM=2, NCOPIES=KLEV)
  ZCOEF(KIDIA:KFDIA,:)=MIN(MAX(ZCOEF(KIDIA:KFDIA,:),0.),1.)
  WHERE (GWORK2(KIDIA:KFDIA,:)) 
    PEMF(KIDIA:KFDIA,:)     = PEMF(KIDIA:KFDIA,:)     * ZCOEF(KIDIA:KFDIA,:)
    PFRAC_UP(KIDIA:KFDIA,:) = PFRAC_UP(KIDIA:KFDIA,:) * ZCOEF(KIDIA:KFDIA,:)
  ENDWHERE
ENDIF

END SUBROUTINE COMPUTE_UPDRAFT
