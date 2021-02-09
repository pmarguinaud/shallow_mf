!     ######spl
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

USE PARKIND1, ONLY : JPRB

!     #################################################################
!
!
!!****  *MF_TURB* - computes the MF_turbulent source terms for the prognostic
!!                  variables. 
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to compute the source terms in 
!!    the evolution equations due to the MF turbulent mixing. 
!!      The source term is computed as the divergence of the turbulent fluxes.
!
!!**  METHOD
!!    ------
!!    
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!     
!!
!!    MODIFICATIONS
!!    -------------
!!  10/2009     (C.Lac)        Introduction of different PTSTEP according to the
!!                              advection schemes
!!  09/2010     (V.Masson)     Optimization
!!     S. Riette Jan 2012: support for both order of vertical levels
!!                         suppression of useless initialisations
!!
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CMFSHALL
!
USE MODI_SHUMAN_MF
USE MODI_TRIDIAG_MASSFLUX
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
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
LOGICAL,                INTENT(IN)   :: OMIXUV      ! True if mixing of momentum
LOGICAL,                INTENT(IN)   :: ONOMIXLG  ! False if mixing of lagrangian tracer
INTEGER,                INTENT(IN)   :: KSV_LGBEG ! first index of lag. tracer
INTEGER,                INTENT(IN)   :: KSV_LGEND ! last  index of lag. tracer
REAL,                   INTENT(IN)   :: PIMPL       ! degree of implicitness
REAL,                 INTENT(IN)     ::  PTSTEP   ! Dynamical timestep 
REAL,                 INTENT(IN)     ::  PTSTEP_MET! Timestep for meteorological variables                        
REAL,                 INTENT(IN)     ::  PTSTEP_SV! Timestep for tracer variables
!
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PDZZ        ! metric coefficients

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PRHODJ      ! dry density * Grid size

!   Conservative var. at t-dt
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PTHLM        ! conservative pot. temp.
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PRTM         ! water var.  where 
!  Virtual potential temperature at t-dt
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PTHVM 
!  Momentum at t-dt
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PUM
REAL, DIMENSION(KLON,KLEV), INTENT(IN) ::  PVM
!  scalar variables at t-dt
REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN) ::  PSVM
!
! Tendencies of conservative variables
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PTHLDT

REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PRTDT 
! Tendencies of momentum
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PUDT
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT) ::  PVDT
! Tendencies of scalar variables
REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT) ::  PSVDT


! Updraft characteritics
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   ::  PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP
REAL, DIMENSION(KLON,KLEV,KSV), INTENT(IN) ::  PSV_UP
! Fluxes
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  ::  PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF

REAL, DIMENSION(KLON,KLEV,KSV), INTENT(OUT)::  PFLXZSVMF
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!
!
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!


temp (REAL, ZVARS, (KLON,KLEV))
temp (REAL, ZMEMF, (KLON,KLEV))
temp (REAL, ZMZM , (KLON,KLEV))

!
INTEGER :: ISV,JSV          !number of scalar variables and Loop counter

init_stack ()

alloc (ZMZM)
alloc (ZMEMF)
alloc (ZVARS)

!
!----------------------------------------------------------------------------
!
!*      1.PRELIMINARIES
!         -------------
!
!
! number of scalar var
ISV=KSV

!
PFLXZSVMF(KIDIA:KFDIA,:,:) = 0.
PSVDT(KIDIA:KFDIA,:,:) = 0.

!
!----------------------------------------------------------------------------
!
!*      2. COMPUTE THE MEAN FLUX OF CONSERVATIVE VARIABLES at time t-dt
!          (equation (3) of Soares et al)
!          + THE MEAN FLUX OF THETA_V (buoyancy flux)
!          -----------------------------------------------
!   ( Resulting fluxes are in flux level (w-point) as PEMF and PTHL_UP )
!

CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PTHLM,ZMZM,KSTPT,KSTSZ,PSTACK)
PFLXZTHMF(KIDIA:KFDIA,:) = PEMF(KIDIA:KFDIA,:)*(PTHL_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PRTM,ZMZM,KSTPT,KSTSZ,PSTACK)
PFLXZRMF(KIDIA:KFDIA,:) =  PEMF(KIDIA:KFDIA,:)*(PRT_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PTHVM,ZMZM,KSTPT,KSTSZ,PSTACK)
PFLXZTHVMF(KIDIA:KFDIA,:) = PEMF(KIDIA:KFDIA,:)*(PTHV_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

IF (OMIXUV) THEN
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PUM,ZMZM,KSTPT,KSTSZ,PSTACK)
  PFLXZUMF(KIDIA:KFDIA,:) =  PEMF(KIDIA:KFDIA,:)*(PU_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PVM,ZMZM,KSTPT,KSTSZ,PSTACK)
  PFLXZVMF(KIDIA:KFDIA,:) =  PEMF(KIDIA:KFDIA,:)*(PV_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))
ELSE
  PFLXZUMF(KIDIA:KFDIA,:) = 0.
  PFLXZVMF(KIDIA:KFDIA,:) = 0.
ENDIF
!
!
!----------------------------------------------------------------------------
!
!*      3. COMPUTE TENDENCIES OF CONSERVATIVE VARIABLES (or treated as such...)
!          (implicit formulation)
!          --------------------------------------------
!

ZMEMF(KIDIA:KFDIA,:) = - PEMF(KIDIA:KFDIA,:)

!
!
! 3.1 Compute the tendency for the conservative potential temperature
!     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
!
CALL TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PTHLM,PFLXZTHMF,ZMEMF,PTSTEP_MET,PIMPL,  &
                      PDZZ,PRHODJ,ZVARS,KSTPT,KSTSZ,PSTACK )
! compute new flux
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,ZVARS,ZMZM,KSTPT,KSTSZ,PSTACK)
PFLXZTHMF(KIDIA:KFDIA,:) = PEMF(KIDIA:KFDIA,:)*(PTHL_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

!!! compute THL tendency
!
PTHLDT(KIDIA:KFDIA,:)= (ZVARS(KIDIA:KFDIA,:)-PTHLM(KIDIA:KFDIA,:))/PTSTEP_MET

!
! 3.2 Compute the tendency for the conservative mixing ratio
!
CALL TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PRTM,PFLXZRMF,ZMEMF,PTSTEP_MET,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS,KSTPT,KSTSZ,PSTACK )
! compute new flux
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,ZVARS,ZMZM,KSTPT,KSTSZ,PSTACK)
PFLXZRMF(KIDIA:KFDIA,:) =  PEMF(KIDIA:KFDIA,:)*(PRT_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

!!! compute RT tendency
PRTDT(KIDIA:KFDIA,:) = (ZVARS(KIDIA:KFDIA,:)-PRTM(KIDIA:KFDIA,:))/PTSTEP_MET
!

IF (OMIXUV) THEN
  !
  ! 3.3 Compute the tendency for the (non conservative but treated as it) zonal momentum
  !     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
  !

  CALL TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PUM,PFLXZUMF,ZMEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS,KSTPT,KSTSZ,PSTACK )
  ! compute new flux
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,ZVARS,ZMZM,KSTPT,KSTSZ,PSTACK)
  PFLXZUMF(KIDIA:KFDIA,:) = PEMF(KIDIA:KFDIA,:)*(PU_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

  ! compute U tendency
  PUDT(KIDIA:KFDIA,:)= (ZVARS(KIDIA:KFDIA,:)-PUM(KIDIA:KFDIA,:))/PTSTEP

  !
  !
  ! 3.4 Compute the tendency for the (non conservative but treated as it for the time beiing)
  !                                  meridian momentum
  !     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
  !
  CALL TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PVM,PFLXZVMF,ZMEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS,KSTPT,KSTSZ,PSTACK )
  ! compute new flux
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,ZVARS,ZMZM,KSTPT,KSTSZ,PSTACK)
  PFLXZVMF(KIDIA:KFDIA,:) = PEMF(KIDIA:KFDIA,:)*(PV_UP(KIDIA:KFDIA,:)-ZMZM(KIDIA:KFDIA,:))

  ! compute V tendency
  PVDT(KIDIA:KFDIA,:)= (ZVARS(KIDIA:KFDIA,:)-PVM(KIDIA:KFDIA,:))/PTSTEP
ELSE
  PUDT(KIDIA:KFDIA,:)=0.
  PVDT(KIDIA:KFDIA,:)=0.
ENDIF

DO JSV=1,ISV 

  IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
  
  !*     compute mean flux of scalar variables at time t-dt
  !   ( Resulting fluxes are in flux level (w-point) as PEMF and PTHL_UP )

  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PSVM(:,:,JSV),ZMZM,KSTPT,KSTSZ,PSTACK)
  PFLXZSVMF(KIDIA:KFDIA,:,JSV) = PEMF(KIDIA:KFDIA,:)*(PSV_UP(KIDIA:KFDIA,:,JSV)-ZMZM(KIDIA:KFDIA,:))
  
  !
  ! 3.5 Compute the tendency for scalar variables
  !     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
  !
  CALL TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PSVM(:,:,JSV),PFLXZSVMF(:,:,JSV),&
                        ZMEMF,PTSTEP_SV,PIMPL,PDZZ,PRHODJ,ZVARS,KSTPT,KSTSZ,PSTACK )
  ! compute new flux
  CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,ZVARS,ZMZM,KSTPT,KSTSZ,PSTACK)
  PFLXZSVMF(KIDIA:KFDIA,:,JSV) = PEMF(KIDIA:KFDIA,:)*(PSV_UP(KIDIA:KFDIA,:,JSV)-ZMZM(KIDIA:KFDIA,:))

  ! compute Sv tendency
  PSVDT(KIDIA:KFDIA,:,JSV)= (ZVARS(KIDIA:KFDIA,:)-PSVM(KIDIA:KFDIA,:,JSV))/PTSTEP_SV

ENDDO
!
END SUBROUTINE MF_TURB    
