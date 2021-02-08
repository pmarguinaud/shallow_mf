!     ######spl
      SUBROUTINE MF_TURB(KLON,KLEV,KSV,KKA,KKB,KKE,KKU,KKL,OMIXUV,    &
                ONOMIXLG,KSV_LGBEG,KSV_LGEND,                         &
                PIMPL, PTSTEP, PTSTEP_MET, PTSTEP_SV,                 &
                PDZZ,                                                 &
                PRHODJ,                                               &
                PTHLM,PTHVM,PRTM,PUM,PVM,PSVM,                        &
                PTHLDT,PRTDT,PUDT,PVDT,PSVDT,                         &
                PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP,PSV_UP,       &
                PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF,      &
                PFLXZSVMF                                             )

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
!
!
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!

REAL, DIMENSION(KLON,KLEV) :: ZVARS
REAL, DIMENSION(KLON,KLEV) :: ZMEMF
REAL, DIMENSION(KLON,KLEV) :: ZMZM

!
INTEGER :: ISV,JSV          !number of scalar variables and Loop counter
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
PFLXZSVMF = 0.
PSVDT = 0.

!
!----------------------------------------------------------------------------
!
!*      2. COMPUTE THE MEAN FLUX OF CONSERVATIVE VARIABLES at time t-dt
!          (equation (3) of Soares et al)
!          + THE MEAN FLUX OF THETA_V (buoyancy flux)
!          -----------------------------------------------
!   ( Resulting fluxes are in flux level (w-point) as PEMF and PTHL_UP )
!

CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,PTHLM(:,:),ZMZM)
PFLXZTHMF(:,:) = PEMF(:,:)*(PTHL_UP(:,:)-ZMZM)

CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,PRTM(:,:),ZMZM)
PFLXZRMF(:,:) =  PEMF(:,:)*(PRT_UP(:,:)-ZMZM)

CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,PTHVM(:,:),ZMZM)
PFLXZTHVMF(:,:) = PEMF(:,:)*(PTHV_UP(:,:)-ZMZM)

IF (OMIXUV) THEN
  CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,PUM(:,:),ZMZM)
  PFLXZUMF(:,:) =  PEMF(:,:)*(PU_UP(:,:)-ZMZM)
  CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,PVM(:,:),ZMZM)
  PFLXZVMF(:,:) =  PEMF(:,:)*(PV_UP(:,:)-ZMZM)
ELSE
  PFLXZUMF(:,:) = 0.
  PFLXZVMF(:,:) = 0.
ENDIF
!
!
!----------------------------------------------------------------------------
!
!*      3. COMPUTE TENDENCIES OF CONSERVATIVE VARIABLES (or treated as such...)
!          (implicit formulation)
!          --------------------------------------------
!

ZMEMF = - PEMF

!
!
! 3.1 Compute the tendency for the conservative potential temperature
!     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
!
CALL TRIDIAG_MASSFLUX(KLON,KLEV,KKA,KKB,KKE,KKU,KKL,PTHLM,PFLXZTHMF,ZMEMF,PTSTEP_MET,PIMPL,  &
                      PDZZ,PRHODJ,ZVARS )
! compute new flux
CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,ZVARS(:,:),ZMZM)
PFLXZTHMF(:,:) = PEMF(:,:)*(PTHL_UP(:,:)-ZMZM)

!!! compute THL tendency
!
PTHLDT(:,:)= (ZVARS(:,:)-PTHLM(:,:))/PTSTEP_MET

!
! 3.2 Compute the tendency for the conservative mixing ratio
!
CALL TRIDIAG_MASSFLUX(KLON,KLEV,KKA,KKB,KKE,KKU,KKL,PRTM(:,:),PFLXZRMF,ZMEMF,PTSTEP_MET,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )
! compute new flux
CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,ZVARS(:,:),ZMZM)
PFLXZRMF(:,:) =  PEMF(:,:)*(PRT_UP(:,:)-ZMZM)

!!! compute RT tendency
PRTDT(:,:) = (ZVARS(:,:)-PRTM(:,:))/PTSTEP_MET
!

IF (OMIXUV) THEN
  !
  ! 3.3 Compute the tendency for the (non conservative but treated as it) zonal momentum
  !     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
  !

  CALL TRIDIAG_MASSFLUX(KLON,KLEV,KKA,KKB,KKE,KKU,KKL,PUM,PFLXZUMF,ZMEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )
  ! compute new flux
  CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,ZVARS(:,:),ZMZM)
  PFLXZUMF(:,:) = PEMF(:,:)*(PU_UP(:,:)-ZMZM)

  ! compute U tendency
  PUDT(:,:)= (ZVARS(:,:)-PUM(:,:))/PTSTEP

  !
  !
  ! 3.4 Compute the tendency for the (non conservative but treated as it for the time beiing)
  !                                  meridian momentum
  !     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
  !
  CALL TRIDIAG_MASSFLUX(KLON,KLEV,KKA,KKB,KKE,KKU,KKL,PVM,PFLXZVMF,ZMEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )
  ! compute new flux
  CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,ZVARS(:,:),ZMZM)
  PFLXZVMF(:,:) = PEMF(:,:)*(PV_UP(:,:)-ZMZM)

  ! compute V tendency
  PVDT(:,:)= (ZVARS(:,:)-PVM(:,:))/PTSTEP
ELSE
  PUDT(:,:)=0.
  PVDT(:,:)=0.
ENDIF

DO JSV=1,ISV 

  IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
  
  !*     compute mean flux of scalar variables at time t-dt
  !   ( Resulting fluxes are in flux level (w-point) as PEMF and PTHL_UP )

  CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,PSVM(:,:,JSV),ZMZM)
  PFLXZSVMF(:,:,JSV) = PEMF(:,:)*(PSV_UP(:,:,JSV)-ZMZM)
  
  !
  ! 3.5 Compute the tendency for scalar variables
  !     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
  !
  CALL TRIDIAG_MASSFLUX(KLON,KLEV,KKA,KKB,KKE,KKU,KKL,PSVM(:,:,JSV),PFLXZSVMF(:,:,JSV),&
                        ZMEMF,PTSTEP_SV,PIMPL,PDZZ,PRHODJ,ZVARS )
  ! compute new flux
  CALL MZM_MF(KLON,KLEV,KKA,KKU,KKL,ZVARS,ZMZM)
  PFLXZSVMF(:,:,JSV) = PEMF(:,:)*(PSV_UP(:,:,JSV)-ZMZM)

  ! compute Sv tendency
  PSVDT(:,:,JSV)= (ZVARS(:,:)-PSVM(:,:,JSV))/PTSTEP_SV

ENDDO
!
END SUBROUTINE MF_TURB    
