!     ######spl
      SUBROUTINE COMPUTE_MF_CLOUD(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,KRR,KRRL,KRRI,HMF_CLOUD,&
                                  PFRAC_ICE,                                            &
                                  PRC_UP,PRI_UP,PEMF,                                   &
                                  PTHL_UP, PRT_UP, PFRAC_UP,                            &
                                  PTHV_UP, PFRAC_ICE_UP, PRSAT_UP,                      &
                                  PEXNM, PTHLM, PRTM, PTHM, PTHVM, PRM,                 &
                                  PDZZ, PZZ, KKLCL,                                     &
                                  PPABSM, PRHODREF,                                     &
                                  PRC_MF, PRI_MF, PCF_MF, PSIGMF, PDEPTH,KSTPT,KSTSZ,PSTACK    )

#include "temp.h"

!     #################################################################
!!
!!****  *COMPUTE_MF_CLOUD* - 
!!       compute diagnostic subgrid cumulus cloud caracteristics
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to compute the cloud fraction and 
!!      the mean cloud content associated with clouds described by the 
!!      mass flux scheme
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
!!     REFERENCE
!!     ---------
!!
!!
!!     AUTHOR
!!     ------
!!
!!    MODIFICATIONS
!!    -------------
!!      Original
!!      S. Riette Dec 2010 BIGA case
!!      S. Riette Aug 2011 code is split into subroutines
!!      S. Riette Jan 2012: support for both order of vertical levels
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODI_COMPUTE_MF_CLOUD_DIRECT
!
USE PARKIND1, ONLY : JPRB

IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
!
INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KKA          ! near ground array index
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKU          ! uppest atmosphere array index
INTEGER,                INTENT(IN)   ::  KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
INTEGER,                INTENT(IN)   ::  KRR          ! number of moist var.
INTEGER,                INTENT(IN)   ::  KRRL         ! number of liquid water var.
INTEGER,                INTENT(IN)   ::  KRRI         ! number of ice water var.
CHARACTER (LEN=4),      INTENT(IN)   ::  HMF_CLOUD    ! Type of statistical cloud scheme
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PFRAC_ICE    ! liquid/ice fraction
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PRC_UP,PRI_UP,PEMF! updraft characteritics
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHL_UP, PRT_UP   ! rc,w,Mass Flux,Thetal,rt
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PFRAC_UP          ! Updraft Fraction
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHV_UP           ! updraft thetaV
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PFRAC_ICE_UP      ! liquid/solid fraction in updraft
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PRSAT_UP          ! Rsat in updraft
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PEXNM             ! exner function
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHLM, PRTM       ! cons. var. at t-dt
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PTHM, PTHVM       ! theta and thetaV
REAL, DIMENSION(KLON,KLEV,KRR), INTENT(IN)   ::  PRM               ! water var. at t-dt
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PDZZ, PZZ
INTEGER, DIMENSION(KLON),  INTENT(IN)   ::  KKLCL             ! index of updraft condensation level
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   ::  PPABSM, PRHODREF  ! environement
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PRC_MF, PRI_MF    ! cloud content (INPUT=environment, OUTPUT=conv. cloud)
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PCF_MF            ! and cloud fraction for MF scheme
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  ::  PSIGMF            ! SQRT(variance) for statistical cloud scheme
REAL, DIMENSION(KLON),     INTENT(IN)   ::  PDEPTH            ! Deepness of cloud

INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)

init_stack ()


!
!                       1.2  Declaration of local variables
!
!------------------------------------------------------------------------

!                     1. INITIALISATION
!
!
!                     2.1 Internal domain

PRC_MF(KIDIA:KFDIA,:) = 0.
PRI_MF(KIDIA:KFDIA,:) = 0.
PCF_MF(KIDIA:KFDIA,:) = 0.
PSIGMF(KIDIA:KFDIA,:) = 0.

IF (HMF_CLOUD == 'DIRE') THEN
  !Direct cloud scheme
  CALL COMPUTE_MF_CLOUD_DIRECT(KLON,KIDIA,KFDIA, KLEV, KKB, KKE, KKL, &
                              &KKLCL, PFRAC_UP, PRC_UP, PRI_UP,&
                              &PRC_MF, PRI_MF, PCF_MF,ISTPT,KSTSZ,PSTACK)
  !
ELSE
  WRITE(*,*) ' STOP'
  WRITE(*,*) ' Shallow convection cloud scheme not valid : HMF_CLOUD =',TRIM(HMF_CLOUD)
  CALL ABORT
  STOP
ENDIF


END SUBROUTINE COMPUTE_MF_CLOUD
