!     ######spl
#ifdef USE_ACC
!$acc routine (COMPUTE_MF_CLOUD_DIRECT)
#endif
      SUBROUTINE COMPUTE_MF_CLOUD_DIRECT(KLON,KIDIA,KFDIA, KLEV, KKB, KKE, KKL, &
                                        &KKLCL, PFRAC_UP, PRC_UP, PRI_UP,&
                                        &PRC_MF, PRI_MF, PCF_MF,KSTPT,KSTSZ,PSTACK)

#include "temp.h"

!     #################################################################
!!
!!****  *COMPUTE_MF_CLOUD_DIRECT* -
!!       compute diagnostic subgrid cumulus cloud caracteristics with a direct scheme
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
!!      Updraft variables are used directly to diagnose subgrid clouds
!!      This scheme may be activated only if the selected updraft model
!!      gives the updraft fraction as an output
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
!!     S. Riette moving of code previously in compute_mf_cloud code
!!
!!    MODIFICATIONS
!!    -------------
!!      Original 25 Aug 2011
!!      S. Riette Jan 2012: support for both order of vertical levels
!!      S. Riette Apr 2013: computation begins one level lower (to be able to have a cloud
!!                          on mass level just below the first saturated flux level)
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
USE MODD_CMFSHALL, ONLY : XKCF_MF
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!*                    0.1  Declaration of Arguments
!
INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KKB            ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE            ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKL            ! +1 if grid goes from ground to atmosphere top, -1 otherwise
INTEGER, DIMENSION(KLON),  INTENT(IN)   :: KKLCL          ! index of updraft condensation level
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   :: PFRAC_UP       ! Updraft Fraction
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)   :: PRC_UP,PRI_UP  ! updraft characteritics
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  :: PRC_MF, PRI_MF ! cloud content
REAL, DIMENSION(KLON,KLEV),   INTENT(OUT)  :: PCF_MF         ! and cloud fraction for MF scheme
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!
!*                    0.1  Declaration of local variables
!
INTEGER  :: JI,JK, JK0


!
!*                    0.2 Initialisation
!
!
!*      1. COMPUTATION OF SUBGRID CLOUD
!          ----------------------------

!
! Warning: updraft variables are on flux levels
! and PRC_MF, PRI_MF and PCF_MF are on mass levels
PRC_MF(KIDIA:KFDIA,:)=0.
PRI_MF(KIDIA:KFDIA,:)=0.
PCF_MF(KIDIA:KFDIA,:)=0.

DO JI=KIDIA,KFDIA
  JK0=KKLCL(JI)-KKL ! first mass level with cloud
  JK0=MAX(JK0, MIN(KKB,KKE)) !protection if KKL=1
  JK0=MIN(JK0, MAX(KKB,KKE)) !protection if KKL=-1
  DO JK=JK0,KKE-KKL,KKL
    PCF_MF(JI,JK ) = MAX( 0., MIN(1.,XKCF_MF *0.5* (       &
                &    PFRAC_UP(JI,JK) +  PFRAC_UP(JI,JK+KKL) ) ))
    PRC_MF(JI,JK)  = 0.5* XKCF_MF * ( PFRAC_UP(JI,JK)*PRC_UP(JI,JK)  &
                         + PFRAC_UP(JI,JK+KKL)*PRC_UP(JI,JK+KKL) )
    PRI_MF(JI,JK)  = 0.5* XKCF_MF * ( PFRAC_UP(JI,JK)*PRI_UP(JI,JK)  &
                         + PFRAC_UP(JI,JK+KKL)*PRI_UP(JI,JK+KKL) )
  END DO
END DO


END SUBROUTINE COMPUTE_MF_CLOUD_DIRECT
