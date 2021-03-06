!     ######spl
#ifdef USE_ACC
!$acc routine (COMPUTE_FRAC_ICE2D)
#endif
      SUBROUTINE COMPUTE_FRAC_ICE2D(KLON,KIDIA,KFDIA,KLEV,HFRAC_ICE,PFRAC_ICE,PT,KSTPT,KSTSZ,PSTACK)
!    ##########################################################

#include "temp.h"

!
!
!!****  *COMPUTE_FRAC_ICE* - computes ice fraction
!!
!!    PURPOSE
!!    -------
!!
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
!!      Julien PERGAUD      * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         13/03/06
!!      S. Riette        April 2011 optimisation
!!
!! --------------------------------------------------------------------------
!       0. DECLARATIONS
!          ------------
!
USE PARKIND1, ONLY : JPRB
USE MODI_COMPUTE_FRAC_ICE1D
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER             , INTENT(IN)    :: KLON
INTEGER             , INTENT(IN)    :: KIDIA
INTEGER             , INTENT(IN)    :: KFDIA
INTEGER             , INTENT(IN)    :: KLEV
CHARACTER*1         , INTENT(IN)    :: HFRAC_ICE ! scheme to use
REAL, DIMENSION(KLON,KLEV), INTENT(IN)    :: PT        ! Temperature
REAL, DIMENSION(KLON,KLEV), INTENT(INOUT) :: PFRAC_ICE ! Ice fraction (1 for ice only, 0 for liquid only)
INTEGER             , INTENT(IN)    :: KSTSZ
INTEGER             , INTENT(IN)    :: KSTPT
REAL                , INTENT(INOUT) :: PSTACK (KSTSZ)
!-------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: JK

init_stack ()

!-------------------------------------------------------------------------
!
!       0.3  Initialisation
!
!
!----------------------------------------------------------------------------
!
!       1 Compute FRAC_ICE
!         ----------------
!
DO JK=1, KLEV
  CALL COMPUTE_FRAC_ICE1D(KLON,KIDIA,KFDIA,HFRAC_ICE,PFRAC_ICE(:,JK),PT(:,JK),ISTPT,KSTSZ,PSTACK)
ENDDO


END SUBROUTINE COMPUTE_FRAC_ICE2D
