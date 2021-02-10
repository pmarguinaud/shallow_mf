!     ######spl
      MODULE MODD_NEB

#include "create.h"

!     #############################
!
!!****  *MODD_NEB* - Declaration of nebulosity constants
!!
!!    PURPOSE
!!    -------
!!      The purpose of this declarative module is to declare some
!!      constants for nebulosity calculation
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!       S. Riette (Meteo France)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    24 Aug 2011
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL,SAVE          :: XTMINMIX   ! minimum temperature of mixed phase
create (XTMINMIX)
REAL,SAVE          :: XTMAXMIX   ! maximum temperature of mixed phase
create (XTMAXMIX)
!
!
END MODULE MODD_NEB
