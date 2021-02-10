!     ######spl
      MODULE MODD_CMFSHALL

#include "create.h"

!     #############################
!
!!****  *MODD_CMFSHALL* - Declaration of Mass flux scheme constants 
!!
!!    PURPOSE
!!    -------
!!      The purpose of this declarative module is to declare some
!!      constants for Mass Flux Scheme in the shallow convection 
!!      parameterization.  
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
!!       S. Malardel, J. Pergaud (Meteo France)      
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/07       
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------

IMPLICIT NONE

REAL,SAVE          :: XALP_PERT   ! coefficient for the perturbation of
                                create (XALP_PERT)
! theta_l and r_t at the first level of 
                                ! the updraft
REAL,SAVE          ::    XABUO    ! coefficient of the buoyancy term in the w_up equation
create (XABUO)
REAL,SAVE          ::    XBENTR   ! coefficient of the entrainment term in the w_up equation
create (XBENTR)
REAL,SAVE          ::    XBDETR   ! coefficient of the detrainment term in the w_up equation
create (XBDETR)
REAL,SAVE          ::    XCMF     ! coefficient for the mass flux at the first level 
                                create (XCMF)
! of the updraft (closure)
REAL,SAVE          :: XENTR_MF    ! entrainment constant (m/Pa) = 0.2 (m) 
create (XENTR_MF)
REAL,SAVE          :: XCRAD_MF    ! cloud radius in cloudy part
create (XCRAD_MF)
REAL,SAVE          :: XENTR_DRY   ! coefficient for entrainment in dry part 
create (XENTR_DRY)
REAL,SAVE          :: XDETR_DRY   ! coefficient for detrainment in dry part
create (XDETR_DRY)
REAL,SAVE          :: XDETR_LUP   ! coefficient for detrainment in dry part
create (XDETR_LUP)
REAL,SAVE          :: XKCF_MF     ! coefficient for cloud fraction
create (XKCF_MF)
REAL,SAVE          :: XKRC_MF     ! coefficient for convective rc
create (XKRC_MF)
REAL,SAVE          :: XTAUSIGMF
create (XTAUSIGMF)
REAL,SAVE          :: XPRES_UV    ! coefficient for pressure term in wind
                                  create (XPRES_UV)
! mixing

REAL,SAVE          :: XALPHA_MF   ! coefficient for cloudy fraction
create (XALPHA_MF)
REAL,SAVE          :: XSIGMA_MF   ! coefficient for sigma computation

create (XSIGMA_MF)
REAL,SAVE          :: XFRAC_UP_MAX! maximum Updraft fraction


create (XFRAC_UP_MAX)
!  Parameter for Rio et al (2010) formulation for entrainment and detrainment

REAL,SAVE          :: XA1      ! a1
create (XA1)
REAL,SAVE          :: XB       ! b
create (XB)
REAL,SAVE          :: XC       ! c
create (XC)
REAL,SAVE          :: XBETA1   ! beta1

create (XBETA1)
!  Parameters for closure assumption of Hourdin et al 2002

REAL,SAVE          :: XR      ! Aspect ratio of updraft

create (XR)
!  Thermodynamic parameter

REAL,SAVE          :: XLAMBDA      ! Lambda to compute ThetaS1 from ThetaL

create (XLAMBDA)
END MODULE MODD_CMFSHALL
