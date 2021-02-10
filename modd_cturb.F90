!     ######spl
          MODULE MODD_CTURB

#include "create.h"

!      #######################
!
!!****   *MODD_CTURB*  - declaration of the turbulent scheme constants
!!
!!     PURPOSE
!!     -------
!        The purpose of this declarative module is to declare the 
!      turbulence scheme constants.
!
!!
!!**   IMPLICIT ARGUMENTS
!!     ------------------
!!       NONE
!!
!!     REFERENCE
!!     ---------
!!       Book 2 of Meso-NH documentation (MODD_CTURB)
!!       Book 1 of Meso-NH documentation (Chapter Turbulence)
!!
!!     AUTHOR
!!     ------
!1       Joan Cuxart         * INM  and Meteo-France *
!!
!!     MODIFICATIONS
!!     -------------
!!       Original            08/08/94
!!     Nov 06, 2002 (V. Masson)  add XALPSBL and XASBL
!!     May 06                    Remove EPS
!----------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!           ------------
!
IMPLICIT NONE
!
REAL,SAVE :: XCMFS        ! constant for the momentum flux due to shear   
create (XCMFS)
REAL,SAVE :: XCMFB        ! constant for the momentum flux due to buoyancy
create (XCMFB)
REAL,SAVE :: XCSHF        ! constant for the sensible heat flux 
create (XCSHF)
REAL,SAVE :: XCHF         ! constant for the humidity flux 
create (XCHF)
REAL,SAVE :: XCTV         ! constant for the temperature variance
create (XCTV)
REAL,SAVE :: XCHV         ! constant for the humidity variance
create (XCHV)
REAL,SAVE :: XCHT1        ! first ct. for the humidity-temperature correlation
create (XCHT1)
REAL,SAVE :: XCHT2        ! second ct. for the humidity-temperature correlation
create (XCHT2)
!
REAL,SAVE :: XCPR1        ! first ct. for the turbulent Prandtl numbers
create (XCPR1)
REAL,SAVE :: XCPR2        ! second ct. for the turbulent Prandtl numbers
create (XCPR2)
REAL,SAVE :: XCPR3        ! third ct. for the turbulent Prandtl numbers
create (XCPR3)
REAL,SAVE :: XCPR4        ! fourth ct. for the turbulent Prandtl numbers
create (XCPR4)
REAL,SAVE :: XCPR5        ! fifth ct. for the turbulent Prandtl numbers
create (XCPR5)
!
REAL,SAVE :: XCET         ! constant into the transport term of the TKE eq.
create (XCET)
REAL,SAVE :: XCED         ! constant into the dissipation term of the TKE eq.
create (XCED)
!
REAL,SAVE :: XCDP         ! ct. for the production term in the dissipation eq.
create (XCDP)
REAL,SAVE :: XCDD         ! ct. for the destruction term in the dissipation eq.
create (XCDD)
REAL,SAVE :: XCDT         ! ct. for the transport term in the dissipation eq.
create (XCDT)
!
REAL,SAVE :: XTKEMIN      ! mimimum value for the TKE
create (XTKEMIN)
!
REAL,SAVE :: XLINI        ! initial value for BL mixing length
create (XLINI)
REAL,SAVE :: XLINF        ! to prevent division by zero in the BL algorithm
create (XLINF)
!
REAL,SAVE :: XALPSBL      ! constant linking TKE and friction velocity in the SBL
create (XALPSBL)
REAL,SAVE :: XASBL        ! constant used to define mixing length in the SBL
create (XASBL)
!
REAL,SAVE :: XCEP         ! Constant for wind pressure-correlations
create (XCEP)
REAL,SAVE :: XA0          ! Constant a0 for wind pressure-correlations
create (XA0)
REAL,SAVE :: XA2          ! Constant a2 for wind pressure-correlations
create (XA2)
REAL,SAVE :: XA3          ! Constant a3 for wind pressure-correlations
create (XA3)
REAL,SAVE :: XA5          ! Constant a5 for temperature pressure-correlations
create (XA5)
REAL,SAVE :: XCTD         ! Constant for temperature and vapor dissipation
create (XCTD)
REAL,SAVE :: XCTP         ! Constant for temperature and vapor pressure-correlations
create (XCTP)
!
REAL,SAVE :: XPHI_LIM     ! Threshold value for Phi3 and Psi3
create (XPHI_LIM)
REAL,SAVE :: XSBL_O_BL    ! SBL height / BL height ratio
create (XSBL_O_BL)
REAL,SAVE :: XFTOP_O_FSURF! Fraction of surface (heat or momentum) flux used to define top of BL
create (XFTOP_O_FSURF)
!
LOGICAL,SAVE :: LHARAT    ! SWITCH HARATU
create (LHARAT)
END MODULE MODD_CTURB
