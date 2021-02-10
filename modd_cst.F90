!     ######spl
      MODULE MODD_CST      

#include "create.h"

!     ###############
!
!!****  *MODD_CST* - declaration of Physic constants 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     Physics constants.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_CST)
!!          
!!    AUTHOR
!!    ------
!!      V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/05/94  
!!      J. Stein    02/01/95  add xrholw                    
!!      J.-P. Pinty 13/12/95  add XALPI,XBETAI,XGAMI
!!      J. Stein    25/07/97  add XTH00                    
!!      V. Masson   05/10/98  add XRHOLI
!!      C. Mari     31/10/00  add NDAYSEC
!!      V. Masson   01/03/03  add conductivity of ice
!!      R. El Khatib 04/08/14 add pre-computed quantities
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
REAL,SAVE :: XPI                ! Pi
create (XPI)
!
REAL,SAVE :: XDAY,XSIYEA,XSIDAY ! day duration, sideral year duration,
                                create (XSIDAY)
create (XSIYEA)
create (XDAY)
! sideral day duration
!
REAL,SAVE :: XKARMAN            ! von karman constant
create (XKARMAN)
REAL,SAVE :: XLIGHTSPEED        ! light speed
create (XLIGHTSPEED)
REAL,SAVE :: XPLANCK            ! Planck constant
create (XPLANCK)
REAL,SAVE :: XBOLTZ             ! Boltzman constant 
create (XBOLTZ)
REAL,SAVE :: XAVOGADRO          ! Avogadro number
create (XAVOGADRO)
!
REAL,SAVE :: XRADIUS,XOMEGA     ! Earth radius, earth rotation
create (XOMEGA)
create (XRADIUS)
REAL,SAVE :: XG                 ! Gravity constant
create (XG)
!
REAL,SAVE :: XP00               ! Reference pressure
create (XP00)
!
REAL,SAVE :: XSTEFAN,XI0        ! Stefan-Boltzman constant, solar constant
create (XI0)
create (XSTEFAN)
!
REAL,SAVE :: XMD,XMV            ! Molar mass of dry air and molar mass of vapor
create (XMV)
create (XMD)
REAL,SAVE :: XRD,XRV            ! Gaz constant for dry air, gaz constant for vapor
create (XRV)
create (XRD)
REAL,SAVE :: XEPSILO            ! XMV/XMD 
create (XEPSILO)
REAL,SAVE :: XCPD,XCPV          ! Cpd (dry air), Cpv (vapor)
create (XCPV)
create (XCPD)
REAL,SAVE :: XRHOLW             ! Volumic mass of liquid water
create (XRHOLW)
REAL,SAVE :: XCL,XCI            ! Cl (liquid), Ci (ice)
create (XCI)
create (XCL)
REAL,SAVE :: XTT                ! Triple point temperature
create (XTT)
REAL,SAVE :: XLVTT              ! Vaporization heat constant
create (XLVTT)
REAL,SAVE :: XLSTT              ! Sublimation heat constant
create (XLSTT)
REAL,SAVE :: XLMTT              ! Melting heat constant
create (XLMTT)
REAL,SAVE :: XESTT              ! Saturation vapor pressure  at triple point
                                create (XESTT)
! temperature  
REAL,SAVE :: XALPW,XBETAW,XGAMW ! Constants for saturation vapor 
                                create (XGAMW)
create (XBETAW)
create (XALPW)
!  pressure  function 
REAL,SAVE :: XALPI,XBETAI,XGAMI ! Constants for saturation vapor
                                create (XGAMI)
create (XBETAI)
create (XALPI)
!  pressure  function over solid ice
REAL,SAVE :: XCONDI             ! thermal conductivity of ice (W m-1 K-1)
create (XCONDI)
REAL, SAVE        :: XTH00      ! reference value  for the potential
                                create (XTH00)
! temperature
REAL,SAVE :: XRHOLI             ! Volumic mass of liquid water
create (XRHOLI)
!
INTEGER, SAVE :: NDAYSEC        ! Number of seconds in a day
create (NDAYSEC)
!
REAL,SAVE :: RDSRV              !  XRD/XRV
create (RDSRV)
REAL,SAVE :: RDSCPD             !  XRD/XCPD
create (RDSCPD)
REAL,SAVE :: RINVXP00           !  1./XP00

create (RINVXP00)
END MODULE MODD_CST
