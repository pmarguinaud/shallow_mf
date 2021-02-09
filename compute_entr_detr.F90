!     ######spl
          SUBROUTINE COMPUTE_ENTR_DETR(KLON,KIDIA,KFDIA,KLEV,KK,KKB,KKE,KKL,OTEST,OTESTLCL,&
                            HFRAC_ICE,PFRAC_ICE,PRHODREF,&
                            PPRE_MINUS_HALF,&
                            PPRE_PLUS_HALF,PZZ,PDZZ,&
                            PTHVM,PTHLM,PRTM,PW_UP2,PTH_UP,&
                            PTHL_UP,PRT_UP,PLUP,&
                            PRC_UP,PRI_UP,PTHV_UP,&
                            PRSAT_UP,PRC_MIX,PRI_MIX,      &
                            PENTR,PDETR,PENTR_CLD,PDETR_CLD,&
                            PBUO_INTEG_DRY,PBUO_INTEG_CLD,&
                            PPART_DRY,KSTPT,KSTSZ,PSTACK)

          
#include "temp.h"

USE PARKIND1, ONLY : JPRB
!         #############################################################

!!
!!***COMPUTE_ENTR_DETR* - calculates caracteristics of the updraft or downdraft
!!                       using model of the EDMF scheme 
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to compute entrainement and
!!      detrainement at one level of the updraft
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
!!       Book 1 of Meso-NH documentation (chapter Convection)
!!       
!!
!!     AUTHOR
!!     ------
!!    J.Pergaud : 2009
!!
!!    MODIFICATIONS
!!    -------------
!!      Y.Seity (06/2010) Bug correction
!!      V.Masson (09/2010) Optimization
!!      S. Riette april 2011 : ice added, protection against zero divide by Yves Bouteloup
!!                             protection against too big ZPART_DRY, interface modified
!!      S. Riette Jan 2012: support for both order of vertical levels
!!      P.Marguinaud Jun 2012: fix uninitialized variable
!!      P.Marguinaud Nov 2012: fix gfortran bug
!!      S. Riette Apr 2013: bugs correction, rewriting (for optimisation) and
!!                          improvement of continuity at the condensation level
!!      S. Riette Nov 2013: protection against zero divide for min value of dry PDETR
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!                         
USE MODD_CST
!
USE MODD_CMFSHALL
!
USE MODI_TH_R_FROM_THL_RT_1D 

IMPLICIT NONE
!
!                         
!*                    1.1  Declaration of Arguments
!
!
INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KK
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
LOGICAL,DIMENSION(KLON),   INTENT(IN)   :: OTEST ! test to see if updraft is running
LOGICAL,DIMENSION(KLON),   INTENT(IN)   :: OTESTLCL !test of condensation 
CHARACTER*1,            INTENT(IN)   :: HFRAC_ICE ! frac_ice can be compute using
                                              ! Temperature (T) or prescribed
                                              ! (Y)
REAL, DIMENSION(KLON), INTENT(IN)      :: PFRAC_ICE ! fraction of ice
!
!    prognostic variables at t- deltat
!
REAL, DIMENSION(KLON),     INTENT(IN) ::  PRHODREF  !rhodref
REAL, DIMENSION(KLON),     INTENT(IN) ::  PPRE_MINUS_HALF ! Pressure at flux level KK
REAL, DIMENSION(KLON),     INTENT(IN) ::  PPRE_PLUS_HALF ! Pressure at flux level KK+KKL
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PZZ       !  Height at the flux point
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PDZZ       !  metrics coefficient
REAL, DIMENSION(KLON,KLEV),   INTENT(IN) ::  PTHVM      ! ThetaV environment 

!
!   thermodynamical variables which are transformed in conservative var.
!
REAL, DIMENSION(KLON,KLEV), INTENT(IN)     ::  PTHLM     ! Thetal
REAL, DIMENSION(KLON,KLEV), INTENT(IN)     ::  PRTM      ! total mixing ratio 
REAL, DIMENSION(KLON,KLEV), INTENT(IN)     ::  PW_UP2    ! Vertical velocity^2
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PTH_UP,PTHL_UP,PRT_UP  ! updraft properties
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PLUP      ! LUP compute from the ground
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PRC_UP,PRI_UP   ! Updraft cloud content
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PTHV_UP ! Thetav of updraft
REAL, DIMENSION(KLON),   INTENT(IN)     ::  PRSAT_UP ! Mixing ratio at saturation in updraft
REAL, DIMENSION(KLON),   INTENT(INOUT)  ::  PRC_MIX, PRI_MIX      ! Mixture cloud content
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PENTR     ! Mass flux entrainment of the updraft
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PDETR     ! Mass flux detrainment of the updraft
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PENTR_CLD ! Mass flux entrainment of the updraft in cloudy part
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PDETR_CLD ! Mass flux detrainment of the updraft in cloudy part
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PBUO_INTEG_DRY, PBUO_INTEG_CLD! Integral Buoyancy
REAL, DIMENSION(KLON),   INTENT(OUT)    ::  PPART_DRY ! ratio of dry part at the transition level
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!
!
!                       1.2  Declaration of local variables
!
!

! Variables for cloudy part
! fraction of env. mass in the muxtures
temp (REAL, ZKIC_F2, (KLON))
temp (REAL, ZKIC, (KLON))
! factor entrainment detrainment
temp (REAL, ZDELTA, (KLON))
temp (REAL, ZEPSI, (KLON))
! factor entrainment detrainment
temp (REAL, ZEPSI_CLOUD, (KLON))
REAL                           :: ZCOEFFMF_CLOUD ! factor for compputing entr. detr.
! Thetal and rt in the mixtures
temp (REAL, ZMIXRT, (KLON))
temp (REAL, ZMIXTHL, (KLON))
! Theta and Thetav  of mixtures
temp (REAL, ZTHMIX, (KLON))
! mixing ratios in mixtures
temp (REAL, ZRIMIX, (KLON))
temp (REAL, ZRCMIX, (KLON))
temp (REAL, ZRVMIX, (KLON))
! Theta and Thetav  of mixtures
temp (REAL, ZTHVMIX_F2, (KLON))
temp (REAL, ZTHVMIX, (KLON))
! thv_up at flux point kk+kkl
temp (REAL, ZTHV_UP_F2, (KLON))
! working arrays (mixing ratio at saturation)
temp (REAL, ZRSATI, (KLON))
temp (REAL, ZRSATW, (KLON))
! theta V of environment at the bottom of cloudy part  
temp (REAL, ZTHV, (KLON))
REAL                           :: ZKIC_INIT      !Initial value of ZKIC
              ! Variation of Thvup between bottom and top of cloudy part

temp (REAL, ZCOTHVU, (KLON))
! Variables for dry part
! saturating vapor pressure
temp (REAL, ZFOESI, (KLON))
temp (REAL, ZFOESW, (KLON))
! d.Rsat/dP
temp (REAL, ZDRSATODP, (KLON))
! Temperature
temp (REAL, ZT, (KLON))
! Work array
temp (REAL, ZWK, (KLON))
! Variables for dry and cloudy parts
! Variation of Thv between mass points kk and kk+kkl
temp (REAL, ZCOEFF_PLUS_HALF, (KLON))
temp (REAL, ZCOEFF_MINUS_HALF, (KLON))
! pressure at the bottom of the cloudy part
temp (REAL, ZPRE, (KLON))
temp (REAL, ZG_O_THVREF, (KLON))
! fraction of ice
temp (REAL, ZFRAC_ICE, (KLON))
REAL                           :: ZRVORD               ! RV/RD
! Delta Z used in computations
temp (REAL, ZDZ, (KLON))
temp (REAL, ZTHV_PLUS_HALF, (KLON))
temp (REAL, ZTHV_MINUS_HALF, (KLON))
temp (REAL, ZDZ_STOP, (KLON))
INTEGER :: JI


alloc (ZDZ)
alloc (ZTHV_PLUS_HALF)
alloc (ZTHV_MINUS_HALF)
alloc (ZDZ_STOP)
alloc (ZFRAC_ICE)
alloc (ZG_O_THVREF)
alloc (ZPRE)
alloc (ZCOEFF_PLUS_HALF)
alloc (ZCOEFF_MINUS_HALF)
alloc (ZWK)
alloc (ZT)
alloc (ZDRSATODP)
alloc (ZFOESI)
alloc (ZFOESW)
alloc (ZCOTHVU)
alloc (ZTHV)
alloc (ZRSATI)
alloc (ZRSATW)
alloc (ZTHV_UP_F2)
alloc (ZTHVMIX_F2)
alloc (ZTHVMIX)
alloc (ZRIMIX)
alloc (ZRCMIX)
alloc (ZRVMIX)
alloc (ZTHMIX)
alloc (ZMIXRT)
alloc (ZMIXTHL)
alloc (ZEPSI_CLOUD)
alloc (ZDELTA)
alloc (ZEPSI)
alloc (ZKIC_F2)
alloc (ZKIC)

!----------------------------------------------------------------------------------
                        
!                1.3 Initialisation

  
  ZRVORD   = XRV / XRD   !=1.607
  ZG_O_THVREF(KIDIA:KFDIA)=XG/PTHVM(KIDIA:KFDIA,KK)
  ZCOEFFMF_CLOUD=XENTR_MF * XG / XCRAD_MF
  
  ZFRAC_ICE(KIDIA:KFDIA)=PFRAC_ICE(KIDIA:KFDIA) ! to not modify fraction of ice
 
  ZPRE(KIDIA:KFDIA)=PPRE_MINUS_HALF(KIDIA:KFDIA)
  ZMIXTHL(KIDIA:KFDIA)=0.1
  ZMIXRT(KIDIA:KFDIA)=0.1

!                1.4 Estimation of PPART_DRY
  WHERE(OTEST(KIDIA:KFDIA))
    WHERE(OTESTLCL(KIDIA:KFDIA))
      !No dry part when condensation level is reached
      PPART_DRY(KIDIA:KFDIA)=0.
      ZDZ_STOP(KIDIA:KFDIA)=0.
      ZPRE(KIDIA:KFDIA)=PPRE_MINUS_HALF(KIDIA:KFDIA)
    ELSEWHERE
      !Temperature at flux level KK
      ZT(KIDIA:KFDIA)=PTH_UP(KIDIA:KFDIA)*(PPRE_MINUS_HALF(KIDIA:KFDIA)/XP00) ** (XRD/XCPD)
      !Saturating vapor pressure at flux level KK
      ZFOESW(KIDIA:KFDIA) = MIN(EXP( XALPW - XBETAW/ZT(KIDIA:KFDIA) - XGAMW*LOG(ZT(KIDIA:KFDIA))  ), &
                                0.99*PPRE_MINUS_HALF(KIDIA:KFDIA))
      ZFOESI(KIDIA:KFDIA) = MIN(EXP( XALPI - XBETAI/ZT(KIDIA:KFDIA) - XGAMI*LOG(ZT(KIDIA:KFDIA))  ), &
                                0.99*PPRE_MINUS_HALF(KIDIA:KFDIA))
      !Computation of d.Rsat / dP (partial derivations with respect to P and T
      !and use of T=Theta*(P/P0)**(R/Cp) to transform dT into dP with theta_up
      !constant at the vertical)
      ZDRSATODP(KIDIA:KFDIA)=(XBETAW/ZT(KIDIA:KFDIA)-XGAMW)*(1-ZFRAC_ICE(KIDIA:KFDIA))&
                            +(XBETAI/ZT(KIDIA:KFDIA)-XGAMI)*ZFRAC_ICE(KIDIA:KFDIA)
      ZDRSATODP(KIDIA:KFDIA)=((XRD/XCPD)*ZDRSATODP(KIDIA:KFDIA)-1.)*PRSAT_UP(KIDIA:KFDIA)/ &
                  &(PPRE_MINUS_HALF(KIDIA:KFDIA)-(ZFOESW(KIDIA:KFDIA)*(1-ZFRAC_ICE(KIDIA:KFDIA)) &
                            + ZFOESI(KIDIA:KFDIA)*ZFRAC_ICE(KIDIA:KFDIA)))
      !Use of d.Rsat / dP and pressure at flux level KK to find pressure (ZPRE)
      !where Rsat is equal to PRT_UP
      ZPRE(KIDIA:KFDIA)=PPRE_MINUS_HALF(KIDIA:KFDIA)+(PRT_UP(KIDIA:KFDIA)-PRSAT_UP(KIDIA:KFDIA))/ZDRSATODP(KIDIA:KFDIA)
      !Fraction of dry part (computed with pressure and used with heights, no
      !impact found when using log function here and for pressure on flux levels
      !computation)
      PPART_DRY(KIDIA:KFDIA)=MAX(0., MIN(1., (PPRE_MINUS_HALF(KIDIA:KFDIA)-ZPRE(KIDIA:KFDIA))&
                                /(PPRE_MINUS_HALF(KIDIA:KFDIA)-PPRE_PLUS_HALF(KIDIA:KFDIA))))
      !Height above flux level KK of the cloudy part
      ZDZ_STOP(KIDIA:KFDIA) = (PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))*PPART_DRY(KIDIA:KFDIA)
    ENDWHERE
  ENDWHERE

!               1.5 Gradient and flux values of thetav
  IF(KK/=KKB)THEN
    ZCOEFF_MINUS_HALF(KIDIA:KFDIA)=((PTHVM(KIDIA:KFDIA,KK)-PTHVM(KIDIA:KFDIA,KK-KKL))/PDZZ(KIDIA:KFDIA,KK))
    ZTHV_MINUS_HALF(KIDIA:KFDIA) = PTHVM(KIDIA:KFDIA,KK) - ZCOEFF_MINUS_HALF(KIDIA:KFDIA)&
                                   *0.5*(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))
  ELSE
    ZCOEFF_MINUS_HALF(KIDIA:KFDIA)=0.
    ZTHV_MINUS_HALF(KIDIA:KFDIA) = PTHVM(KIDIA:KFDIA,KK)
  ENDIF
  ZCOEFF_PLUS_HALF(KIDIA:KFDIA)  = ((PTHVM(KIDIA:KFDIA,KK+KKL)-PTHVM(KIDIA:KFDIA,KK))/PDZZ(KIDIA:KFDIA,KK+KKL))
  ZTHV_PLUS_HALF(KIDIA:KFDIA)  = PTHVM(KIDIA:KFDIA,KK) + ZCOEFF_PLUS_HALF(KIDIA:KFDIA)&
                                   *0.5*(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))

!               2  Dry part computation:
!                  Integral buoyancy and computation of PENTR and PDETR for dry part
!               --------------------------------------------------------------------

  WHERE(OTEST(KIDIA:KFDIA) .AND. PPART_DRY(KIDIA:KFDIA)>0.)
    !Buoyancy computation in two parts to use change of gradient of theta v of environment
    !Between flux level KK and min(mass level, bottom of cloudy part)
    ZDZ(KIDIA:KFDIA)=MIN(ZDZ_STOP(KIDIA:KFDIA),(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))*0.5)
    PBUO_INTEG_DRY(KIDIA:KFDIA) = ZG_O_THVREF(KIDIA:KFDIA)*ZDZ(KIDIA:KFDIA)*&
                (0.5 * (  - ZCOEFF_MINUS_HALF(KIDIA:KFDIA))*ZDZ(KIDIA:KFDIA)  &
                  - ZTHV_MINUS_HALF(KIDIA:KFDIA) + PTHV_UP(KIDIA:KFDIA) )

    !Between mass flux KK and bottom of cloudy part (if above mass flux)
    ZDZ(KIDIA:KFDIA)=MAX(0., ZDZ_STOP(KIDIA:KFDIA)-(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))*0.5)
    PBUO_INTEG_DRY(KIDIA:KFDIA) = PBUO_INTEG_DRY(KIDIA:KFDIA) + ZG_O_THVREF(KIDIA:KFDIA)*ZDZ(KIDIA:KFDIA)*&
                (0.5 * (  - ZCOEFF_PLUS_HALF(KIDIA:KFDIA))*ZDZ(KIDIA:KFDIA) &
                  - PTHVM(KIDIA:KFDIA,KK) + PTHV_UP(KIDIA:KFDIA) )

    !Entr//Detr. computation
    WHERE (PBUO_INTEG_DRY(KIDIA:KFDIA)>=0.)
      PENTR(KIDIA:KFDIA) = 0.5/(XABUO-XBENTR*XENTR_DRY)*&
                 LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/PW_UP2(KIDIA:KFDIA,KK))* &
                 PBUO_INTEG_DRY(KIDIA:KFDIA))
      PDETR(KIDIA:KFDIA) = 0.
    ELSEWHERE
      PENTR(KIDIA:KFDIA) = 0.
      PDETR(KIDIA:KFDIA) = 0.5/(XABUO)*&
                 LOG(1.+ (2.*(XABUO)/PW_UP2(KIDIA:KFDIA,KK))* &
                 (-PBUO_INTEG_DRY(KIDIA:KFDIA)))
    ENDWHERE
    PENTR(KIDIA:KFDIA) = XENTR_DRY*PENTR(KIDIA:KFDIA)/(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))    
    PDETR(KIDIA:KFDIA) = XDETR_DRY*PDETR(KIDIA:KFDIA)/(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))
    !Minimum value of detrainment
    ZWK(KIDIA:KFDIA)=PLUP(KIDIA:KFDIA)-0.5*(PZZ(KIDIA:KFDIA,KK)+PZZ(KIDIA:KFDIA,KK+KKL))
    ZWK(KIDIA:KFDIA)=SIGN(MAX(1., ABS(ZWK(KIDIA:KFDIA))), ZWK(KIDIA:KFDIA)) ! ZWK must not be zero
    PDETR(KIDIA:KFDIA) = MAX(PPART_DRY(KIDIA:KFDIA)*XDETR_LUP/ZWK(KIDIA:KFDIA), PDETR(KIDIA:KFDIA))
  ELSEWHERE
    !No dry part, consation reached (OTESTLCL)
    PBUO_INTEG_DRY(KIDIA:KFDIA) = 0.
    PENTR(KIDIA:KFDIA)=0.
    PDETR(KIDIA:KFDIA)=0.
  ENDWHERE

!               3  Wet part computation
!               -----------------------

!               3.1 Integral buoyancy for cloudy part

  ! Compute theta_v of updraft at flux level KK+KKL                   
  !MIX variables are used to avoid declaring new variables
  !but we are dealing with updraft and not mixture
  ZRCMIX(KIDIA:KFDIA)=PRC_UP(KIDIA:KFDIA)
  ZRIMIX(KIDIA:KFDIA)=PRI_UP(KIDIA:KFDIA)
  CALL TH_R_FROM_THL_RT_1D(KLON,KIDIA,KFDIA,HFRAC_ICE,ZFRAC_ICE,&
               PPRE_PLUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX,&
               ZRSATW, ZRSATI,KSTPT,KSTSZ,PSTACK)
  ZTHV_UP_F2(KIDIA:KFDIA) = ZTHMIX(KIDIA:KFDIA)*(1.+ZRVORD*ZRVMIX(KIDIA:KFDIA))/(1.+PRT_UP(KIDIA:KFDIA))

  ! Integral buoyancy for cloudy part
  WHERE(OTEST(KIDIA:KFDIA) .AND. PPART_DRY(KIDIA:KFDIA)<1.)
    !Gradient of Theta V updraft over the cloudy part, assuming that thetaV updraft don't change
    !between flux level KK and bottom of cloudy part
    ZCOTHVU(KIDIA:KFDIA)=(ZTHV_UP_F2(KIDIA:KFDIA)-PTHV_UP(KIDIA:KFDIA))/((PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))&
                        *(1-PPART_DRY(KIDIA:KFDIA)))

    !Computation in two parts to use change of gradient of theta v of environment
    !Between bottom of cloudy part (if under mass level) and mass level KK
    ZDZ(KIDIA:KFDIA)=MAX(0., 0.5*(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))-ZDZ_STOP(KIDIA:KFDIA))
    PBUO_INTEG_CLD(KIDIA:KFDIA) = ZG_O_THVREF(KIDIA:KFDIA)*ZDZ(KIDIA:KFDIA)*&
              (0.5*( ZCOTHVU(KIDIA:KFDIA) - ZCOEFF_MINUS_HALF(KIDIA:KFDIA))*ZDZ(KIDIA:KFDIA) &
                - (PTHVM(KIDIA:KFDIA,KK)-ZDZ(KIDIA:KFDIA)*ZCOEFF_MINUS_HALF(KIDIA:KFDIA)) + PTHV_UP(KIDIA:KFDIA) )

    !Between max(mass level, bottom of cloudy part) and flux level KK+KKL
    ZDZ(KIDIA:KFDIA)=(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))&
                     -MAX(ZDZ_STOP(KIDIA:KFDIA),0.5*(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK)))
    PBUO_INTEG_CLD(KIDIA:KFDIA) = PBUO_INTEG_CLD(KIDIA:KFDIA)+ZG_O_THVREF(KIDIA:KFDIA)*ZDZ(KIDIA:KFDIA)*&
                        (0.5*( ZCOTHVU(KIDIA:KFDIA) - ZCOEFF_PLUS_HALF(KIDIA:KFDIA))*ZDZ(KIDIA:KFDIA)&
                - (PTHVM(KIDIA:KFDIA,KK)+(0.5*((PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK)))-&
                   ZDZ(KIDIA:KFDIA))*ZCOEFF_PLUS_HALF(KIDIA:KFDIA)) +&
                PTHV_UP(KIDIA:KFDIA) )

  ELSEWHERE
    !No cloudy part
    PBUO_INTEG_CLD(KIDIA:KFDIA)=0.
  ENDWHERE

!               3.2 Critical mixed fraction for KK+KKL flux level (ZKIC_F2) and
!                   for bottom of cloudy part (ZKIC), then a mean for the cloudy part
!                   (put also in ZKIC)
!
!                   computation by estimating unknown  
!                   T^mix r_c^mix and r_i^mix from enthalpy^mix and r_w^mix
!                   We determine the zero crossing of the linear curve
!                   evaluating the derivative using ZMIXF=0.1
                
  ZKIC_INIT=0.1  ! starting value for critical mixed fraction for CLoudy Part

  !  Compute thetaV of environment at the bottom of cloudy part
  !    and cons then non cons. var. of mixture at the bottom of cloudy part

  !   JI computed to avoid KKL(KK-KKL) being < KKL*KKB
  JI=KKL*MAX(KKL*(KK-KKL),KKL*KKB)

  WHERE(OTEST(KIDIA:KFDIA) .AND. PPART_DRY(KIDIA:KFDIA)>0.5)
    ZDZ(KIDIA:KFDIA)=ZDZ_STOP(KIDIA:KFDIA)-0.5*(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))
    ZTHV(KIDIA:KFDIA)= PTHVM(KIDIA:KFDIA,KK)+ZCOEFF_PLUS_HALF(KIDIA:KFDIA)*ZDZ(KIDIA:KFDIA)
    ZMIXTHL(KIDIA:KFDIA) = ZKIC_INIT * &
                 (PTHLM(KIDIA:KFDIA,KK)+ZDZ(KIDIA:KFDIA)*(PTHLM(KIDIA:KFDIA,KK+KKL)-PTHLM(KIDIA:KFDIA,KK))&
                 /PDZZ(KIDIA:KFDIA,KK+KKL)) + &
                 (1. - ZKIC_INIT)*PTHL_UP(KIDIA:KFDIA)
    ZMIXRT(KIDIA:KFDIA)  = ZKIC_INIT * &
                 (PRTM(KIDIA:KFDIA,KK)+ZDZ(KIDIA:KFDIA)*(PRTM(KIDIA:KFDIA,KK+KKL)-PRTM(KIDIA:KFDIA,KK))&
                 /PDZZ(KIDIA:KFDIA,KK+KKL)) +   &
                 (1. - ZKIC_INIT)*PRT_UP(KIDIA:KFDIA)
  ELSEWHERE(OTEST(KIDIA:KFDIA))
    ZDZ(KIDIA:KFDIA)=0.5*(PZZ(KIDIA:KFDIA,KK+KKL)-PZZ(KIDIA:KFDIA,KK))-ZDZ_STOP(KIDIA:KFDIA)
    ZTHV(KIDIA:KFDIA)= PTHVM(KIDIA:KFDIA,KK)-ZCOEFF_MINUS_HALF(KIDIA:KFDIA)*ZDZ(KIDIA:KFDIA)
    ZMIXTHL(KIDIA:KFDIA) = ZKIC_INIT * &
                 (PTHLM(KIDIA:KFDIA,KK)-ZDZ(KIDIA:KFDIA)*(PTHLM(KIDIA:KFDIA,KK)-PTHLM(KIDIA:KFDIA,JI))/PDZZ(KIDIA:KFDIA,KK)) + &
                 (1. - ZKIC_INIT)*PTHL_UP(KIDIA:KFDIA)
    ZMIXRT(KIDIA:KFDIA)  = ZKIC_INIT * &
                 (PRTM(KIDIA:KFDIA,KK)-ZDZ(KIDIA:KFDIA)*(PRTM(KIDIA:KFDIA,KK)-PRTM(KIDIA:KFDIA,JI))/PDZZ(KIDIA:KFDIA,KK)) + &
                 (1. - ZKIC_INIT)*PRT_UP(KIDIA:KFDIA)
  ENDWHERE
  CALL TH_R_FROM_THL_RT_1D(KLON,KIDIA,KFDIA,HFRAC_ICE,ZFRAC_ICE,&
               ZPRE,ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,PRC_MIX,PRI_MIX,&
               ZRSATW, ZRSATI,KSTPT,KSTSZ,PSTACK)
  ZTHVMIX(KIDIA:KFDIA) = ZTHMIX(KIDIA:KFDIA)*(1.+ZRVORD*ZRVMIX(KIDIA:KFDIA))/(1.+ZMIXRT(KIDIA:KFDIA))

  !  Compute cons then non cons. var. of mixture at the flux level KK+KKL  with initial ZKIC
  ZMIXTHL(KIDIA:KFDIA) = ZKIC_INIT * 0.5*(PTHLM(KIDIA:KFDIA,KK)+PTHLM(KIDIA:KFDIA,KK+KKL))+(1. - ZKIC_INIT)*PTHL_UP(KIDIA:KFDIA)
  ZMIXRT(KIDIA:KFDIA)  = ZKIC_INIT * 0.5*(PRTM(KIDIA:KFDIA,KK)+PRTM(KIDIA:KFDIA,KK+KKL))+(1. - ZKIC_INIT)*PRT_UP(KIDIA:KFDIA)
  CALL TH_R_FROM_THL_RT_1D(KLON,KIDIA,KFDIA,HFRAC_ICE,ZFRAC_ICE,&
               PPRE_PLUS_HALF,ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,PRC_MIX,PRI_MIX,&
               ZRSATW, ZRSATI,KSTPT,KSTSZ,PSTACK)
  ZTHVMIX_F2(KIDIA:KFDIA) = ZTHMIX(KIDIA:KFDIA)*(1.+ZRVORD*ZRVMIX(KIDIA:KFDIA))/(1.+ZMIXRT(KIDIA:KFDIA))

  !Computation of mean ZKIC over the cloudy part
  WHERE (OTEST(KIDIA:KFDIA))
    ! Compute ZKIC at the bottom of cloudy part
    ! Thetav_up at bottom is equal to Thetav_up at flux level KK
    WHERE (ABS(PTHV_UP(KIDIA:KFDIA)-ZTHVMIX(KIDIA:KFDIA))<1.E-10)
      ZKIC(KIDIA:KFDIA)=1.
    ELSEWHERE
      ZKIC(KIDIA:KFDIA) = MAX(0.,PTHV_UP(KIDIA:KFDIA)-ZTHV(KIDIA:KFDIA))*ZKIC_INIT /  &  
                   (PTHV_UP(KIDIA:KFDIA)-ZTHVMIX(KIDIA:KFDIA))
    ENDWHERE
    ! Compute ZKIC_F2 at flux level KK+KKL
    WHERE (ABS(ZTHV_UP_F2(KIDIA:KFDIA)-ZTHVMIX_F2(KIDIA:KFDIA))<1.E-10)
      ZKIC_F2(KIDIA:KFDIA)=1.
    ELSEWHERE
      ZKIC_F2(KIDIA:KFDIA) = MAX(0.,ZTHV_UP_F2(KIDIA:KFDIA)-ZTHV_PLUS_HALF(KIDIA:KFDIA))*ZKIC_INIT /  &  
                   (ZTHV_UP_F2(KIDIA:KFDIA)-ZTHVMIX_F2(KIDIA:KFDIA))
    ENDWHERE
    !Mean ZKIC over the cloudy part
    ZKIC(KIDIA:KFDIA)=MAX(MIN(0.5*(ZKIC(KIDIA:KFDIA)+ZKIC_F2(KIDIA:KFDIA)),1.),0.)
  ENDWHERE

!               3.3 Integration of PDF
!                   According to Kain and Fritsch (1990), we replace delta Mt
!                   in eq. (7) and (8) using eq. (5). Here we compute the ratio
!                   of integrals without computing delta Me

  !Constant PDF
  !For this PDF, eq. (5) is delta Me=0.5*delta Mt
  WHERE(OTEST(KIDIA:KFDIA))
    ZEPSI(KIDIA:KFDIA) = ZKIC(KIDIA:KFDIA)**2. !integration multiplied by 2
    ZDELTA(KIDIA:KFDIA) = (1.-ZKIC(KIDIA:KFDIA))**2. !idem
  ENDWHERE

  !Triangular PDF
  !Calculus must be verified before activating this part, but in this state,
  !results on ARM case are almost identical
  !For this PDF, eq. (5) is also delta Me=0.5*delta Mt
  !WHERE(OTEST)
  !  !Integration multiplied by 2
  !  WHERE(ZKIC<0.5)
  !    ZEPSI(:)=8.*ZKIC(:)**3/3.
  !    ZDELTA(:)=1.-4.*ZKIC(:)**2+8.*ZKIC(:)**3/3.
  !  ELSEWHERE
  !    ZEPSI(:)=5./3.-4*ZKIC(:)**2+8.*ZKIC(:)**3/3.
  !    ZDELTA(:)=8.*(1.-ZKIC(:))**3/3.
  !  ENDWHERE
  !ENDWHERE

!               3.4 Computation of PENTR and PDETR
  WHERE (OTEST(KIDIA:KFDIA))
    ZEPSI_CLOUD(KIDIA:KFDIA)=MIN(ZDELTA(KIDIA:KFDIA),ZEPSI(KIDIA:KFDIA))
    PENTR_CLD(KIDIA:KFDIA) = (1.-PPART_DRY(KIDIA:KFDIA))*ZCOEFFMF_CLOUD*PRHODREF(KIDIA:KFDIA)*ZEPSI_CLOUD(KIDIA:KFDIA)
    PDETR_CLD(KIDIA:KFDIA) = (1.-PPART_DRY(KIDIA:KFDIA))*ZCOEFFMF_CLOUD*PRHODREF(KIDIA:KFDIA)*ZDELTA(KIDIA:KFDIA)
    PENTR(KIDIA:KFDIA) = PENTR(KIDIA:KFDIA)+PENTR_CLD(KIDIA:KFDIA)
    PDETR(KIDIA:KFDIA) = PDETR(KIDIA:KFDIA)+PDETR_CLD(KIDIA:KFDIA)
  ELSEWHERE
    PENTR_CLD(KIDIA:KFDIA) = 0.
    PDETR_CLD(KIDIA:KFDIA) = 0.
  ENDWHERE

END SUBROUTINE COMPUTE_ENTR_DETR
