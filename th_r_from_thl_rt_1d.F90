!     ######spl
      SUBROUTINE TH_R_FROM_THL_RT_1D(KLON,KIDIA,KFDIA,HFRAC_ICE,PFRAC_ICE,PP,        &
                                  PTHL, PRT, PTH, PRV, PRL, PRI,         &
                                  PRSATW, PRSATI,KSTPT,KSTSZ,PSTACK)

#include "temp.h"

!     #################################################################
!
!
!!****  *TH_R_FROM_THL_RT_1D* - computes the non-conservative variables
!!                          from conservative variables
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
!!      S. Riette April 2011 : ice added, allow ZRLTEMP to be negative
!!                             we use dQsat/dT to help convergence
!!                             use of optional PRR, PRS, PRG, PRH
!!
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE PARKIND1, ONLY : JPRB
USE MODI_COMPUTE_FRAC_ICE1D
USE MODD_CST!, ONLY: XP00, XRD, XCPD, XCPV, XCL, XCI, XLVTT, XTT, XLSTT
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER             , INTENT(IN) :: KLON
INTEGER             , INTENT(IN) :: KIDIA
INTEGER             , INTENT(IN) :: KFDIA
CHARACTER*1         , INTENT(IN) :: HFRAC_ICE
REAL, DIMENSION(KLON), INTENT(INOUT) :: PFRAC_ICE
REAL, DIMENSION(KLON), INTENT(IN) :: PP          ! Pressure
REAL, DIMENSION(KLON), INTENT(IN) :: PTHL    ! thetal to transform into th
REAL, DIMENSION(KLON),INTENT(IN)  :: PRT    ! Total mixing ratios to transform into rv,rc and ri
REAL, DIMENSION(KLON), INTENT(OUT):: PTH    ! th
REAL, DIMENSION(KLON), INTENT(OUT):: PRV    ! vapor mixing ratio
REAL, DIMENSION(KLON), INTENT(INOUT):: PRL    ! vapor mixing ratio
REAL, DIMENSION(KLON), INTENT(INOUT):: PRI    ! vapor mixing ratio
REAL, DIMENSION(KLON), INTENT(OUT)  :: PRSATW ! estimated mixing ration at saturation over water
REAL, DIMENSION(KLON), INTENT(OUT)  :: PRSATI ! estimated mixing ration at saturation over ice
INTEGER             , INTENT(IN) :: KSTSZ
INTEGER             , INTENT(IN) :: KSTPT
REAL                , INTENT(INOUT):: PSTACK (KSTSZ)
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
INTEGER                       :: II ! Loop control
INTEGER                       :: JITER ! number of iterations
INTEGER                       :: J

temp (REAL, ZEXN, (KLON))

temp (REAL, ZCPH2  , (KLON))
temp (REAL, ZRLTEMP, (KLON))
temp (REAL, ZCPH   , (KLON))
temp (REAL, ZRVSAT , (KLON))

temp (REAL, ZLSOCPEXN, (KLON))
temp (REAL, ZLVOCPEXN, (KLON))
temp (REAL, ZT       , (KLON))

temp (REAL, ZDRSATODTI, (KLON))
temp (REAL, ZDRSATODTW, (KLON))
temp (REAL, ZDRSATODT , (KLON))

temp (REAL, ZFOESI, (KLON))
temp (REAL, ZFOESW, (KLON))

temp (REAL, Z1PRT, (KLON))
temp (REAL, Z99PP, (KLON))
temp (REAL, ZLOGT, (KLON))

REAL(KIND=JPRB) :: ZVAR1, ZVAR2, ZTPOW2, ZDELT

init_stack ()

alloc (Z1PRT)
alloc (Z99PP)
alloc (ZLOGT)
alloc (ZFOESI)
alloc (ZFOESW)
alloc (ZDRSATODTI)
alloc (ZDRSATODTW)
alloc (ZDRSATODT)
alloc (ZLSOCPEXN)
alloc (ZLVOCPEXN)
alloc (ZT)
alloc (ZCPH2)
alloc (ZRLTEMP)
alloc (ZCPH)
alloc (ZRVSAT)
alloc (ZEXN)

!----------------------------------------------------------------------------
!
!*      1 Initialisation
!         --------------
!
!
!
!Number of iterations
JITER=2
!
!Computation of ZCPH2 depending on dummy arguments received
ZCPH2(KIDIA:KFDIA)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZEXN(KIDIA:KFDIA)=(PP(KIDIA:KFDIA)/XP00) ** RDSCPD

DO J=KIDIA,KFDIA
Z99PP(J)=0.99*PP(J)
PRV(J)=PRT(J)-PRL(J)-PRI(J)
ZCPH(J)=XCPD+ XCPV * PRV(J)+ XCL * PRL(J) + XCI * PRI(J) + ZCPH2(J)
ZVAR2=ZCPH(J)*ZEXN(J)
ZDELT=(PTHL(J)*ZEXN(J))-XTT
ZLVOCPEXN(J) = (XLVTT + (XCPV-XCL) * ZDELT) /ZVAR2
ZLSOCPEXN(J) = (XLSTT + (XCPV-XCI) * ZDELT) /ZVAR2 
PTH(J)=PTHL(J)+ZLVOCPEXN(J)*PRL(J)+ZLSOCPEXN(J)*PRI(J)
Z1PRT(J)=1+PRT(J)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  ZT(KIDIA:KFDIA)=PTH(KIDIA:KFDIA)*ZEXN(KIDIA:KFDIA)

  !Computation of liquid/ice fractions
  PFRAC_ICE(KIDIA:KFDIA) = 0.
  DO J=KIDIA, KFDIA
    IF(PRL(J)+PRI(J) > 1.E-20) THEN
      PFRAC_ICE(J) = PRI(J) / (PRL(J)+PRI(J))
    ENDIF
  ENDDO
  CALL COMPUTE_FRAC_ICE1D(KLON,KIDIA,KFDIA,HFRAC_ICE,PFRAC_ICE,ZT,KSTPT,KSTSZ,PSTACK)

  !Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZLOGT(KIDIA:KFDIA)=LOG(ZT(KIDIA:KFDIA))

  DO J=KIDIA,KFDIA

  ZFOESW(J) = MIN(EXP( XALPW - XBETAW/ZT(J) - XGAMW*ZLOGT(J)  ), Z99PP(J))
  ZFOESI(J) = MIN(EXP( XALPI - XBETAI/ZT(J) - XGAMI*ZLOGT(J)  ), Z99PP(J))
  PRSATW(J) = XRD/XRV*ZFOESW(J)/PP(J) / (1.+(XRD/XRV-1.)*ZFOESW(J)/PP(J))
  PRSATI(J) = XRD/XRV*ZFOESI(J)/PP(J) / (1.+(XRD/XRV-1.)*ZFOESI(J)/PP(J))
  ZTPOW2=ZT(J)**2
  ZDRSATODTW(J) = PRSATW(J) / (1.+(XRD/XRV-1.)*ZFOESW(J)/PP(J) ) &
                   * (XBETAW/ZTPOW2 - XGAMW/ZT(J))*Z1PRT(J)
  ZDRSATODTI(J) = PRSATI(J) / (1.+(XRD/XRV-1.)*ZFOESI(J)/PP(J) ) &
                   * (XBETAI/ZTPOW2 - XGAMI/ZT(J))*Z1PRT(J)
  !PRSATW(J) =  QSAT(ZT(J),PP(J)) !qsatw
  !PRSATI(J) = QSATI(ZT(J),PP(J)) !qsati
  !ZDRSATODTW(J) =  DQSAT(ZT(J),PP(J),PRSATW(J))*Z1PRT(J)
  !ZDRSATODTI(J) = DQSATI(ZT(J),PP(J),PRSATI(J))*Z1PRT(J)
  PRSATW(J) = PRSATW(J)*Z1PRT(J)
  PRSATI(J) = PRSATI(J)*Z1PRT(J)
  ZRVSAT(J) = PRSATW(J)*(1-PFRAC_ICE(J)) + PRSATI(J)*PFRAC_ICE(J)
  ZDRSATODT(J) = (ZDRSATODTW(J)*(1-PFRAC_ICE(J))+ &
            & ZDRSATODTI(J)*PFRAC_ICE(J))

  !Computation of new PRL, PRI and PRV
  !Correction term applied to (PRV(J)-ZRVSAT(J)) is computed assuming that
  !ZLVOCPEXN, ZLSOCPEXN and ZCPH don't vary to much with T. It takes into account
  !the variation (estimated linear) of Qsat with T
  ZRLTEMP(J)=(PRV(J)-ZRVSAT(J))/ &
                &(1 + ZDRSATODT(J)*ZEXN(J)* &
                &     (ZLVOCPEXN(J)*(1-PFRAC_ICE(J))+ZLSOCPEXN(J)*PFRAC_ICE(J)))
  ZRLTEMP(J)=MIN(MAX(-PRL(J)-PRI(J), ZRLTEMP(J)),PRV(J))
  PRV(J)=PRV(J)-ZRLTEMP(J)
  PRL(J)=PRL(J)+PRI(J)+ZRLTEMP(J)
  PRI(J)=PFRAC_ICE(J)     * (PRL(J))
  PRL(J)=(1-PFRAC_ICE(J)) * (PRT(J) - PRV(J))

  !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
  ZCPH(J)=XCPD+ XCPV * PRV(J)+ XCL * PRL(J) + XCI * PRI(J) + ZCPH2(J)

  !Computation of L/Cph/EXN, then new PTH
  ZVAR2=ZCPH(J)*ZEXN(J)
  ZLVOCPEXN(J) = (XLVTT + (XCPV-XCL) * (ZT(J)-XTT)) /ZVAR2
  ZLSOCPEXN(J) = (XLSTT + (XCPV-XCI) * (ZT(J)-XTT)) /ZVAR2
  PTH(J)=PTHL(J)+ZLVOCPEXN(J)*PRL(J)+ZLSOCPEXN(J)*PRI(J)

  !Computation of estimated mixing ration at saturation
  !To compute the adjustement a first order development was used
  ZVAR1=PTH(J)*ZEXN(J)-ZT(J)
  PRSATW(J)=PRSATW(J) + ZDRSATODTW(J)*ZVAR1
  PRSATI(J)=PRSATI(J) + ZDRSATODTI(J)*ZVAR1

  ENDDO
ENDDO


END SUBROUTINE TH_R_FROM_THL_RT_1D
