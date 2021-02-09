!     ######spl
      SUBROUTINE THL_RT_FROM_TH_R_MF( KLON,KIDIA,KFDIA,KLEV,KRR,KRRL,KRRI,       &
                                      PTH, PR, PEXN, &
                                      PTHL, PRT,KSTPT,KSTSZ,PSTACK                      )
!     #################################################################
!
!!
!!****  *THL_RT_FROM_TH_R* - computes the conservative variables THL and RT
!!                           from TH and the non precipitating water species
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
!!      V. Masson               * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         20/09/02
!!      Externalisation of computations done in TURB and MF_TURB (Malardel and Pergaud, fev. 2007)
!!      V.Masson : Optimization
!!      S. Riette 2011 suppression of PLVOCPEXN and PLSOCPEXN
!!
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CST
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL          ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI          ! number of ice water var.

REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PTH      ! theta
REAL, DIMENSION(KLON,KLEV,KRR), INTENT(IN) :: PR       ! water species
REAL, DIMENSION(KLON,KLEV), INTENT(IN)   :: PEXN    ! exner function

REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  :: PTHL     ! th_l
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)  :: PRT      ! total non precip. water
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!

!----------------------------------------------------------------------------
REAL, DIMENSION(KLON,KLEV) :: ZCP, ZT
REAL, DIMENSION(KLON,KLEV) :: ZLVOCPEXN, ZLSOCPEXN
INTEGER :: JRR
!----------------------------------------------------------------------------
!
!
!temperature
ZT(KIDIA:KFDIA,:) = PTH(KIDIA:KFDIA,:) * PEXN(KIDIA:KFDIA,:)

!Cp
ZCP(KIDIA:KFDIA,:)=XCPD
IF (KRR > 0) ZCP(KIDIA:KFDIA,:) = ZCP(KIDIA:KFDIA,:) + XCPV * PR(KIDIA:KFDIA,:,1)
DO JRR = 2,1+KRRL  ! loop on the liquid components
  ZCP(KIDIA:KFDIA,:)  = ZCP(KIDIA:KFDIA,:) + XCL * PR(KIDIA:KFDIA,:,JRR)
END DO
DO JRR = 2+KRRL,1+KRRL+KRRI ! loop on the solid components
  ZCP(KIDIA:KFDIA,:)  = ZCP(KIDIA:KFDIA,:)  + XCI * PR(KIDIA:KFDIA,:,JRR)
END DO

IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN
    !ZLVOCPEXN and ZLSOCPEXN
    ZLVOCPEXN(KIDIA:KFDIA,:)=(XLVTT + (XCPV-XCL) *  (ZT(KIDIA:KFDIA,:)-XTT) ) / ZCP(KIDIA:KFDIA,:) / PEXN(KIDIA:KFDIA,:)
    ZLSOCPEXN(KIDIA:KFDIA,:)=(XLSTT + (XCPV-XCI) *  (ZT(KIDIA:KFDIA,:)-XTT) ) / ZCP(KIDIA:KFDIA,:) / PEXN(KIDIA:KFDIA,:)
    ! Rnp 
    PRT(KIDIA:KFDIA,:)  = PR(KIDIA:KFDIA,:,1)  + PR(KIDIA:KFDIA,:,2)  + PR(KIDIA:KFDIA,:,4)
    ! Theta_l 
    PTHL(KIDIA:KFDIA,:)  = PTH(KIDIA:KFDIA,:)  - ZLVOCPEXN(KIDIA:KFDIA,:) * PR(KIDIA:KFDIA,:,2) &
                           - ZLSOCPEXN(KIDIA:KFDIA,:) * PR(KIDIA:KFDIA,:,4)
  ELSE
    !ZLVOCPEXN
    ZLVOCPEXN(KIDIA:KFDIA,:)=(XLVTT + (XCPV-XCL) *  (ZT(KIDIA:KFDIA,:)-XTT) ) / ZCP(KIDIA:KFDIA,:) / PEXN(KIDIA:KFDIA,:)
    ! Rnp
    PRT(KIDIA:KFDIA,:)  = PR(KIDIA:KFDIA,:,1)  + PR(KIDIA:KFDIA,:,2) 
    ! Theta_l
    PTHL(KIDIA:KFDIA,:) = PTH(KIDIA:KFDIA,:)  - ZLVOCPEXN(KIDIA:KFDIA,:) * PR(KIDIA:KFDIA,:,2)
  END IF
ELSE
  ! Rnp = rv
  PRT(KIDIA:KFDIA,:)  = PR(KIDIA:KFDIA,:,1)
  ! Theta_l = Theta
  PTHL(KIDIA:KFDIA,:) = PTH(KIDIA:KFDIA,:)
END IF
END SUBROUTINE THL_RT_FROM_TH_R_MF
