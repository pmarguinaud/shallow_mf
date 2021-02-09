!     ######spl
       SUBROUTINE TRIDIAG_MASSFLUX(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PVARM,PF,PDFDT,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,PVARP,KSTPT,KSTSZ,PSTACK             )

       
#include "temp.h"

USE PARKIND1, ONLY : JPRB
!      #################################################
!
!
!!****   *TRIDIAG_MASSFLUX* - routine to solve a time implicit scheme
!!
!!
!!     PURPOSE
!!     -------
!        The purpose of this routine is to give a field PVARP at t+1, by 
!      solving an implicit TRIDIAGonal system obtained by the 
!      discretization of the vertical turbulent diffusion. It should be noted 
!      that the degree of implicitness can be varied (PIMPL parameter) and that
!      the function of F(T) must have been linearized.
!      PVARP is localized at a mass point.
!
!!**   METHOD
!!     ------
!!
!!        [T(+) - T(-)]/2Dt = -d{ F + dF/dT *impl*[T(+) + T(-)] }/dz
!!
!!     It is discretized as follows:
!!
!!    PRHODJ(k)*PVARP(k)/PTSTEP
!!              = 
!!    PRHODJ(k)*PVARM(k)/PTSTEP 
!!  - (PRHODJ(k+1)+PRHODJ(k)  )/2. * PF(k+1)/PDZZ(k+1)
!!  + (PRHODJ(k)  +PRHODJ(k-1))/2. * PF(k)  /PDZZ(k)
!!  + (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1) * PVARM(k+1)/PDZZ(k+1)
!!  - (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1) * PVARP(k+1)/PDZZ(k+1)
!!  + (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1) * PVARM(k)  /PDZZ(k+1)
!!  - (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1) * PVARP(k)  /PDZZ(k+1)
!!  - (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)   * PVARM(k)  /PDZZ(k)
!!  + (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)   * PVARP(k)  /PDZZ(k)
!!  - (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)   * PVARM(k-1)/PDZZ(k)
!!  + (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)   * PVARP(k-1)/PDZZ(k)
!!
!!
!!    The system to solve is:
!!
!!      A*PVARP(k-1) + B*PVARP(k) + C*PVARP(k+1) = Y(k)
!!
!!
!!    The RHS of the linear system in PVARP writes:
!!
!! y(k)    = PRHODJ(k)*PVARM(k)/PTSTEP
!!  - (PRHODJ(k+1)+PRHODJ(k)  )/2. * PF(k+1)/PDZZ(k+1)
!!  + (PRHODJ(k)  +PRHODJ(k-1))/2. * PF(k)  /PDZZ(k)
!!  + (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1) * PVARM(k+1)/PDZZ(k+1)
!!  + (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1) * PVARM(k)  /PDZZ(k+1)
!!  - (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)   * PVARM(k)  /PDZZ(k)
!!  - (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)   * PVARM(k-1)/PDZZ(k)
!!
!!                      
!!        Then, the classical TRIDIAGonal algorithm is used to invert the 
!!     implicit operator. Its matrix is given by:
!!
!!     ( b(KKB)   c(KKB)      0        0        0         0        0        0  )
!!     ( a(KKB+1) b(KKB+1) c(KKB+1)    0  ...    0        0        0        0  ) 
!!     (   0      a(KKB+2) b(KKB+2) c(KKB+2).    0        0        0        0  ) 
!!      .......................................................................
!!     (   0   ...   0     a(k)     b(k)     c(k)         0   ...  0        0  ) 
!!      .......................................................................
!!     (   0         0        0        0        0 ...a(KKE-1) b(KKE-1) c(KKE-1))
!!     (   0         0        0        0        0 ...     0   a(KKE)   b(KKE)  )
!!
!!     KKB and KKE represent the first and the last inner mass levels of the
!!     model. The coefficients are:
!!         
!! a(k) = - (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)  /PDZZ(k)
!! b(k) =    PRHODJ(k) / PTSTEP
!!        + (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1)/PDZZ(k+1)
!!        - (PRHODJ(k)  +PRHODJ(k-1))/2. * 0.5*PIMPL* PDFDT(k)  /PDZZ(k)
!! c(k) = + (PRHODJ(k+1)+PRHODJ(k)  )/2. * 0.5*PIMPL* PDFDT(k+1)/PDZZ(k+1)
!!
!!          for all k /= KKB or KKE
!!
!!
!! b(KKB) =  PRHODJ(KKB) / PTSTEP
!!          +(PRHODJ(KKB+1)+PRHODJ(KKB))/2.*0.5*PIMPL*PDFDT(KKB+1)/PDZZ(KKB+1)
!! c(KKB) = +(PRHODJ(KKB+1)+PRHODJ(KKB))/2.*0.5*PIMPL*PDFDT(KKB+1)/PDZZ(KKB+1)
!!
!! b(KKE) =  PRHODJ(KKE) / PTSTEP
!!          -(PRHODJ(KKE)+PRHODJ(KKE-1))/2.*0.5*PIMPL*PDFDT(KKE)/PDZZ(KKE)
!! a(KKE) = -(PRHODJ(KKE)+PRHODJ(KKE-1))/2.*0.5*PIMPL*PDFDT(KKE)/PDZZ(KKE)
!!
!!
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!
!!     REFERENCE
!!     ---------
!!       Press et al: Numerical recipes (1986) Cambridge Univ. Press
!!
!!     AUTHOR
!!     ------
!!       V. Masson and S. Malardel         * Meteo-France *   
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        07/2006
!!       V.Masson : Optimization
!!       S. Riette Jan 2012: support for both order of vertical levels
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
USE MODD_PARAMETERS, ONLY: JPVEXT
USE MODI_SHUMAN_MF
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
INTEGER,                INTENT(IN)   :: KLON
INTEGER,                INTENT(IN)   :: KIDIA
INTEGER,                INTENT(IN)   :: KFDIA
INTEGER,                INTENT(IN)   :: KLEV
INTEGER,                INTENT(IN)   :: KKA          ! near ground array index
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKU          ! uppest atmosphere array index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PVARM   ! variable at t-1      at mass point
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PF      ! flux in dT/dt=-dF/dz at flux point
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PDFDT   ! dF/dT                at flux point
REAL,                   INTENT(IN) :: PTSTEP  ! Double time step
REAL,                   INTENT(IN) :: PIMPL   ! implicit weight
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PDZZ    ! Dz                   at flux point
REAL, DIMENSION(KLON,KLEV), INTENT(IN) :: PRHODJ  ! (dry rho)*J          at mass point
!
REAL, DIMENSION(KLON,KLEV), INTENT(OUT):: PVARP   ! variable at t+1      at mass point
INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!
!
!*       0.2 declarations of local variables
!

temp (REAL, ZRHODJ_DFDT_O_DZ, (KLON,KLEV))
temp (REAL, ZMZM_RHODJ      , (KLON,KLEV))

temp (REAL, ZC, (KLON,KLEV))
temp (REAL, ZB, (KLON,KLEV))
temp (REAL, ZA, (KLON,KLEV))
 
temp (REAL, ZGAM, (KLON,KLEV))
temp (REAL, ZY  , (KLON,KLEV))
! RHS of the equation, 3D work array
temp (REAL, ZBET, (KLON))
! 2D work array
INTEGER                              :: JK            ! loop counter

init_stack ()

alloc (ZBET)
alloc (ZGAM)
alloc (ZY)
alloc (ZC)
alloc (ZB)
alloc (ZA)
alloc (ZMZM_RHODJ)
alloc (ZRHODJ_DFDT_O_DZ)

!
! ---------------------------------------------------------------------------
!                                              
!*      1.  Preliminaries
!           -------------
!
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PRHODJ,ZMZM_RHODJ,KSTPT,KSTSZ,PSTACK)
ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,:) = ZMZM_RHODJ(KIDIA:KFDIA,:)*PDFDT(KIDIA:KFDIA,:)/PDZZ(KIDIA:KFDIA,:)
!
ZA(KIDIA:KFDIA,:)=0.
ZB(KIDIA:KFDIA,:)=0.
ZC(KIDIA:KFDIA,:)=0.
ZY(KIDIA:KFDIA,:)=0.
!
!
!*      2.  COMPUTE THE RIGHT HAND SIDE
!           ---------------------------
!
ZY(KIDIA:KFDIA,KKB) = PRHODJ(KIDIA:KFDIA,KKB)*PVARM(KIDIA:KFDIA,KKB)/PTSTEP             &
    - ZMZM_RHODJ(KIDIA:KFDIA,KKB+KKL) * PF(KIDIA:KFDIA,KKB+KKL)/PDZZ(KIDIA:KFDIA,KKB+KKL)     &
    + ZMZM_RHODJ(KIDIA:KFDIA,KKB  ) * PF(KIDIA:KFDIA,KKB  )/PDZZ(KIDIA:KFDIA,KKB  )     &
    + ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKB+KKL) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,KKB+KKL)    &
    + ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKB+KKL) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,KKB  )
!
DO JK=2+JPVEXT,KLEV-JPVEXT-1
  ZY(KIDIA:KFDIA,JK) = PRHODJ(KIDIA:KFDIA,JK)*PVARM(KIDIA:KFDIA,JK)/PTSTEP          &
    - ZMZM_RHODJ(KIDIA:KFDIA,JK+KKL) * PF(KIDIA:KFDIA,JK+KKL)/PDZZ(KIDIA:KFDIA,JK+KKL)    &
    + ZMZM_RHODJ(KIDIA:KFDIA,JK  ) * PF(KIDIA:KFDIA,JK  )/PDZZ(KIDIA:KFDIA,JK  )    &
    + ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK+KKL) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,JK+KKL)  &
    + ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK+KKL) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,JK  )  &
    - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK  ) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,JK  )  &
    - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK  ) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,JK-KKL)
END DO
! 
IF (JPVEXT==0) THEN
  ZY(KIDIA:KFDIA,KKE) = PRHODJ(KIDIA:KFDIA,KKE)*PVARM(KIDIA:KFDIA,KKE)/PTSTEP 
ELSE
  ZY(KIDIA:KFDIA,KKE) = PRHODJ(KIDIA:KFDIA,KKE)*PVARM(KIDIA:KFDIA,KKE)/PTSTEP &
   - ZMZM_RHODJ(KIDIA:KFDIA,KKE+KKL) * PF(KIDIA:KFDIA,KKE+KKL)/PDZZ(KIDIA:KFDIA,KKE+KKL) &
   + ZMZM_RHODJ(KIDIA:KFDIA,KKE  ) * PF(KIDIA:KFDIA,KKE  )/PDZZ(KIDIA:KFDIA,KKE  ) &
   - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKE ) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,KKE  ) &
   - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKE ) * 0.5*PIMPL * PVARM(KIDIA:KFDIA,KKE-KKL)
ENDIF
!
!
!*       3.  INVERSION OF THE TRIDIAGONAL SYSTEM
!            -----------------------------------
!
IF ( PIMPL > 1.E-10 ) THEN
!
!*       3.1 arrays A, B, C
!            --------------
!
  ZB(KIDIA:KFDIA,KKB) =   PRHODJ(KIDIA:KFDIA,KKB)/PTSTEP                   &
                + ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKB+KKL) * 0.5*PIMPL
  ZC(KIDIA:KFDIA,KKB) =   ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKB+KKL) * 0.5*PIMPL

  DO JK=2+JPVEXT,KLEV-JPVEXT-1
    ZA(KIDIA:KFDIA,JK) = - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK  ) * 0.5*PIMPL
    ZB(KIDIA:KFDIA,JK) =   PRHODJ(KIDIA:KFDIA,JK)/PTSTEP                   &
                 + ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK+KKL) * 0.5*PIMPL &
                 - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK  ) * 0.5*PIMPL
    ZC(KIDIA:KFDIA,JK) =   ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,JK+KKL) * 0.5*PIMPL
  END DO

  ZA(KIDIA:KFDIA,KKE) = - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKE  ) * 0.5*PIMPL
  ZB(KIDIA:KFDIA,KKE) =   PRHODJ(KIDIA:KFDIA,KKE)/PTSTEP                   &
                - ZRHODJ_DFDT_O_DZ(KIDIA:KFDIA,KKE  ) * 0.5*PIMPL
!
!*       3.2 going up
!            --------
!
  ZBET(KIDIA:KFDIA) = ZB(KIDIA:KFDIA,KKB)  ! bet = b(KKB)
  PVARP(KIDIA:KFDIA,KKB) = ZY(KIDIA:KFDIA,KKB) / ZBET(KIDIA:KFDIA)

  !
  DO JK = KKB+KKL,KKE-KKL,KKL
    ZGAM(KIDIA:KFDIA,JK) = ZC(KIDIA:KFDIA,JK-KKL) / ZBET(KIDIA:KFDIA)
                                                    ! gam(k) = c(k-1) / bet
    ZBET(KIDIA:KFDIA)    = ZB(KIDIA:KFDIA,JK) - ZA(KIDIA:KFDIA,JK) * ZGAM(KIDIA:KFDIA,JK)
                                                    ! bet = b(k) - a(k)* gam(k)  
    PVARP(KIDIA:KFDIA,JK)= ( ZY(KIDIA:KFDIA,JK) - ZA(KIDIA:KFDIA,JK) * PVARP(KIDIA:KFDIA,JK-KKL) ) / ZBET(KIDIA:KFDIA)
                                        ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
  END DO 
  ! special treatment for the last level
  ZGAM(KIDIA:KFDIA,KKE) = ZC(KIDIA:KFDIA,KKE-KKL) / ZBET(KIDIA:KFDIA)
                                                    ! gam(k) = c(k-1) / bet
  ZBET(KIDIA:KFDIA)     = ZB(KIDIA:KFDIA,KKE) - ZA(KIDIA:KFDIA,KKE) * ZGAM(KIDIA:KFDIA,KKE)
                                                    ! bet = b(k) - a(k)* gam(k)  
  PVARP(KIDIA:KFDIA,KKE)= ( ZY(KIDIA:KFDIA,KKE) - ZA(KIDIA:KFDIA,KKE) * PVARP(KIDIA:KFDIA,KKE-KKL) ) / ZBET(KIDIA:KFDIA)
                                       ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
!
!*       3.3 going down
!            ----------
!
  DO JK = KKE-KKL,KKB,-KKL
    PVARP(KIDIA:KFDIA,JK) = PVARP(KIDIA:KFDIA,JK) - ZGAM(KIDIA:KFDIA,JK+KKL) * PVARP(KIDIA:KFDIA,JK+KKL)
  END DO
!
!
ELSE
  !!! EXPLICIT FORMULATION
  !
  DO JK=1+JPVEXT,KLEV-JPVEXT
    PVARP(KIDIA:KFDIA,JK) = ZY(KIDIA:KFDIA,JK) * PTSTEP / PRHODJ(KIDIA:KFDIA,JK)
  ENDDO
  !
END IF 
!
!
!*       4.  FILL THE UPPER AND LOWER EXTERNAL VALUES
!            ----------------------------------------
!
PVARP(KIDIA:KFDIA,KKA)=PVARP(KIDIA:KFDIA,KKB)
PVARP(KIDIA:KFDIA,KKU)=PVARP(KIDIA:KFDIA,KKE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIDIAG_MASSFLUX
