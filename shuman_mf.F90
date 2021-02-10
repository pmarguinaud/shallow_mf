!     ###############################
#ifdef USE_ACC
!$acc routine (MZM_MF)
#endif
      SUBROUTINE MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PA,PMZM,KSTPT,KSTSZ,PSTACK)
!     ###############################
!
!!****  *MZM* -  SHUMAN_MF operator : mean operator in z direction for a
!!                                 mass variable
!!
!!    PURPOSE
!!    -------
!       The purpose of this function  is to compute a mean
!     along the z direction (K index) for a field PA localized at a mass
!     point. The result is localized at a z-flux point (w point).
!
!!**  METHOD
!!    ------
!!        The result PMZM(:,:,k) is defined by 0.5*(PA(:,:,k)+PA(:,:,k-1))
!!        At k=1, PMZM(:,:,1) is defined by PA(:,:,1).
!!
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (SHUMAN_MF operators)
!!      Technical specifications Report of The Meso-NH (chapters 3)
!!
!!
!!    AUTHOR
!!    ------
!!      V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/07/94
!!                   optimisation                 20/08/00 J. Escobar
!!      S. Riette, Jan 2012: Simplification and suppression of array overflow
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of argument and result
!              ------------------------------------
!
INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KIDIA
INTEGER,              INTENT(IN)       :: KFDIA
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     ! variable at mass localization
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)      :: PMZM   ! result at flux localization
INTEGER,              INTENT(IN)       :: KSTSZ
INTEGER,              INTENT(IN)       :: KSTPT
REAL   ,              INTENT(INOUT)    :: PSTACK (KSTSZ)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JK             ! Loop index in z direction
!
!
!-------------------------------------------------------------------------------
!
!*       1.    DEFINITION OF MZM
!              ------------------
!
DO JK=2,KLEV-1
  PMZM(KIDIA:KFDIA,JK) = 0.5*( PA(KIDIA:KFDIA,JK)+PA(KIDIA:KFDIA,JK-KKL) )
END DO
PMZM(KIDIA:KFDIA,KKA) = PA(KIDIA:KFDIA,KKA)
PMZM(KIDIA:KFDIA,KKU) = 0.5*( PA(KIDIA:KFDIA,KKU)+PA(KIDIA:KFDIA,KKU-KKL) )
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MZM_MF
!     ###############################
#ifdef USE_ACC
!$acc routine (DZM_MF)
#endif
      SUBROUTINE DZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PA,PDZM,KSTPT,KSTSZ,PSTACK)
!     ###############################
!
!!****  *DZM* -  SHUMAN_MF operator : finite difference operator in z direction
!!                                  for a variable at a mass localization
!!
!!    PURPOSE
!!    -------
!       The purpose of this function  is to compute a finite difference
!     along the z direction (K index) for a field PA localized at a mass
!     point. The result is localized at a z-flux point (w point).
!
!!**  METHOD
!!    ------
!!        The result PDZM(:,j,:) is defined by (PA(:,:,k)-PA(:,:,k-1))
!!        At k=1, PDZM(:,:,k) is defined by 0.
!!
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (SHUMAN_MF operators)
!!      Technical specifications Report of The Meso-NH (chapters 3)
!!
!!
!!    AUTHOR
!!    ------
!!      V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/07/94
!!                   optimisation                 20/08/00 J. Escobar
!!      S. Riette, Jan 2012: Simplification and suppression of array overflow
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of argument and result
!              ------------------------------------
!
INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KIDIA
INTEGER,              INTENT(IN)       :: KFDIA
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     ! variable at mass
                                                 ! localization
REAL, DIMENSION(KLON,KLEV), INTENT(OUT)      :: PDZM   ! result at flux
                                                 INTEGER,              INTENT(IN)       :: KSTSZ
INTEGER,              INTENT(IN)       :: KSTPT
REAL   ,              INTENT(INOUT)    :: PSTACK (KSTSZ)
! side
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JK            ! Loop index in z direction
!
!-------------------------------------------------------------------------------
!
!*       1.    DEFINITION OF DZM
!              ------------------
!
DO JK=2,KLEV-1
  PDZM(KIDIA:KFDIA,JK) = PA(KIDIA:KFDIA,JK) - PA(KIDIA:KFDIA,JK-KKL)
END DO
PDZM(KIDIA:KFDIA,KKA) = 0.
PDZM(KIDIA:KFDIA,KKU) = PA(KIDIA:KFDIA,KKU) - PA(KIDIA:KFDIA,KKU-KKL)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DZM_MF

