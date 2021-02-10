!     ######spl
#ifdef USE_ACC
!$acc routine (COMPUTE_BL89_ML)
#endif
      SUBROUTINE COMPUTE_BL89_ML(KLON,KIDIA,KFDIA,KLEV,KKA,KKB,KKE,KKU,KKL,PDZZ2D, &
             PTKEM_DEP,PG_O_THVREF,PVPT,KK,OUPORDN,OFLUX,PLWORK,KSTPT,KSTSZ,PSTACK)

      
#include "temp.h"

USE PARKIND1, ONLY : JPRB
!     ###################################################################
!!
!!     COMPUTE_BL89_ML routine to:
!!       1/ compute upward or downward mixing length with BL89 formulation
!!
!!    AUTHOR
!!    ------
!!     J. PERGAUD
!!
!!    MODIFICATIONS
!!    -------------
!!     Original   19/01/06
!!     S. Riette Jan 2012: support for both order of vertical levels and cleaning
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!              ------------
!
!!!!!!!!!!!!
!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!! WARNING !!
!!!!!!!!!!!!
!!!!!!!!!!!!
!Any modification done to this routine must be copied in bl89.f90.
!This routine was inlined in bl89 for numerical performance reasons
!but algorithm must remain the same.
!!!!!!!!!!!!
!
USE MODD_CTURB
USE MODD_PARAMETERS, ONLY: JPVEXT
USE MODI_SHUMAN_MF
!
IMPLICIT NONE
!
!          0.1 arguments
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
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)  :: PDZZ2D        ! height difference between two mass levels
REAL, DIMENSION(KLON),     INTENT(IN)  :: PTKEM_DEP     ! TKE to consume
REAL, DIMENSION(KLON),     INTENT(IN)  :: PG_O_THVREF   ! g/ThetaVRef at the departure point
REAL, DIMENSION(KLON,KLEV),   INTENT(IN)  :: PVPT          ! ThetaV on mass levels
INTEGER,                INTENT(IN)  :: KK            ! index of departure level
LOGICAL,                INTENT(IN)  :: OUPORDN       ! switch to compute upward (true) or
                                                     !   downward (false) mixing length
LOGICAL,                INTENT(IN)  :: OFLUX         ! Computation must be done from flux level
REAL, DIMENSION(KLON),     INTENT(OUT) :: PLWORK        ! Resulting mixing length

INTEGER,                INTENT(IN)   :: KSTSZ
INTEGER,                INTENT(IN)   :: KSTPT
REAL   ,                INTENT(INOUT):: PSTACK (KSTSZ)
!          0.2 Local variable
!
! Temporary mixing length
temp (REAL, ZLWORK2, (KLON))
temp (REAL, ZLWORK1, (KLON))
! TKE and potential energy
temp (REAL, ZPOTE, (KLON))
temp (REAL, ZINTE, (KLON))
!   between 2 levels
! Thetav on departure point
temp (REAL, ZVPT_DEP, (KLON))
!
temp (REAL, ZHLVPT, (KLON,KLEV))
temp (REAL, ZDELTVPT, (KLON,KLEV))
!Virtual Potential Temp at Half level and DeltaThv between
                      !2 mass levels

INTEGER :: IIJU                 !Internal Domain
INTEGER :: J1D                  !horizontal loop counter
INTEGER :: JKK                  !loop counters
REAL    :: ZTEST,ZTEST0,ZTESTM  !test for vectorization

init_stack ()

alloc (ZHLVPT)
alloc (ZDELTVPT)
alloc (ZVPT_DEP)
alloc (ZPOTE)
alloc (ZINTE)
alloc (ZLWORK2)
alloc (ZLWORK1)

!-------------------------------------------------------------------------------------
!
!*       1.    INITIALISATION
!              --------------
IIJU=KLON
!
CALL DZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PVPT,ZDELTVPT,ISTPT,KSTSZ,PSTACK)
ZDELTVPT(KIDIA:KFDIA,KKA)=0.
WHERE (ABS(ZDELTVPT(KIDIA:KFDIA,:))<XLINF)
  ZDELTVPT(KIDIA:KFDIA,:)=XLINF
END WHERE
!
CALL MZM_MF(KLON,KIDIA,KFDIA,KLEV,KKA,KKU,KKL,PVPT, ZHLVPT,ISTPT,KSTSZ,PSTACK)
!
!We consider that gradient between mass levels KKB and KKB+KKL is the same as
!the gradient between flux level KKB and mass level KKB
ZDELTVPT(KIDIA:KFDIA,KKB)=PDZZ2D(KIDIA:KFDIA,KKB)*ZDELTVPT(KIDIA:KFDIA,KKB+KKL)/PDZZ2D(KIDIA:KFDIA,KKB+KKL)
ZHLVPT(KIDIA:KFDIA,KKB)=PVPT(KIDIA:KFDIA,KKB)-ZDELTVPT(KIDIA:KFDIA,KKB)*0.5
!
!
!
!*       2.    CALCULATION OF THE UPWARD MIXING LENGTH
!              ---------------------------------------
!

IF (OUPORDN.EQV..TRUE.) THEN 
 ZINTE(KIDIA:KFDIA)=PTKEM_DEP(KIDIA:KFDIA)
 PLWORK(KIDIA:KFDIA)=0.
 ZTESTM=1.
 IF(OFLUX)THEN
   ZVPT_DEP(KIDIA:KFDIA)=ZHLVPT(KIDIA:KFDIA,KK) ! departure point is on flux level
   !We must compute what happens between flux level KK and mass level KK
   DO J1D=KIDIA,KFDIA
     ZTEST0=0.5+SIGN(0.5,ZINTE(J1D)) ! test if there's energy to consume
     ! Energy consumed if parcel cross the entire layer
     ZPOTE(J1D) = ZTEST0*(PG_O_THVREF(J1D)      *      &
         (0.5*(ZHLVPT(J1D,KK)+ PVPT(J1D,KK)) - ZVPT_DEP(J1D)))  * &
         PDZZ2D(J1D,KK)*0.5
     ! Test if it rests some energy to consume
     ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
     ! Length travelled by parcel if it rests energy to consume
     ZLWORK1(J1D)=PDZZ2D(J1D,KK)*0.5
     ! Lenght travelled by parcel to nullify energy
     ZLWORK2(J1D)=        ( - PG_O_THVREF(J1D) *                     &
            (  ZHLVPT(J1D,KK) - ZVPT_DEP(J1D) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF(J1D) * (ZHLVPT(J1D,KK) - ZVPT_DEP(J1D)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF(J1D)                        &
                 * ZDELTVPT(J1D,KK) / PDZZ2D(J1D,KK) ))    ) /             &
        ( PG_O_THVREF(J1D) * ZDELTVPT(J1D,KK) / PDZZ2D(J1D,KK) ) 
      ! Effective length travelled by parcel
      PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D))
      ! Rest of energy to consume
      ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
   ENDDO
 ELSE
   ZVPT_DEP(KIDIA:KFDIA)=PVPT(KIDIA:KFDIA,KK) ! departure point is on mass level
 ENDIF

 DO JKK=KK+KKL,KKE,KKL
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=KIDIA,KFDIA
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = ZTEST0*(PG_O_THVREF(J1D)      *      &
            (ZHLVPT(J1D,JKK) - ZVPT_DEP(J1D)))  * PDZZ2D(J1D,JKK) !particle keeps its temperature
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        !ZLWORK2 jump of the last reached level
        ZLWORK2(J1D)=        ( - PG_O_THVREF(J1D) *                     &
            (  PVPT(J1D,JKK-KKL) - ZVPT_DEP(J1D) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF(J1D) * (PVPT(J1D,JKK-KKL) - ZVPT_DEP(J1D)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF(J1D)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF(J1D) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      !
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D))
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
!!
!*       2.    CALCULATION OF THE DOWNWARD MIXING LENGTH
!              ---------------------------------------
!

IF (OUPORDN.EQV..FALSE.) THEN 
 IF(OFLUX) THEN
   WRITE(*,*) ' STOP'                                                     
   WRITE(*,*) ' OFLUX OPTION NOT CODED FOR DOWNWARD MIXING LENGTH' 
   CALL ABORT
   STOP
 ENDIF
 ZINTE(KIDIA:KFDIA)=PTKEM_DEP(KIDIA:KFDIA)
 PLWORK(KIDIA:KFDIA)=0.
 ZTESTM=1.
 DO JKK=KK,KKB,-KKL
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=KIDIA,KFDIA
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = -ZTEST0*(PG_O_THVREF(J1D)      *      &
            (ZHLVPT(J1D,JKK) - PVPT(J1D,KK)))  * PDZZ2D(J1D,JKK) !particle keeps its temperature
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        ZLWORK2(J1D)=        ( + PG_O_THVREF(J1D) *                     &
            (  PVPT(J1D,JKK) - PVPT(J1D,KK) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF(J1D) * (PVPT(J1D,JKK) - PVPT(J1D,KK)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF(J1D)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF(J1D) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      !
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D)) 
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
  
END SUBROUTINE COMPUTE_BL89_ML
