!     ##################
      MODULE MODI_SHUMAN_MF
!     ##################
!
INTERFACE
!
FUNCTION DZM_MF(KLON,KLEV,KKA,KKU,KKL,PA)  RESULT(PDZM)
INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     ! variable at mass
                                                 ! localization
REAL, DIMENSION(KLON,KLEV) :: PDZM   ! result at flux
                                                 ! side
END FUNCTION DZM_MF
!
FUNCTION MZM_MF(KLON,KLEV,KKA,KKU,KKL,PA)  RESULT(PMZM)
INTEGER,              INTENT(IN)       :: KLON
INTEGER,              INTENT(IN)       :: KLEV
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(KLON,KLEV), INTENT(IN)       :: PA     ! variable at mass localization
REAL, DIMENSION(KLON,KLEV) :: PMZM   ! result at flux localization
END FUNCTION MZM_MF
!
END INTERFACE
!
END MODULE MODI_SHUMAN_MF
