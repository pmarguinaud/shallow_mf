!     ##################
      MODULE MODI_SHUMAN_MF
!     ##################
!
INTERFACE
!
FUNCTION DZF_MF(KKA,KKU,KKL,PA)  RESULT(PDZF)
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:), INTENT(IN)       :: PA     ! variable at flux
                                                 !  side
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2)) :: PDZF   ! result at mass
                                                 ! localization
END FUNCTION DZF_MF
!
FUNCTION DZM_MF(KKA,KKU,KKL,PA)  RESULT(PDZM)
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:), INTENT(IN)       :: PA     ! variable at mass
                                                 ! localization
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2)) :: PDZM   ! result at flux
                                                 ! side
END FUNCTION DZM_MF
!
FUNCTION MZF_MF(KKA,KKU,KKL,PA)  RESULT(PMZF)
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:), INTENT(IN)       :: PA     ! variable at flux
                                                 !  side
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2)) :: PMZF   ! result at mass
                                                 ! localization
END FUNCTION MZF_MF
!
FUNCTION MZM_MF(KKA,KKU,KKL,PA)  RESULT(PMZM)
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)       :: KKL    ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:), INTENT(IN)       :: PA     ! variable at mass localization
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2)) :: PMZM   ! result at flux localization
END FUNCTION MZM_MF
!
FUNCTION GZ_M_W_MF(KKA,KKU,KKL,PY,PDZZ) RESULT(PGZ_M_W)
INTEGER,              INTENT(IN)       :: KKA, KKU ! near ground and uppest atmosphere array indexes
INTEGER,              INTENT(IN)  :: KKL  ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:), INTENT(IN)  :: PDZZ ! Metric coefficient d*zz
REAL, DIMENSION(:,:), INTENT(IN)  :: PY   ! variable at mass localization
REAL, DIMENSION(SIZE(PY,1),SIZE(PY,2)) :: PGZ_M_W  ! result at flux side
END FUNCTION GZ_M_W_MF
!
END INTERFACE
!
END MODULE MODI_SHUMAN_MF
