SUBROUTINE SHALLOW_MF_LOAD_ALL (CDNAME)

USE LOAD_MOD, ONLY : OPEN_LOAD, CLOSE_LOAD, ILUN_IN, LOAD
IMPLICIT NONE

CHARACTER (LEN=*) :: CDNAME

LOGICAL, SAVE :: LLDONE = .FALSE.
INTEGER :: IDUM

IF (.NOT. LLDONE) THEN
  CALL OPEN_LOAD (CDNAME)
  CALL LOAD_MODD_NEB
  CALL LOAD_MODD_CST
  CALL LOAD_MODD_CMFSHALL
  CALL LOAD_MODD_CTURB
  CALL CLOSE_LOAD
ENDIF

LLDONE = .TRUE.

CONTAINS

SUBROUTINE LOAD_MODD_NEB

USE MODD_NEB

REAL :: ZTMP

CALL LOAD (ILUN_IN, XTMINMIX)

#ifdef USE_ACC
ZTMP = XTMINMIX
!$acc parallel
XTMINMIX = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XTMAXMIX)


#ifdef USE_ACC
ZTMP = XTMAXMIX
!$acc parallel
XTMAXMIX = ZTMP
!$acc end parallel
#endif

END SUBROUTINE

SUBROUTINE LOAD_MODD_CTURB

USE MODD_CTURB

REAL :: ZTMP
LOGICAL :: LLTMP

CALL LOAD (ILUN_IN, XCMFS)

#ifdef USE_ACC
ZTMP = XCMFS
!$acc parallel
XCMFS = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCMFB)

#ifdef USE_ACC
ZTMP = XCMFB
!$acc parallel
XCMFB = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCSHF)

#ifdef USE_ACC
ZTMP = XCSHF
!$acc parallel
XCSHF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCHF)

#ifdef USE_ACC
ZTMP = XCHF
!$acc parallel
XCHF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCTV)

#ifdef USE_ACC
ZTMP = XCTV
!$acc parallel
XCTV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCHV)

#ifdef USE_ACC
ZTMP = XCHV
!$acc parallel
XCHV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCHT1)

#ifdef USE_ACC
ZTMP = XCHT1
!$acc parallel
XCHT1 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCHT2)

#ifdef USE_ACC
ZTMP = XCHT2
!$acc parallel
XCHT2 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPR1)

#ifdef USE_ACC
ZTMP = XCPR1
!$acc parallel
XCPR1 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPR2)

#ifdef USE_ACC
ZTMP = XCPR2
!$acc parallel
XCPR2 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPR3)

#ifdef USE_ACC
ZTMP = XCPR3
!$acc parallel
XCPR3 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPR4)

#ifdef USE_ACC
ZTMP = XCPR4
!$acc parallel
XCPR4 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPR5)

#ifdef USE_ACC
ZTMP = XCPR5
!$acc parallel
XCPR5 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCET)

#ifdef USE_ACC
ZTMP = XCET
!$acc parallel
XCET = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCDP)

#ifdef USE_ACC
ZTMP = XCDP
!$acc parallel
XCDP = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCDD)

#ifdef USE_ACC
ZTMP = XCDD
!$acc parallel
XCDD = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCDT)

#ifdef USE_ACC
ZTMP = XCDT
!$acc parallel
XCDT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XTKEMIN)

#ifdef USE_ACC
ZTMP = XTKEMIN
!$acc parallel
XTKEMIN = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLINI)

#ifdef USE_ACC
ZTMP = XLINI
!$acc parallel
XLINI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLINF)

#ifdef USE_ACC
ZTMP = XLINF
!$acc parallel
XLINF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XALPSBL)

#ifdef USE_ACC
ZTMP = XALPSBL
!$acc parallel
XALPSBL = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XASBL)

#ifdef USE_ACC
ZTMP = XASBL
!$acc parallel
XASBL = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCEP)

#ifdef USE_ACC
ZTMP = XCEP
!$acc parallel
XCEP = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XA0)

#ifdef USE_ACC
ZTMP = XA0
!$acc parallel
XA0 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XA2)

#ifdef USE_ACC
ZTMP = XA2
!$acc parallel
XA2 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XA3)

#ifdef USE_ACC
ZTMP = XA3
!$acc parallel
XA3 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XA5)

#ifdef USE_ACC
ZTMP = XA5
!$acc parallel
XA5 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCTD)

#ifdef USE_ACC
ZTMP = XCTD
!$acc parallel
XCTD = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCTP)

#ifdef USE_ACC
ZTMP = XCTP
!$acc parallel
XCTP = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XPHI_LIM)

#ifdef USE_ACC
ZTMP = XPHI_LIM
!$acc parallel
XPHI_LIM = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XSBL_O_BL)

#ifdef USE_ACC
ZTMP = XSBL_O_BL
!$acc parallel
XSBL_O_BL = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XFTOP_O_FSURF)

#ifdef USE_ACC
ZTMP = XFTOP_O_FSURF
!$acc parallel
XFTOP_O_FSURF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, LHARAT)


#ifdef USE_ACC
LLTMP = LHARAT
!$acc parallel
LHARAT = LLTMP
!$acc end parallel
#endif

END SUBROUTINE

SUBROUTINE LOAD_MODD_CMFSHALL

USE MODD_CMFSHALL

REAL :: ZTMP

REAL :: ZTMP

CALL LOAD (ILUN_IN, XALP_PERT)

#ifdef USE_ACC
ZTMP = XALP_PERT
!$acc parallel
XALP_PERT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XABUO)

#ifdef USE_ACC
ZTMP = XABUO
!$acc parallel
XABUO = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XBENTR)

#ifdef USE_ACC
ZTMP = XBENTR
!$acc parallel
XBENTR = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XBDETR)

#ifdef USE_ACC
ZTMP = XBDETR
!$acc parallel
XBDETR = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCMF)

#ifdef USE_ACC
ZTMP = XCMF
!$acc parallel
XCMF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XENTR_MF)

#ifdef USE_ACC
ZTMP = XENTR_MF
!$acc parallel
XENTR_MF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCRAD_MF)

#ifdef USE_ACC
ZTMP = XCRAD_MF
!$acc parallel
XCRAD_MF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XENTR_DRY)

#ifdef USE_ACC
ZTMP = XENTR_DRY
!$acc parallel
XENTR_DRY = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XDETR_DRY)

#ifdef USE_ACC
ZTMP = XDETR_DRY
!$acc parallel
XDETR_DRY = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XDETR_LUP)

#ifdef USE_ACC
ZTMP = XDETR_LUP
!$acc parallel
XDETR_LUP = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XKCF_MF)

#ifdef USE_ACC
ZTMP = XKCF_MF
!$acc parallel
XKCF_MF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XKRC_MF)

#ifdef USE_ACC
ZTMP = XKRC_MF
!$acc parallel
XKRC_MF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XTAUSIGMF)

#ifdef USE_ACC
ZTMP = XTAUSIGMF
!$acc parallel
XTAUSIGMF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XPRES_UV)

#ifdef USE_ACC
ZTMP = XPRES_UV
!$acc parallel
XPRES_UV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XALPHA_MF)

#ifdef USE_ACC
ZTMP = XALPHA_MF
!$acc parallel
XALPHA_MF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XSIGMA_MF)

#ifdef USE_ACC
ZTMP = XSIGMA_MF
!$acc parallel
XSIGMA_MF = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XFRAC_UP_MAX)

#ifdef USE_ACC
ZTMP = XFRAC_UP_MAX
!$acc parallel
XFRAC_UP_MAX = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XA1)

#ifdef USE_ACC
ZTMP = XA1
!$acc parallel
XA1 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XB)

#ifdef USE_ACC
ZTMP = XB
!$acc parallel
XB = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XC)

#ifdef USE_ACC
ZTMP = XC
!$acc parallel
XC = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XBETA1)

#ifdef USE_ACC
ZTMP = XBETA1
!$acc parallel
XBETA1 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XR)

#ifdef USE_ACC
ZTMP = XR
!$acc parallel
XR = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLAMBDA)

#ifdef USE_ACC
ZTMP = XLAMBDA
!$acc parallel
XLAMBDA = ZTMP
!$acc end parallel
#endif

END SUBROUTINE

SUBROUTINE LOAD_MODD_CST

USE MODD_CST

REAL :: ZTMP

CALL LOAD (ILUN_IN, XPI)

#ifdef USE_ACC
ZTMP = XPI
!$acc parallel
XPI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XDAY)

#ifdef USE_ACC
ZTMP = XDAY
!$acc parallel
XDAY = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XSIYEA)

#ifdef USE_ACC
ZTMP = XSIYEA
!$acc parallel
XSIYEA = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XSIDAY)

#ifdef USE_ACC
ZTMP = XSIDAY
!$acc parallel
XSIDAY = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XKARMAN)

#ifdef USE_ACC
ZTMP = XKARMAN
!$acc parallel
XKARMAN = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLIGHTSPEED)

#ifdef USE_ACC
ZTMP = XLIGHTSPEED
!$acc parallel
XLIGHTSPEED = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XPLANCK)

#ifdef USE_ACC
ZTMP = XPLANCK
!$acc parallel
XPLANCK = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XBOLTZ)

#ifdef USE_ACC
ZTMP = XBOLTZ
!$acc parallel
XBOLTZ = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XAVOGADRO)

#ifdef USE_ACC
ZTMP = XAVOGADRO
!$acc parallel
XAVOGADRO = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XRADIUS)

#ifdef USE_ACC
ZTMP = XRADIUS
!$acc parallel
XRADIUS = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XOMEGA)

#ifdef USE_ACC
ZTMP = XOMEGA
!$acc parallel
XOMEGA = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XG)

#ifdef USE_ACC
ZTMP = XG
!$acc parallel
XG = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XP00)

#ifdef USE_ACC
ZTMP = XP00
!$acc parallel
XP00 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XSTEFAN)

#ifdef USE_ACC
ZTMP = XSTEFAN
!$acc parallel
XSTEFAN = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XI0)

#ifdef USE_ACC
ZTMP = XI0
!$acc parallel
XI0 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XMD)

#ifdef USE_ACC
ZTMP = XMD
!$acc parallel
XMD = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XMV)

#ifdef USE_ACC
ZTMP = XMV
!$acc parallel
XMV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XRD)

#ifdef USE_ACC
ZTMP = XRD
!$acc parallel
XRD = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XRV)

#ifdef USE_ACC
ZTMP = XRV
!$acc parallel
XRV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XEPSILO)

#ifdef USE_ACC
ZTMP = XEPSILO
!$acc parallel
XEPSILO = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPD)

#ifdef USE_ACC
ZTMP = XCPD
!$acc parallel
XCPD = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCPV)

#ifdef USE_ACC
ZTMP = XCPV
!$acc parallel
XCPV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XRHOLW)

#ifdef USE_ACC
ZTMP = XRHOLW
!$acc parallel
XRHOLW = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCL)

#ifdef USE_ACC
ZTMP = XCL
!$acc parallel
XCL = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCI)

#ifdef USE_ACC
ZTMP = XCI
!$acc parallel
XCI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XTT)

#ifdef USE_ACC
ZTMP = XTT
!$acc parallel
XTT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLVTT)

#ifdef USE_ACC
ZTMP = XLVTT
!$acc parallel
XLVTT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLSTT)

#ifdef USE_ACC
ZTMP = XLSTT
!$acc parallel
XLSTT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XLMTT)

#ifdef USE_ACC
ZTMP = XLMTT
!$acc parallel
XLMTT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XESTT)

#ifdef USE_ACC
ZTMP = XESTT
!$acc parallel
XESTT = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XALPW)

#ifdef USE_ACC
ZTMP = XALPW
!$acc parallel
XALPW = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XBETAW)

#ifdef USE_ACC
ZTMP = XBETAW
!$acc parallel
XBETAW = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XGAMW)

#ifdef USE_ACC
ZTMP = XGAMW
!$acc parallel
XGAMW = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XALPI)

#ifdef USE_ACC
ZTMP = XALPI
!$acc parallel
XALPI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XBETAI)

#ifdef USE_ACC
ZTMP = XBETAI
!$acc parallel
XBETAI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XGAMI)

#ifdef USE_ACC
ZTMP = XGAMI
!$acc parallel
XGAMI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XCONDI)

#ifdef USE_ACC
ZTMP = XCONDI
!$acc parallel
XCONDI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XTH00)

#ifdef USE_ACC
ZTMP = XTH00
!$acc parallel
XTH00 = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, XRHOLI)

#ifdef USE_ACC
ZTMP = XRHOLI
!$acc parallel
XRHOLI = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, NDAYSEC)

#ifdef USE_ACC
ZTMP = NDAYSEC
!$acc parallel
NDAYSEC = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, RDSRV)

#ifdef USE_ACC
ZTMP = RDSRV
!$acc parallel
RDSRV = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, RDSCPD)

#ifdef USE_ACC
ZTMP = RDSCPD
!$acc parallel
RDSCPD = ZTMP
!$acc end parallel
#endif

CALL LOAD (ILUN_IN, RINVXP00)


#ifdef USE_ACC
ZTMP = RINVXP00
!$acc parallel
RINVXP00 = ZTMP
!$acc end parallel
#endif

END SUBROUTINE

END SUBROUTINE

