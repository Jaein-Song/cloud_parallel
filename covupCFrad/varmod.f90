!MODULE: Variable Declare for covup_dailyCFradial.f90 and following subroutines
!Made by J. Song, Mar 22, 2017
!1. Vars for Main body
!1.1. File I/O Vars
!2. Vars for Subroutines
!2.1 Vars for ncread
!2.2 Vars for qc
!2.3 Vars for ncwrite

MODULE varmod 
IMPLICIT NONE

!1. Vars for Main body
!1.1. File I/O Vars
CHARACTER(len=150)                  :: Ifname,IfnameC,OfnameNO,Ofname14,Ofname15,Ofname17
INTEGER                             :: Nfile,NfileC,tlen,fflag,fflagC,Cflag,Dflag,dti,dhi
INTEGER                             :: flag_noqc,flag_qc14,flag_qc15,flag_qc17
integer,Dimension(:)                :: qcflags(4)
!2. Vars for subroutines
!2.1 Vars for Subroutine ncread
INTEGER                             :: nid,nidC,sts,tdim,rdim,fdim
INTEGER                             :: tcsid,Refhid,Refvid,Velhid,Velvid,SpWhid,SpWvid,LDRaid,SNRhid,SNRvid
CHARACTER(len=4)                    :: timdim
CHARACTER(len=5)                    :: randim
CHARACTER(len=9)                    :: frqdim
CHARACTER(len=21)                   :: tcs,tcsC
REAL                                :: PRFIND(2),PRFINC(2),NVINC,NVIND,AeIND,AeINC,HorBeamWIND,HorBeamWINC
INTEGER                             :: latitudeID,latitudeIDC,longitudeID,longitudeIDC,altitudeID,altitudeIDC,Lat,LatC,Lon,LonC,Alt,AltC
INTEGER                             :: FreqMode,FreqModeC,frequencyID, frequencyIDC, pulse_widthID,pulse_widthIDC,prt_modeID,prt_modeIDC,prtID,prtIDC,prt_ratioID,prt_ratioIDC
INTEGER                             :: nyquist_velocityID,nyquist_velocityIDC,n_samplesID,n_samplesIDC,radar_antenna_gain_hID,radar_antenna_gain_hIDC,radar_beam_width_hID,radar_beam_width_hIDC
CHARACTER(len=6)                    :: prtmodeI,prtmodeIC
REAL,ALLOCATABLE,DIMENSION(:)       :: frequencyarrI,frequencyarrIC,pulse_widtharrI,pulse_widtharrIC,PRTI,PRTIC,WavePRFC,prt_ratioarrI,prt_ratioarrIC,prtarD,prtarC
REAL,ALLOCATABLE,DIMENSION(:)       :: nyq_VelarrI,nyq_VelarrIC,PWD,PWC,prtrD,prtrC,nyqD,nyqC,n_samplesC,n_samplesarrI,n_samplesarrIC
REAL                                :: AeI,AeIC,HorBeamWI,HorBeamWIC
integer,ALLOCATABLE,dimension(:)    :: n_samplesD

INTEGER                             :: yeD,mnD,daD,hoD,miD,seD,yeC,mnC,daC,hoC,miC,seC,timeC,timeD,tscl
INTEGER                             :: ftlen,ftlenC
!REAL                                :: Zsf,Vsf,Wsf,Ssf,Lsf,offset
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhN,RefvN,VelhN,VelvN,SpWhN,SpWvN,SNRhN,SNRvN,LDRaN
REAL,ALLOCATABLE,DIMENSION(:,:)     :: RefhINC,RefvINC,VelhINC,VelvINC,SpWhINC,SpWvINC,SNRhINC,SNRvINC,LDRaINC
REAL,ALLOCATABLE,DIMENSION(:,:)     :: RefhIND,RefvIND,VelhIND,VelvIND,SpWhIND,SpWvIND,SNRhIND,SNRvIND,LDRaIND
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhNC,RefvNC,VelhNC,VelvNC,SpWhNC,SpWvNC,SNRhNC,SNRvNC,LDRaNC
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhND,RefvND,VelhND,VelvND,SpWhND,SpWvND,SNRhND,SNRvND,LDRaND
REAL,ALLOCATABLE,DIMENSION(:,:)     :: RefhI,RefvI,VelhI,VelvI,SpWhI,SpWvI,SNRhI,SNRvI,LDRaI

!2.2. Vars for qc
INTEGER                             :: ti,hi,Qflags,Qflagsv,Refhf,Refvf,VelhF,VelvF,LDRaF,binlenC,frqlen,frqlenC
REAL                                :: Refhs,Refvs,Velhs,Velvs,SpWhs,SpWvs,SNRhs,SNRvs,LDRas,LDRms,LDRTXTm,LDRDOSm,RefhDOSm,VelhTXTm,Refh10,LDRa10,Refv10,SNRh10,SNRv10
INTEGER,ALLOCATABLE,DIMENSION(:)    :: RefhONO,RefvONO,VelhONO,VelvONO,SpWhONO,SpWvONO,SNRhONO,SNRvONO,LDRaONO
INTEGER,ALLOCATABLE,DIMENSION(:)    :: RefhO14,RefvO14,VelhO14,VelvO14,SpWhO14,SpWvO14,SNRhO14,SNRvO14,LDRaO14
INTEGER,ALLOCATABLE,DIMENSION(:)    :: RefhO15,RefvO15,VelhO15,VelvO15,SpWhO15,SpWvO15,SNRhO15,SNRvO15,LDRaO15
INTEGER,ALLOCATABLE,DIMENSION(:)    :: RefhO17,RefvO17,VelhO17,VelvO17,SpWhO17,SpWvO17,SNRhO17,SNRvO17,LDRaO17
INTEGER,ALLOCATABLE,DIMENSION(:)    :: Qflag,Qflagv
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: LDRDOS,RefhDOS,VelhDOS,Refhmask
REAL,ALLOCATABLE,DIMENSION(:,:)     :: LDRTXT,LDRlow,LDRTXTlow,VelhTXT,genTXT
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: echopdf
real,parameter                      :: ratio_qc17=0.5
!QC15
INTEGER                             :: maxmask,echoi,scani,vari,pdfi,pdfj,echox,mi,typei,typex,ldrcount,lowmaskcount
INTEGER                             :: dum_mask,check_mask,iOpLDR,wflag
REAL,DIMENSION(:,:)                 :: genPDF(100,2)
integer,ALLOCATABLE,DIMENSION(:,:)  :: echotype
INTEGER,DIMENSION(:)                :: typecount(3)
integer,ALLOCATABLE,DIMENSION(:)    :: maskcount

! vars for noqc
INTEGER                             :: LDRmi,nCase,cellwh
INTEGER,PARAMETER                   :: cellsize=7
!2.3 Vars for ncwrite
INTEGER                             :: qci
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhWNO,RefvWNO,VelhWNO,VelvWNO,SpWhWNO,SpWvWNO,SNRhWNO,SNRvWNO,LDRaWNO
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: Z1t,Z2t,Vr1t,Vr2t,SW1t,SW2t,SNR1t,SNR2t,LDRt
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhW14,RefvW14,VelhW14,VelvW14,SpWhW14,SpWvW14,SNRhW14,SNRvW14,LDRaW14
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhW15,RefvW15,VelhW15,VelvW15,SpWhW15,SpWvW15,SNRhW15,SNRvW15,LDRaW15
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhW17,RefvW17,VelhW17,VelvW17,SpWhW17,SpWvW17,SNRhW17,SNRvW17,LDRaW17
REAL                                :: Ae,HorBeamW,nyq_Vel

INTEGER,PARAMETER                   :: fv=-32768,echolen=4, scanlen=3, varlen=5
REAL,PARAMETER                      :: Zsf=0.01,Vsf=0.01,Wsf=0.01,Ssf=0.01,Lsf=0.01,offset=0.0

END MODULE varmod
