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
CHARACTER(len=150)                  :: Ifname,outfn,cfname
INTEGER                             :: Nfile,tlen,fflag,dti,dhi,file_start,file_end,ftlen_NC,fflag_in
INTEGER                             :: qcstat,filei, nbin, Iend
integer,Dimension(:)                :: qcflags(4)
CHARACTER(len=20)                   :: filehead
!2. Vars for subroutines
!2.1 Vars for Subroutine ncread
INTEGER                             :: nid,sts,tdim,rdim,fdim
INTEGER                             :: tcsid,Refhid,Refvid,Velhid,Velvid,SpWhid,SpWvid,LDRaid,SNRhid,SNRvid
CHARACTER(len=4)                    :: timdim
CHARACTER(len=5)                    :: randim
CHARACTER(len=9)                    :: frqdim
CHARACTER(len=21)                   :: tcs
REAL                                :: PRFIN(2),NVIN,AeIN,HorBeamWIN
INTEGER                             :: latitudeID,longitudeID,altitudeID,Lat,Lon,Alt
INTEGER                             :: FreqMode,FreqModeC,frequencyID, pulse_widthID,prt_modeID,prtID,prt_ratioID
INTEGER                             :: nyquist_velocityID,n_samplesID,radar_antenna_gain_hID,radar_beam_width_hID
CHARACTER(len=6)                    :: prtmodeI
REAL,ALLOCATABLE,DIMENSION(:)       :: frequencyarrI,pulse_widtharrI,PRTI,prt_ratioarrI,prtar,WavePRF,n_samples
REAL,ALLOCATABLE,DIMENSION(:)       :: nyq_VelarrI,PW,prtr,nyq,n_samplesarrI
REAL                                :: AeI,AeIC,HorBeamWI,HorBeamWIC

INTEGER                             :: ye,mn,da,ho,mi,se,time,tscl
INTEGER                             :: ftlen,ftlen_ceil
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhN,RefvN,VelhN,VelvN,SpWhN,SpWvN,SNRhN,SNRvN,LDRaN
REAL,ALLOCATABLE,DIMENSION(:,:)     :: RefhIN,RefvIN,VelhIN,VelvIN,SpWhIN,SpWvIN,SNRhIN,SNRvIN,LDRaIN
REAL,ALLOCATABLE,DIMENSION(:,:)     :: RefhI,RefvI,VelhI,VelvI,SpWhI,SpWvI,SNRhI,SNRvI,LDRaI

!2.2. Vars for qc
INTEGER                             :: ti,hi,Qflags,Qflagsv,Refhf,Refvf,VelhF,VelvF,LDRaF,binlen,frqlen
REAL                                :: Refhs,Refvs,Velhs,Velvs,SpWhs,SpWvs,SNRhs,SNRvs,LDRas,LDRms,Refh10,LDRa10,Refv10,SNRh10,SNRv10
INTEGER,ALLOCATABLE,DIMENSION(:)    :: RefhO,RefvO,VelhO,VelvO,SpWhO,SpWvO,SNRhO,SNRvO,LDRaO
INTEGER,ALLOCATABLE,DIMENSION(:)    :: Qflag,Qflagv
real,parameter                      :: ratio_qc17=0.8

! vars for noqc
INTEGER                             :: LDRmi,nCase,cellwh
INTEGER,PARAMETER                   :: cellsize=7
!2.3 Vars for ncwrite
INTEGER                             :: qci
INTEGER,ALLOCATABLE,DIMENSION(:,:)  :: RefhW,RefvW,VelhW,VelvW,SpWhW,SpWvW,SNRhW,SNRvW,LDRaW
REAL                                :: Ae,HorBeamW,nyq_Vel

!Vars for qc17_ceil
INTEGER                             :: nid_ceil,tdim_ceil,ncbhid,cbh1id,vcbhid,llcflag,num_valid_cell,min_hi,QC_flag
CHARACTER(len=8)                    :: timdim_ceil
REAL,ALLOCATABLE,DIMENSION(:)       :: ncbh,cbh1,vcbh
REAL                                :: cbh,count,valid,cvratio,Ref_low_thres
!PARAMETERS
INTEGER,PARAMETER                   :: fv=-32768,echolen=4, scanlen=3, varlen=5
REAL,PARAMETER                      :: Zsf=0.01,Vsf=0.01,Wsf=0.01,Ssf=0.01,Lsf=0.01,offset=0.0

END MODULE varmod
