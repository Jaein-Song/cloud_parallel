!= (MODULE) PARAMETER ===============================================!

      MODULE mod_para

!==== FILE INFO

      !CHARACTER(LEN=200),DIMENSION(5000) :: ifile
      CHARACTER(LEN=110)        :: ifile
      CHARACTER(LEN=110)        :: outfn,outfnno,outfn14,outfn15
      INTEGER                   :: flagno,flag14,flag15
!==== HEAD

      !  File level
      CHARACTER(LEN=8)   :: FileID
      REAL               :: VerNo
      INTEGER            :: HeaderLen,qcstat
      !  Station parameter
      CHARACTER(LEN=16)  :: Station, RadarType
      CHARACTER(LEN=40)  :: Temp1
      REAL               :: Lon, Lat, Alt, Azi

      !  Performance parameter
      CHARACTER(LEN=39)  :: Temp2
      INTEGER            :: Ae, WaveLen, FreqMode,StartDist
      INTEGER(KIND=2)    :: Dist, BinLen, BinNum
      INTEGER(KIND=1)    :: IsFilter
      REAL               :: VerBeamW, HorBeamW
      REAL               :: PulseW, TranPp, TranAp, Prf
      REAL               :: MwaveF, MwaveP, TranPm
      REAL(KIND=4),DIMENSION(2) :: Loss, DRange, Prmin, CalCoeff

      !  Observation parameter
      CHARACTER(LEN=20)  :: Temp3
      INTEGER(KIND=1)    :: TimeP
      INTEGER(KIND=2)    :: Syear, Eyear
      INTEGER(KIND=1)    :: Smon, Sday, Shour, Smin, Ssec
      INTEGER(KIND=1)    :: Emon, Eday, Ehour, Emin, Esec
      INTEGER(KIND=1)    :: DSPPro
      INTEGER(KIND=2)    :: PulseNum, CoAccNum, SpecAccNum
      INTEGER(KIND=1)    :: FilNum, WinN, ScanMode
      INTEGER(KIND=2)    :: VolLayerNum, VolLayerNo
      INTEGER            :: ChLen, RayNum
      REAL               :: NoiseThres, AzBgn, ElBgn
      REAL               :: StepAngle, ScanSpeed
      REAL               :: AzLowLimit, AzUpLimit, ElLowLimit, ElUpLimit
      INTEGER(KIND=1),DIMENSION(2) :: ChFlag

!==== RAY HEAD

      CHARACTER(LEN=20),ALLOCATABLE   :: RecTime(:)
      INTEGER,ALLOCATABLE             :: RayNo(:)
      INTEGER,ALLOCATABLE             :: VolLayNo(:)
      REAL,ALLOCATABLE                :: AZ(:), EL(:)
      REAL,ALLOCATABLE                :: WavePRF(:)

!==== BLOCK

      !  Base data
      REAL,ALLOCATABLE,DIMENSION(:,:) :: LDR, Z1, Vr1, SW1, SNR1
      REAL,ALLOCATABLE,DIMENSION(:,:) :: LDRo, Z1o, Vr1o, SW1o, SNR1o
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z2, Vr2, SW2, SNR2
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z2o, Vr2o, SW2o, SNR2o

      REAL,ALLOCATABLE,DIMENSION(:,:) :: LDRt, Z1t, Vr1t, SW1t, SNR1t
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z2t, Vr2t, SW2t, SNR2t
      integer,allocatable,dimension(:,:)::echo,mask
      !  Spectrum data
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: SPall1, Spall2

      !  IQ data
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: IQall1, IQall2

!==== ETC
     
      CHARACTER(LEN=70) :: a1
      CHARACTER(LEN=17) :: a2 
      CHARACTER(LEN=6) :: a3 
      CHARACTER(LEN=50), DIMENSION(5000) :: outfile
      CHARACTER(LEN=17),DIMENSION(5000) :: scanmd
      INTEGER,DIMENSION(5000) :: iyr, imn, idy, ihh, imm
      INTEGER  :: ih, im, flag, sflag
      INTEGER  :: it, ir, ib, ip, tn, rn, bn, pn, cn

      REAL,DIMENSION(0:23,0:59,1000) :: rLDR, rZ1, rVr1, rSW1, rSNR1
      REAL,DIMENSION(0:23,0:59,1000) :: rZ2, rVr2, rSW2, rSNR2

      END MODULE mod_para
