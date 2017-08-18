!= (SUBROUTINE) HEAD READ ================================================!

      SUBROUTINE read_head

      USE mod_para

      IMPLICIT NONE

      !  File level
      READ(1) FileID, VerNo, HeaderLen

      !  Station parameter
      READ(1) Station, RadarType, Lon, Lat, Alt, Azi, Temp1

      !  Performance parameter
      READ(1) Ae, VerBeamW, HorBeamW, WaveLen, PulseW
      READ(1) TranPp, TranAp, FreqMode
      READ(1) Prf, Loss, DRange, Prmin
      READ(1) CalCoeff, MwaveF, MwaveP, TranPm, Dist
      READ(1) BinLen, BinNum, StartDist, IsFilter, Temp2

      !  Observation parameter
      READ(1) TimeP
      READ(1) Syear, Smon, Sday, Shour, Smin, Ssec
      READ(1) Eyear, Emon, Eday, Ehour, Emin, Esec
      READ(1) DSPPro, PulseNum, CoAccNum, SpecAccNum
      READ(1) NoiseThres, FilNum, WinN, ScanMode
      READ(1) VolLayerNum, VolLayerNo
      READ(1) AzBgn, ElBgn
      READ(1) StepAngle, ScanSpeed, RayNum
      READ(1) ChFlag, ChLen
      READ(1) AzLowLimit, AzUpLimit, ElLowLimit, ElUpLimit, Temp3

      END SUBROUTINE read_head

!= (SUBROUTINE) WRITE ====================================================!

      SUBROUTINE write_head

      USE mod_para

      IMPLICIT NONE

      !  File level
      PRINT*,'-- File level -------------------------------'
      PRINT*,'FileID              : ', FileID
      PRINT*,'Version No.         : ', VerNo
      PRINT*,'FileHeaderLength    : ', HeaderLen

      !  Station parameter
      PRINT*,'-- Station parameter ------------------------'
      PRINT*,'Station             : ', Station
      PRINT*,'RadarType           : ', RadarType
      PRINT*,'Lon/Lat             : ', Lon, ' / ', Lat
      PRINT*,'Altitude            : ', Alt
      PRINT*,'Azimuth             : ', Azi

      !  Performance parameter
      PRINT*,'-- Performance parameter --------------------'
      PRINT*,'Antenna gain(dB)    : ', Ae
      PRINT*,'Vertical Beam Width : ', VerBeamW
      PRINT*,'Horizon. Beam Width : ', HorBeamW
      PRINT*,'Wave Length         : ', WaveLen
      PRINT*,'Pulse Width         : ', PulseW
      PRINT*,'Power of peak       : ', TranPp
      PRINT*,'Average power       : ', TranAp
      PRINT*,''
      PRINT*,'Frequency mode      : ', FreqMode
      PRINT*,'PRF                 : ', Prf
      PRINT*,'Loss                : ', Loss
      PRINT*,'Dynamic range       : ', DRange
      PRINT*,'Flexibility         : ', Prmin
      PRINT*,'Calibration factor  : ', CalCoeff
      PRINT*,'Freq. of main wave  : ', MwaveF
      PRINT*,'Power of main wave  : ', MwaveP
      PRINT*,'Transmitter power   : ', TranPm
      PRINT*,'Detection dist.     : ', Dist
      PRINT*,'Bin Length          : ', BinLen
      PRINT*,'Bin Number          : ', BinNum
      PRINT*,'Sampling start dist.: ', StartDist
      PRINT*,'Filtering           : ', IsFilter

      !  Observation parameter
      PRINT*,'-- Observation parameter --------------------'
      PRINT*,'Source of time      : ', TimeP
      PRINT*,'Start year          : ', Syear
      PRINT*,'Start month         : ', Smon
      PRINT*,'Start day           : ', Sday
      PRINT*,'Start hour          : ', Shour
      PRINT*,'Start min           : ', Smin
      PRINT*,'Start sec           : ', Ssec
      PRINT*,''
      PRINT*,'End year            : ', Eyear
      PRINT*,'End month           : ', Emon
      PRINT*,'End day             : ', Eday
      PRINT*,'End hour            : ', Ehour
      PRINT*,'End min             : ', Emin
      PRINT*,'End sec             : ', Esec
      PRINT*,''
      PRINT*,'Sig. treat. method  : ', DSPPro
      PRINT*,'Quantity of pulse   : ', PulseNum
      PRINT*,'Accu.Q. phase para  : ', CoAccNum
      PRINT*,'Accu.Q. Nphase para : ', SpecAccNum
      PRINT*,'Noise rm threshold  : ', NoiseThres
      PRINT*,'Filter No.          : ', FilNum
      PRINT*,'Window category     : ', WinN
      PRINT*,'Scanning method     : ', ScanMode
      PRINT*,'Num. of Lay. 3Dscan : ', VolLayerNum
      PRINT*,'No. of Lay. 3Dscan  : ', VolLayerNo
      PRINT*,'Start Azi. angle    : ', AzBgn
      PRINT*,'Start Ele. angle    : ', ElBgn
      PRINT*,'Stepping angle      : ', StepAngle
      PRINT*,'Scanning Speed      : ', ScanSpeed
      PRINT*,'Ray number          : ', RayNum
      PRINT*,'Channel data label  : ', ChFlag
      PRINT*,'Len. of data block  : ', ChLen
      PRINT*,'Azi. low limit      : ', AzLowLimit
      PRINT*,'Azi. up limit       : ', AzUpLimit
      PRINT*,'Ele. low limit      : ', ElLowLimit
      PRINT*,'Ele. up limit       : ', ElUpLimit

      END SUBROUTINE write_head

