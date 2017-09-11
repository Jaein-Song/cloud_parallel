!= Written by J. E. Lee, ORD, NIMS =========================================!
!modified for unified daily QC fortran program by Jae In Song, OFRD, NIMS
!= (SUBROUTINE) WRITE a CF-Radial ==========================================!

      SUBROUTINE ncwrite
      USE netcdf
      USE varmod
      IMPLICIT NONE

!= ID ================================================================!
 
      INTEGER :: ncid, status
      INTEGER :: tID, rID, sweepID, freqID
      INTEGER :: string_length_4ID, string_length_6ID
      INTEGER :: string_length_10ID, string_length_21ID
      INTEGER :: string_length_24ID, string_length_34ID

      INTEGER :: volume_numberID
      INTEGER :: time_coverage_startID, time_coverage_endID

      INTEGER :: timeID, rangeID
  
      
      INTEGER :: sweep_numberID, sweep_modeID, fixed_angleID
      INTEGER :: sweep_start_ray_indexID, sweep_end_ray_indexID

      INTEGER :: azimuthID, elevationID

      INTEGER :: ref_hID, ref_vID, vel_hID, vel_vID, sw_hID, sw_vID
      INTEGER :: snr_hID, snr_vID, ldrID

      INTEGER,DIMENSION(2) :: dimids

!= Parameter used to make variable ===========:WRITE==========================!

      INTEGER :: NumVolLayer, AZ, EL,BL,VolLayerNum,rn,bn
      REAL        :: PulseW
      INTEGER                      :: irn, ibn        
      CHARACTER(LEN=3),ALLOCATABLE :: iii(:)
      INTEGER,ALLOCATABLE :: inthh(:), intmin(:), intss(:), intiii(:)
      INTEGER,dimension(:),ALLOCATABLE:: yyyy,mm,dd,hh,min,ss
      character(len=4) :: qctype
      CHARACTER(LEN=21) :: starttime
      CHARACTER(LEN=21) :: endtime

      REAL,ALLOCATABLE  :: timearr(:), rangearr(:)!,prt_mode(:)

      INTEGER             :: sweepIndex
      INTEGER,ALLOCATABLE :: sweepNumArr(:)
      REAL,ALLOCATABLE    :: fixed_angleArr(:)
      INTEGER,ALLOCATABLE :: sweepStartRayArr(:), sweepEndRayArr(:)  

      INTEGER :: sTimeIndex, eTimeIndex

      INTEGER(KIND=2) :: FillValue
      REAL            :: scaleFactor!, offSet

      REAL,ALLOCATABLE :: frequencyarr(:), pulse_widtharr(:),prt_arr(:)
      REAL,ALLOCATABLE :: prt_ratioarr(:), nyq_Velarr(:)
      REAL,ALLOCATABLE :: n_samplesarr(:)
      CHARACTER(len=7),Parameter::RadarType='HMB-KSM'
    rn=tlen
    bn=1000
      ALLOCATE(inthh(rn)) ; ALLOCATE(intmin(rn))
      ALLOCATE(intss(rn)) ; ALLOCATE(intiii(rn))

      ALLOCATE(timearr(rn)) ; ALLOCATE(rangearr(bn))
      VolLayerNum=1
      ALLOCATE(sweepNumArr(VolLayerNum))
      ALLOCATE(fixed_angleArr(VolLayerNum))
      ALLOCATE(sweepStartRayArr(VolLayerNum))
      ALLOCATE(sweepEndRayArr(VolLayerNum))

      ALLOCATE(frequencyarr(FreqMode))
      ALLOCATE(pulse_widtharr(rn))
      ALLOCATE(prt_arr(rn))
      ALLOCATE(prt_ratioarr(rn))
      ALLOCATE(nyq_Velarr(rn))
      ALLOCATE(n_samplesarr(rn))


        if (qcstat.le.2) then
        qctype='noQC'
        elseif (qcstat.eq.14) then
        qctype='QC14'
        elseif (qcstat.eq.15) then
        qctype='QC15'
        elseif (qcstat.eq.17) then
        qctype='QC17'
        endif
        HorBeamW=HorBeamWI
        Ae=AeI
        nyq_Velarr=nyq
!        prt_mode=prtmodeI
        n_samplesarr=n_samples
        prt_ratioarr=prtr
        pulse_widtharr=PW
        prt_arr=prtar
        starttime = tcs(1:10)// 'T00:00:00Z'
      endtime = tcs(1:10)// 'T23:59:59Z'
   
!= (Make variable) ===================================================!
      != number of volume
      NumVolLayer = VolLayerNum  

      != starttime and endtime
      

      != timearr and rangearr
      do irn = 1, rn
        timearr(irn) = 86400/tlen*(irn-1)
      enddo
      BL=15
      do ibn = 1, bn
        rangearr(ibn) = BL * (ibn - 1) + BL / 2. 
      enddo
 
      != moments data 
      scaleFactor = 0.01
      !offset = 0.0
      FillValue = -32768

      if ( PulseW .eq. 0.4 ) then 
        nyq_Velarr = 22.4
      else if ( PulseW .EQ. 0.2 ) then 
        nyq_Velarr = 22.4
      else if ( PulseW .EQ. 0.4 ) then 
        nyq_Velarr = 11.2
      end if


!= (Create the netCDF file) ==========================================!

      status=NF90_CREATE(outfn, NF90_CLOBBER, ncid)

!= (Define the dimensions) ===========================================!
	
      status=NF90_DEF_DIM(ncid, "time", rn, tID)				! time
      status=NF90_DEF_DIM(ncid, "range", bn, rID)				! range
      status=NF90_DEF_DIM(ncid, "sweep", NumVolLayer, sweepID)			! sweep
      status=NF90_DEF_DIM(ncid, "frequency", 1, freqID)				! frequency
      status=NF90_DEF_DIM(ncid, "string_length_4", 4, string_length_4ID)							! string_length_4
      status=NF90_DEF_DIM(ncid, "string_length_6", 6, string_length_6ID)							! string_length_6
      status=NF90_DEF_DIM(ncid, "string_length_10", 10, string_length_10ID)							! string_length_10
      status=NF90_DEF_DIM(ncid, "string_length_21", 21,  string_length_21ID)							! string_length_21
      status=NF90_DEF_DIM(ncid, "string_length_24", 24, string_length_24ID)							! string_length_24
      status=NF90_DEF_DIM(ncid, "string_length_34", 34, string_length_34ID)							! string_length_34

!= (Define variables) Global variable ================================!
 
      status=NF90_DEF_VAR(ncid, "volume_number", NF90_INT,volume_numberID)
      status=NF90_PUT_ATT(ncid, volume_numberID, "long_name",  "Volume number")
      status=NF90_PUT_ATT(ncid, volume_numberID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, volume_numberID, "standard_name", "data_volume_index_number")						! volume number 

      status=NF90_DEF_VAR(ncid, "time_coverage_start", NF90_CHAR, string_length_21ID, time_coverage_startID)
      status=NF90_PUT_ATT(ncid, time_coverage_startID, "long_name", "Time corresponding to first radial in file")
      status=NF90_PUT_ATT(ncid, time_coverage_startID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, time_coverage_startID, "standard_name", "data_volume_start_time_utc")						! time_coverage_start 

      status=NF90_DEF_VAR(ncid, "time_coverage_end", NF90_CHAR, string_length_21ID, time_coverage_endID)
      status=NF90_PUT_ATT(ncid, time_coverage_endID, "long_name", "Time corresponding to last radial in file")
      status=NF90_PUT_ATT(ncid, time_coverage_endID, "units","unitless")
      status=NF90_PUT_ATT(ncid, time_coverage_endID, "standard_name", "data_volume_end_time_utc")						! time_coverage_end

!= (Define variables) Coordinate variables ===========================!
  
      status=NF90_DEF_VAR(ncid, "time", NF90_DOUBLE, tID, timeID)
      status=NF90_PUT_ATT(ncid, timeID, "long_name",  "time in seconds since volume start")
      status=NF90_PUT_ATT(ncid, timeID, "units", "seconds since "//starttime)
      status=NF90_PUT_ATT(ncid, timeID, "standard_name", "time")
      status=NF90_PUT_ATT(ncid, timeID, "comment", "times are relative to the volume start time")				! time 

      status=NF90_DEF_VAR(ncid, "range", NF90_FLOAT, rID, rangeID)
      status=NF90_PUT_ATT(ncid, rangeID, "long_name", "Range to meausrement volume")
      status=NF90_PUT_ATT(ncid, rangeID, "units", "m")
      status=NF90_PUT_ATT(ncid, rangeID, "standard_name", "projection_range_coordinate")
      status=NF90_PUT_ATT(ncid, rangeID, "spacing_is_constant", "true")    
      status=NF90_PUT_ATT(ncid, rangeID, "meters_to_center_of_first_gate", rangearr(1)) 
      status=NF90_PUT_ATT(ncid, rangeID, "meters_to_between_gates", float(BinLen))  
      status=NF90_PUT_ATT(ncid, rangeID, "axis", "radial_range_coordinate")						! range

!= (Define variables) Location variables =============================!
  
      status=NF90_DEF_VAR(ncid, "latitude", NF90_DOUBLE, latitudeID)
      status=NF90_PUT_ATT(ncid, latitudeID, "long_name", "Latitude")
      status=NF90_PUT_ATT(ncid, latitudeID, "units", "degree_N")
      status=NF90_PUT_ATT(ncid, latitudeID, "standard_name", "latitude")	! latitude 

      status=NF90_DEF_VAR(ncid, "longitude", NF90_DOUBLE, longitudeID)
      status=NF90_PUT_ATT(ncid, longitudeID, "long_name", "Longitude")
      status=NF90_PUT_ATT(ncid, longitudeID, "units", "degree_E")
      status=NF90_PUT_ATT(ncid, longitudeID, "standard_name", "longitude")								! longitude 

      status=NF90_DEF_VAR(ncid, "altitude", NF90_DOUBLE, altitudeID)
      status=NF90_PUT_ATT(ncid, altitudeID, "long_name", "Altitude")
      status=NF90_PUT_ATT(ncid, altitudeID, "units", "m")
      status=NF90_PUT_ATT(ncid, altitudeID, "standard_name",  "altitude")								! altitude 

!= (Define variables) Sweep variables ================================!
  
      status=NF90_DEF_VAR(ncid, "sweep_number", NF90_INT, sweepID, sweep_numberID)
      status=NF90_PUT_ATT(ncid, sweep_numberID, "long_name", "Sweep number")
      status=NF90_PUT_ATT(ncid, sweep_numberID, "units", "count")
      status=NF90_PUT_ATT(ncid, sweep_numberID, "standard_name", "sweep_index_number_0_based")						 ! sweep_number 

      dimids=(/ string_length_24ID, sweepID /)
      status=NF90_DEF_VAR(ncid, "sweep_mode", NF90_CHAR, dimids,sweep_modeID)
       status=NF90_PUT_ATT(ncid, sweep_modeID, "long_name",  "Sweep mode")
      status=NF90_PUT_ATT(ncid, sweep_modeID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, sweep_modeID, "standard_name", "scan_mode_for_sweep")
      status=NF90_PUT_ATT(ncid, sweep_modeID, "comment", "possible values: PPI, sPPI, RHI, VOL, THI, sPPI(3D), sRHI(3D)")		 ! sweep_mode

      status=NF90_DEF_VAR(ncid, "fixed_angle", NF90_FLOAT, sweepID, fixed_angleID)
      status=NF90_PUT_ATT(ncid, fixed_angleID, "long_name", "Target angle for sweep")
      status=NF90_PUT_ATT(ncid, fixed_angleID, "units", "degree")
      status=NF90_PUT_ATT(ncid, fixed_angleID, "standard_name", "target_fixed_angle")						         ! fixed_angle 

!= (Define variables) Sensor pointing variables ======================!
  
      status=NF90_DEF_VAR(ncid, "azimuth", NF90_FLOAT, tID,azimuthID)
      status=NF90_PUT_ATT(ncid, azimuthID, "long_name", "Azimuth angle from true north")
      status=NF90_PUT_ATT(ncid, azimuthID, "units", "degree")
      status=NF90_PUT_ATT(ncid, azimuthID, "standard_name","beam_azimuth_angle")
      status=NF90_PUT_ATT(ncid, azimuthID, "axis", "radial_azimuth_coordinate")						 ! azimuth 

      status=NF90_DEF_VAR(ncid, "elevation", NF90_FLOAT, tID, elevationID)
      status=NF90_PUT_ATT(ncid, elevationID, "long_name", "Elevation angle from horizontal")
      status=NF90_PUT_ATT(ncid, elevationID, "units", "degree")
      status=NF90_PUT_ATT(ncid, elevationID, "standard_name","beam_elevation_angle")
      status=NF90_PUT_ATT(ncid, elevationID, "axis","radial_elevation_coordinate")						 ! elevation 

!= (Define variables) moments data ===================================!
  
      dimids=(/ rID, tID /)

      status=NF90_DEF_VAR(ncid, "reflectivity_h", NF90_SHORT, dimids, ref_hID)
      status=NF90_PUT_ATT(ncid, ref_hID, "long_name",  "Equivalent reflectivity factor ch. h")
      status=NF90_PUT_ATT(ncid, ref_hID, "units", "dBZ")
      status=NF90_PUT_ATT(ncid, ref_hID, "standard_name", "equivalent_reflectivity_factor ch. h" )
      status=NF90_PUT_ATT(ncid, ref_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, ref_hID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, ref_hID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, ref_hID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, ref_hID, "coordinates", "elevation azimuth range")					 	 ! reflectivity_h

      status=NF90_DEF_VAR(ncid, "reflectivity_v", NF90_SHORT, dimids, ref_vID)
      status=NF90_PUT_ATT(ncid, ref_vID, "long_name", "Equivalent reflectivity factor ch. v")
      status=NF90_PUT_ATT(ncid, ref_vID, "units", "dBZ")
      status=NF90_PUT_ATT(ncid, ref_vID, "standard_name", "equivalent_reflectivity_factor ch. v" )
      status=NF90_PUT_ATT(ncid, ref_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, ref_vID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, ref_vID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, ref_vID, "comment",  "Multiply the scale_factor, then add the add_offset")      
      status=NF90_PUT_ATT(ncid, ref_vID, "coordinates", "elevation azimuth range")					 	 ! reflectivity_v

      status=NF90_DEF_VAR(ncid, "mean_doppler_velocity_h", NF90_SHORT, dimids, vel_hID)
      status=NF90_PUT_ATT(ncid, vel_hID, "long_name",  "Mean Doppler velocity ch. h")
      status=NF90_PUT_ATT(ncid, vel_hID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, vel_hID, "standard_name",  "radial_velocity_of_scatterers_away_from_instrument")
      status=NF90_PUT_ATT(ncid, vel_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, vel_hID, "scale_factor", scaleFactor) 
      status=NF90_PUT_ATT(ncid, vel_hID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, vel_hID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, vel_hID, "coordinates", "elevation azimuth range")						 ! mean_doppler_velocity_h

      status=NF90_DEF_VAR(ncid, "mean_doppler_velocity_v", NF90_SHORT, dimids, vel_vID)
      status=NF90_PUT_ATT(ncid, vel_vID, "long_name",  "Mean Doppler velocity ch. v")
      status=NF90_PUT_ATT(ncid, vel_vID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, vel_vID, "standard_name",  "radial_velocity_of_scatterers_away_from_instrument")
      status=NF90_PUT_ATT(ncid, vel_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, vel_vID, "scale_factor", scaleFactor) 
      status=NF90_PUT_ATT(ncid, vel_vID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, vel_vID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, vel_vID, "coordinates", "elevation azimuth range")						 ! mean_doppler_velocity_v

      status=NF90_DEF_VAR(ncid, "spectral_width_h", NF90_SHORT,  dimids, sw_hID)
      status=NF90_PUT_ATT(ncid, sw_hID, "long_name", "Spectrum width ch. h")
      status=NF90_PUT_ATT(ncid, sw_hID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, sw_hID, "standard_name",  "doppler_spectrum_width ch.h")
      status=NF90_PUT_ATT(ncid, sw_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, sw_hID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, sw_hID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, sw_hID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, sw_hID, "coordinates", "elevation azimuth range")						 ! spectral_width_h

      status=NF90_DEF_VAR(ncid, "spectral_width_v", NF90_SHORT,  dimids, sw_vID)
      status=NF90_PUT_ATT(ncid, sw_vID, "long_name", "Spectrum width ch. v")
      status=NF90_PUT_ATT(ncid, sw_vID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, sw_vID, "standard_name", "doppler_spectrum_width ch.v")
      status=NF90_PUT_ATT(ncid, sw_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, sw_vID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, sw_vID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, sw_vID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, sw_vID, "coordinates", "elevation azimuth range")						 ! spectral_width_w

      status=NF90_DEF_VAR(ncid, "snr_h", NF90_SHORT, dimids, snr_hID)
      status=NF90_PUT_ATT(ncid, snr_hID, "long_name", "Signal-to-noise-ratio ch. h") 
      status=NF90_PUT_ATT(ncid, snr_hID, "units", "dB")
      status=NF90_PUT_ATT(ncid, snr_hID, "standard_name",  "signal_to_noise_ratio ch. h")
      status=NF90_PUT_ATT(ncid, snr_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, snr_hID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, snr_hID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, snr_hID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, snr_hID, "coordinates", "elevation azimuth range")						 ! snr_h

      status=NF90_DEF_VAR(ncid, "snr_v", NF90_SHORT, dimids, snr_vID)
      status=NF90_PUT_ATT(ncid, snr_vID, "long_name", "Signal-to-noise-ratio ch. v") 
      status=NF90_PUT_ATT(ncid, snr_vID, "units", "dB")
      status=NF90_PUT_ATT(ncid, snr_vID, "standard_name", "signal_to_noise_ratio ch. v")
      status=NF90_PUT_ATT(ncid, snr_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, snr_vID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, snr_vID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, snr_vID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, snr_vID, "coordinates", "elevation azimuth range")						 ! snr_v

      status=NF90_DEF_VAR(ncid, "linear_depolarization_ratio", NF90_SHORT, dimids, ldrID)
      status=NF90_PUT_ATT(ncid, ldrID, "long_name", "Linear depolarization ratio") 
      status=NF90_PUT_ATT(ncid, ldrID, "units", "dB")
      status=NF90_PUT_ATT(ncid, ldrID, "standard_name", "log_linear_depolarization_ratio")
      status=NF90_PUT_ATT(ncid, ldrID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, ldrID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, ldrID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, ldrID, "comment", "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, ldrID, "coordinates", "elevation azimuth range")						 ! linear_depolarization_ratio

!= (Define variables) Instrument_parameters ==========================!

      status=NF90_DEF_VAR(ncid, "frequency", NF90_FLOAT, freqID, frequencyID)  
      status=NF90_PUT_ATT(ncid, frequencyID, "long_name", "Operating frequency")
      status=NF90_PUT_ATT(ncid, frequencyID, "units", "Hz")
      status=NF90_PUT_ATT(ncid, frequencyID, "meta_group","instrument_parameters")
      status=NF90_PUT_ATT(ncid, frequencyID, "standard_name", "radiation_frequency")							! frequency

      status=NF90_DEF_VAR(ncid, "pulse_width", NF90_FLOAT, tID,  pulse_widthID)
      status=NF90_PUT_ATT(ncid, pulse_widthID, "long_name", "Pulse width")
      status=NF90_PUT_ATT(ncid, pulse_widthID, "units", "s") 
      status=NF90_PUT_ATT(ncid, pulse_widthID, "meta_group", "instrument_parameters")
      status=NF90_PUT_ATT(ncid, pulse_widthID, "standard_name","transmitter_pulse_width")						! pulse_width

      dimids=(/ string_length_6ID, sweepID /)
      status=NF90_DEF_VAR(ncid, "prt_mode", NF90_CHAR, dimids, prt_modeID)
      status=NF90_PUT_ATT(ncid, prt_modeID, "long_name","PRT mode")
      status=NF90_PUT_ATT(ncid, prt_modeID, "units","unitless")
      status=NF90_PUT_ATT(ncid, prt_modeID, "meta_group","instrument_parameters")
      status=NF90_PUT_ATT(ncid, prt_modeID, "options", "fixed, staggered, dual") 
      status=NF90_PUT_ATT(ncid, prt_modeID, "standard_name", "transmit_pulse_mode")							! pulse_mode

      status=NF90_DEF_VAR(ncid, "prt", NF90_FLOAT, tID, prtID) 
      status=NF90_PUT_ATT(ncid, prtID, "long_name", "Pulse repetition time")
      status=NF90_PUT_ATT(ncid, prtID, "units", "s")
      status=NF90_PUT_ATT(ncid, prtID, "meta_group", "instrument_parameters")							
      status=NF90_PUT_ATT(ncid, prtID, "standard_name", "pulse_repetition_time")							! prt				

      status=NF90_DEF_VAR(ncid, "prt_ratio", NF90_FLOAT, tID, prt_ratioID)								
      status=NF90_PUT_ATT(ncid, prt_ratioID, "long_name", "Pulse repetition time ratio")
      status=NF90_PUT_ATT(ncid, prt_ratioID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, prt_ratioID, "meta_group", "instrument_parameters")							
      status=NF90_PUT_ATT(ncid, prt_ratioID, "standard_name", "pulse_repetition_time_ratio")						! prt_ratio

      status=NF90_DEF_VAR(ncid, "nyquist_velocity", NF90_FLOAT, tID,  nyquist_velocityID)							
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "long_name",  "Unambiguous Doppler velocity")
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "meta_group", "instrument_parameters" )
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "standard_name",  "unambiguous_doppler_velocity")						! nyquist_velocity

      status=NF90_DEF_VAR(ncid, "n_samples", NF90_INT, tID,  n_samplesID)
      status=NF90_PUT_ATT(ncid, n_samplesID, "long_name", "Number of samples used to compute moments")
      status=NF90_PUT_ATT(ncid, n_samplesID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, n_samplesID, "meta_group", "instrument_parameters")
      status=NF90_PUT_ATT(ncid, n_samplesID, "standard_name", "number_of_samples_used_to_compute_moments")				! n_samples

!= (Define variables) Radar_parameters ===============================!

      status=NF90_DEF_VAR(ncid, "radar_antenna_gain_h", NF90_FLOAT,  radar_antenna_gain_hID)
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "long_name", "Nominal radar antenna gain, h channel")
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "units", "dB")
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "meta_group","radar_parameters")
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "standard_name", "nominal_radar_antenna_gain_h_channel")			! radar_antenna_gain_h

      status=NF90_DEF_VAR(ncid, "radar_beam_width_h", NF90_FLOAT,  radar_beam_width_hID)
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "long_name", "Radar beam width, horizontal channel")
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "units","degree")
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "meta_group", "radar_parameters")
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "standard_name", "half_power_radar_beam_width_h_channel")					! radar_beam_width_h

!= (Define global attributes) ========================================!

      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "Conventions", "CF/Radial instrument_parameters radar_parameters")			! Conventions
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "version", "1.0")			! version
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "title", "BS Ka-band cloud radar Moments")					! title
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "institution", "National Institute of Meteorological Sciences, NIMS")			! institution
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "references", "CfRadial Data File Format Document Version 1.3")			! references
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "source", "/data2/ncio/CLD_258/hmbprodat/")					! source
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "history",  "Created by J. Lee, Apr 1, 2016, and Modified by J. Song Mar 15,2017")	! history
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "comment", qctype)	! comment
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "instrument_name", RadarType)								! instrument_name

!= (End definitions) ==================================================!

      status=NF90_ENDDEF(ncid)

!= (Write variables) Global variable =================================!

      status=NF90_PUT_VAR(ncid, volume_numberID, 0)				! volume_number
      status=NF90_PUT_VAR(ncid, time_coverage_startID, starttime)		! time_coverage_start
      status=NF90_PUT_VAR(ncid, time_coverage_endID, endtime)			! time_coverage_end

!= (Write variables) Coordinate variables ============================!

      status=NF90_PUT_VAR(ncid, timeID, timearr)				! time
      status=NF90_PUT_VAR(ncid, rangeID, rangearr)				! range

!= (Write variables) Location variables ==============================!

      status=NF90_PUT_VAR(ncid, latitudeID, Lat)				! latitude
      status=NF90_PUT_VAR(ncid, longitudeID, Lon)				! longitude
      status=NF90_PUT_VAR(ncid, altitudeID, Alt)				! altitude

!= (Write variables) Sweep variables =================================!

      status=NF90_PUT_VAR(ncid, sweep_numberID, sweepNumArr)			! sweep_number
      status=NF90_PUT_VAR(ncid, sweep_modeID, 'Pointing')
      status=NF90_PUT_VAR(ncid, fixed_angleID, fixed_angleArr)			! fixed_angle0
      status=NF90_PUT_VAR(ncid, sweep_start_ray_indexID,sweepStartRayArr)	! sweep_start_ray_index
      status=NF90_PUT_VAR(ncid, sweep_end_ray_indexID,-999)       ! sweep_end_ray_index

!= (Write variables) Sensor pointing variables =======================!
    AZ=0;EL=90
      status=NF90_PUT_VAR(ncid, azimuthID, AZ)					! azimuth	
      status=NF90_PUT_VAR(ncid, elevationID, EL) 				! elevation

!= (Write variables) moments data ====================================!

      status=NF90_PUT_VAR(ncid, ref_hID, RefhW)		! reflectivity_h
      status=NF90_PUT_VAR(ncid, ref_vID, RefvW)		! reflectivity_v
      status=NF90_PUT_VAR(ncid, vel_hID, VelhW)		! mean_doppler_velocity_h
      status=NF90_PUT_VAR(ncid, vel_vID, VelvW)		! mean_doppler_velocity_v
      status=NF90_PUT_VAR(ncid, sw_hID, SpWhW)		! spectral_width_h
      status=NF90_PUT_VAR(ncid, sw_vID, SpWvW)		! spectral_width_v
      status=NF90_PUT_VAR(ncid, snr_hID, SNRhW)		! snr_h
      status=NF90_PUT_VAR(ncid, snr_vID, SNRvW)		! snr_v
      status=NF90_PUT_VAR(ncid, ldrID, LDRaW)		! ldr_h

!= (Write variables) Instrument_parameters ===========================!

      status=NF90_PUT_VAR(ncid, frequencyID, 33.44)			! frequency
      status=NF90_PUT_VAR(ncid, pulse_widthID, pulse_widtharr)			! pulse_width

      if ( FreqMode .EQ. 1 ) then 
	status=NF90_PUT_VAR(ncid, prt_modeID, "fixed") 	 
      else if ( FreqMode .EQ. 2 ) then
	status=NF90_PUT_VAR(ncid, prt_modeID, "dual") 	 
      end if									! prt_mode

      status=NF90_PUT_VAR(ncid, prtID, prt_arr)				! prt

      if ( FreqMode .EQ. 2 ) then 
	status=NF90_PUT_VAR(ncid, prt_ratioID, prt_ratioarr)			! prt_ratio
      end if

      status=NF90_PUT_VAR(ncid, nyquist_velocityID, nyq_Velarr)			! nyquist_velocity

      status=NF90_PUT_VAR(ncid, n_samplesID, n_samplesarr)			! n_samples

!= (Write variables) Radar_parameters ================================!
      status=NF90_PUT_VAR(ncid, radar_antenna_gain_hID, Ae)			! radar_antenna_gain_h 
      status=NF90_PUT_VAR(ncid, radar_beam_width_hID, HorBeamW)			! radar_beam_width_h 
!= (Close the netCDF file) ===========================================!

      status = NF90_CLOSE(ncid)
 
    

      DEALLOCATE(inthh) ; DEALLOCATE(intmin)
      DEALLOCATE(intss) ; DEALLOCATE(intiii)

      DEALLOCATE(timearr) ; DEALLOCATE(rangearr)
      VolLayerNum=1
      DEALLOCATE(sweepNumArr)
      DEALLOCATE(fixed_angleArr)
      DEALLOCATE(sweepStartRayArr)
      DEALLOCATE(sweepEndRayArr)

      DEALLOCATE(frequencyarr)
      DEALLOCATE(pulse_widtharr)
      DEALLOCATE(prt_arr)
      DEALLOCATE(prt_ratioarr)
      DEALLOCATE(nyq_Velarr)
      DEALLOCATE(n_samplesarr)
      END SUBROUTINE ncwrite