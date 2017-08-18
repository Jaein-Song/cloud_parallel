!= Written by J. E. Lee, ORD, NIMS =========================================!

!= (SUBROUTINE) WRITE a CF-Radial ==========================================!

      SUBROUTINE write_cfradial
      USE netcdf
      USE mod_para

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
  
      INTEGER :: latitudeID, longitudeID, altitudeID
      
      INTEGER :: sweep_numberID, sweep_modeID, fixed_angleID
      INTEGER :: sweep_start_ray_indexID, sweep_end_ray_indexID

      INTEGER :: azimuthID, elevationID

      INTEGER :: ref_hID, ref_vID, vel_hID, vel_vID, sw_hID, sw_vID
      INTEGER :: snr_hID, snr_vID, ldrID

      INTEGER :: frequencyID, pulse_widthID, prt_modeID
      INTEGER :: prtID, prt_ratioID, nyquist_velocityID, n_samplesID
      
      INTEGER :: radar_antenna_gain_hID, radar_beam_width_hID

      INTEGER,DIMENSION(2) :: dimids

!= Parameter used to make variable =====================================!

      INTEGER :: NumVolLayer      

      INTEGER                      :: irn, ibn        
      CHARACTER(LEN=4),ALLOCATABLE :: yyyy(:)
      CHARACTER(LEN=2),ALLOCATABLE :: mm(:), dd(:), hh(:), min(:), ss(:)
      CHARACTER(LEN=3),ALLOCATABLE :: iii(:)
      INTEGER,ALLOCATABLE :: inthh(:), intmin(:), intss(:), intiii(:)
 
      CHARACTER(LEN=21) :: starttime
      CHARACTER(LEN=21) :: endtime

      REAL,ALLOCATABLE  :: timearr(:), rangearr(:)

      INTEGER             :: sweepIndex,maskid,echoid
      INTEGER,ALLOCATABLE :: sweepNumArr(:)
      REAL,ALLOCATABLE    :: fixed_angleArr(:)
      INTEGER,ALLOCATABLE :: sweepStartRayArr(:), sweepEndRayArr(:)  
      INTEGER :: sTimeIndex, eTimeIndex

      INTEGER(KIND=2) :: FillValue
      REAL            :: scaleFactor, offSet

      REAL,ALLOCATABLE :: frequencyarr(:), fulse_widtharr(:)
      REAL,ALLOCATABLE :: prt_ratioarr(:), nyq_Velarr(:)
      REAL,ALLOCATABLE :: n_samplesarr(:)
  
      ALLOCATE(yyyy(rn)) 
      ALLOCATE(mm(rn)) ; ALLOCATE(dd(rn))
      ALLOCATE(hh(rn)) ; ALLOCATE(min(rn))
      ALLOCATE(ss(rn)) ; ALLOCATE(iii(rn))

      ALLOCATE(inthh(rn)) ; ALLOCATE(intmin(rn))
      ALLOCATE(intss(rn)) ; ALLOCATE(intiii(rn))

      ALLOCATE(timearr(rn)) ; ALLOCATE(rangearr(bn))

      ALLOCATE(sweepNumArr(VolLayerNum))
      ALLOCATE(fixed_angleArr(VolLayerNum))
      ALLOCATE(sweepStartRayArr(VolLayerNum))
      ALLOCATE(sweepEndRayArr(VolLayerNum))

      ALLOCATE(frequencyarr(FreqMode))
      ALLOCATE(fulse_widtharr(rn))
      ALLOCATE(prt_ratioarr(rn))
      ALLOCATE(nyq_Velarr(rn))
      ALLOCATE(n_samplesarr(rn))

      ALLOCATE(Z1t(bn,rn))   ;    ALLOCATE(Z2t(bn,rn))
      ALLOCATE(Vr1t(bn,rn))  ;    ALLOCATE(Vr2t(bn,rn))
      ALLOCATE(SW1t(bn,rn))  ;    ALLOCATE(SW2t(bn,rn))
      ALLOCATE(SNR1t(bn,rn)) ;   ALLOCATE(SNR2t(bn,rn))
      ALLOCATE(LDRt(bn,rn))

!= (Make variable) ===================================================!
   
      != number of volume
      NumVolLayer = VolLayerNum  

      != starttime and endtime
      do irn = 1, rn
        yyyy(irn) = RecTime(irn)(1:4) 
        mm(irn) = RecTime(irn)(5:6)
	dd(irn) = RecTime(irn)(7:8)
	hh(irn) = RecTime(irn)(9:10)
	min(irn) = RecTime(irn)(11:12)
	ss(irn) = RecTime(irn)(13:14)
	iii(irn) = RecTime(irn)(15:17) 
      enddo

      starttime = yyyy(1)//'-'//mm(1)//'-'//dd(1)//
     & 'T'//hh(1)//':'//min(1)//':'//ss(1)//'Z'
      endtime = yyyy(RayNum)//'-'//mm(RayNum)//'-'//dd(RayNum)//
     & 'T'//hh(RayNum)//':'//min(RayNum)//':'//ss(RayNum)//'Z'

      != timearr and rangearr
      do irn = 1, rn
        read( hh(irn), '(I10)' ) inthh(irn)
        read( min(irn), '(I10)' ) intmin(irn)
        read( ss(irn), '(I10)' ) intss(irn)
        read( iii(irn), '(I10)' ) intiii(irn)
      enddo
      do irn = 1, rn
        timearr(irn) = inthh(irn) * 60 * 60 + intmin(irn) * 60 
     &			+ intss(irn) + intiii(irn) / 1000.  
      enddo
      
      timearr = timearr - timearr(1)

      do ibn = 1, bn
        rangearr(ibn) = BinLen * (ibn - 1) + BinLen / 2. 
      enddo

      != sweepNumArr, sweep start ray index, sweep end ray index, and fixed_angleArr
      do sweepIndex = 1, NumVolLayer
        sweepNumArr(sweepIndex) = sweepIndex - 1
      enddo

      sweepStartRayArr = -999
      sweepEndRayArr = -999

      sweepIndex = 1 
      do irn = 1, rn-1
!        print*, irn, rn, EL(irn), AZ(irn), VolLayNo(irn), sweepIndex
        if ( VolLayNo(irn) .EQ. sweepIndex .AND.
     &       VolLayNo(irn) .EQ. VolLayNo(irn+1) ) then 
	  sweepStartRayArr(sweepIndex) = irn - 1
	  sweepIndex = sweepIndex + 1
	endif
	sweepEndRayArr(sweepIndex-1) = irn - 1
      enddo
 
      do sweepIndex = 1, NumVolLayer
        irn = sweepStartRayArr(sweepIndex) +
     & (sweepEndRayArr(sweepIndex)-sweepStartRayArr(sweepIndex))/2
	if ( ScanMode .EQ. 2 .OR. ScanMode .EQ. 6 ) then
	  fixed_angleArr(sweepIndex) = Az(irn)
        else
          fixed_angleArr(sweepIndex) = El(irn)      
	endif
      enddo

!      print*, VolLayNo(1:300)
!      print*, El(1:300)
!      print*, AZ(1:300)
!      print*, fixed_angleArr

!      print*, 'sweep_start_ray_index: ', sweepStartRayArr
!      print*, 'sweep_end_ray_index: ', sweepEndRayArr

      != moments data 
      scaleFactor = 0.01
      offSet = 0.0
      FillValue = -32768

      != frequency arr, fulse_width arr, prt_ratio arr, and n_samples arr
      frequencyarr(1) = WavePRF(1)
      if ( FreqMode .NE. 1 ) then 
        do irn = 1, rn 
          if ( frequencyarr(1) .NE. WavePRF(irn) ) then 
            frequencyarr(2) = WavePRF(irn)  
	  endif
 	enddo
      endif

      fulse_widtharr = PulseW * 10E-6

      if ( FreqMode .EQ. 2 ) then 
        prt_ratioarr = frequencyarr(1) / frequencyarr(2)
      endif
      
      n_samplesarr = PulseNum

      if ( PulseW .EQ. 0.1 ) then 
        nyq_Velarr = 22.4
      else if ( PulseW .EQ. 0.2 ) then 
        nyq_Velarr = 22.4
      else if ( PulseW .EQ. 0.4 ) then 
        nyq_Velarr = 11.2
      end if

      ! = Azi and AZ correction (Period: 2013. 07. 28. ~ 2014. 04. 25.)
      if ( Azi .EQ. 217.182 ) then
        Azi = 127.879
        
	do irn = 1, rn 
          if ( AZ(irn) .LT. 90 ) then 
            AZ(irn) = AZ(irn) - 90 + 360   
	  else if ( AZ(irn) .GE. 90 ) then  
            AZ(irn) = AZ(irn) - 90 
	  end if  
 	enddo
      endif 
!= (Create the netCDF file) ==========================================!

      status=NF90_CREATE(outfn, NF90_CLOBBER, ncid)

!= (Define the dimensions) ===========================================!
	
      status=NF90_DEF_DIM(ncid, "time", rn, tID)				! time
      status=NF90_DEF_DIM(ncid, "range", bn, rID)				! range
      status=NF90_DEF_DIM(ncid, "sweep", NumVolLayer, sweepID)			! sweep
      status=NF90_DEF_DIM(ncid, "frequency", 2, freqID)				! frequency
      status=NF90_DEF_DIM(ncid, "string_length_4", 4, 
     & string_length_4ID)							! string_length_4
      status=NF90_DEF_DIM(ncid, "string_length_6", 6, 
     & string_length_6ID)							! string_length_6
      status=NF90_DEF_DIM(ncid, "string_length_10", 10, 
     & string_length_10ID)							! string_length_10
      status=NF90_DEF_DIM(ncid, "string_length_21", 21, 
     & string_length_21ID)							! string_length_21
      status=NF90_DEF_DIM(ncid, "string_length_24", 24, 
     & string_length_24ID)							! string_length_24
      status=NF90_DEF_DIM(ncid, "string_length_34", 34, 
     & string_length_34ID)							! string_length_34

!= (Define variables) Global variable ================================!
 
      status=NF90_DEF_VAR(ncid, "volume_number", NF90_INT,
     & volume_numberID)
      status=NF90_PUT_ATT(ncid, volume_numberID, "long_name", 
     & "Volume number")
      status=NF90_PUT_ATT(ncid, volume_numberID, "units", 
     & "unitless")
      status=NF90_PUT_ATT(ncid, volume_numberID, "standard_name", 
     & "data_volume_index_number")						! volume number 

      status=NF90_DEF_VAR(ncid, "time_coverage_start", NF90_CHAR,
     & string_length_21ID, time_coverage_startID)
      status=NF90_PUT_ATT(ncid, time_coverage_startID, "long_name", 
     & "Time corresponding to first radial in file")
      status=NF90_PUT_ATT(ncid, time_coverage_startID, "units", 
     & "unitless")
      status=NF90_PUT_ATT(ncid, time_coverage_startID, "standard_name", 
     & "data_volume_start_time_utc")						! time_coverage_start 

      status=NF90_DEF_VAR(ncid, "time_coverage_end", NF90_CHAR,
     & string_length_21ID, time_coverage_endID)
      status=NF90_PUT_ATT(ncid, time_coverage_endID, "long_name", 
     & "Time corresponding to last radial in file")
      status=NF90_PUT_ATT(ncid, time_coverage_endID, "units", 
     & "unitless")
      status=NF90_PUT_ATT(ncid, time_coverage_endID, "standard_name", 
     & "data_volume_end_time_utc")						! time_coverage_end

!= (Define variables) Coordinate variables ===========================!
  
      status=NF90_DEF_VAR(ncid, "time", NF90_DOUBLE, tID, timeID)
      status=NF90_PUT_ATT(ncid, timeID, "long_name", 
     & "time in seconds since volume start")
      status=NF90_PUT_ATT(ncid, timeID, "units", 
     & "seconds since "//starttime)
      status=NF90_PUT_ATT(ncid, timeID, "standard_name", "time")
      status=NF90_PUT_ATT(ncid, timeID, "comment", 
     & "times are relative to the volume start time")				! time 

      status=NF90_DEF_VAR(ncid, "range", NF90_FLOAT, rID, rangeID)
      status=NF90_PUT_ATT(ncid, rangeID, "long_name", 
     & "Range to meausrement volume")
      status=NF90_PUT_ATT(ncid, rangeID, "units", "m")
      status=NF90_PUT_ATT(ncid, rangeID, "standard_name", 
     & "projection_range_coordinate")
      status=NF90_PUT_ATT(ncid, rangeID, "spacing_is_constant", "true")    
      status=NF90_PUT_ATT(ncid, rangeID, 
     & "meters_to_center_of_first_gate", rangearr(1)) 
      status=NF90_PUT_ATT(ncid, rangeID, "meters_to_between_gates", 
     & float(BinLen))  
      status=NF90_PUT_ATT(ncid, rangeID, "axis",
     & "radial_range_coordinate")						! range

!= (Define variables) Location variables =============================!
  
      status=NF90_DEF_VAR(ncid, "latitude", NF90_DOUBLE, latitudeID)
      status=NF90_PUT_ATT(ncid, latitudeID, "long_name", "Latitude")
      status=NF90_PUT_ATT(ncid, latitudeID, "units", "degree_N")
      status=NF90_PUT_ATT(ncid, latitudeID, "standard_name", "latitude")	! latitude 

      status=NF90_DEF_VAR(ncid, "longitude", NF90_DOUBLE, longitudeID)
      status=NF90_PUT_ATT(ncid, longitudeID, "long_name", "Longitude")
      status=NF90_PUT_ATT(ncid, longitudeID, "units", "degree_E")
      status=NF90_PUT_ATT(ncid, longitudeID, "standard_name",
     & "longitude")								! longitude 

      status=NF90_DEF_VAR(ncid, "altitude", NF90_DOUBLE, altitudeID)
      status=NF90_PUT_ATT(ncid, altitudeID, "long_name", "Altitude")
      status=NF90_PUT_ATT(ncid, altitudeID, "units", "m")
      status=NF90_PUT_ATT(ncid, altitudeID, "standard_name", 
     & "altitude")								! altitude 

!= (Define variables) Sweep variables ================================!
  
      status=NF90_DEF_VAR(ncid, "sweep_number", NF90_INT, sweepID, 
     & sweep_numberID)
      status=NF90_PUT_ATT(ncid, sweep_numberID, "long_name", 
     & "Sweep number")
      status=NF90_PUT_ATT(ncid, sweep_numberID, "units", "count")
      status=NF90_PUT_ATT(ncid, sweep_numberID, "standard_name", 
     & "sweep_index_number_0_based")						 ! sweep_number 

      dimids=(/ string_length_24ID, sweepID /)
      status=NF90_DEF_VAR(ncid, "sweep_mode", NF90_CHAR, dimids, 
     & sweep_modeID)
       status=NF90_PUT_ATT(ncid, sweep_modeID, "long_name", 
     & "Sweep mode")
      status=NF90_PUT_ATT(ncid, sweep_modeID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, sweep_modeID, "standard_name", 
     & "scan_mode_for_sweep")
      status=NF90_PUT_ATT(ncid, sweep_modeID, "comment", 
     & "possible values: PPI, sPPI, RHI, VOL, THI, sPPI(3D), sRHI(3D)")		 ! sweep_mode

      status=NF90_DEF_VAR(ncid, "fixed_angle", NF90_FLOAT, sweepID, 
     & fixed_angleID)
       status=NF90_PUT_ATT(ncid, fixed_angleID, "long_name", 
     & "Target angle for sweep")
      status=NF90_PUT_ATT(ncid, fixed_angleID, "units", "degree")
      status=NF90_PUT_ATT(ncid, fixed_angleID, "standard_name", 
     & "target_fixed_angle")						         ! fixed_angle 

      status=NF90_DEF_VAR(ncid, "sweep_start_ray_index", NF90_INT, 
     & sweepID, sweep_start_ray_indexID)
      status=NF90_PUT_ATT(ncid, sweep_start_ray_indexID, "long_name", 
     & "Index of first ray in sweep, 0-based")
      status=NF90_PUT_ATT(ncid, sweep_start_ray_indexID, "units", 
     & "count")
      status=NF90_PUT_ATT(ncid, sweep_start_ray_indexID, 
     & "standard_name", "index_of_first_ray_in_sweep")				 ! sweep_start_ray_index 

      status=NF90_DEF_VAR(ncid, "sweep_end_ray_index", NF90_INT, 
     & sweepID, sweep_end_ray_indexID)
      status=NF90_PUT_ATT(ncid, sweep_end_ray_indexID, "long_name", 
     & "Index of last ray in sweep, 0-based")
      status=NF90_PUT_ATT(ncid, sweep_end_ray_indexID, "units", 
     & "count")
      status=NF90_PUT_ATT(ncid, sweep_end_ray_indexID, 
     & "standard_name", "index_of_last_ray_in_sweep")				 ! sweep_end_ray_index 

!= (Define variables) Sensor pointing variables ======================!
  
      status=NF90_DEF_VAR(ncid, "azimuth", NF90_FLOAT, tID,
     & azimuthID)
      status=NF90_PUT_ATT(ncid, azimuthID, "long_name", 
     & "Azimuth angle from true north")
      status=NF90_PUT_ATT(ncid, azimuthID, "units", "degree")
      status=NF90_PUT_ATT(ncid, azimuthID, "standard_name",
     & "beam_azimuth_angle")
      status=NF90_PUT_ATT(ncid, azimuthID, "axis",
     & "radial_azimuth_coordinate")						 ! azimuth 

      status=NF90_DEF_VAR(ncid, "elevation", NF90_FLOAT, tID,
     & elevationID)
      status=NF90_PUT_ATT(ncid, elevationID, "long_name", 
     & "Elevation angle from horizontal")
      status=NF90_PUT_ATT(ncid, elevationID, "units", "degree")
      status=NF90_PUT_ATT(ncid, elevationID, "standard_name",
     & "beam_elevation_angle")
      status=NF90_PUT_ATT(ncid, elevationID, "axis",
     & "radial_elevation_coordinate")						 ! elevation 

!= (Define variables) moments data ===================================!
  
      dimids=(/ rID, tID /)

      status=NF90_DEF_VAR(ncid, "reflectivity_h", NF90_SHORT, dimids,
     & ref_hID)
      status=NF90_PUT_ATT(ncid, ref_hID, "long_name", 
     & "Equivalent reflectivity factor ch. h")
      status=NF90_PUT_ATT(ncid, ref_hID, "units", "dBZ")
      status=NF90_PUT_ATT(ncid, ref_hID, "standard_name", 
     & "equivalent_reflectivity_factor ch. h" )
      status=NF90_PUT_ATT(ncid, ref_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, ref_hID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, ref_hID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, ref_hID, "comment", 
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, ref_hID, "coordinates",
     & "elevation azimuth range")					 	 ! reflectivity_h
      status=NF90_DEF_VAR(ncid,'mask',NF90_SHORT,dimids,maskid)
      status=NF90_PUT_ATT(ncid, maskid, "_FillValue", FillValue)
      status=NF90_DEF_VAR(ncid,'echo',NF90_SHORT,dimids,echoid)
      status=NF90_PUT_ATT(ncid, echoid, "_FillValue", FillValue)

      status=NF90_DEF_VAR(ncid, "reflectivity_v", NF90_SHORT, dimids,
     & ref_vID)
      status=NF90_PUT_ATT(ncid, ref_vID, "long_name", 
     & "Equivalent reflectivity factor ch. v")
      status=NF90_PUT_ATT(ncid, ref_vID, "units", "dBZ")
      status=NF90_PUT_ATT(ncid, ref_vID, "standard_name", 
     & "equivalent_reflectivity_factor ch. v" )
      status=NF90_PUT_ATT(ncid, ref_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, ref_vID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, ref_vID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, ref_vID, "comment", 
     & "Multiply the scale_factor, then add the add_offset")      
      status=NF90_PUT_ATT(ncid, ref_vID, "coordinates",
     & "elevation azimuth range")					 	 ! reflectivity_v

      status=NF90_DEF_VAR(ncid, "mean_doppler_velocity_h", NF90_SHORT,
     & dimids, vel_hID)
      status=NF90_PUT_ATT(ncid, vel_hID, "long_name", 
     & "Mean Doppler velocity ch. h")
      status=NF90_PUT_ATT(ncid, vel_hID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, vel_hID, "standard_name", 
     & "radial_velocity_of_scatterers_away_from_instrument")
      status=NF90_PUT_ATT(ncid, vel_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, vel_hID, "scale_factor", scaleFactor) 
      status=NF90_PUT_ATT(ncid, vel_hID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, vel_hID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, vel_hID, "coordinates",
     & "elevation azimuth range")						 ! mean_doppler_velocity_h

      status=NF90_DEF_VAR(ncid, "mean_doppler_velocity_v", NF90_SHORT,
     & dimids, vel_vID)
      status=NF90_PUT_ATT(ncid, vel_vID, "long_name", 
     & "Mean Doppler velocity ch. v")
      status=NF90_PUT_ATT(ncid, vel_vID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, vel_vID, "standard_name", 
     & "radial_velocity_of_scatterers_away_from_instrument")
      status=NF90_PUT_ATT(ncid, vel_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, vel_vID, "scale_factor", scaleFactor) 
      status=NF90_PUT_ATT(ncid, vel_vID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, vel_vID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, vel_vID, "coordinates",
     & "elevation azimuth range")						 ! mean_doppler_velocity_v

      status=NF90_DEF_VAR(ncid, "spectral_width_h", NF90_SHORT, 
     & dimids, sw_hID)
      status=NF90_PUT_ATT(ncid, sw_hID, "long_name",
     & "Spectrum width ch. h")
      status=NF90_PUT_ATT(ncid, sw_hID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, sw_hID, "standard_name", 
     & "doppler_spectrum_width ch.h")
      status=NF90_PUT_ATT(ncid, sw_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, sw_hID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, sw_hID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, sw_hID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, sw_hID, "coordinates",
     & "elevation azimuth range")						 ! spectral_width_h

      status=NF90_DEF_VAR(ncid, "spectral_width_v", NF90_SHORT, 
     & dimids, sw_vID)
      status=NF90_PUT_ATT(ncid, sw_vID, "long_name",
     & "Spectrum width ch. v")
      status=NF90_PUT_ATT(ncid, sw_vID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, sw_vID, "standard_name", 
     & "doppler_spectrum_width ch.v")
      status=NF90_PUT_ATT(ncid, sw_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, sw_vID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, sw_vID, "add_offset", offSet) 
      status=NF90_PUT_ATT(ncid, sw_vID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, sw_vID, "coordinates",
     & "elevation azimuth range")						 ! spectral_width_w

      status=NF90_DEF_VAR(ncid, "snr_h", NF90_SHORT, dimids, snr_hID)
      status=NF90_PUT_ATT(ncid, snr_hID, "long_name",
     & "Signal-to-noise-ratio ch. h") 
      status=NF90_PUT_ATT(ncid, snr_hID, "units", "dB")
      status=NF90_PUT_ATT(ncid, snr_hID, "standard_name", 
     & "signal_to_noise_ratio ch. h")
      status=NF90_PUT_ATT(ncid, snr_hID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, snr_hID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, snr_hID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, snr_hID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, snr_hID, "coordinates",
     & "elevation azimuth range")						 ! snr_h

      status=NF90_DEF_VAR(ncid, "snr_v", NF90_SHORT, dimids, snr_vID)
      status=NF90_PUT_ATT(ncid, snr_vID, "long_name",
     & "Signal-to-noise-ratio ch. v") 
      status=NF90_PUT_ATT(ncid, snr_vID, "units", "dB")
      status=NF90_PUT_ATT(ncid, snr_vID, "standard_name", 
     & "signal_to_noise_ratio ch. v")
      status=NF90_PUT_ATT(ncid, snr_vID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, snr_vID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, snr_vID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, snr_vID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, snr_vID, "coordinates",
     & "elevation azimuth range")						 ! snr_v

      status=NF90_DEF_VAR(ncid, "linear_depolarization_ratio",
     & NF90_SHORT, dimids, ldrID)
      status=NF90_PUT_ATT(ncid, ldrID, "long_name",
     & "Linear depolarization ratio") 
      status=NF90_PUT_ATT(ncid, ldrID, "units", "dB")
      status=NF90_PUT_ATT(ncid, ldrID, "standard_name", 
     & "log_linear_depolarization_ratio")
      status=NF90_PUT_ATT(ncid, ldrID, "_FillValue", FillValue)
      status=NF90_PUT_ATT(ncid, ldrID, "scale_factor", scaleFactor)
      status=NF90_PUT_ATT(ncid, ldrID, "add_offset", offSet)
      status=NF90_PUT_ATT(ncid, ldrID, "comment",
     & "Multiply the scale_factor, then add the add_offset")
      status=NF90_PUT_ATT(ncid, ldrID, "coordinates",
     & "elevation azimuth range")						 ! linear_depolarization_ratio

!= (Define variables) Instrument_parameters ==========================!

      status=NF90_DEF_VAR(ncid, "frequency", NF90_FLOAT, freqID,
     & frequencyID)  
      status=NF90_PUT_ATT(ncid, frequencyID, "long_name", 
     & "Operating frequency")
      status=NF90_PUT_ATT(ncid, frequencyID, "units", "Hz")
      status=NF90_PUT_ATT(ncid, frequencyID, "meta_group",
     & "instrument_parameters")
      status=NF90_PUT_ATT(ncid, frequencyID, "standard_name",
     & "radiation_frequency")							! frequency

      status=NF90_DEF_VAR(ncid, "pulse_width", NF90_FLOAT, tID, 
     & pulse_widthID)
      status=NF90_PUT_ATT(ncid, pulse_widthID, "long_name",
     & "Pulse width")
      status=NF90_PUT_ATT(ncid, pulse_widthID, "units", "s") 
      status=NF90_PUT_ATT(ncid, pulse_widthID, "meta_group",
     & "instrument_parameters")
      status=NF90_PUT_ATT(ncid, pulse_widthID, "standard_name",
     & "transmitter_pulse_width")						! pulse_width

      dimids=(/ string_length_6ID, sweepID /)
      status=NF90_DEF_VAR(ncid, "prt_mode", NF90_CHAR,
     & dimids, prt_modeID)
      status=NF90_PUT_ATT(ncid, prt_modeID, "long_name",
     & "PRT mode")
      status=NF90_PUT_ATT(ncid, prt_modeID, "units",
     & "unitless")
      status=NF90_PUT_ATT(ncid, prt_modeID, "meta_group",
     & "instrument_parameters")
      status=NF90_PUT_ATT(ncid, prt_modeID, "options",
     & "fixed, staggered, dual") 
      status=NF90_PUT_ATT(ncid, prt_modeID, "standard_name",
     & "transmit_pulse_mode")							! pulse_mode

      status=NF90_DEF_VAR(ncid, "prt", NF90_FLOAT, tID, prtID) 
      status=NF90_PUT_ATT(ncid, prtID, "long_name",
     & "Pulse repetition time")
      status=NF90_PUT_ATT(ncid, prtID, "units", "s")
      status=NF90_PUT_ATT(ncid, prtID, "meta_group",
     & "instrument_parameters")							
      status=NF90_PUT_ATT(ncid, prtID, "standard_name",
     & "pulse_repetition_time")							! prt				

      status=NF90_DEF_VAR(ncid, "prt_ratio", NF90_FLOAT, tID,
     & prt_ratioID)								
      status=NF90_PUT_ATT(ncid, prt_ratioID, "long_name",
     & "Pulse repetition time ratio")
      status=NF90_PUT_ATT(ncid, prt_ratioID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, prt_ratioID, "meta_group",
     & "instrument_parameters")							
      status=NF90_PUT_ATT(ncid, prt_ratioID, "standard_name",
     & "pulse_repetition_time_ratio")						! prt_ratio

      status=NF90_DEF_VAR(ncid, "nyquist_velocity", NF90_FLOAT, tID, 
     & nyquist_velocityID)							
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "long_name", 
     & "Unambiguous Doppler velocity")
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "units", "m/s")
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "meta_group", 
     & "instrument_parameters" )
      status=NF90_PUT_ATT(ncid, nyquist_velocityID, "standard_name", 
     & "unambiguous_doppler_velocity")						! nyquist_velocity

      status=NF90_DEF_VAR(ncid, "n_samples", NF90_INT, tID, 
     & n_samplesID)
      status=NF90_PUT_ATT(ncid, n_samplesID, "long_name",
     & "Number of samples used to compute moments")
      status=NF90_PUT_ATT(ncid, n_samplesID, "units", "unitless")
      status=NF90_PUT_ATT(ncid, n_samplesID, "meta_group",
     & "instrument_parameters")
      status=NF90_PUT_ATT(ncid, n_samplesID, "standard_name",
     & "number_of_samples_used_to_compute_moments")				! n_samples

!= (Define variables) Radar_parameters ===============================!

      status=NF90_DEF_VAR(ncid, "radar_antenna_gain_h", NF90_FLOAT, 
     & radar_antenna_gain_hID)
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "long_name",
     & "Nominal radar antenna gain, h channel")
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "units", "dB")
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID, "meta_group",
     & "radar_parameters")
      status=NF90_PUT_ATT(ncid, radar_antenna_gain_hID,
     & "standard_name", "nominal_radar_antenna_gain_h_channel")			! radar_antenna_gain_h

      status=NF90_DEF_VAR(ncid, "radar_beam_width_h", NF90_FLOAT, 
     & radar_beam_width_hID)
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "long_name",
     & "Radar beam width, horizontal channel")
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "units",
     & "degree")
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "meta_group",
     & "radar_parameters")
      status=NF90_PUT_ATT(ncid, radar_beam_width_hID, "standard_name",
     & "half_power_radar_beam_width_h_channel")					! radar_beam_width_h
!= (Define global attributes) ========================================!

      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "Conventions",
     & "CF/Radial instrument_parameters radar_parameters")			! Conventions
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "version", "1.0")			! version
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "title",
     & "BS Ka-band cloud radar Moments")					! title
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "institution",
     & "National Institute of Meteorological Sciences, NIMS")			! institution
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "references",
     & "CfRadial Data File Format Document Version 1.3")			! references
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "source",
     & "/data2/ncio/CLD_258/hmbprodat/")					! source
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "history", 
     & "Created by J. Lee, Apr 1, 2016, and Modified by J. Song Mar 15,
     &2017")	! history
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "comment", "None")			! comment
      status=NF90_PUT_ATT(ncid, NF90_GLOBAL, "instrument_name",		
     & RadarType)								! instrument_name

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

      if ( ScanMode .EQ. 0 ) then 
        status=NF90_PUT_VAR(ncid, sweep_modeID, 'PPI')			
      elseif ( ScanMode .EQ. 1 ) then 
        status=NF90_PUT_VAR(ncid, sweep_modeID, 'sPPI')		
      elseif ( ScanMode .EQ. 2 ) then 
	status=NF90_PUT_VAR(ncid, sweep_modeID, 'RHI')		
      elseif ( ScanMode .EQ. 3 ) then
        status=NF90_PUT_VAR(ncid, sweep_modeID, 'VOL')	
      elseif ( ScanMode .EQ. 4 ) then 
        status=NF90_PUT_VAR(ncid, sweep_modeID, 'Pointing')
      elseif ( ScanMode .EQ. 5 ) then 
        status=NF90_PUT_VAR(ncid, sweep_modeID, 'sPPI(3D)' )
      elseif ( ScanMode .EQ. 6 ) then 
        status=NF90_PUT_VAR(ncid, sweep_modeID, 'sRHI(3D)')
      end if									! sweep_mode
      
      status=NF90_PUT_VAR(ncid, fixed_angleID, fixed_angleArr)			! fixed_angle

      status=NF90_PUT_VAR(ncid, sweep_start_ray_indexID,
     & sweepStartRayArr)							! sweep_start_ray_index
      status=NF90_PUT_VAR(ncid, sweep_end_ray_indexID,
     & sweepEndRayArr)								! sweep_end_ray_index

!= (Write variables) Sensor pointing variables =======================!

      status=NF90_PUT_VAR(ncid, azimuthID, AZ)					! azimuth	
      status=NF90_PUT_VAR(ncid, elevationID, EL) 				! elevation

!= (Write variables) moments data ====================================!
      Z1t=TRANSPOSE(Z1)      ;      Z2t=TRANSPOSE(Z2)
      Vr1t=TRANSPOSE(Vr1)    ;      Vr2t=TRANSPOSE(Vr2)
      SW1t=TRANSPOSE(SW1)    ;      SW2t=TRANSPOSE(SW2)
      SNR1t=TRANSPOSE(SNR1)  ;      SNR2t=TRANSPOSE(SNR2)
      LDRt=TRANSPOSE(LDR)

      do irn=1,rn 
        do ibn=1,bn
          if ( Z1t(ibn,irn) .EQ. fillValue ) then 
 	    Z1t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( Z2t(ibn,irn) .EQ. fillValue ) then 
 	    Z2t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( Vr1t(ibn,irn) .EQ. fillValue ) then 
 	    Vr1t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( Vr2t(ibn,irn) .EQ. fillValue ) then 
 	    Vr2t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( SW1t(ibn,irn) .EQ. fillValue ) then 
 	    SW1t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( SW2t(ibn,irn) .EQ. fillValue ) then 
 	    SW2t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( SNR1t(ibn,irn) .EQ. fillValue ) then 
 	    SNR1t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( SNR2t(ibn,irn) .EQ. fillValue ) then 
 	    SNR2t(ibn,irn)=fillValue*scaleFactor    
	  endif
	  if ( LDRt(ibn,irn) .EQ. fillValue ) then 
 	    LDRt(ibn,irn)=fillValue*scaleFactor    
	  endif
	enddo
      enddo
      status=NF90_PUT_VAR(ncid, ref_hID, Z1t/scaleFactor)		! reflectivity_h
      status=NF90_PUT_VAR(ncid, ref_vID, Z2t/scaleFactor)		! reflectivity_v
      status=NF90_PUT_VAR(ncid, vel_hID, Vr1t/scaleFactor)		! mean_doppler_velocity_h
      status=NF90_PUT_VAR(ncid, vel_vID, Vr2t/scaleFactor)		! mean_doppler_velocity_v
      status=NF90_PUT_VAR(ncid, sw_hID, SW1t/scaleFactor)		! spectral_width_h
      status=NF90_PUT_VAR(ncid, sw_vID, SW2t/scaleFactor)		! spectral_width_v
      status=NF90_PUT_VAR(ncid, snr_hID, SNR1t/scaleFactor)		! snr_h
      status=NF90_PUT_VAR(ncid, snr_vID, SNR2t/scaleFactor)		! snr_v
      status=NF90_PUT_VAR(ncid, ldrID, LDRt/scaleFactor)		! ldr_h
      if (qcstat.eq.15) then
      status=NF90_PUT_VAR(ncid, maskid, transpose(mask))		! ldr_h
      status=NF90_PUT_VAR(ncid, echoid, transpose(echo))		! ldr_h
      endif
!= (Write variables) Instrument_parameters ===========================!

      status=NF90_PUT_VAR(ncid, frequencyID, frequencyarr)			! frequency
      status=NF90_PUT_VAR(ncid, pulse_widthID, fulse_widtharr)			! pulse_width

      if ( FreqMode .EQ. 1 ) then 
	status=NF90_PUT_VAR(ncid, prt_modeID, "fixed") 	 
      else if ( FreqMode .EQ. 2 ) then
	status=NF90_PUT_VAR(ncid, prt_modeID, "dual") 	 
      end if									! prt_mode

      status=NF90_PUT_VAR(ncid, prtID, 1/WavePRF)				! prt

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
      deallocate(yyyy) 
      deallocate(mm) ; deallocate(dd)
      deallocate(hh) ; deallocate(min)
      deallocate(ss) ; deallocate(iii)

      deallocate(inthh) ; deallocate(intmin)
      deallocate(intss) ; deallocate(intiii)

      !deallocate(timearr(rn)) ; deallocate(rangearr(bn))
      deallocate(timearr) ; deallocate(rangearr)

      deallocate(sweepNumArr) 
      deallocate(sweepStartRayArr) ; deallocate(sweepEndRayArr)

      deallocate(frequencyarr)
      deallocate(fulse_widtharr)
      deallocate(prt_ratioarr)
      deallocate(nyq_Velarr)
      deallocate(n_samplesarr)
  
      deallocate(Z1t)
      deallocate(Z2t)
      deallocate(Vr1t)
      deallocate(Vr2t)
      deallocate(SW1t)
      deallocate(SW2t)
      deallocate(SNR1t)   
      deallocate(SNR2t)
      deallocate(LDRt)
      if (qcstat.eq.15) deallocate(echo,mask)

      END SUBROUTINE write_cfradial
