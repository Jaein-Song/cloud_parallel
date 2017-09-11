subroutine ncread
    use varmod
    use netcdf
!subroutine for netcdf file read.
!Made by J. Song, Mar. 24, 2017
!1.   read cfradial file
!1.1. read dimension
!1.2. Allocate data
!1.3. read Metadata (Head)
!1.4. read Data     (Block)
!3. Convert variables from char to int or int to real etc.
! FILE FLOW::
!   ND or NC > IND or INC *transpose, int to float
Iend=0
do filei=1,Nfile
    read(11,*) file_start,file_end
    read(11,'(a150)') Ifname
    
!1.   read unfiltered (D) cfradial file
    sts=NF90_OPEN(Ifname,nf90_nowrite,nid); CALL CE(sts,'"'//Ifname//'"'//'\n')
    !1.1. read dimension
    !timelength
    sts=NF90_INQ_DIMID(nid,'time',tdim)
    sts=NF90_INQUIRE_DIMENSION(nid,tdim,timdim,ftlen_NC)
    !binlength
    sts=NF90_INQ_DIMID(nid,'range',rdim)
    sts=NF90_INQUIRE_DIMENSION(nid,rdim,randim,binlen)
    !prflength
    sts=NF90_INQ_DIMID(nid,'frequency',fdim)
    sts=NF90_INQUIRE_DIMENSION(nid,fdim,frqdim,frqlen)
    binlen=1000
    !1.2. Allocate data
    if (filei.gt.1) DEALLOCATE(frequencyarrI,PRTI,pulse_widtharrI,prt_ratioarrI,nyq_VelarrI,n_samplesarrI,WavePRF)
    if (filei.eq.1) ALLOCATE(RefhI(ftlen,binlen),VelhI(ftlen,binlen),SpWhI(ftlen,binlen),SNRhI(ftlen,binlen))
    if (filei.eq.1) ALLOCATE(RefvI(ftlen,binlen),VelvI(ftlen,binlen),SpWvI(ftlen,binlen),SNRvI(ftlen,binlen),LDRaI(ftlen,binlen),WavePRF(ftlen_NC))
    ALLOCATE(frequencyarrI(frqlen),PRTI(ftlen_NC),pulse_widtharrI(ftlen_NC),prt_ratioarrI(ftlen_NC),nyq_VelarrI(ftlen_NC),n_samplesarrI(ftlen_NC))
    ALLOCATE(RefhN(binlen,ftlen_NC),VelhN(binlen,ftlen_NC),SpWhN(binlen,ftlen_NC),SNRhN(binlen,ftlen_NC))
    ALLOCATE(RefvN(binlen,ftlen_NC),VelvN(binlen,ftlen_NC),SpWvN(binlen,ftlen_NC),SNRvN(binlen,ftlen_NC),LDRaN(binlen,ftlen_NC))

    if (Ifname(1:1).ne.'/') then
        fflag_in=0
    elseif (Ifname(1:1).eq.'E') then
        fflag_in=0
    elseif (binlen.ne.1000) then
        fflag_in=2
    elseif (ftlen_NC.lt.500)then
        fflag_in=2
    else
        fflag_in=1
    endif
    if (fflag_in.ne.1) fflag=fflag_in
    if (fflag_in.eq.1) then
        !1.3. read Metadata (Head)
        sts=NF90_INQ_VARID(nid,'latitude', latitudeID)
        sts=NF90_INQ_VARID(nid,'longitude', longitudeID)
        sts=NF90_INQ_VARID(nid,'altitude', altitudeID)
        sts=NF90_GET_VAR(nid, latitudeID,Lat)
        sts=NF90_GET_VAR(nid, longitudeID,Lon)
        sts=NF90_GET_VAR(nid, altitudeID,Alt)
        sts=NF90_INQ_VARID(nid,'frequency', frequencyID)
        sts=NF90_GET_VAR(nid, frequencyID, frequencyarrI)
        sts=NF90_INQ_VARID(nid,'pulse_width', pulse_widthID)
        sts=NF90_GET_VAR(nid, pulse_widthID, pulse_widtharrI)
        sts=NF90_INQ_VARID(nid,'prt_mode', prt_modeID)
        sts=NF90_GET_VAR(nid, prt_modeID, prtmodeI)
        if (prtmodeI(1:1).eq.'d') then
            FreqMode=2
        else
            FreqMode=1
        endif
        sts=NF90_INQ_VARID(nid, 'prt', prtID)
        sts=NF90_GET_VAR(nid, prtID, PRTI)
        do ti=1,ftlen_NC
            WavePRF(ti)=1./PRTI(ti)
        enddo
        if ( FreqMode .EQ. 2 ) then 
            sts=NF90_INQ_VARID(nid, 'prt_ratio', prt_ratioID)
            sts=NF90_GET_VAR(nid, prt_ratioID, prt_ratioarrI)
        endif
        sts=NF90_INQ_VARID(nid,'nyquist_velocity', nyquist_velocityID)
        sts=NF90_GET_VAR(nid, nyquist_velocityID, nyq_VelarrI)
        sts=NF90_INQ_VARID(nid,'n_samplesID', n_samplesID)
        sts=NF90_GET_VAR(nid, n_samplesID, n_samplesarrI)
        sts=NF90_INQ_VARID(nid, 'radar_antenna_gain_h',radar_antenna_gain_hID)
        sts=NF90_GET_VAR(nid, radar_antenna_gain_hID, AeI)
        sts=NF90_INQ_VARID(nid, 'radar_beam_width_h',radar_beam_width_hID)
        sts=NF90_GET_VAR(nid, radar_beam_width_hID, HorBeamWI)

        !1.3. read Data     (Block)
        !basetime
        sts=NF90_INQ_VARID(nid,'time_coverage_start',tcsid); CALL CE(sts,'get time cverage start id failed')
        sts=NF90_GET_VAR(nid,tcsid,tcs); CALL CE(sts,'get time coverage start var failed')
        !Refh
        sts=NF90_INQ_VARID(nid,'reflectivity_h',Refhid); CALL CE(sts,'get reflectivity_h (D) id failed')
        sts=NF90_GET_VAR(nid,Refhid,RefhN); CALL CE(sts,'get reflectivity_h (D) var failed')
        !Refv
        sts=NF90_INQ_VARID(nid,'reflectivity_v',Refvid); CALL CE(sts,'get reflectivity_v (D) id failed')
        sts=NF90_GET_VAR(nid,Refvid,RefvN); CALL CE(sts,'get reflectivity_v (D) var failed')
        !Velh
        sts=NF90_INQ_VARID(nid,'mean_doppler_velocity_h',Velhid); CALL CE(sts,'get mean_doppler_velocity_h (D) id failed')
        sts=NF90_GET_VAR(nid,Velhid,VelhN); CALL CE(sts,'get mean_doppler_velocity_v (D) var failed')
        !Velv
        sts=NF90_INQ_VARID(nid,'mean_doppler_velocity_v',Velvid); CALL CE(sts,'get mean_doppler_velocity_v (D) id failed')
        sts=NF90_GET_VAR(nid,Velvid,VelvN); CALL CE(sts,'get mean_doppler_velocity_v (D) var failed')
        !SpWh
        sts=NF90_INQ_VARID(nid,'spectral_width_h',SpWhid); CALL CE(sts,'get spectral_width_h (D) id failed')
        sts=NF90_GET_VAR(nid,SpWhid,SpWhN); CALL CE(sts,'get spectral_width_h (D) var failed')
        !SpWv
        sts=NF90_INQ_VARID(nid,'spectral_width_v',SpWvid); CALL CE(sts,'get spectral_width_v (D) id failed')
        sts=NF90_GET_VAR(nid,SpWvid,SpWvN); CALL CE(sts,'get spectral_width_v (D) var failed')
        !SNRh
        sts=NF90_INQ_VARID(nid,'snr_h',SNRhid); CALL CE(sts,'get snr_h (D) id failed')
        sts=NF90_GET_VAR(nid,SNRhid,SNRhN); CALL CE(sts,'get snr_h (D) var failed')
        !SNRv
        sts=NF90_INQ_VARID(nid,'snr_v',SNRvid); CALL CE(sts,'get snr_v (D) id failed')
        sts=NF90_GET_VAR(nid,SNRvid,SNRvN); CALL CE(sts,'get snr_v (D) var failed')
        !LDR
        sts=NF90_INQ_VARID(nid,'linear_depolarization_ratio',LDRaid); CALL CE(sts,'get LDR (D) id failed')
        sts=NF90_GET_VAR(nid,LDRaid,LDRaN); CALL CE(sts,'get LDR (D) var failed')
        !Close NCfile
        sts=NF90_CLOSE(nid)

        tscl=1440/tlen
!3. Convert variables from char to int or int to real etc.
        nbin=file_end-file_start+1
        Iend=Iend+nbin

        do ti=1,nbin
            do hi=1,binlen
                RefhI(Iend+ti,hi)=RefhN(hi,file_start-1+ti)*Zsf+offset
                RefvI(Iend+ti,hi)=RefvN(hi,file_start-1+ti)*Zsf+offset
                VelhI(Iend+ti,hi)=VelhN(hi,file_start-1+ti)*Vsf+offset
                VelvI(Iend+ti,hi)=VelvN(hi,file_start-1+ti)*Vsf+offset
                SpWhI(Iend+ti,hi)=SpWhN(hi,file_start-1+ti)*Wsf+offset
                SpWvI(Iend+ti,hi)=SpWvN(hi,file_start-1+ti)*Wsf+offset
                SNRhI(Iend+ti,hi)=SNRhN(hi,file_start-1+ti)*Ssf+offset
                SNRvI(Iend+ti,hi)=SNRvN(hi,file_start-1+ti)*Ssf+offset
                LDRaI(Iend+ti,hi)=LDRaN(hi,file_start-1+ti)*Lsf+offset
            enddo
        enddo
        if (frqlen.eq.1) then
            PRFIN(1)=frequencyarrI(1)
        else
            PRFIN(1)=frequencyarrI(1)
            PRFIN(2)=frequencyarrI(2)
        endif
        !DEALLOCATE(frequencyarrI,PRTI,pulse_widtharrI,prt_ratioarrI,nyq_VelarrI,n_samplesarrI,WavePRF)
        DEALLOCATE(RefhN,VelhN,SpWhN,SNRhN)
        DEALLOCATE(RefvN,VelvN,SpWvN,SNRvN,LDRaN)
    endif
enddo

end subroutine ncread


subroutine CE(sts,msg)
use netcdf
implicit none
integer,intent(in) ::sts
character(*)::msg
if (sts.ne.nf90_noerr)then
    print*,msg,NF90_STRERROR(sts)
endif
endsubroutine

subroutine tims2i(tst,ye,mo,da,ho,mi,se)
implicit none
character(len=21),intent(in)  :: tst
integer          ,intent(out) ::ye,mo,da,ho,mi,se
read(tst(1:4),*) ye
read(tst(6:7),*) mo
read(tst(9:10),*) da
read(tst(12:13),*) ho
read(tst(15:16),*) mi
read(tst(18:19),*) se
endsubroutine
