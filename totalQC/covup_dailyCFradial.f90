PROGRAM covup_dailyCFradial
!This program merges REGULAR THI scan data files (2min int) obtained in one day 
!and this fortran 90 file is the main body of the program
!Made by J. Song, Mar 21, 2017
!The flow of this program:
!1. call Modules: Variables, NETCDF
!2.

!1. Call Modules
USE varmod
USE NETCDF
use inputlists
IMPLICIT NONE

!2. Open Input File List
open(11,file=totifl1,status='old')
read(11,*) qcstat
read(11,'(a150)') outfn
read(11,*) tlen

if (qcstat.eq.172) then
    open(33,file=totcfl1,status='old')
    read(33,'(a150)'),cfname
    close(unit=33, status="keep")
    print*,cfname
    sts=NF90_OPEN(cfname,nf90_nowrite,nid_ceil); CALL CE(sts,'"'//Ifname//'"'//'\n')
    sts=NF90_INQ_DIMID(nid_ceil,'time_avg',tdim_ceil)
    sts=NF90_INQUIRE_DIMENSION(nid_ceil,tdim_ceil,timdim_ceil,ftlen_ceil)
    allocate(ncbh(ftlen_ceil),cbh1(ftlen_ceil),vcbh(ftlen_ceil))
    sts=NF90_INQ_VARID(nid_ceil,'ncbh',ncbhid); CALL CE(sts,'ncbh id failed')
    sts=NF90_GET_VAR(nid_ceil,ncbhid,ncbh); CALL CE(sts,'get ncbh failed')
    sts=NF90_INQ_VARID(nid_ceil,'valid_cbh1',vcbhid); CALL CE(sts,'vcbh id failed')
    sts=NF90_GET_VAR(nid_ceil,vcbhid,vcbh); CALL CE(sts,'get vcbh failed')
    sts=NF90_INQ_VARID(nid_ceil,'first_cbh_avg',cbh1id); CALL CE(sts,'ncbh id failed')
    sts=NF90_GET_VAR(nid_ceil,cbh1id,cbh1); CALL CE(sts,'get time cverage start var failed')
    sts=NF90_CLOSE(nid_ceil)
endif

!ALLOCATE NAN files
ALLOCATE(RefhW(1000,tlen),RefvW(1000,tlen),VelhW(1000,tlen),VelvW(1000,tlen),SpWhW(1000,tlen),SpWvW(1000,tlen),SNRhW(1000,tlen),SNRvW(1000,tlen),LDRaW(1000,tlen))
ALLOCATE(prtar(tlen),PW(tlen),prtr(tlen),nyq(tlen),n_samples(tlen))
RefhW=fv
RefvW=fv
VelhW=fv
VelvW=fv
SpWhW=fv
SpWvW=fv
SNRhW=fv
SNRvW=fv
LDRaW=fv
PW=fv
prtr=fv
nyq=fv
n_samples=fv
prtar=fv
read(11,'(a20)') filehead
if (filehead(1:1).eq.'E') then
    fflag=0
else
    read(filehead,*) time,Nfile,ftlen
    fflag=1
endif
print*,outfn,outfn(1:1)

do while (fflag==1)
    call ncread
    if (fflag.eq.1)then
        PW(time)        = pulse_widtharrI(1)
        prtr(time)      = prt_ratioarrI(1)
        nyq(time)       = nyq_VelarrI(1)
        n_samples(time) = ftlen
        prtar(time)     = PRTI(1)
        DEALLOCATE(frequencyarrI,PRTI,pulse_widtharrI,prt_ratioarrI,nyq_VelarrI,n_samplesarrI,WavePRF)
        if (qcstat.eq.0) call noqcavg
        if (qcstat.eq.1) call noqcavg
        if (qcstat.eq.14) call knuavg
        if (qcstat.eq.15) call knuavg
        if (qcstat.eq.17) call qc17avg
        if (qcstat.eq.172) call qc17_ceil

        do hi = 1, 1000
            RefhW(hi,time)=RefhO(hi)
            RefvW(hi,time)=RefvO(hi)
            VelhW(hi,time)=VelhO(hi)
            VelvW(hi,time)=VelvO(hi)
            SpWhW(hi,time)=SpWhO(hi)
            SpWvW(hi,time)=SpWvO(hi)
            SNRhW(hi,time)=SNRhO(hi)
            SNRvW(hi,time)=SNRvO(hi)
            LDRaW(hi,time)=LDRaO(hi)
        end do
        DEALLOCATE(RefhO,VelhO,SpWhO,SNRhO,LDRaO)
        DEALLOCATE(RefvO,VelvO,SpWvO,SNRvO)
        read(11,'(a20)') filehead

        if (filehead(1:1).eq.'E') then
            fflag=0
        else
            read(filehead,*) time,Nfile,ftlen
            fflag=1
        endif
    elseif (fflag.eq.2) then
        PW(time)        = fv
        prtr(time)      = fv
        nyq(time)       = fv
        prtar(time)     = fv
        n_samples(time) = 0
        DEALLOCATE(frequencyarrI,PRTI,pulse_widtharrI,prt_ratioarrI,nyq_VelarrI,n_samplesarrI,WavePRF)        
        do hi = 1, 1000
            RefhW(hi,time)=fv
            RefvW(hi,time)=fv
            VelhW(hi,time)=fv
            VelvW(hi,time)=fv
            SpWhW(hi,time)=fv
            SpWvW(hi,time)=fv
            SNRhW(hi,time)=fv
            SNRvW(hi,time)=fv
            LDRaW(hi,time)=fv
        end do
    endif
enddo
if (qcstat.eq.17) call maskingQC
if (qcstat.eq.172) call maskingQC
print*,outfn
call ncwrite
DEALLOCATE(RefhW,RefvW,VelhW,VelvW,SpWhW,SpWvW,SNRhW,SNRvW,LDRaW)
DEALLOCATE(PW,prtr,nyq,n_samples,prtar)

END PROGRAM
