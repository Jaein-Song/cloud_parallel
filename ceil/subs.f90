!SUBROUTINES======================================================================
subroutine readlv3
    use varmod
    implicit none
open(unit=31, file=idir2, status="old")
read(31,*)dummy
read(31,*)dummy
length=0
ios=0
    !print*,length,ios,dummy
do while ( ios.ge.0 ) 
    length=length+1
    read(31,'(250a)',iostat=ios)dummy
!    print*,length,ios,dummy
enddo
close(31)
!print*,length
open(unit=31, file=idir2, status="old")
read(31,*)dummy
read(31,*)dummy
allocate(lv3_time(length),lv3_blh1(length),lv3_blh2(length),lv3_blh3(length),lv3_bli1(length),lv3_bli2(length),lv3_bli3(length),lv3_cbh1(length),lv3_cbh2(length),lv3_cbh3(length))
do lengthi=1,length-1
    read(31,*)ymd,hms_lv3, UNIXTIME, CEILOMETER, PERIOD, SAMPLE_COUNT,lv3_blh1(lengthi), lv3_bli1(lengthi), lv3_blh2(lengthi),lv3_bli2(lengthi),lv3_blh3(lengthi),lv3_bli3(lengthi), CLOUD_STATUS, lv3_cbh1(lengthi), lv3_cbh2(lengthi), lv3_cbh3(lengthi), PARAMETERS
    read(hms_lv3,'(I2,1X,I2,1X,I2)')hh,mm,ss
    lv3_time(lengthi)=hh*3600+mm*60+ss
enddo
END SUBROUTINE
!===================================================================================================================================
SUBROUTINE qcflagcheck(hex4,flagarr)
IMPLICIT NONE   
        character(len=4),intent(in   )          :: hex4
        integer,dimension(:),intent(   out)        :: flagarr(16)
        integer                                 :: i,j,bin_index
        real                                    :: dec
        do i=1,4
                call htd(hex4(i:i),5,dec,0,0,0)
                do j=1,4
                bin_index=4-j
                if (dec.ge.2**bin_index) then
                        flagarr((i-1)*4+j)=1
                        dec=dec-2**bin_index
                else
                        flagarr((i-1)*4+j)=0
                endif
                enddo
        enddo
ENDSUBROUTINE
!===================================================================================================================================
subroutine htd(hex,l,deco,ii,jj,stat)
implicit none

        character(len=1),intent(in)    :: hex
        integer,intent(in)      :: l,ii,jj
        real                    :: dec,deco
        integer                 :: stat
        if(hex.eq.'0')then
                dec=0       
        elseif(hex.eq.'1')then
                dec=1
        elseif(hex.eq.'2')then
                dec=2       
        elseif(hex.eq.'3')then
                dec=3       
        elseif(hex.eq.'4')then
                dec=4       
        elseif(hex.eq.'5')then
                dec=5       
        elseif(hex.eq.'6')then
                dec=6       
        elseif(hex.eq.'7')then
                dec=7       
        elseif(hex.eq.'8')then
                dec=8       
        elseif(hex.eq.'9')then
                dec=9       
        elseif((hex.eq.'a').or.(hex.eq.'A'))then
                dec=10      
        elseif((hex.eq.'b').or.(hex.eq.'B'))then
                dec=11      
        elseif((hex.eq.'c').or.(hex.eq.'C'))then
                dec=12      
        elseif((hex.eq.'d').or.(hex.eq.'D'))then
                dec=13      
        elseif((hex.eq.'e').or.(hex.eq.'E'))then
                dec=14      
        elseif((hex.eq.'f').or.(hex.eq.'F'))then
                dec=15      
        else
                print*,'Invalid HEX digit?',hex,ii,jj
                dec=0
                stat=1
        endif
        dec=dec*16**(5-l)
        deco=deco+dec
endsubroutine
!===================================================================================================================================
SUBROUTINE meantime(thresh_low,thresh_high,var,varlen1,varlen2,vartime,timelen,var_avged,count_valid,count_total,fvalue,bflag)
!!!!!!!var dimension must be set as (time,height)
use varmod
IMPLICIT NONE
!INTENT VARIABLES
        integer,                                intent(in   ) :: thresh_high,thresh_low,varlen1,varlen2,timelen,bflag
        integer,dimension(:),                   intent(in   ) :: vartime(varlen1)
        real,   dimension(:,:),                 intent(in   ) :: var(varlen1,varlen2)
        integer,dimension(:),                   intent(  out) :: count_total(timelen)
        integer,dimension(:,:),                 intent(  out) :: count_valid(timelen,varlen2)
        real,   dimension(:,:),                 intent(  out) :: var_avged(timelen,varlen2)
!INHERENT VARIABLES
        integer                                               :: ti,time_start,time_end,pos_marker,pos_flag,time_intv
        real                                                  :: fvalue
        time_intv=86400/timelen
!Count cells for each timestep
        ti=1

        do i = 1, timelen
                time_start=time_intv*(i-1)
                time_end=time_intv*i
                count_total(i)=0
                pos_flag=0
                if ( (ti.le.varlen1)) then
                        if ((vartime(ti).ge.time_start).and.(vartime(ti).lt.time_end) ) pos_flag=1
                endif
                do while (pos_flag.eq.1)
                        count_total(i)=count_total(i)+1
                        ti=ti+1
                        pos_flag=0
                        if ( (ti.le.varlen1)) then
                                if ((vartime(ti).ge.time_start).and.(vartime(ti).lt.time_end) ) pos_flag=1
                        endif
                end do
        end do
!AVG for each timestep
        pos_marker=1
        do ti=1,timelen
                if (bflag.eq.1) then
                        QCflags(ti)=0
                        aflags(ti)=0
                        wflags(ti)=0
                        sflags(ti)=0
                        do k=1,16
                                aflaga(ti,k)=0
                                wflaga(ti,k)=0
                                sflaga(ti,k)=0
                        enddo
                endif
                do j=1,varlen2
                        count_valid(ti,j)       = 0
                        var_avged(ti,j)         = 0
                        do i=pos_marker,pos_marker+count_total(ti)-1
                                if (bflag.eq.1) then
                                do k=1,16
                                        aflags(ti)=alarm_flag(i,k)+aflags(ti)
                                        wflags(ti)=warning_flag(i,k)+wflags(ti)
                                        sflags(ti)=status_flag(i,k)+sflags(ti)
                                        aflaga(ti,k)=aflaga(ti,k)+alarm_flag(i,k)
                                        wflaga(ti,k)=wflaga(ti,k)+warning_flag(i,k)
                                        sflaga(ti,k)=sflaga(ti,k)+status_flag(i,k)
                                enddo
                                     if (QCflag(i).lt.1)then
                                            if ( (var(i,j).gt.thresh_low).and.(var(i,j).le.thresh_high) ) then
                                                count_valid(ti,j)       = count_valid(ti,j)+1
                                                var_avged(ti,j)         = var_avged(ti,j)+var(i,j)
                                            end if
                                     else
                                            QCflags(ti)=QCflags(ti)+1
                                     endif
                                else
                                        if ( (var(i,j).gt.thresh_low).and.(var(i,j).le.thresh_high) ) then
                                                count_valid(ti,j)       = count_valid(ti,j)+1
                                                var_avged(ti,j)         = var_avged(ti,j)+var(i,j)
                                       endif
                                endif
                        end do
                        if ( count_valid(ti,j).ge.1 ) then
                            var_avged(ti,j)=var_avged(ti,j)/count_valid(ti,j)
                        else
                            var_avged(ti,j)=fvalue
                        endif
                end do
                pos_marker=pos_marker+count_total(ti)
        end do
END SUBROUTINE
