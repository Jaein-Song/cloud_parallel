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
subroutine htd(hex,l,deco,ii,jj,stat)
!use varmod
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
        elseif(hex.eq.'a')then
                dec=10      
        elseif(hex.eq.'b')then
                dec=11      
        elseif(hex.eq.'c')then
                dec=12      
        elseif(hex.eq.'d')then
                dec=13      
        elseif(hex.eq.'e')then
                dec=14      
        elseif(hex.eq.'f')then
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
SUBROUTINE meantime(thresh_low,thresh_high,var,varlen1,varlen2,vartime,timelen,var_avged,count_valid,count_total,fv)
!!!!!!!var dimension must be set as (time,height)
IMPLICIT NONE
!INTENT VARIABLES
        integer,                                intent(in   ) :: thresh_high,thresh_low,varlen1,varlen2,timelen
        integer,dimension(:),                   intent(in   ) :: vartime(varlen1)
        real,   dimension(:,:),                 intent(in   ) :: var(varlen1,varlen2)
 !       integer,dimension(:),   allocatable,    intent(  out) :: time_avged
        integer,dimension(:),                   intent(  out) :: count_total(timelen)
        integer,dimension(:,:),                 intent(  out) :: count_valid(timelen,varlen2)
        real,   dimension(:,:),                 intent(  out) :: var_avged(timelen,varlen2)
!INHERENT VARIABLES
        integer                                               :: i,j,ti,time_start,time_end,pos_marker,pos_flag,time_intv
        real                                                  :: fv
        time_intv=86400/timelen
!        timelen=86400/time_intv
!        allocate(var_avged(timelen,varlen2),count_total(timelen),count_valid(timelen,varlen2))
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
!        print*,'loopend'
!AVG for each timestep
        pos_marker=1
        do ti=1,timelen
!                print*,pos_marker
                do j=1,varlen2
!                print*,ti,j,count_total(ti)
                        count_valid(ti,j)       = 0
                        var_avged(ti,j)         = 0
                        do i=pos_marker,pos_marker+count_total(ti)-1
!                        print*,'before if1'
!                        print*,i,j
                                if ( (var(i,j).gt.thresh_low).and.(var(i,j).le.thresh_high) ) then
                                        count_valid(ti,j)       = count_valid(ti,j)+1
                                        var_avged(ti,j)         = var_avged(ti,j)+var(i,j)
                                end if
                        end do
!                print*,'before if2'
                        if ( count_valid(ti,j).ge.1 ) then
                            var_avged(ti,j)=var_avged(ti,j)/count_valid(ti,j)
                        else
                            var_avged(ti,j)=fv
                        endif
!                print*,ti,'end'
                end do
                !print*,ti,pos_marker,ti,count_total(ti)
                !print*,var(pos_marker:count_total(ti)+pos_marker,1)
                !print*,var_avged
                pos_marker=pos_marker+count_total(ti)
        end do
        !print*,'var', var
        !print*,'va',var_avged
END SUBROUTINE
