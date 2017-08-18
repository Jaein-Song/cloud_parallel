PROGRAM ceil
use netcdf
use varmod
use inputlists
use Sorting
IMPLICIT NONE
!Check File Name
	print*, ceilifl
        open(unit=11, file=ceilifl,  status="old", action="read")
        read (11,*),lv3_flag      !lv3 file existance
        read (11,'(a80)'),idir          !raw file
        read (11,'(a80)'),odir          !raw file
        if (lv3_flag.eq.1) read (11,'(a80)'),idir2         !level 3
        read (11,'(a80)'),odir2         !level 3
        print*,lv3_flag
        print*,idir
        print*,idir2
        print*,odir
        print*,odir2

        tint=86400/nts
!Confirm File Size
	open(UNIT=1,FILE=idir,STATUS='old')
        read(1,*),dummy
        read(1,*),dummy
        cflag=0
        eflag=0 
        do while(cflag.eq.0)
                eflag=eflag+1
                read(1,*),dummy
                read(1,*),dummy
                read(1,*),ncbh,dummy
                read(1,*),ntest
                if (ntest.ne.100) then
                        read(1,*),dummy
                endif
                read(1,*),bsc(1:7700)
                read(1,*),dummy
                read(1,*),dummy!,dummy
                if(dummy.eq.esign)then
                        print*,'eof',dummy
                        cflag=1
                        exit
                endif
        end do
        close(unit=1,status='keep')
!Read file data
        open(unit=2,file=idir,status='old')
        read(2,*),dummy
                print*,dummy
        read(2,*),dummy
                print*,dummy
        nt=eflag
        nh=7700/5
        allocate(t(nt),h(nh),bscd(nt,nh),cbh1o(nt),cbh2o(nt),cbh3o(nt))
        bscd=0
!START HEX 2!DEC=================================================================
        do i=1,eflag
                !print*,'1',dummy
                read(2,'(1X,I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') yyyy,da,mo,ho,mi,se
                t(i)=ho*3600+mi*60+se
                !print*,dummy
                !print*,'2',i,yyyy,da,mo,ho,mi,se
                read(2,*),dummy
                read(2,*),ncbh,cdum1,cdum2,cdum3
                ch1=ncbh(1:1)
                if (ch1.eq.'1') then
                        read(cdum1,*),cbh1o(i)
                        cbh2o(i)=fv
                        cbh3o(i)=fv
                else if (ch1.eq.'2') then
                        read(cdum1,*),cbh1o(i)
                        read(cdum2,*),cbh2o(i)
                        cbh3o(i)=fv
                else if (ch1.eq.'3') then
                        read(cdum1,*),cbh1o(i)
                        read(cdum2,*),cbh2o(i)
                        read(cdum3,*),cbh3o(i)
                else
                        cbh1o(i)=fv
                        cbh2o(i)=fv
                        cbh3o(i)=fv
               endif
                read(2,*),ntest
                read(2,*),dummy
                read(2,*),bsc
                !print*,'3',i,bsc(1:19)
                read(2,*),dummy
                !print*,'4',i,dummy
                !read(2,*),dummy!,dummy
                !print*,'5',i,dummy
                read(2,'(1X,I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') yyyy,da,mo,ho,mi,se
                do j=1,nh
                        h(j)=j*10
                        do l=1,5
                                k=(j-1)*5+l
                                stat=0
                                call htd(bsc(k:k),l,bscd(i,j),ii,jj,stat)
                                bscd(i,j)=bscd(i,j)/(10.**8)
                                if(stat.eq.1)then
                                        print*,stat,i,j,k,l
                                endif
                        enddo
                                if(bscd(i,j).ge.16.**4)then
                                        bscd(i,j)=fv
                                endif
                enddo
        end do
        close(unit=2,status='keep')
!Read LV3 data
        if (lv3_flag.eq.1) call readlv3
!2-min AVG cbh123o->cbh123, bsc->bscs,lv3_blh123->blh123
        allocate(bscs(nts,nhs),hs(nhs),ts(nts),cbh1(nts),cbh2(nts),cbh3(nts),blh1(nts),blh2(nts),blh3(nts),bli1(nts),bli2(nts),bli3(nts))
        hs=h
        do i=1,nts
                ts(i)=tint*(i-1)
        enddo
        allocate(count_bsc(nts),count_cbh(nts),count_blh(nts),valid_bsc(nts,nhs),valid_cbh1(nts),valid_cbh2(nts),valid_cbh3(nts),valid_blh1(nts),valid_blh2(nts),valid_blh3(nts),valid_bli1(nts),valid_bli2(nts),valid_bli3(nts))
        print*,'bsc'
        call meantime(0,1000000,bscd,nt,nh,t,86400/tint,bscs,valid_bsc,count_bsc,fillvalue)
        print*,'cbh1'
        call meantime(0,16000,cbh1o,length-1,1,t,86400/tint,cbh1,valid_cbh1,count_cbh,fillvalue)
        print*,'cbh2'
        call meantime(0,16000,cbh2o,length-1,1,t,86400/tint,cbh2,valid_cbh2,count_cbh,fillvalue)
        print*,'cbh3'
        call meantime(0,16000,cbh3o,length-1,1,t,86400/tint,cbh3,valid_cbh3,count_cbh,fillvalue)
        if (lv3_flag.eq.1) then
                print*,'blh1'
                call meantime(0,16000,lv3_blh1,length-1,1,lv3_time,86400/tint,blh1,valid_blh1,count_blh,fillvalue)
                print*,'blh2'
                call meantime(0,16000,lv3_blh2,length-1,1,lv3_time,86400/tint,blh2,valid_blh2,count_blh,fillvalue)
                print*,'blh3'
                call meantime(0,16000,lv3_blh3,length-1,1,lv3_time,86400/tint,blh3,valid_blh3,count_blh,fillvalue)
                print*,'bli1'
                call meantime(0,16000,lv3_bli1,length-1,1,lv3_time,86400/tint,bli1,valid_bli1,count_blh,fillvalue)
                print*,'bli2'
                call meantime(0,16000,lv3_bli2,length-1,1,lv3_time,86400/tint,bli2,valid_bli2,count_blh,fillvalue)
                print*,'bli3'
                call meantime(0,16000,lv3_bli3,length-1,1,lv3_time,86400/tint,bli3,valid_bli3,count_blh,fillvalue)
                print*,'bend'
        else
                blh1=fillvalue
                blh2=fillvalue
                blh3=fillvalue
                bli1=fillvalue
                bli2=fillvalue
                bli3=fillvalue
                valid_blh1=fillvalue
                valid_blh2=fillvalue
                valid_blh3=fillvalue
                valid_bli1=fillvalue
                valid_bli2=fillvalue
                valid_bli3=fillvalue
                count_blh=0
        endif
        call ncwrite
END PROGRAM    
