PROGRAM ceil
use netcdf
use varmod
use inputlists
use sorting
IMPLICIT NONE
!Check File Name
	print*, ceilifl
        open(unit=11, file=ceilifl,  status="old", action="read")
        read (11,*),lv3_flag      !lv3 file existance
        read (11,*),tint          !raw file
        read (11,'(a250)'),idir          !raw file
        read (11,'(a250)'),odir          !raw file
        if (lv3_flag.eq.1) read (11,'(a250)'),idir2         !level 3
        read (11,'(a250)'),odir2         !level 3
        print*,'lv3flag',lv3_flag
        print*, 'tint'
        print*,'idir',idir
        print*,'idir2',idir2
        print*,'odir',odir
        print*,'odir2',odir2
        
        nts=1440/tint
        tint=tint*60
!Confirm File Size
	open(UNIT=1,FILE=idir,STATUS='old')
        read(1,'(a250)',iostat=ios),dummy
        read(1,'(a250)',iostat=ios),dummy
        cflag=0
        eflag=0 
        mflag=0
        ios=0
        do while(cflag.eq.0)
                eflag=eflag+1
                        read(1,'(a250)',iostat=ios),dummy
                if (ios.eq.0) then
                        read(1,'(a250)',iostat=ios),dummy
                        mflag=1
                else
                        mflag=0
                endif
                if (ios.eq.0) then
                        read(1,'(a250)',iostat=ios),ncbh,dummy
                        mflag=1
                else
                        mflag=0
                endif
                if (ios.eq.0) then
                        read(1,*,iostat=ios),ntest
                        mflag=1
                else
                        mflag=0
                endif
                if (ios.eq.0) then
                        if (ntest.ne.100) then
                                read(1,'(a250)',iostat=ios),dummy
                        endif
                        mflag=1
                else
                        mflag=0
                endif
                if (ios.eq.0) then
                        read(1,'(a7700)',iostat=ios),bsc(1:7700)
                        mflag=1
                        if (bsc(7699:7699).eq.' ') ios=1 
                else
                        mflag=0
                endif
                if (ios.eq.0) then
                        read(1,'(a250)',iostat=ios),dummy
                        mflag=1
                else
                        mflag=0
                endif
                if (ios.eq.0) then
                        read(1,'(a250)',iostat=ios),dummy!,dummy
                        mflag=1
                else
                        mflag=0
                endif
                if(dummy.eq.esign)then
                        print*,'eof',dummy
                        cflag=1
                        exit
                endif
                if (mflag.eq.0) then
                        cflag=1
                        eflag=eflag-1
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
        allocate(t(nt),h(nh),bscd(nt,nh),cbh1o(nt),cbh2o(nt),cbh3o(nt),vviso(nt))
        allocate(scalefo(nt),hreso(nt),hleno(nt),Lengo(nt),Ltempo(nt),Wtranso(nt),Tangleo(nt),Blito(nt),Ptypeo(nt),Gtypeo(nt),Btypeo(nt),sampro(nt),sumbo(nt),QCflag(nt),alarm_flag(nt,16),warning_flag(nt,16),status_flag(nt,16))
        bscd=0
!START HEX 2!DEC=================================================================
        do i=1,eflag
                read(2,'(1X,I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') yyyy,da,mo,ho,mi,se
                t(i)=ho*3600+mi*60+se
                read(2,*),dummy
                print*,'yyyy ',yyyy,da,mo,ho,mi,se
                print*,dummy
                read(2,'(I1,A1,1X,A5,1X,A5,1X,A5,1X,A4,A4,A4)'),ch1,awstat,cdum1,cdum2,cdum3,warning,alarm,state
                vviso(i)=fv
                if (ch1.eq.1) then
                        read(cdum1,*),cbh1o(i)
                        cbh2o(i)=fv
                        cbh3o(i)=fv
                else if (ch1.eq.2) then
                        read(cdum1,*),cbh1o(i)
                        read(cdum2,*),cbh2o(i)
                        cbh3o(i)=fv
                else if (ch1.eq.3) then
                        read(cdum1,*),cbh1o(i)
                        read(cdum2,*),cbh2o(i)
                        read(cdum3,*),cbh3o(i)
                elseif (ch1.eq.4) then
                        read(cdum1,*),vviso(i)
                        cbh1o(i)=fv
                        cbh2o(i)=fv
                        cbh3o(i)=fv
                else
                        cbh1o(i)=fv
                        cbh2o(i)=fv
                        cbh3o(i)=fv
                endif
                if (awstat(1:1).eq.'0')then
                        QCflag(i)=0
                        do j=1,16
                                alarm_flag(i,j)         = 0
                                warning_flag(i,j)       = 0
                        enddo
                        call qcflagcheck(state,status_flag(i,:))
                elseif (awstat(1:1).eq.'W')then
                        QCflag(i)=1
                        do j=1,16
                                alarm_flag(i,j)         = 0
                        enddo
                        call qcflagcheck(state,status_flag(i,:))
                        call qcflagcheck(warning,warning_flag(i,:))
                elseif (awstat(1:1).eq.'A')then
                        QCflag(i)=2
                        call qcflagcheck(state,status_flag(i,:))
                        call qcflagcheck(warning,warning_flag(i,:))
                        call qcflagcheck(alarm,alarm_flag(i,:))
                endif
                !SPARES
                do j=5,16
                        if ((j.eq.5).or.((j.ge.9).and.(j.lt.15))) then
                                alarm_flag(i,j)         = 0
                        endif
                        if ((j.eq.5).or.(j.eq.7).or.(j.eq.16))then
                                warning_flag(i,j)       = 0
                        endif
                        if ((j.eq.8).or.(j.ge.12))then
                                status_flag(i,j)        = 0
                        endif
                        
                enddo
                read(2,*),ntest
                read(2,'(a250)')dummy
                read(dummy(1:6),*),scalefo(i)
                read(dummy(7:9),*),hreso(i)
                read(dummy(10:14),*),hleno(i)
                read(dummy(15:18),*),Lengo(i)
                read(dummy(19:22),*),Ltempo(i)
                read(dummy(23:26),*),Wtranso(i)
                read(dummy(27:29),*),Tangleo(i)
                read(dummy(30:34),*),Blito(i)
                read(dummy(35:35),*),Ptypes
                read(dummy(40:40),*),Gtypes
                read(dummy(41:41),*),Btypes
                read(dummy(42:44),*),sampro(i)
                read(dummy(45:47),*),sumbo(i)
                Ptypeo(i)=fv
                Gtypeo(i)=fv
                Btypeo(i)=fv
                if (Ptypes(1:1).eq.'L')then
                Ptypeo(i)=1
                else 
                Ptypeo(i)=0
                endif
                if (Gtypes(1:1).eq.'H')then
                Gtypeo(i)=1
                else 
                Gtypeo(i)=0
                endif
                if (Btypes(1:1).eq.'N')then
                Btypeo(i)=1
                else 
                Btypeo(i)=0
                endif
                read(2,*),bsc
                read(2,*),dummy
                read(2,'(a1)'),dummy
                do j=1,nh
                        h(j)=j*10
                        do l=1,5
                                k=(j-1)*5+l
                                stat=0
                                call htd(bsc(k:k),l,bscd(i,j),ii,jj,stat)
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
        allocate(bscs(nts,nhs),hs(nhs),ts(nts),cbh1(nts),cbh2(nts),cbh3(nts),vvis(nts),blh1(nts),blh2(nts),blh3(nts),bli1(nts),bli2(nts),bli3(nts),sumb(nts))
        hs=h
        do i=1,nts
                ts(i)=tint*(i-1)
        enddo
        allocate(count_bsc(nts),count_cbh(nts),count_blh(nts),valid_bsc(nts,nhs),valid_cbh1(nts),valid_cbh2(nts),valid_cbh3(nts),valid_blh1(nts),valid_blh2(nts),valid_blh3(nts),valid_bli1(nts),valid_bli2(nts),valid_bli3(nts),valid_vvis(nts),count_vvis(nts),valid_sumb(nts),count_sumb(nts))
        allocate(aflags(nts),wflags(nts),sflags(nts),aflaga(nts,16),sflaga(nts,16),wflaga(nts,16),QCflags(nts))
        print*,'bsc'
        call meantime(0,1000000,bscd,nt,nh,t,86400/tint,bscs,valid_bsc,count_bsc,fillvalue,1)
        print*,'cbh1'
        call meantime(0,16000,cbh1o,length-1,1,t,86400/tint,cbh1,valid_cbh1,count_cbh,fillvalue,1)
        print*,'cbh2'
        call meantime(0,16000,cbh2o,length-1,1,t,86400/tint,cbh2,valid_cbh2,count_cbh,fillvalue,1)
        print*,'cbh3'
        call meantime(0,16000,cbh3o,length-1,1,t,86400/tint,cbh3,valid_cbh3,count_cbh,fillvalue,1)
        print*,'vertical_visibility'
        call meantime(0,16000,vviso,length-1,1,t,86400/tint,vvis,valid_vvis,count_vvis,fillvalue,1)
        print*,'sum'
        call meantime(0,16000,sumbo,length-1,1,t,86400/tint,sumb,valid_sumb,count_sumb,fillvalue,1)
        if (lv3_flag.eq.1) then
                print*,'blh1'
                call meantime(0,16000,lv3_blh1,length-1,1,lv3_time,86400/tint,blh1,valid_blh1,count_blh,fillvalue,0)
                print*,'blh2'
                call meantime(0,16000,lv3_blh2,length-1,1,lv3_time,86400/tint,blh2,valid_blh2,count_blh,fillvalue,0)
                print*,'blh3'
                call meantime(0,16000,lv3_blh3,length-1,1,lv3_time,86400/tint,blh3,valid_blh3,count_blh,fillvalue,0)
                print*,'bli1'
                call meantime(0,16000,lv3_bli1,length-1,1,lv3_time,86400/tint,bli1,valid_bli1,count_blh,fillvalue,0)
                print*,'bli2'
                call meantime(0,16000,lv3_bli2,length-1,1,lv3_time,86400/tint,bli2,valid_bli2,count_blh,fillvalue,0)
                print*,'bli3'
                call meantime(0,16000,lv3_bli3,length-1,1,lv3_time,86400/tint,bli3,valid_bli3,count_blh,fillvalue,0)
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
