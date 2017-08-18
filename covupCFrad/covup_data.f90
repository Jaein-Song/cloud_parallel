PROGRAM covup_data
!This program is made by Jae In Song, March 15, 2017
!The purpose of this program is covering up the data not made due to server change

!INITIALIZE
use mod_para
use varmod
use inputlists
IMPLICIT NONE
!open input/ouput file name list
open(11,file=covifl1,status='old')
read(11,*) flagno, flag14, flag15
!do while (flagno+flag14+flag15.eq.3) 
!        read(11,*) flagno, flag14, flag15
!end do
read(11,'(a110)') ifile
if (flagno.lt.1) then
open(22,file=covofl1,status='old')
read(22,'(a110)') outfnno
endif
if (flag14.lt.1) then
open(23,file=covofl2,status='old')
read(23,'(a110)') outfn14
endif
if (flag15.lt.1) then
open(24,file=covofl3,status='old')
read(24,'(a110)') outfn15
endif
print*,flagno,flag14,flag15
print*,ifile
flag=1
do while (flag==1)
        open(1,file=ifile,form='unformatted',access='stream',status='old')
!print*,'I:',ifile       
!print*,'O:',outfn       

       !Read Binary file (BASEFILE) Head and Block, subroutine Courtesy of J. E. Lee
        call read_head
        if (BinLen==15) then
              call read_base
              ftlenC=rn
              binlenC=bn
              close(1)
              if (ir.eq.rn) then
        		if (flagno.lt.1) then
                		qcstat=0
	                	print*,'Ino:',ifile       
        		outfn=outfnno
                		call write_cfradial
	                	print*,'O:',outfn
        		endif
              		if (flag14.lt.1) then
				qcstat=14
                		print*,'I14:',ifile       
                		outfn=outfn14
                		call qc14
                		call write_cfradial
                		print*,'O:',outfn
              		endif
              		if (flag15.lt.1) then
                		qcstat=15
                		print*,'I15:',ifile       
                		outfn=outfn15
				print*,'call qc15'
                		call qc15
                		allocate(echo(rn,bn),mask(rn,bn))
                		echo=echotype
                		mask=Refhmask
                		call write_cfradial
                		DEALLOCATE(Refhmask,genTXT,echopdf,echotype,maskcount)
                		print*,'O:',outfn
              		endif
              print*, 'success:',ifile
              else
              print*, 'fail:',outfn
!             print*,'deallocating done, checking the EOF'
              endif
              DEALLOCATE(RayNo)
              DEALLOCATE(RecTime)
              DEALLOCATE(AZ)
              DEALLOCATE(EL)
              DEALLOCATE(VolLayNo)
              DEALLOCATE(WavePRF)
        
              DEALLOCATE(LDR)
              DEALLOCATE(Z1)  ;  DEALLOCATE(Vr1)
              DEALLOCATE(SW1) ;  DEALLOCATE(SNR1)
              DEALLOCATE(Z2)  ;  DEALLOCATE(Vr2)
              DEALLOCATE(SW2) ;  DEALLOCATE(SNR2)
              DEALLOCATE(LDRo)
              DEALLOCATE(Z1o)  ;  DEALLOCATE(Vr1o)
              DEALLOCATE(SW1o) ;  DEALLOCATE(SNR1o)
              DEALLOCATE(Z2o)  ;  DEALLOCATE(Vr2o)
              DEALLOCATE(SW2o) ;  DEALLOCATE(SNR2o)
        else
                close(1)
        endif


        read(11,'(a110)') ifile
        if(ifile(1:1).eq.'E')then
                flag=0
        else 
                read(ifile(1:1),*) flagno
                read(ifile(3:3),*) flag14
                read(ifile(5:5),*) flag15
        read(11,'(a110)') ifile
        endif
        if (flag.eq.1)then
                if (flagno.lt.1) read(22,'(a110)') outfnno
                if (flag14.lt.1) read(23,'(a110)') outfn14
                if (flag15.lt.1) read(24,'(a110)') outfn15
        endif
enddo
ENDPROGRAM
