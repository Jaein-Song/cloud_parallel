SUBROUTINE qc15
!Quallity Check & Merge data: Original algorithm made by Bo Young Ye (IDL, C)
!Translated to Fortran by Jae In Song
!pdfflag: 1: Precip, 2: Insect, 3:LowN, 4: HighN
!scanflag: 1: AT, 2: RT, 3: THI ->txttype
!varflag: 1: LDR, 2: Zhh, 3: Zhv, 4: VRhh, 5: VRhv

!Cloud count only if when the echo satisfies below condition
!0.     INITIALLIZE: set module, allocate vars
!1.     Pre-QC: remove vertical noise (mainly caused by machine problem), low-level noise
!2.     PDF-Masking QC
!3.     Echo type for each mask
!4.     filter out low-level insect echo
!5.     set fillvalue for unvalid cell
!6.     Average data
!6.0.   Echo existance
!6.1.   Average Data
!6.2.   Fill Fillvalue if not valid
!7.     DEALLOCATE

!0.     INITIALLIZE: set module, allocate vars
    USE varmod
    use mod_para
    IMPLICIT NONE
    ALLOCATE(Qflag(ftlenC),Qflagv(ftlenC))
    ALLOCATE(RefhI(ftlenC,binlenC),VelhI(ftlenC,binlenC),SpWhI(ftlenC,binlenC),SNRhI(ftlenC,binlenC),LDRaI(ftlenC,binlenC))
    ALLOCATE(RefvI(ftlenC,binlenC),VelvI(ftlenC,binlenC),SpWvI(ftlenC,binlenC),SNRvI(ftlenC,binlenC))
    ALLOCATE(Refhmask(ftlenC,binlenC),genTXT(ftlenC,binlenC),echopdf(echolen,ftlenC,binlenC),echotype(ftlenC,binlenC))
RefhI=Z1o
RefvI=Z2o
VelhI=Vr1o
VelvI=Vr2o
SpWhI=SW1o
SpWvI=SW2o
SNRhI=SNR1o
SNRvI=SNR2o
LDRaI=LDRo
call Maskcomp(RefhI,ftlenC,binlenC,fv,-90,Refhmask,maxmask)
ALLOCATE(maskcount(maxmask))
!1.     Pre-QC: remove vertical noise (mainly caused by machine problem), low-level noise
do ti = 1, ftlenC
    Qflags=0
    wflag=0
    hi=931
    do while (wflag.eq.0)
    !((Qflags.eq.0).or.(hi.le.binlenC))
        if ( LDRaI(ti,hi).ge.-5 ) then
            Qflags=1
        end if
        hi=hi+1
        if (hi.le.binlenC)then
        wflag=Qflags
        else 
        wflag=1
        endif
    end do
    if ( Qflags.eq.1 ) then
        do hi = 1, binlenC
            RefhI(ti,hi)=fv
            VelhI(ti,hi)=fv
            SpWhI(ti,hi)=fv
            SNRhI(ti,hi)=fv
            RefvI(ti,hi)=fv
            VelvI(ti,hi)=fv
            SpWvI(ti,hi)=fv
            SNRvI(ti,hi)=fv
            LDRaI(ti,hi)=fv
            Refhmask(ti,hi)=fv
        end do
    else
        do hi = 1, 20
            RefhI(ti,hi)=fv
            VelhI(ti,hi)=fv
            SpWhI(ti,hi)=fv
            SNRhI(ti,hi)=fv
            RefvI(ti,hi)=fv
            VelvI(ti,hi)=fv
            SpWvI(ti,hi)=fv
            SNRvI(ti,hi)=fv
            LDRaI(ti,hi)=fv
            Refhmask(ti,hi)=fv
        end do
    end if
     
end do
!2.     PDF-Masking QC
do echoi = 1, echolen !echo pdf
    do ti=1,ftlenC
    do hi=1,binlenC
    echopdf(echoi,ti,hi)=-999
    enddo
    enddo
    do scani = 1, scanlen
        do vari = 1, varlen
            do ti=1,ftlenC
            do hi=1,binlenC
            genTXT(ti,hi)=fv
            end do
            end do
            if ( vari.eq.1 ) call TXTcalc15(LDRaI,scani,ftlenC,binlenC,fv,genTXT)
            if ( vari.eq.2 ) call TXTcalc15(RefhI,scani,ftlenC,binlenC,fv,genTXT)
            if ( vari.eq.3 ) call TXTcalc15(RefvI,scani,ftlenC,binlenC,fv,genTXT)
            if ( vari.eq.4 ) call TXTcalc15(VelhI,scani,ftlenC,binlenC,fv,genTXT)
            if ( vari.eq.5 ) call TXTcalc15(VelvI,scani,ftlenC,binlenC,fv,genTXT)
            call importpdf(echoi,scani,vari,genPDF)
            do ti=1,ftlenC
            do hi=1,binlenC
                if (genTXT(ti,hi).gt.-90) then
                    pdfi=1
                    do while (pdfi.lt.100)
                        if ((genTXT(ti,hi).ge.genPDF(pdfi,1)).and.(genTXT(ti,hi).ge.genPDF(pdfi+1,1))) then
                            echopdf(echoi,ti,hi)=genPDF(pdfi,2)+echopdf(echoi,ti,hi)
                            pdfi=100
                        endif
                        pdfi=pdfi+1
                    end do
                endif
            end do
            end do
        end do
    end do
end do
do ti=1,ftlenC
    do hi = 1, binlenC
        echox=-999
        do echoi = 1,4
            if ( echopdf(echoi,ti,hi).gt.echox ) then
                echox=echopdf(echoi,ti,hi)
                echotype(ti,hi)=echoi
            end if
        enddo
        if (echox.le.-999) then
            echotype(ti,hi)=fv
        elseif (echotype(ti,hi).eq.4) then
            echotype(ti,hi) = 3
        endif
!        print*,'ti','hi','echotype'
!        print*,ti,hi,echotype(ti,hi)
    end do
end do
!3.     Echo type for each mask
do mi=1,maxmask
typecount(1)=0
typecount(2)=0
typecount(3)=0
maskcount(mi)=0
    do ti=1,ftlenC
        do hi=1,binlenC
            if (Refhmask(ti,hi).eq.mi) then
                if (echotype(ti,hi).eq.1) then
                    typecount(1)=typecount(1)+1
                elseif (echotype(ti,hi).eq.2) then
                    typecount(2)=typecount(2)+1
                elseif (echotype(ti,hi).eq.3) then
                    typecount(3)=typecount(3)+1
                endif
                maskcount(mi)=maskcount(mi)+1
            endif
        enddo
    end do
    if (typecount(1)+typecount(2)+typecount(3).gt.0) then
        typex=1
        do typei = 2, 3
            if (typecount(typei).ge.typecount(typex)) typex=typei
        end do
        do ti=1,ftlenC
            do hi=1,binlenC
                if (Refhmask(ti,hi).eq.mi) then
                    echotype(ti,hi)=typex
                    !print*, ti,hi,Refhmask(ti,hi),mi, typex
                endif
            end do
        end do
!        print*,mi,typex
    endif
end do

!4.     filter out low-level insect echo
lowmaskcount=0
ldrcount=0
LDRas=0
do ti=1,ftlenC
    do hi=21,201
        if (Refhmask(ti,hi).gt.-1) lowmaskcount=lowmaskcount+1
        if (LDRaI(ti,hi).gt.-90) then 
            LDRas=LDRas+LDRaI(ti,hi)
            ldrcount=ldrcount+1
        endif
    end do
end do
LDRas=LDRas/ldrcount
iOpLDR=0
if ((lowmaskcount.gt.50000).and.(LDRas.lt.-15)) then
    dum_mask=0
    do mi = 1, maxmask
        if ( maskcount(mi).gt.dum_mask ) then
            do ti = 1,ftlenC
                do hi = 1, binlenC
                    if ( Refhmask(ti,hi).eq.mi ) echotype(ti,hi)=1
                end do
            end do
            dum_mask=maskcount(mi)
            iOpLDR=1
        end if
    end do
end if
if (iOpLDR.eq.1) then
    do ti = 1, ftlenC
        do hi = 1, binlenC
            if ( LDRaI(ti,hi).gt.-15 ) echotype(ti,hi)=2
        end do
    end do
endif

!5.     set fillvalue for unvalid cell
do ti = 1, ftlenC
    do hi = 1, 333
        if ( echotype(ti,hi).ne.1 ) then
            Refhmask(ti,hi)=fv
            RefhI(ti,hi)=fv
            VelhI(ti,hi)=fv
            SpWhI(ti,hi)=fv
            SNRhI(ti,hi)=fv
            RefvI(ti,hi)=fv
            VelvI(ti,hi)=fv
            SpWvI(ti,hi)=fv
            SNRvI(ti,hi)=fv
            LDRaI(ti,hi)=fv
        end if
    end do
    do hi = 334,binlenC
        if ( echotype(ti,hi).eq.3 ) then
            Refhmask(ti,hi)=fv
            RefhI(ti,hi)=fv
            VelhI(ti,hi)=fv
            SpWhI(ti,hi)=fv
            SNRhI(ti,hi)=fv
            RefvI(ti,hi)=fv
            VelvI(ti,hi)=fv
            SpWvI(ti,hi)=fv
            SNRvI(ti,hi)=fv
            LDRaI(ti,hi)=fv
        end if
    end do
end do
Z1=RefhI
Z2=RefvI
Vr1=VelhI
Vr2=VelvI
SW1=SpWhI
SW2=SpWvI
SNR1=SNRhI
SNR2=SNRvI
LDR=LDRaI
!7.     DEALLOCATE
    DEALLOCATE(Qflag,Qflagv)
    DEALLOCATE(RefhI,VelhI,SpWhI,SNRhI,LDRaI)
    DEALLOCATE(RefvI,VelvI,SpWvI,SNRvI)
 
    !DEALLOCATE(Refhmask,genTXT,echopdf,echotype,maskcount)

END SUBROUTINE qc15


!MASKING SUBROUTINE
subroutine Maskcomp(var,varlen1,varlen2,fillv,thresh,mask,maskX)
    implicit NONE
    Integer,              intent(in   )     :: varlen1,varlen2
    integer,                 intent(in   )     :: fillv,thresh
    Real,Dimension(:,:),  intent(in   )     :: var(varlen1,varlen2)
    integer,Dimension(:,:),intent(  out)    :: mask(varlen1,varlen2)
    Integer,intent(  out)                   :: maskX
    integer                                 ::i,j,ID,IDi,IDn,jflag
    Integer,dimension(:)                    :: IDl(5)
    
    ID=1
    i=1
    do i=1,varlen1
    !print*,i,j,varlen1,varlen2
    j=1
    do while (j.le.varlen2)
    !print*,'i:',i,'varlen1',varlen1, 'j:',j,'varlen2',varlen2
   !      if (i.eq.182) print*,'here',i,j,varlen2,var(i,j),thresh
        if (j.le.varlen2) then
            if (var(i,j).ge.thresh) then
                jflag=1
            else
                jflag=0
            endif
        else   
            jflag=0
        endif
        do while (jflag.eq.1)
            mask(i,j)=ID
            j=j+1
            if (j.le.varlen2) then
                if (var(i,j).gt.thresh) then
                    jflag=1
                else 
                    jflag=0
                endif
            else
                jflag=0
            endif
        end do
        ID=ID+1
        if (j.le.varlen2) then
            if (var(i,j).lt.thresh) then
                jflag=1
            else
                jflag=0
            endif
        else   
            jflag=0
        endif
        do while (jflag.eq.1)
            mask(i,j)=fillv
            j=j+1
            if (j.le.varlen2) then
                if (var(i,j).lt.thresh) then
                    jflag=1
                else 
                    jflag=0
                endif
            else
                jflag=0
            endif
        end do
    end do
    end do
    do i = 2, varlen1-1
    j=1
    IDl(1)=mask(i,j)
    IDl(2)=mask(i-1,j)
    IDl(3)=mask(i+1,j)
    IDl(4)=mask(i,j+1)
    IDn=fillv
    do IDi=1,4
        if (IDl(IDi).gt.-1) then
            IF (IDn.lt.-1) then
                IDn=IDl(IDi)
            elseif (IDl(IDi).gt.IDn) then
                 IDl(IDi)=IDn
            else
                 IDn=IDl(IDi)
            endif
        else
            IDl(IDi)=fillv
        endif
    end do
    mask(i,j)=IDl(1)
    mask(i-1,j)=IDl(2)
    mask(i+1,j)=IDl(3)
    mask(i,j+1)=IDl(4)

    do j = 2, varlen2-1
        IDl(1)=mask(i,j)
        IDl(2)=mask(i-1,j)
        IDl(3)=mask(i+1,j)
        IDl(4)=mask(i,j+1)
        IDl(5)=mask(i,j-1)
        IDn=fillv
        do IDi=1,5
            if (IDl(IDi).gt.-1) then
                IF (IDn.lt.-1) then
                    IDn=IDl(IDi)
                elseif (IDl(IDi).gt.IDn) then
                     IDl(IDi)=IDn
                else
                     IDn=IDl(IDi)
                endif
            else
                IDl(IDi)=fillv
            endif
        end do
        mask(i,j)=IDl(1)
        mask(i-1,j)=IDl(2)
        mask(i+1,j)=IDl(3)
        mask(i,j+1)=IDl(4)
        mask(i,j-1)=IDl(5)
    end do

    j=varlen2
    IDl(1)=mask(i,j)
    IDl(2)=mask(i-1,j)
    IDl(3)=mask(i+1,j)
    IDl(4)=mask(i,j-1)
    IDn=fillv
    do IDi=1,4
        if (IDl(IDi).gt.-1) then
            IF (IDn.lt.-1) then
                IDn=IDl(IDi)
            elseif (IDl(IDi).gt.IDn) then
                 IDl(IDi)=IDn
            else
                 IDn=IDl(IDi)
            endif
        else
            IDl(IDi)=fillv
        endif
    end do
    mask(i,j)=IDl(1)
    mask(i-1,j)=IDl(2)
    mask(i+1,j)=IDl(3)
    mask(i,j-1)=IDl(4)
    end do
    maskX=0
    do i=1,varlen1
    do j=1,varlen2
    if (mask(i,j)>maskX) maskX=mask(i,j)
    end do
    end do
end subroutine Maskcomp


subroutine TXTcalc15(var,txttype,varlen1,varlen2,fillv,txtout)
    !pdfflag: 1: Precip, 2: Insect, 3:LowN, 4: HighN
    !scanflag: 1: AT, 2: RT, 3: THI ->txttype
    !varflag: 1: LDR, 2: Zhh, 3: Zhv, 4: VRhh, 5: VRhv
    implicit NONE
    integer,intent(in)              :: txttype,varlen1,varlen2
    integer,intent(in)                 :: fillv
    real,dimension(:,:),intent(in)  :: var(varlen1,varlen2)
    real,dimension(:,:),intent(out) ::txtout(varlen1,varlen2)

    integer                         :: i,j,si,ei,sj,ej,ii,jj
    integer, parameter              :: itxt=33
    real                            :: s1, s2, m, c
    if (txttype.eq.3) then
        do i=1,varlen1
            do j=1,varlen2
             txtout(i,j)=var(i,j)
          end do
        end do
    elseif (txttype.eq.1) then
        do i = 1, varlen1
            do j = 1, varlen2
                if ( var(i,j).gt. -90 ) then
                    if ( j.le.itxt ) sj=1
                    if ( j.gt.itxt ) sj=j-itxt
                    if ( j.lt.varlen2-itxt ) ej=j+itxt
                    if ( j.ge.varlen2-itxt ) ej=varlen2
                    s1=0
                    s2=0
                    c=0
                    do jj = sj,ej
                        if ( var(i,jj).gt.-90 ) then
                            c=c+1
                            s1=s1+var(i,jj)
                            s2=s2+var(i,jj)**2
                        end if
                    end do
                    s1=s1/c
                    s2=s2/c
                    txtout(i,j)=sqrt(s2-s1**2)
                else
                    txtout(i,j)=fillv
                end if
            end do
        end do
    else
        do j = 1, varlen2
            do i = 1, varlen1
                if ( var(i,j).gt. -90 ) then
                    if ( i.lt.itxt ) si=1
                    if ( i.gt.itxt ) si=i-itxt
                    if ( i.lt.varlen1-itxt ) ei=i+itxt
                    if ( i.ge.varlen1-itxt ) ei=varlen1
                    s1=0
                    s2=0
                    c=0
                    do ii = si,ei
                        if ( var(ii,j).gt.-90 ) then
                            c=c+1
                            s1=s1+var(ii,j)
                            s2=s2+var(ii,j)**2
                        end if
                    end do
                    s1=s1/c
                    s2=s2/c
                    txtout(i,j)=sqrt(s2-s1**2)
                else
                    txtout(i,j)=fillv
                end if
            end do
        end do
    endif
end subroutine
