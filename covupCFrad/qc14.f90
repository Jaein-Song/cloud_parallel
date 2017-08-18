SUBROUTINE qc14
!Quallity Check & Merge data
!Cloud count only if when the echo satisfies below condition
!0.     INITIALLIZE: set module, allocate vars                      
!1.     Mean LDR test: nCase 1 or 3                                 }┐
!2.     Case QC                                                     }|
!2.1.   Case 1. :remove 'insect' cell                               }|
!2.2.   Case 2&3 (LDR<-15) : TXT test                               }├ KNU QC14 ALGORITHM (COURTESY OF BO YOUNG YE)
!2.3.   Case 2.                                                     }|
!2.4.   Case 3. no special processes                                }|
!2.5.   for Case 2 and 3, low level (<300m) filter with velocity    }┘
!3.     Average data                                                }┐
!3.0.   Echo existance                                              }├ J.E.LEE averaging
!3.1.   Average Data                                                }|
!3.2.   Fill Fillvalue if not valid                                 }┘
!4.     DEALLOCATE


!0. INITIALLIZE: set module, allocate vars
    USE varmod
    use mod_para
    IMPLICIT NONE
    ALLOCATE(Qflag(ftlenC),Qflagv(ftlenC))
    ALLOCATE(RefhI(ftlenC,binlenC),VelhI(ftlenC,binlenC),SpWhI(ftlenC,binlenC),SNRhI(ftlenC,binlenC),LDRaI(ftlenC,binlenC),RefhDOS(ftlenC,binlenC),VelhDOS(ftlenC,binlenC))
    ALLOCATE(RefvI(ftlenC,binlenC),VelvI(ftlenC,binlenC),SpWvI(ftlenC,binlenC),SNRvI(ftlenC,binlenC),LDRTXT(ftlenC,binlenC),LDRDOS(ftlenC,binlenC),VelhTXT(ftlenC,binlenC))
    ALLOCATE(LDRlow(ftlenC,71),LDRTXTlow(ftlenC,71))
    print*,ftlenC,binlenC,rn,bn
    print*,size(RefhI),size(Z1o)
RefhI=Z1o
RefvI=Z2o
VelhI=Vr1o
VelvI=Vr2o
SpWhI=SW1o
SpWvI=SW2o
SNRhI=SNR1o
SNRvI=SNR2o
LDRaI=LDRo
LDRas=0
LDRmi=0
cellwh=(cellsize-1)/2
!1. Mean LDR test
DO ti=1,ftlenC
    DO hi=31,101
        LDRlow(ti,hi-30)=LDRaI(ti,hi)
        IF (LDRaI(ti,hi).gt.-100) then
            LDRas=LDRas+LDRaI(ti,hi)
            LDRmi=LDRmi+1
        ENDIF
    ENDDO
ENDDO
IF (LDRmi.gt.0) then
    LDRas=LDRas/LDRmi
    IF (LDRas.ge.-15) then
        nCase=1
    ELSE
        nCase=3
    ENDIF
ELSE
    nCase=3
ENDIF
print*, LDRas, LDRmi
!2. Case QC
IF (nCase.eq.1) THEN
!2.1. Case 1. 
!remove 'insect' cell
    do ti = 1, ftlenC
        do hi = 1, binlenC
            if ( LDRaI(ti,hi).ge.-15 ) then
                LDRaI(ti,hi)=fv
            end if
        end do
    end do
    call DOScalc(LDRaI,fv,ftlenC,binlenC,cellwh,LDRDOS)
    !do DOS filtering
    do ti = 1, ftlenC
        do hi = 1, 333
            if ( LDRDOS(ti,hi).lt.47 ) then
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
        do hi = 334, binlenC
            if ( LDRDOS(ti,hi).lt.47 ) then
                RefvI(ti,hi)=fv
                VelvI(ti,hi)=fv
                SpWvI(ti,hi)=fv
                SNRvI(ti,hi)=fv
                LDRaI(ti,hi)=fv
            end if
        end do
    end do
ELSE
!2.2. Case 2&3 (LDR<-15) : TXT test 
    call TXTcalc(LDRlow,fv,ftlenC,71,cellwh,LDRTXTlow,LDRTXTm)
    IF (LDRTXTm.gt.2) then
!2.3. Case 2.
        nCase=2 !mixed
        call TXTcalc(LDRaI,fv,ftlenC,binlenC,cellwh,LDRTXT,LDRTXTm)
        do ti=1,ftlenC
            do hi=1,binlenC
                if ((LDRTXT(ti,hi).ge.4).or.(LDRaI(ti,hi).ge.0)) then
                RefhI(ti,hi)=fv
                VelhI(ti,hi)=fv
                SpWhI(ti,hi)=fv
                SNRhI(ti,hi)=fv
                RefvI(ti,hi)=fv
                VelvI(ti,hi)=fv
                SpWvI(ti,hi)=fv
                SNRvI(ti,hi)=fv
                LDRaI(ti,hi)=fv
                endif
            enddo
        enddo
        call DOScalc(LDRaI,fv,ftlenC,binlenC,cellwh,LDRDOS)
        call DOScalc(RefhI,fv,ftlenC,binlenC,cellwh,RefhDOS)
        do ti = 1, ftlenC
            do hi = 1, 333
                IF (LDRDOS(ti,hi).lt.15) then
                    RefvI(ti,hi)=fv
                    VelvI(ti,hi)=fv
                    SpWvI(ti,hi)=fv
                    SNRvI(ti,hi)=fv
                    LDRaI(ti,hi)=fv
                ENDIF
                IF (RefhDOS(ti,hi).lt.15) then
                    RefhI(ti,hi)=fv
                    VelhI(ti,hi)=fv
                    SpWhI(ti,hi)=fv
                    SNRhI(ti,hi)=fv
                ENDIF
            end do
        end do
    ELSE
!2.4. Case 3. no special processes
        nCase=3
    ENDIF
!2.5. for Case 2 and 3, low level (<300m) filter with velocity
call TXTcalc(VelhI,fv,ftlenC,binlenC,cellwh,VelhTXT,VelhTXTm)
    do ti = 1, ftlenC
        do hi=1,20
            if ( VelhTXT(ti,hi).ge.5 ) then
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
    call DOScalc(VelhI,fv,ftlenC,binlenC,cellwh,VelhDOS)
    do ti = 1, ftlenC
        do hi=1,20
            if ( VelhDOS(ti,hi).lt.5 ) then
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
ENDIF
print*,nCase
Z1=RefhI
Z2=RefvI
Vr1=VelhI
Vr2=VelvI
SW1=SpWhI
SW2=SpWvI
SNR1=SNRhI
SNR2=SNRvI
LDR=LDRaI
    DEALLOCATE(Qflag,Qflagv)
    DEALLOCATE(RefhI,VelhI,SpWhI,SNRhI,LDRaI,RefhDOS,VelhDOS)
    DEALLOCATE(RefvI,VelvI,SpWvI,SNRvI,LDRTXT,LDRDOS,VelhTXT)
    DEALLOCATE(LDRlow,LDRTXTlow)
END SUBROUTINE qc14


SUBROUTINE TXTcalc(var,fillvalue,varlen1,varlen2,cwh,TXT,TXTmean)
IMPLICIT NONE
integer,            intent(in)  :: varlen1,varlen2,cwh,fillvalue
real,dimension(:,:),intent(in)  :: var(varlen1,varlen2)
integer                         :: TXTi,TXTj,TXTcelli,TXTcellj,varmi,varcount
integer,dimension(:,:)          :: nanflag(varlen1,varlen2)
real                            :: varms,varmst
real,               intent(out) :: TXTmean
real,dimension(:,:),intent(out) :: TXT(varlen1,varlen2)

varmst=0
varcount=0

DO TXTi=1+cwh,varlen1-cwh
    DO TXTj=1+cwh,varlen2-cwh
        varmi=0
        varms=0
        !in-cell calculation
        DO TXTcelli=TXTi-cwh,TXTi+cwh
            DO TXTcellj=TXTj-cwh,TXTj+cwh
                IF (var(TXTcelli,TXTcellj).gt.-100) THEN
                    varmi=varmi+1
                    varms=(var(TXTcelli,TXTcellj)-var(TXTi,TXTj))**2.+varms
                ENDIF
            ENDDO
        ENDDO
        IF (varmi.ge.1) then
            varms=varms/varmi
            TXT(TXTi,TXTj)=varms
        ELSE
            TXT(TXTi,TXTj)=fillvalue
        ENDIF
        if (var(TXTi,TXTj).gt.-100) then
            varmst=varmst+TXT(TXTi,TXTj)
            varcount=varcount+1
        endif
        !End in-cell calculation
    ENDDO
ENDDO
IF (varcount.gt.0) then
    TXTmean=varmst/varcount
ELSE
    TXTmean=fillvalue
ENDIF
ENDSUBROUTINE TXTcalc

!DOS calculation
SUBROUTINE DOScalc(var,fillvalue,varlen1,varlen2,cwh,DOS)
IMPLICIT NONE
integer,            intent(in)     :: varlen1,varlen2,cwh,fillvalue
real,dimension(:,:),intent(in)     :: var(varlen1,varlen2)
integer                            :: DOSi,DOSj,DOScelli,DOScellj,varmi,varcount
integer,dimension(:,:),intent(out) :: DOS(varlen1,varlen2)

do DOSi = 1+cwh, varlen1-cwh
    do DOSj = 1+cwh, varlen2-cwh
    varcount=0
    !in-cell count
        do DOScelli = DOSi-cwh, DOSi+cwh
            do DOScellj = DOSj-cwh, DOSj+cwh
                if ( var(DOScelli,DOScellj).gt.-90 ) varcount=varcount+1
            end do
        end do
        DOS(DOSi,DOSj)=varcount
    end do
end do
ENDSUBROUTINE DOScalc
