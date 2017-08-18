SUBROUTINE noqcavg 
!no quality check, just average
!Cloud count only if when the echo satisfies below condition

!0. Echo existance: Echo must exist
!1. Average data
!2. Fill Fillvalue if echo is not valid
    USE varmod
    IMPLICIT NONE
    ALLOCATE(Qflag(ftlen),Qflagv(ftlen))

    ALLOCATE(RefhO(binlen),VelhO(binlen),SpWhO(binlen),SNRhO(binlen),LDRaO(binlen))
    ALLOCATE(RefvO(binlen),VelvO(binlen),SpWvO(binlen),SNRvO(binlen))

    DO hi=1,binlen
        Qflags=0
        Qflagsv=0
        DO ti=1,ftlen
        Qflag(ti)=0
        Qflagv(ti)=0
!0. Echo existance
            IF (RefhI(ti,hi).gt.-100) then
                RefhF=1
            else
                RefhF=0
            ENDIF
            IF (RefvI(ti,hi).gt.-100) then
                RefvF=1
            else
                RefvF=0
            ENDIF
            Qflag(ti)=RefhF
            Qflags=Qflags+Qflag(ti)
            Qflagv(ti)=RefvF
            Qflagsv=Qflagsv+Qflagv(ti)
        ENDDO
!existance check
            IF ( Qflags.gt.0 ) then
!1. Average Data
            Refhs=0;
            Refvs=0;
            Velhs=0;
            Velvs=0;
            SpWhs=0;
            SpWvs=0;
            SNRhs=0;
            SNRvs=0;
            LDRas=0;
               do ti = 1, ftlen
                   Refh10=10**(0.1*RefhI(ti,hi))
                   Refhs=Refh10*Qflag(ti)+Refhs
                   Velhs=VelhI(ti,hi)*Qflag(ti)+Velhs
                   SpWhs=SpWhI(ti,hi)*Qflag(ti)+SpWhs
                   SNRh10=10**(0.1*SNRhI(ti,hi))
                   SNRhs=SNRh10*Qflag(ti)+SNRhs
                   !LDRa10=10**(0.1*LDRaI(ti,hi))
                   !LDRas=LDRa10*Qflag(ti)+LDRas
               end do
               RefhO(hi)=(10*log10(Refhs/Qflags))/Zsf
               VelhO(hi)=(Velhs/Qflags)/Vsf
               SpWhO(hi)=(SpWhs/Qflags)/Wsf
               SNRhO(hi)=(10*log10(SNRhs/Qflags))/Ssf
               if (Qflagsv.gt.0)then
                    do ti=1,ftlen
                        Refv10=10**(0.1*RefvI(ti,hi))
                        Refvs=Refv10*Qflagv(ti)+Refvs
                        Velvs=VelvI(ti,hi)*Qflagv(ti)+Velvs
                        SpWvs=SpWvI(ti,hi)*Qflagv(ti)+SpWvs
                        SNRv10=10**(0.1*SNRvI(ti,hi))
                        SNRvs=SNRv10*Qflagv(ti)+SNRvs
                    enddo
                    RefvO(hi)=(10*log10(Refvs/Qflagsv))/Zsf
                    VelvO(hi)=(Velvs/Qflagsv)/Vsf
                    SpWvO(hi)=(SpWvs/Qflagsv)/Wsf
                    SNRvO(hi)=(10*log10(SNRvs/Qflagsv))/Ssf
                    LDRas=10*log10(Refvs/Qflagsv)-10*log10(Refhs/Qflagsv)
                    LDRaO(hi)=LDRas/Lsf
                    else
                    RefvO(hi)=fv
                    VelvO(hi)=fv
                    SpWvO(hi)=fv
                    SNRvO(hi)=fv
                    LDRaO(hi)=fv
               endif
               !LDRaO(hi)=(10*log10(LDRas/Qflags))/Lsf
!Average Data done
            else
!2. Fill Fillvalue if not valid
               RefhO(hi)=fv
               RefvO(hi)=fv
               VelhO(hi)=fv
               VelvO(hi)=fv
               SpWhO(hi)=fv
               SpWvO(hi)=fv
               SNRhO(hi)=fv
               SNRvO(hi)=fv
               LDRaO(hi)=fv
            ENDIF
    ENDDO
DEALLOCATE(Qflag,Qflagv)
DEALLOCATE(RefhI,VelhI,SpWhI,SNRhI,LDRaI)
DEALLOCATE(RefvI,VelvI,SpWvI,SNRvI)
END SUBROUTINE noqcavg