SUBROUTINE qc17avg
!Quallity Check & Merge data
!Cloud count only if when the echo satisfies below condition
!0. ALLOCATE and initialize variables
!1. COPOL QC
!   1.0. Echo existance: Echo must exist
!   1.1. Radial Velhoity must be in the range -4 and 5
!   1.2. EXCEPTION: If Z is higher than 0, then the range be -10 and 5
!        Reason for this filter: Vertical Cloud and rain drops are very small, and for noise, it flucates
!   1.3 LDR condition: echo must be smaller than -5 (not in absolute value)
!        Reason for this filter:In common, LDR is smaller than -10, and since it has other filters, it could be more generous for graupels or etc
!2. Xpol QC
!   2.0. xpol Echo existance
!   2.1. Radial Velocity condition
!   2.2.Exception for Radial Velocity condition
!3. Data process
!   3.1. Frequency Check: If 80% of echo at same altitude satisfies the QC above
!   3.2. Average data
!4. Data process for xpol (only if copol exists)
!   4.1. Frequency Check
!   4.2. Average Data
!   4.3. Fill fillvalue for xpol
!5. Fill Fillvalue if echo is not valid

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
!1.  COPOL QC
!1.0 Echo existance
            IF (RefhI(ti,hi).gt.-100) then
                RefhF=1
!1.1 Radial Velocity condition
                IF (VelhI(ti,hi).lt.5) then
                    IF (RefhI(ti,hi).lt.0) then
                        IF (VelhI(ti,hi).gt.-5) then
                            VelhF=1
                        else
                            VelhF=0
                        ENDIF
                    else
!1.2. Exception Z>0: V>-10
                         IF (VelhI(ti,hi).gt.-10) then
                            VelhF=1
                        else
                            VelhF=0
                        ENDIF
                    ENDIF
                else
                    VelhF=0
                ENDIF
                if ((RefhI(ti,hi).lt.-25).and.(hi.le.200)) then !under 3km(regarded as boundary layer) no convective cloud, but at the top of the convective cloud, could be
                    if ( (VelhI(ti,hi).lt.1).and.(VelhI(ti,hi).gt.-1) ) then
                        VelhF=1
                    else
                        velhf=0
                    end if
                endif
!copol Velocity Condition Check END
!1.3. LDR check
                IF (RefhI(ti,hi).gt.0) then !MELTINGLAYAER
                        IF (LDRaI(ti,hi).lt.-5) then
                                LDRaF=1
                        else   
                                LDRaF=0
                        ENDIF
                else
                        IF (LDRaI(ti,hi).lt.-15) then !INSECT/DUST CLUTTER, EXCEPTION FOR ICE (Sato and Okamoto, 2006; JGR)
                                LDRaF=1
                        else   
                                LDRaF=0
                        ENDIF
                endif
            else
                RefhF=0
            ENDIF
!Do the same thing for the xpol
!2.0 xpol Echo existance
            IF (RefvI(ti,hi).gt.-100) then
                RefvF=1
!2.1 Radial Velocity condition
                IF (VelvI(ti,hi).lt.5) then
                    IF (RefvI(ti,hi).lt.0) then
                        IF (VelvI(ti,hi).gt.-5) then
                            VelvF=1
                        else
                            VelvF=0
                        ENDIF
                    else
!2.2. Exception Z>0: V>-10
                         IF (VelvI(ti,hi).gt.-10) then
                            VelvF=1
                        else
                            VelvF=0
                        ENDIF
                    ENDIF
                else
                    VelvF=0
                ENDIF
!Velocity Condition Check END
            else
                RefvF=0
            ENDIF
            Qflagv(ti)=RefvF*VelvF
            Qflagsv=Qflagsv+Qflagv(ti)
            Qflag(ti)=RefhF*VelhF*LDRaF
            Qflags=Qflags+Qflag(ti)
        ENDDO
!3. Data process for COPOL
!3.1. Frequency Check
            IF ( Qflags.ge.ratio_qc17*ftlen ) then
!3.2. Average Data
            Refhs=0;
            Velhs=0;
            SpWhs=0;
            SNRhs=0;
               do ti = 1, ftlen
                   Refh10=10**(0.1*RefhI(ti,hi))
                   Refhs=Refh10*Qflag(ti)+Refhs
                   Velhs=VelhI(ti,hi)*Qflag(ti)+Velhs
                   SpWhs=SpWhI(ti,hi)*Qflag(ti)+SpWhs
                   SNRh10=10**(0.1*SNRhI(ti,hi))
                   SNRhs=SNRh10*Qflag(ti)+SNRhs
               end do
               RefhO(hi)=(10*log10(Refhs/Qflags))/Zsf
               VelhO(hi)=(Velhs/Qflags)/Vsf
               SpWhO(hi)=(SpWhs/Qflags)/Wsf
               SNRhO(hi)=(10*log10(SNRhs/Qflags))/Ssf
!4. Data process for xpol only if copol data exisist
!4.1. frequency check for xpol
               IF ( Qflagsv.ge.ratio_qc17*ftlen ) then
!4.2. Average Data for xpol
               Refvs=0;
               Velvs=0;
               SpWvs=0;
               SNRvs=0;
                    do ti = 1, ftlen
                        Refv10=10**(0.1*RefvI(ti,hi))
                        Refvs=Refv10*Qflagv(ti)+Refvs
                        Velvs=VelvI(ti,hi)*Qflagv(ti)+VelvI(ti,hi)
                        SpWvs=SpWvI(ti,hi)*Qflagv(ti)+SpWvs
                        SNRv10=10**(0.1*SNRvI(ti,hi))
                        SNRvs=SNRv10*Qflagv(ti)+SNRvs
                    end do
                    RefvO(hi)=(10*log10(Refvs/Qflagsv))/Zsf
                    VelvO(hi)=(Velvs/Qflagsv)/Vsf
                    SpWvO(hi)=(SpWvs/Qflagsv)/Wsf
                    SNRvO(hi)=(10*log10(SNRvs/Qflags))/Ssf
                    LDRas=10*log10(Refvs/Qflagsv)-10*log10(Refhs/Qflags)
                    LDRaO(hi)=LDRas/Lsf
!Average Data done
               else
!4.3 Fill Fillvalue if not valid for xpol
                    RefvO(hi)=fv
                    VelvO(hi)=fv
                    SpWvO(hi)=fv
                    SNRvO(hi)=fv
                    LDRaO(hi)=fv
               ENDIF
            !endif
!Average Data done
            else
!5 Fill Fillvalue if not valid
               RefhO(hi)=fv
               VelhO(hi)=fv
               SpWhO(hi)=fv
               SNRhO(hi)=fv
               RefvO(hi)=fv
               VelvO(hi)=fv
               SpWvO(hi)=fv
               SNRvO(hi)=fv
               LDRaO(hi)=fv
            ENDIF
!        ENDDO
    ENDDO
    DEALLOCATE(Qflag,Qflagv)
    DEALLOCATE(RefhI,VelhI,SpWhI,SNRhI,LDRaI)
    DEALLOCATE(RefvI,VelvI,SpWvI,SNRvI)
END SUBROUTINE qc17avg
