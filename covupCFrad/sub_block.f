!= (SUBROUTINE) /READ/ Base data ==========================================!

      SUBROUTINE read_base

      USE mod_para

      IMPLICIT NONE

      rn=RayNum ; bn=BinNum

      ALLOCATE(RayNo(rn))
      ALLOCATE(RecTime(rn))
      ALLOCATE(AZ(rn))
      ALLOCATE(EL(rn))
      ALLOCATE(VolLayNo(rn))
      ALLOCATE(WavePRF(rn))

      ALLOCATE(LDR(rn,bn))
      ALLOCATE(Z1(rn,bn))  ;  ALLOCATE(Vr1(rn,bn))
      ALLOCATE(SW1(rn,bn)) ;  ALLOCATE(SNR1(rn,bn))
      ALLOCATE(Z2(rn,bn))  ;  ALLOCATE(Vr2(rn,bn))
      ALLOCATE(SW2(rn,bn)) ;  ALLOCATE(SNR2(rn,bn))
      ALLOCATE(Z1o(rn,bn))  ;  ALLOCATE(Vr1o(rn,bn))
      ALLOCATE(SW1o(rn,bn)) ;  ALLOCATE(SNR1o(rn,bn))
      ALLOCATE(Z2o(rn,bn))  ;  ALLOCATE(Vr2o(rn,bn))
      ALLOCATE(SW2o(rn,bn)) ;  ALLOCATE(SNR2o(rn,bn))
      ALLOCATE(LDRo(rn,bn))

      ir=1
      sflag=0
      DO while ((ir.le.rn).and.(sflag.eq.0))
      READ(1,IOSTAT=sflag) RayNo(ir), RecTime(ir), AZ(ir), EL(ir), 
     & VolLayNo(ir), WavePRF(ir)
      if (sflag.eq.0) then
      READ(1,IOSTAT=sflag) (LDR(ir,ib),ib=1,bn)    ! Linear Depolarization Ratio LDR

      READ(1,IOSTAT=sflag) (Z1(ir,ib),ib=1,bn)     ! Reflection rate factor Z (Ch.1)
      READ(1,IOSTAT=sflag) (Vr1(ir,ib),ib=1,bn)    ! Radial speed (Ch.1)
      READ(1,IOSTAT=sflag) (SW1(ir,ib),ib=1,bn)    ! Speed spectrum width (Ch.1)
      READ(1,IOSTAT=sflag) (SNR1(ir,ib),ib=1,bn)   ! Signal Noise Ratio with ground obj.(Ch.1)

      READ(1,IOSTAT=sflag) (Z2(ir,ib),ib=1,bn)     ! Reflection rate factor Z (Ch.2)
      READ(1,IOSTAT=sflag) (Vr2(ir,ib),ib=1,bn)    ! Radial speed (Ch.2)
      READ(1,IOSTAT=sflag) (SW2(ir,ib),ib=1,bn)    ! Speed spectrum width (Ch.2)
      READ(1,IOSTAT=sflag) (SNR2(ir,ib),ib=1,bn)   ! Signal Noise Ratio with ground obj.(Ch.2)
      ir=ir+1
      endif
      ENDDO
      ir=ir-1
      Z1o=Z1
      Z2o=Z2
      Vr1o=Vr1
      Vr2o=Vr2
      SW1o=SW1
      SW2o=sw2
      SNR1o=SNR1
      SNR2o=SNR2
      LDRo=LDR
      ENDSUBROUTINE read_base
