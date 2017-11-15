module varmod
    implicit none
        character(len=*), parameter     :: esign='-File'
        character(len=250)              :: idir,idir2, odir, odir2
        character(len=250)              :: dummy
        character(len=2)                :: ncbh
        character(len=1)                :: awstat,Ptypes,Gtypes,Btypes
        character(len=4)                :: warning,alarm,state
        integer                            :: ncbh1,ncbh2,ncbh3,ntest,startt,lv3_flag,ios,iosflag,bin_index
        character(len=5)                   :: cdum1,cdum2,cdum3
        character                          :: ydm(11),hms(8)!,eflag(5)!,yyyy(4)
        integer                            :: da,mo,ho,mi,se,yyyy,nh,nt,k,l,stat,ii,jj,si,ei,li,tint,nts
        character(len=7700)                :: bsc
        integer                            :: i,j,cflag,eflag,h2d,ch1,mflag!,eflag,
        REAL,ALLOCATABLE                   :: bscd(:,:),bscs(:,:),meds(:),cbh1o(:),cbh2o(:),cbh3o(:),cbh1(:),cbh2(:),cbh3(:),blh1(:),blh2(:),blh3(:),bli1(:),bli2(:),bli3(:),vviso(:),vvis(:),sumbo(:),sumb(:)
        INTEGER,ALLOCATABLE                :: h(:),t(:),ts(:),hs(:),scalefo(:),hreso(:),hleno(:),Lengo(:),Ltempo(:),Wtranso(:),Tangleo(:),Blito(:),Ptypeo(:),Gtypeo(:),Btypeo(:),sampro(:),QCflag(:),QCflags(:),wflags(:),aflags(:),sflags(:)
        INTEGER,parameter                  :: nhs=1540
        INTEGER,dimension(:),allocatable   :: count_bsc,count_cbh,count_blh
        INTEGER,dimension(:),allocatable   :: valid_cbh1,valid_blh1,valid_cbh2,valid_blh2,valid_cbh3,valid_blh3,valid_bli1,valid_bli2,valid_bli3,valid_vvis,count_vvis,valid_sumb,count_sumb,count_
        INTEGER,dimension(:,:),allocatable :: valid_bsc,alarm_flag,warning_flag,status_flag,aflaga,sflaga,wflaga
!Read lv3
        character(len=10)                  :: ymd, hms_lv3,CEILOMETER
        character(len=25)                  :: PARAMETERS
        integer                            :: UNIXTIME, PERIOD, SAMPLE_COUNT, BL_HEIGHT_1, BL_INDEX_1, BL_HEIGHT_2, BL_INDEX_2, BL_HEIGHT_3, BL_INDEX_3, CLOUD_STATUS, CLOUD_1, CLOUD_2, CLOUD_3
        integer,dimension(:),allocatable   :: lv3_time
        real,dimension(:),allocatable      :: lv3_blh1,lv3_blh2, lv3_blh3, lv3_bli1, lv3_bli2, lv3_bli3, lv3_cbh1,lv3_cbh2,lv3_cbh3
        INTEGER                            :: vid_qcflag,vid_aflag,vid_wflag
        integer                            :: length,lengthi,hh,mm,ss

!NETCDF
        CHARACTER(len=*),PARAMETER      :: nam_tim='time',nam_hgt='height',nam_bsc='backscatter',att_unt='units'
        CHARACTER(len=*),PARAMETER      :: nam_cbh1='first_cbh',nam_cbh2='second_cbh',nam_cbh3='third_cbh'
        CHARACTER(len=*),PARAMETER      :: nam_cbh1s='first_cbh_avg',nam_cbh2s='second_cbh_avg',nam_cbh3s='third_cbh_avg'
        CHARACTER(len=*),PARAMETER      :: nam_blh1s='first_blh_avg',nam_blh2s='second_blh_avg',nam_blh3s='third_blh_avg'
        CHARACTER(len=*),PARAMETER      :: nam_bli1s='first_bli_avg',nam_bli2s='second_bli_avg',nam_bli3s='third_bli_avg'
        CHARACTER(len=*),PARAMETER      :: nam_bsct='nbackscatter',nam_bscv='valid_backscatter',nam_cbct='ncbh',nam_bhct='nblh'
        CHARACTER(len=*),PARAMETER      :: nam_cbcv1='valid_cbh1',nam_cbcv2='valid_cbh2',nam_cbcv3='valid_cbh3'
        CHARACTER(len=*),PARAMETER      :: nam_bhcv1='valid_blh1',nam_bhcv2='valid_blh2',nam_bhcv3='valid_blh3'
        CHARACTER(len=*),PARAMETER      :: nam_bicv1='valid_bli1',nam_bicv2='valid_bli2',nam_bicv3='valid_bli3'
        CHARACTER(len=*),PARAMETER      :: nam_vviso='Vertical_Visibility',nam_vvis='Vertical_Visibility_avg'
        CHARACTER(len=*),PARAMETER      :: nam_tims='time_avg',nam_hgts='height',nam_bscs='backscatter_avg'
        CHARACTER(len=*),PARAMETER      :: unt_tim='UTC',unt_hgt='m AGL',unt_bsc='10^-8/(srad*m)',unt_sumb='10^-4/srad'
        CHARACTER(len=*),PARAMETER      :: nam_scale='scale(%)',nam_hres='height resolution (m)',nam_hlen='Length',nam_Leng='Laser Energy', nam_temp='Laser Temperature (C)', nam_Wtrans='Window Transmission (%)', nam_Tangle='Tilt Angle (deg)', nam_Blit='Background Light',nam_Ptype='Pulse type (1:Long, 0:short)',nam_Gtype='Gain type (1:High, 0:Low)',nam_Btype='Bandwidth (1:Narrow, 0:Wide)',nam_sampr='Sampling rate', nam_sumb='Sum of detected and normalized backscatter'
        CHARACTER(len=*),PARAMETER      :: nam_qcflag="QCflag",nam_aflag="number of alarams",nam_wflag="number of warnings"
        INTEGER                         :: ncid,status
        INTEGER                         :: dim_tim,dim_hgt,vid_tim,vid_hgt,dims(2),vid_bsc,dimss(2),vid_bscs,dim_tims,dim_hgts,vid_tims,vid_hgts
        Integer                         :: vid_vviso,vid_vvis,vid_cbh1,vid_cbh2,vid_cbh3,vid_cbh1s,vid_cbh2s,vid_cbh3s,vid_blh1s,vid_blh2s,vid_blh3s,vid_bli1s,vid_bli2s,vid_bli3s
        Integer                         :: vid_bsct,vid_bscv,vid_cbct,vid_cbcv1,vid_cbcv2,vid_cbcv3,vid_bhct,vid_bhcv1,vid_bhcv2,vid_bhcv3,vid_bicv1,vid_bicv2,vid_bicv3
        INTEGER                         :: vid_scalefo,vid_hreso,vid_hleno,vid_Lengo,vid_Ltempo,vid_Wtranso,vid_Tangleo,vid_Blito,vid_Ptypeo,vid_Gtypeo,vid_Btypeo,vid_sampro,vid_sumbo,vid_sumb
        REAL,parameter                  :: fillvalue=-32768
        REAL,parameter                  :: fv=-32768
!ERROR MSG
        INTEGER                         ::  vid_alm01, vid_alm02, vid_alm03, vid_alm04, vid_alm06, vid_alm07, vid_alm08, vid_alm15,vid_alm16
        INTEGER                         ::  vid_wrn01, vid_wrn02, vid_wrn03, vid_wrn04, vid_wrn06, vid_wrn08, vid_wrn09, vid_wrn10,vid_wrn11,vid_wrn12,vid_wrn13,vid_wrn14,vid_wrn15
        INTEGER                         ::  vid_sta01, vid_sta02, vid_sta03, vid_sta04,  vid_sta05, vid_sta06,  vid_sta07, vid_sta09, vid_sta10,vid_sta11
        CHARACTER(len=*),PARAMETER      :: nam_alm01="Alarm01"
        CHARACTER(len=*),PARAMETER      :: nam_alm02="Alarm02"
        CHARACTER(len=*),PARAMETER      :: nam_alm03="Alarm03"
        CHARACTER(len=*),PARAMETER      :: nam_alm04="Alarm04"
        CHARACTER(len=*),PARAMETER      :: nam_alm06="Alarm05"
        CHARACTER(len=*),PARAMETER      :: nam_alm07="Alarm06"
        CHARACTER(len=*),PARAMETER      :: nam_alm08="Alarm07"
        CHARACTER(len=*),PARAMETER      :: nam_alm15="Alarm08"
        CHARACTER(len=*),PARAMETER      :: nam_alm16="Alarm09"

        CHARACTER(len=*),PARAMETER      :: nam_wrn01="Warning01"
        CHARACTER(len=*),PARAMETER      :: nam_wrn02="Warning02"
        CHARACTER(len=*),PARAMETER      :: nam_wrn03="Warning03"
        CHARACTER(len=*),PARAMETER      :: nam_wrn04="Warning04"
        CHARACTER(len=*),PARAMETER      :: nam_wrn06="Warning05"
        CHARACTER(len=*),PARAMETER      :: nam_wrn08="Warning06"
        CHARACTER(len=*),PARAMETER      :: nam_wrn09="Warning07"
        CHARACTER(len=*),PARAMETER      :: nam_wrn10="Warning08"
        CHARACTER(len=*),PARAMETER      :: nam_wrn11="Warning09"
        CHARACTER(len=*),PARAMETER      :: nam_wrn12="Warning10"
        CHARACTER(len=*),PARAMETER      :: nam_wrn13="Warning11"
        CHARACTER(len=*),PARAMETER      :: nam_wrn14="Warning12"
        CHARACTER(len=*),PARAMETER      :: nam_wrn15="Warning13"

        CHARACTER(len=*),PARAMETER      :: nam_sta01="Status01"
        CHARACTER(len=*),PARAMETER      :: nam_sta02="Status02"
        CHARACTER(len=*),PARAMETER      :: nam_sta03="Status03"
        CHARACTER(len=*),PARAMETER      :: nam_sta04="Status04"
        CHARACTER(len=*),PARAMETER      :: nam_sta05="Status05"
        CHARACTER(len=*),PARAMETER      :: nam_sta06="Status06"
        CHARACTER(len=*),PARAMETER      :: nam_sta07="Status07"
        CHARACTER(len=*),PARAMETER      :: nam_sta09="Status08"
        CHARACTER(len=*),PARAMETER      :: nam_sta10="Status09"
        CHARACTER(len=*),PARAMETER      :: nam_sta11="Status10"
        
        CHARACTER(len=*),PARAMETER      :: att_alm01="[Alarm] Transmitter shut-off if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm02="[Alarm] Transmitter failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm03="[Alarm] Receiver failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm04="[Alarm] Voltage dailure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm06="[Alarm] Memory Error if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm07="[Alarm] Light path obstruction if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm08="[Alarm] Receiver saturation if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm15="[Alarm] Coaxial cable failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_alm16="[Alarm] Ceilometer engine board failure if value is 1"

        CHARACTER(len=*),PARAMETER      :: att_wrn01="[Warning] Window Contamination if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn02="[Warning] Battery voltage low if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn03="[Warning] Transmitter expires if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn04="[Warning] High humidity if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn06="[Warning] Blower failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn08="[Warning] Humidity sensor failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn09="[Warning] Heater fault if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn10="[Warning] High background radiance if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn11="[Warning] Ceilometer engine board failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn12="[Warning] Battery failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn13="[Warning] Laser monitor failure if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn14="[Warning] Receiver warning if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_wrn15="[Warning] Tilt angle > 45 degrees warning if value is 1"

        CHARACTER(len=*),PARAMETER      :: att_sta01="[Status] Blower is on if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta02="[Status] Blower heater is on if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta03="[Status] Internal heater is on if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta04="[Status] Working from battery if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta05="[Status] Standby mode is on if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta06="[Status] Self test in progress if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta07="[Status] Manual data acquistion settings are effective if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta09="[Status] Units are meters if on if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta10="[Status] Manual blower control if value is 1"
        CHARACTER(len=*),PARAMETER      :: att_sta11="[Status] Polling mode is on if value is 1"
end module varmod
