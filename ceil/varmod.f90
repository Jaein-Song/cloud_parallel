module varmod
    implicit none
        character(len=*), parameter     :: esign='-File'
        character(len=250)              :: idir,idir2, odir, odir2
        character(len=250)              :: dummy
        character(len=2)                :: ncbh
        character(len=1)                :: ch1
       
        integer                            :: ncbh1,ncbh2,ncbh3,ntest,startt,lv3_flag
        character(len=5)                   :: cdum1,cdum2,cdum3
        character                          :: ydm(11),hms(8)!,eflag(5)!,yyyy(4)
        integer                            :: da,mo,ho,mi,se,yyyy,nh,nt,k,l,stat,ii,jj,si,ei,li,tint
        character(len=7700)                :: bsc
        integer                            :: i,j,cflag,eflag,h2d!,eflag,
        REAL,ALLOCATABLE                   :: bscd(:,:),bscs(:,:),meds(:),cbh1o(:),cbh2o(:),cbh3o(:),cbh1(:),cbh2(:),cbh3(:),blh1(:),blh2(:),blh3(:),bli1(:),bli2(:),bli3(:)
        INTEGER,ALLOCATABLE                :: h(:),t(:),ts(:),hs(:)
        INTEGER,parameter                  :: nts=720,nhs=1540
        INTEGER,dimension(:),allocatable   :: count_bsc,count_cbh,count_blh
        INTEGER,dimension(:),allocatable   :: valid_cbh1,valid_blh1,valid_cbh2,valid_blh2,valid_cbh3,valid_blh3,valid_bli1,valid_bli2,valid_bli3
        INTEGER,dimension(:,:),allocatable :: valid_bsc
!Read lv3
        character(len=10)                  :: ymd, hms_lv3,CEILOMETER
        character(len=25)                  :: PARAMETERS
        integer                            :: UNIXTIME, PERIOD, SAMPLE_COUNT, BL_HEIGHT_1, BL_INDEX_1, BL_HEIGHT_2, BL_INDEX_2, BL_HEIGHT_3, BL_INDEX_3, CLOUD_STATUS, CLOUD_1, CLOUD_2, CLOUD_3
        integer,dimension(:),allocatable   :: lv3_time
        real,dimension(:),allocatable      :: lv3_blh1,lv3_blh2, lv3_blh3, lv3_bli1, lv3_bli2, lv3_bli3, lv3_cbh1,lv3_cbh2,lv3_cbh3
        integer                            :: ios,length,lengthi,hh,mm,ss

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
        !CHARACTER(len=*),PARAMETER      :: nam_bsct='Number of total observation of backscater',nam_bscv='Number of valid observation of backscater',nam_cbct='Number of total observation of cloud base height',nam_bhct='Number of total observation of boundary layer height'
        !CHARACTER(len=*),PARAMETER      :: nam_cbcv1='Number of valid observation of first cloud base height',nam_cbcv2='Number of valid observation of second cloud base height',nam_cbcv3='Number of valid observation of third cloud base height'
        !CHARACTER(len=*),PARAMETER      :: nam_bhcv1='Number of valid observation of first boundary layer height',nam_bhcv2='Number of valid observation of second boundary layer height',nam_bhcv3='Number of valid observation of third boundary layer height'
        !CHARACTER(len=*),PARAMETER      :: nam_bicv1='Number of valid observation of first boundary layer index',nam_bicv2='Number of valid observation of second boundary layer index',nam_bicv3='Number of valid observation of third boundary layer index'
        CHARACTER(len=*),PARAMETER      :: nam_tims='time_avg',nam_hgts='height',nam_bscs='backscatter_avg'
        CHARACTER(len=*),PARAMETER      :: unt_tim='UTC',unt_hgt='m AGL',unt_bsc='1/(srad*m)'
        INTEGER                         :: ncid,status
        INTEGER                         :: dim_tim,dim_hgt,vid_tim,vid_hgt,dims(2),vid_bsc,dimss(2),vid_bscs,dim_tims,dim_hgts,vid_tims,vid_hgts
        Integer                         :: vid_cbh1,vid_cbh2,vid_cbh3,vid_cbh1s,vid_cbh2s,vid_cbh3s,vid_blh1s,vid_blh2s,vid_blh3s,vid_bli1s,vid_bli2s,vid_bli3s
        Integer                         :: vid_bsct,vid_bscv,vid_cbct,vid_cbcv1,vid_cbcv2,vid_cbcv3,vid_bhct,vid_bhcv1,vid_bhcv2,vid_bhcv3,vid_bicv1,vid_bicv2,vid_bicv3
        REAL,parameter                  :: fillvalue=-32768
        REAL,parameter                  :: fv=-32768

end module varmod
