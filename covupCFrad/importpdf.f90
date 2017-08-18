subroutine importpdf(pdfflag,scanflag,varflag,pdf)
!pdfflag: 1: Precip, 2: Insect, 3:LowN, 4: HighN
!scanflag: 1: AT, 2: RT, 3: THI
!varflag: 1: LDR, 2: Zhh, 3: Zhv, 4: VRhh, 5: VRhv
integer,intent(in)              :: pdfflag,scanflag,varflag
real,dimension(:,:),intent(out) :: pdf(100,2)
integer                         :: i
character(len=*),parameter      ::pdfprefix='./covupCFrad/PDF/TOTAL_PDF_'
character(len=*),parameter      ::pdfendfix='_smooth.txt'
character(len=6)                :: pdfname
character(len=3)                :: scanname
character(len=4)                :: varname
integer,dimension(:)               :: lenp(4), lens(3), lenv(5)
lenp(1)=6
lenp(2)=6
lenp(3)=4
lenp(4)=5
lens(1)=2
lens(2)=2
lens(3)=3
lenv(1)=3
lenv(2)=3
lenv(3)=3
lenv(4)=4
lenv(5)=4
if ( pdfflag.eq.1 ) then
    pdfname(1:lenp(pdfflag))='Precip'
elseif (pdfflag.eq.2) then
    pdfname(1:lenp(pdfflag))='Insect'
elseif (pdfflag.eq.3) then
    pdfname(1:lenp(pdfflag))='LowN'
else
    pdfname(1:lenp(pdfflag))='HighN'
end if

if ( scanflag.eq.1 ) then
    scanname(1:lens(scanflag))='AT'
elseif (scanflag.eq.2) then
    scanname(1:lens(scanflag))='RT'
else 
    scanname(1:lens(scanflag))='THI'
end if

if ( varflag.eq.1 ) then
    varname(1:lenv(varflag))='LDR'
elseif (varflag.eq.2) then
    varname(1:lenv(varflag))='Zhh'
elseif (varflag.eq.3) then
    varname(1:lenv(varflag))='Zhv'
elseif (varflag.eq.4) then
    varname(1:lenv(varflag))='VRhh'
else
    varname(1:lenv(varflag))='VRhv'
end if
open(55,file=pdfprefix//pdfname(1:lenp(pdfflag))//'_'//scanname(1:lens(scanflag))//'_'//varname(1:lenv(varflag))//pdfendfix,status='old')
do i = 1, 100
    read(55, *) pdf(i,1),pdf(i,2)
end do
close(55)
endsubroutine
