; ===============================================
; CLOUD RADAR DISPLAY NCL PROGRAM
; ===============================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
begin
;===============================================
; get data
;================================================
infile=systemfunc("echo $Fig_File")
outfile=systemfunc("echo $Fig_File_out")

  f     = addfile (infile, "r")  
  t	= f->time
  h	= f->range
  zt	= f->reflectivity_h
  vt	= f->mean_doppler_velocity_h
  wt	= f->spectral_width_h
  lt	= f->linear_depolarization_ratio
  
z=transpose(zt)*0.01
v=transpose(vt)*0.01
w=transpose(wt)*0.01
l=transpose(lt)*0.01

z!0="Height"
z!1="Time"
z&Time=t/3600
z&Height=h
v!0="Height"
v!1="Time"
v&Time=t/3600
v&Height=h
w!0="Height"
w!1="Time"
w&Time=t/3600
w&Height=h
l!0="Height"
l!1="Time"
l&Time=t/3600
l&Height=h
;================================================
; plot
;=================================================
  wks1   = gsn_open_wks ("x11", outfile)            ; open ps file
  res                      = True                  ; plot mods desired
  res@cnFillOn             = True                  ; turn on color
  res@cnLinesOn            = False                 ; no contour lines
  res@cnFillMode	   ="RasterFill"
;---This resource not needed in V6.1.0
  res@gsnSpreadColors      = True                  ; use full range of colors
  res@lbBoxLinesOn	   = False
  res@lbOrientation	   = "Vertical"
;---This resource defaults to True in NCL V6.1.0


;  res@sfXArray		   = t
;  res@sfYArray 		   = h
;  res@gsnYAxisIrregular2Linear =True
  res@tmXBMode = "Explicit"
  res@tmXBValues = (/"0","3.","6.","9.","12.","15.","18.","21","86400"/)
  res@tmXBMinorValues=ispan(1,23,1)
  res@tmXBLabels = (/"00","03","06","09","12","15","18","21","24"/)
  res@tiXAxisString = "Time (UTC)"
  res@tiYAxisString = "Height (m AGL)"
  res@gsnDraw	= False
  res@gsnFrame	= False
  res@vpWidthF	= 1
  res@vpHeightF	= 0.5625
  plot=new(4,graphic)
;  res@lbLabelAutoStride = False
  res@trXMinF		   =  0
  res@trXMaxF		   =  24
  res@trYMinF		   =  0
  res@trYMaxF		   =  15000
;=====================================
;cn resource
;=====================================
  res@cnFillPalette="NCV_jet"
  clevel=new(241,float)
  clabel=new(241,string)
 do i=0,240
 	clevel(i)=-40+i*0.25
	clabel(i)=sprintf("%3.0f",clevel(i))
 end do
  res@cnExplicitLabelBarLabelsOn =True
  res@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res@cnLabelBarEndLabelsOn = True
  res@cnLevels			= clevel
  res@lbLabelStride		= 40
  res@lbLabelStrings		= clabel
;  res@cnMinLevelValF       = -40	           ; set min contour level
;  res@cnMaxLevelValF       = 20                  ; set max contour level
;  res@cnLevelSpacingF      = 0.5                   ; set contour spacing
  ;res@cnLabelScalingMode   = "ScaleFactor"
;  res@lbLabelAutoStride    = False                  ;
;  res@lbLabelStride	   = 10
;  res@lbLabelStrings=(/"-40","-30","-20","-10","0","10","20"/)

  res@tiMainString	   = "Reflectivity (dBZ)"
 plot(0) = gsn_csm_contour(wks1,z,res) 
delete(res@cnLevels)
delete(res@lbLabelStrings)
delete(res@cnFillPalette)

  res@cnFillPalette="NCV_blu_red"
  clevelv=new(101,float)
  clabelv=new(101,string)
 do i=0,100
 	clevelv(i)=-5.+i*0.1
	clabelv(i)=sprintf("%3.1f",clevelv(i))
 end do
 print(clevelv)
 print(clabelv)
  res@cnExplicitLabelBarLabelsOn =True
  res@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res@cnLabelBarEndLabelsOn = True
  res@cnLevels			= clevelv
  res@lbLabelStride		= 20
  res@lbLabelStrings		= clabelv
  res@tiMainString	   = "Doppler Velocity (ms~S~-1~N~)"
 plot(1) = gsn_csm_contour(wks1,v,res) 
delete(res@cnLevels)
delete(res@lbLabelStrings)

  res@cnFillPalette="NCV_jet"
  clevelw=new(241,float)
  clabelw=new(241,string)
 do i=0,240
 	clevelw(i)=0.+i/600.
	clabelw(i)=sprintf("%4.2f",clevelw(i))
 end do
  res@cnExplicitLabelBarLabelsOn =True
  res@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res@cnLabelBarEndLabelsOn = True
  res@cnLevels			= clevelw
  res@lbLabelStride		= 60
  res@lbLabelStrings		= clabelw
  res@tiMainString	   = "Spectral Width"
 plot(2) = gsn_csm_contour(wks1,w,res) 
delete(res@cnLevels)
delete(res@lbLabelStrings)

  res@cnFillPalette="NCV_jet"
  clevell=new(241,float)
  clabell=new(241,string)
 do i=0,240
 	clevell(i)=-40.+i/6.
	clabell(i)=sprintf("%3.0f",clevell(i))
 end do
  res@cnExplicitLabelBarLabelsOn =True
  res@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res@cnLabelBarEndLabelsOn = True
  res@cnLevels			= clevell
  res@lbLabelStride		= 60
  res@lbLabelStrings		= clabell
  res@tiMainString	   = "LinearDepolarizationRatio"
 plot(3) = gsn_csm_contour(wks1,l,res) 

  
  resp		= True
  resp@gsnDraw	= True
  resp@gsnPanelMainString=infile
  resp@gsnFrame	= True
 gsn_panel(wks1,plot,(/2,2/),resp)
end
