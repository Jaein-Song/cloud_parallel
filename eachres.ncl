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
;infile=systemfunc("echo $Fig_File")
;outfile=systemfunc("echo $Fig_File_out")
infile="/data2/ncio/CLD_258/hmbprodat/CFRADIAL/DAILYMEAN/2017/NCIO_CLD_QC17_CIL__daily_060_20170301.cfradial"
outfile="test"
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
  plot=new(4,graphic)
  
;========================================================================================
;Z plot
;========================================================================================

  res1                      = True                  ; plot mods desired
  res1@cnFillOn             = True                  ; turn on color
  res1@cnLinesOn            = False                 ; no contour lines
  res1@cnFillMode	   ="RasterFill"
  res1@gsnSpreadColors      = True                  ; use full range of colors
  res1@lbBoxLinesOn	   = False
  res1@lbOrientation	   = "Vertical"
  res1@tmXBMode = "Explicit"
  res1@tmXBValues = (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  res1@tmXBMinorValues=ispan(1,23,1)
  res1@tmXBLabels = (/"00","03","06","09","12","15","18","21","24"/)
  res1@tiXAxisString = "Time (UTC)"
  res1@tiYAxisString = "Height (m AGL)"
  res1@gsnDraw	= False
  res1@gsnFrame	= False
  res1@vpWidthF	= 1
  res1@vpHeightF	= 0.5625  
  res1@trXMinF		   =  0
  res1@trXMaxF		   =  24
  res1@trYMinF		   =  0
  res1@trYMaxF		   =  15000
;=====================================
;cn resource
;=====================================
  cmap=read_colormap_file("NCV_jet")
  res1@cnFillColors=cmap
  clevel=new(241,float)
  clabel=new(241,string)
 do i=0,240
 	clevel(i)=-40+i*0.25
	clabel(i)=sprintf("%3.0f",clevel(i))
 end do
  res1@cnExplicitLabelBarLabelsOn =True
  res1@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res1@cnLabelBarEndLabelsOn = True
  res1@cnLevels			= clevel
  res1@lbLabelStride		= 40
  res1@lbLabelStrings		= clabel
  res1@tiMainString	   = "Reflectivity (dBZ)"
 plot(0) = gsn_csm_contour(wks1,z,res1) 
delete(cmap)

;========================================================================================
;V plot
;========================================================================================
  res2                      = True                  ; plot mods desired
  res2@cnFillOn             = True                  ; turn on color
  res2@cnLinesOn            = False                 ; no contour lines
  res2@cnFillMode	   ="RasterFill"
  res2@gsnSpreadColors      = True                  ; use full range of colors
  res2@lbBoxLinesOn	   = False
  res2@lbOrientation	   = "Vertical"
  res2@tmXBMode = "Explicit"
  res2@tmXBValues = (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  res2@tmXBMinorValues=ispan(1,23,1)
  res2@tmXBLabels = (/"00","03","06","09","12","15","18","21","24"/)
  res2@tiXAxisString = "Time (UTC)"
  res2@tiYAxisString = "Height (m AGL)"
  res2@gsnDraw	= False
  res2@gsnFrame	= False
  res2@vpWidthF	= 1
  res2@vpHeightF	= 0.5625  
  res2@trXMinF		   =  0
  res2@trXMaxF		   =  24
  res2@trYMinF		   =  0
  res2@trYMaxF		   =  15000
;=====================================
;cn resource
;=====================================

cmap=read_colormap_file("BlWhRe")
;res2@cnFillPalette=cmap(0:100,0:3)
;cmap2=rgbhsv(cmap(:,0:2))
;rint(cmap2)
;res2@cnFillPalette=cmap
;es2@cnFillColors=cmap
  clevelv=new(101,float)
  clabelv=new(101,string)
 do i=0,100
 	clevelv(i)=-5.+i*0.1
	clabelv(i)=sprintf("%3.1f",clevelv(i))
 end do
  res2@cnExplicitLabelBarLabelsOn =True
  res2@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
;  res2@cnLevelSelectionMode = "ManualLevels"       ; set manual contour levels
;  res2@cnMinLevelValF        =-5.
;  res2@cnMaxLevelValF        =5.
;  res2@cnLevelSpacingF        =0.1
  res2@cnLabelBarEndLabelsOn = True
  res2@cnLevels			= clevelv
  res2@lbLabelStride		= 20
  res2@lbLabelStrings		= clabelv
  res2@tiMainString	   = "Doppler Velocity (ms~S~-1~N~)"
 plot(1) = gsn_csm_contour(wks1,v,res2) 
delete(cmap)
;========================================================================================
;W plot
;========================================================================================
  res3                      = True                  ; plot mods desired
  res3@cnFillOn             = True                  ; turn on color
  res3@cnLinesOn            = False                 ; no contour lines
  res3@cnFillMode	   ="RasterFill"
  res3@gsnSpreadColors      = True                  ; use full range of colors
  res3@lbBoxLinesOn	   = False
  res3@lbOrientation	   = "Vertical"
  res3@tmXBMode = "Explicit"
  res3@tmXBValues = (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  res3@tmXBMinorValues=ispan(1,23,1)
  res3@tmXBLabels = (/"00","03","06","09","12","15","18","21","24"/)
  res3@tiXAxisString = "Time (UTC)"
  res3@tiYAxisString = "Height (m AGL)"
  res3@gsnDraw	= False
  res3@gsnFrame	= False
  res3@vpWidthF	= 1
  res3@vpHeightF	= 0.5625  
  res3@trXMinF		   =  0
  res3@trXMaxF		   =  24
  res3@trYMinF		   =  0
  res3@trYMaxF		   =  15000
;=====================================
;cn resource
;=====================================
cmap=read_colormap_file("NCV_jet")
res3@cnFillColors=cmap
  clevelw=new(241,float)
  clabelw=new(241,string)
 do i=0,240
 	clevelw(i)=0.+i/600.
	clabelw(i)=sprintf("%4.2f",clevelw(i))
 end do
  res3@cnExplicitLabelBarLabelsOn =True
  res3@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res3@cnLabelBarEndLabelsOn = True
  res3@cnLevels			= clevelw
  res3@lbLabelStride		= 60
  res3@lbLabelStrings		= clabelw
  res3@tiMainString	   = "Spectral Width (m s~S~-1~N~)"
 plot(2) = gsn_csm_contour(wks1,w,res3) 
delete(cmap)
;========================================================================================
;L plot
;========================================================================================
  res4                      = True                  ; plot mods desired
  res4@cnFillOn             = True                  ; turn on color
  res4@cnLinesOn            = False                 ; no contour lines
  res4@cnFillMode	   ="RasterFill"
  res4@gsnSpreadColors      = True                  ; use full range of colors
  res4@lbBoxLinesOn	   = False
  res4@lbOrientation	   = "Vertical"
  res4@tmXBMode = "Explicit"
  res4@tmXBValues = (/"0","3","6","9","12","15","18","21","24"/)
  res4@tmXBLabels = (/"00","03","06","09","12","15","18","21","24"/)
  res4@tiXAxisString = "Time (UTC)"
  res4@tiYAxisString = "Height (m AGL)"
  res4@gsnDraw	= False
  res4@gsnFrame	= False
  res4@vpWidthF	= 1
  res4@vpHeightF	= 0.5625  
  res4@trXMinF		   =  0
  res4@trXMaxF		   =  24
  res4@trYMinF		   =  0
  res4@trYMaxF		   =  15000
;=====================================
;cn resource
;=====================================
cmap=read_colormap_file("NCV_jet")
res4@cnFillColors=cmap
  clevell=new(241,float)
  clabell=new(241,string)
 do i=0,240
 	clevell(i)=-40.+i/6.
	clabell(i)=sprintf("%3.0f",clevell(i))
 end do
  res4@cnExplicitLabelBarLabelsOn =True
  res4@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
  res4@cnLabelBarEndLabelsOn = True
  res4@cnLevels			= clevell
  res4@lbLabelStride		= 60
  res4@lbLabelStrings		= clabell
  res4@tiMainString	   = "LinearDepolarizationRatio (dB)"
 plot(3) = gsn_csm_contour(wks1,l,res4) 
  
  resp		= True
  resp@gsnDraw	= True
  resp@gsnPanelMainString="MAIN"
  resp@gsnFrame	= True
 gsn_panel(wks1,plot,(/2,2/),resp)
end
