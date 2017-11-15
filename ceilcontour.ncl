;===============================================
; CEILOMETER DISPLAY NCL PROGRAM
;===============================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
begin
;================================================
; get data
;================================================
;infile=systemfunc("echo $Fig_File_CL")
;outfile=systemfunc("echo $Fig_File_out_CL")
;title=systemfunc("echo $figtitle_CL")
;extend=systemfunc("echo $extend")
infile="./2016/NCIO_CIL_avg2_LV3__daily_005_20160105.cdf"
outfile="out"
title="cil"
extend="png"
fchk=ismissing(infile)
wkstype			= extend
if (extend.eq."png") then
	wkstype@wkWidth		= 3000
	wkstype@wkHeight	= 6000
end if
wks= gsn_open_wks(wkstype,outfile)
gsn_define_colormap(wks,"BlAqGrYeOrReVi200")
;gsn_define_colormap(wks,"NCV_jet")
if(fchk.eq.0) then
	f= addfile(infile,"r")
	t= f->time_avg
	h= f->height
	bs= f->backscatter_avg
	n=f->ncbh
	nb=f->nblh
	cbh1=f->first_cbh_avg
	cbh2=f->second_cbh_avg
	cbh3=f->third_cbh_avg
	blh1=f->first_blh_avg
	blh2=f->second_blh_avg
	blh3=f->third_blh_avg
	bli1=f->first_bli_avg
	bli2=f->second_bli_avg
	bli3=f->third_bli_avg
	qcflag=f->QCflag
	bs=bs;*10^8
	bs!0="Height"
	bs!1="Time"
	bs&Time=t/3600
	bs&Height=h
	
	cbh1!0="Time"
	cbh1&Time=t/3600
	cbh2!0="Time"
	cbh2&Time=t/3600
	cbh3!0="Time"
	cbh3&Time=t/3600

	blh1!0="Time"
	blh1&Time=t/3600
	blh2!0="Time"
	blh2&Time=t/3600
	blh3!0="Time"
	blh3&Time=t/3600

	bli1!0="Time"
	bli1&Time=t/3600
	bli2!0="Time"
	bli2&Time=t/3600
	bli3!0="Time"
	bli3&Time=t/3600

	qcflag!0="Time"
	qcflag&Time=t/3600

	tlen=dimsizes(t)
	maxt=t(tlen-1)/3600
	bs@long_name= "Ceilometer Backscatter"
	bs@units= "10~S~-8~N~m~S~-1~N~sr~S~-1~"
	clevel=new(150,integer)
	clabel=new(151,string)
	;fv=clabel@_FillValue

	hlen=dimsizes(h)
	dnd=new(dimsizes(bs),float)
	dnd@_FillValue=-99
	dndcount=0
	do i=0,tlen-1
		if(n(i).le.0) then
			do j=1,hlen-1
				dnd(j,i)=1.+i/100.+j/100.
			end do
		dndcount=dndcount+1
		else
			dnd(:,i)=-99
		end if
	end do
	dnd!0="Height"
	dnd!1="Time"
	dnd&Time=t/3600
	dnd&Height=h
	
	qcf=new(dimsizes(bs),float)
	do i=0,tlen-1
		do j=1,hlen-1
			qcf(j,i)=qcflag(i)+i/100000.+j/1000000.
		end do
	end do
	qcf!0="Height"
	qcf!1="Time"
	qcf&Time=t/3600
	qcf&Height=h
	;print(qcf)
	dndb=new(dimsizes(bs),float)
	dndb@_FillValue=-99
	dndbcount=0
	do i=0,tlen-1
		if(nb(i).le.0) then
			do j=1,hlen-1
				dndb(j,i)=1.+i/100.+j/100.
			end do
		dndbcount=dndbcount+1
		else 
			dndb(:,i)=-99
		end if
	end do
	dndb!0="Height"
	dndb!1="Time"
	dndb&Time=t/3600
	dndb&Height=h

	dndb=new(dimsizes(bs),float)
	dndb@_FillValue=-99
	dndbcount=0
	do i=0,tlen-1
		if(nb(i).le.0) then
			do j=1,hlen-1
				dndb(j,i)=1.+i/100.+j/100.
			end do
		dndbcount=dndbcount+1
		else
			dndb(:,i)=-99
		end if
	end do
	dndb!0="Height"
	dndb!1="Time"
	dndb&Time=t/3600
	dndb&Height=h

	;do i=0,49
	do i=0,49
        	ii=i
	        dl=2
        	clevel(i)=dl*ii
        	clabel(i)=sprinti("%i",clevel(i))
	end do
	do i=50,99
        	ii=i-50
		dl=20
        	clevel(i)=dl*ii+100
	        clabel(i)=sprinti("%i",clevel(i))
	end do
	do i=100,149
        	ii=i-100
	        dl=200
        	clevel(i)=dl*ii+1000
	        clabel(i)=sprinti("%i",clevel(i))
	end do
	;do i=100,219
       ; 	ii=i-100
;	        dl=200
;        	clevel(i)=dl*ii+1200
;	        clabel(i)=sprinti("%i",clevel(i))
;	end do
	do i=0,25,149
	        clabel(i)=sprinti("%i",clevel(i))
	end do
	xbv=new(9,integer)
	xbl=new(9,integer)
	xbv(0)=0
	xbl(0)=0
	do i=1,8
		xbv(i)=i*10800
		xbl(i)=i*3
	end do
	;clabel(220)=sprinti("%i",25000)
	clabel(150)=sprinti("%i",10000)
;-================================================================
  dres                  = True                  ; plot mods desired
  dres@gsnDraw		= False
  dres@gsnFrame		= False
  dres@cnFillOn         = True                  ; turn on color
  dres@cnLinesOn        = False                 ; no contour lines
  dres@cnFillMode	="CellFill"
  dres@gsnSpreadColors  = True                  ; use full range of colors
  dres@tmXBMode 	= "Explicit"
  dres@tmXBValues 	= (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  dres@tmXBMinorValues	=ispan(1,23,1)
  dres@tmXBLabels 	= (/"00","03","06","09","12","15","18","21","24"/)
  dres@tiXAxisString 	= "Time (UTC)"
  dres@tiYAxisString 	= "Height (m AGL)"
  dres@vpWidthF		= 1
  dres@vpHeightF	= 0.5625  
  dres@trXMinF		=  0
  dres@trXMaxF		=  24
  dres@trYMinF		=  0
  dres@trYMaxF		=  15000
  dres@lbAutoManage	= False
  dres@cnNoDataLabelOn  = False
  dres@lbLabelBarOn	= False
;  cmap_d		= (/"White","Gray")
cmap_d=new((/2,3/), float)
;  dres@cnFillColors	= (/"White","Gray","Gray"/)
;  dres@cnLevelSelectionMode = "ExplicitLevels"       ; set manual contour levels
 dres@cnFillColor	="Gray"
 ;  dres@cnLevels		= (/0.4,9./)
  dres@cnMonoFillColor	=True
  dres@cnInfoLabelOn	=False
 ;dres@cnFillColor	=(/.8,.8,.8/)

	plot=new(5,graphic)
if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then
  plotd0=gsn_csm_contour(wks,dnd,dres)
  plotd1=gsn_csm_contour(wks,dnd,dres)
  plotd2=gsn_csm_contour(wks,dndb,dres)
  plotd3=gsn_csm_contour(wks,dndb,dres)
  plotd4=gsn_csm_contour(wks,dnd,dres)
end if
;==================================================================
; contour_backscatter
;-=================================================================
	res= True
	res@cnFillOn= True
	res@cnLinesOn= False
	res@cnFillMode= "RasterFill"
	res@gsnSpreadColors= True
	res@lbBoxLinesOn= False
	res@lbOrientation="Vertical"
	res@gsnDraw   = False
	res@gsnFrame  = False
	res@tiMainString="Backscatter Coefficient"
	res@cnExplicitLabelBarLabelsOn  = True
	res@cnLevelSelectionMode= "ExplicitLevels"
	res@cnLabelBarEndLabelsOn= True
	res@cnLevels= clevel
	res@lbLabelStride=50
	res@lbLabelStrings= clabel
	res@gsnYAxisIrregular2Linear= True
	res@tmXBMode= "Explicit"
	if (maxt.gt.23)then
	 	 res@tmXBValues 	= (/0,3,6,9,12,15,18,21,maxt/)
	  else
		 res@tmXBValues 	= (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  	end if
	res@tmXBMinorValues	=ispan(1,23,1)
	res@tmXBLabels 	= (/"00","03","06","09","12","15","18","21","24"/)
	res@tiXAxisString 	= "Time (UTC)"
	res@tiYAxisString 	= "Height (m AGL)"
	res@trYMaxF= 15400
	res@vpWidthF  = 1
	res@vpHeightF = 0.4
	plot(0)=gsn_csm_contour(wks,bs,res)
 	if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then 
		 overlay(plot(0),plotd0) 
	end if
;================================================================================
;CBH plot
;================================================================================
	resc=True
	resc@gsnDraw   = False
	resc@gsnFrame  = False
	resc@tiMainString="Cloud Base Height"
	resc@tmXBMode= "Explicit"
	if (maxt.gt.23)then
	 	 resc@tmXBValues 	= (/0,3,6,9,12,15,18,21,maxt/)
	  else
		 resc@tmXBValues 	= (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  	end if
	resc@tmXBMinorValues	=ispan(1,23,1)
	resc@tmXBLabels 	= (/"00","03","06","09","12","15","18","21","24"/)
	resc@tiXAxisString 	= "Time (UTC)"
	resc@tiYAxisString 	= "Height (m AGL)"
	resc@trYMaxF= 15400
	resc@vpWidthF  = 1
	resc@vpHeightF = 0.4
	resc@xyMarkLineMode    = "Markers"                ; choose to use markers
	resc@xyMarkers         =  16                      ; choose type of marker  
	resc@xyMarkerColor     = "Black"               ; Marker color
	plot(1)=gsn_csm_xy(wks,t/3600,cbh1,resc)
 	if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then 
		 overlay(plot(1),plotd1) 
	end if
	plotc2=gsn_csm_xy(wks,t/3600,cbh2,resc)
		 overlay(plot(1),plotc2) 
	plotc3=gsn_csm_xy(wks,t/3600,cbh3,resc)
		 overlay(plot(1),plotc3) 

;================================================================================
;BLH plot
;================================================================================
	resb=True
	resb@gsnDraw   = False
	resb@gsnFrame  = False
	resb@tiMainString="Boundary Layer Height"
	resb@tmXBMode= "Explicit"
	if (maxt.gt.23)then
	 	 resb@tmXBValues 	= (/0,3,6,9,12,15,18,21,maxt/)
	  else
		 resb@tmXBValues 	= (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  	end if
	resb@tmXBMinorValues	=ispan(1,23,1)
	resb@tmXBLabels 	= (/"00","03","06","09","12","15","18","21","24"/)
	resb@tiXAxisString 	= "Time (UTC)"
	resb@tiYAxisString 	= "Height (m AGL)"
	resb@trYMaxF= 5000
	resb@vpWidthF  = 1
	resb@vpHeightF = 0.4
	resb@xyMarkLineMode    = "Markers"                ; choose to use markers
	resb@xyMarkers         =  16                      ; choose type of marker  
	resb@xyMarkerColor     = "Black"               ; Marker color
	resb@xyMarkerSizeF     = 0.01                     ; Marker size (default 0.01)
	plot(2)=gsn_csm_xy(wks,t/3600,blh1,resb)
 	if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then 
		 overlay(plot(2),plotd2) 
	end if
	resb@xyMarkerColor     = "Red"               ; Marker color
	resb@xyMarkerSizeF     = 0.01                     ; Marker size (default 0.01)
	plotb2=gsn_csm_xy(wks,t/3600,blh2,resb)
		 overlay(plot(2),plotb2) 
	resb@xyMarkerColor     = "Blue"               ; Marker color
	resb@xyMarkerSizeF     = 0.01                     ; Marker size (default 0.01)
	plotb3=gsn_csm_xy(wks,t/3600,blh3,resb)
		 overlay(plot(2),plotb3) 
;================================================================================
;BLI plot
;================================================================================
	resi=True
	resi@gsnDraw   = False
	resi@gsnFrame  = False
	resi@tiMainString="Boundary Layer Index"
	resi@tmXBMode= "Explicit"
	if (maxt.gt.23)then
	 	 resi@tmXBValues 	= (/0,3,6,9,12,15,18,21,maxt/)
	  else
		 resi@tmXBValues 	= (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  	end if
	resi@tmXBMinorValues	=ispan(1,23,1)
	resi@tmXBLabels 	= (/"00","03","06","09","12","15","18","21","24"/)
	resi@tiXAxisString 	= "Time (UTC)"
	resi@tiYAxisString 	= "Height (m AGL)"
	resi@trYMaxF= 4
	resi@vpWidthF  = 1
	resi@vpHeightF = 0.4
	resi@xyMarkLineMode    = "Markers"                ; choose to use markers
	resi@xyMarkers         =  16                      ; choose type of marker  
	resi@xyMarkerColor     = "Black"               ; Marker color
	resi@xyMarkerSizeF     = 0.01                     ; Marker size (default 0.01)
	plot(3)=gsn_csm_xy(wks,t/3600,bli1,resi)
 	if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then 
		 overlay(plot(3),plotd2) 
	end if
	resi@xyMarkerColor     = "Red"               ; Marker color
	resi@xyMarkerSizeF     = 0.01                     ; Marker size (default 0.01)
	ploti2=gsn_csm_xy(wks,t/3600,bli2,resi)
		 overlay(plot(3),ploti2) 
	resi@xyMarkerColor     = "Blue"               ; Marker color
	resi@xyMarkerSizeF     = 0.01                     ; Marker size (default 0.01)
	ploti3=gsn_csm_xy(wks,t/3600,bli3,resi)
		 overlay(plot(3),ploti3) 
;================================================================================
;QCflag plot
;================================================================================
	qlevel=(/0,0.5,1,1.5/)
	qlabel=(/"","Good","","Alaram/Warning",""/)
	resq= True
	resq@cnFillOn= True
	resq@cnLinesOn= False
	resq@cnFillMode= "RasterFill"
	resq@gsnSpreadColors= True
	resq@lbBoxLinesOn= False
	resq@lbOrientation="Vertical"
	resq@gsnDraw   = False
	resq@gsnFrame  = False
	resq@tiMainString="QC status"
	resq@cnExplicitLabelBarLabelsOn  = True
	resq@cnLevelSelectionMode= "ExplicitLevels"
	resq@cnLabelBarEndLabelsOn= True
	resq@cnLevels= qlevel
	resq@lbLabelStride=1
	resq@lbLabelStrings= qlabel
	resq@cnFillColors=(/"Green","Green","Green","Red","Red"/)
	resq@gsnYAxisIrregular2Linear= True
	resq@tmXTOn=False
	resq@tmYROn=False
	resq@tmYLOn=False
	resq@tmXBMode= "Explicit"
	if (maxt.gt.23)then
	 	 resq@tmXBValues 	= (/0,3,6,9,12,15,18,21,maxt/)
	  else
		 resq@tmXBValues 	= (/"0","3.","6.","9.","12.","15.","18.","21","24"/)
  	end if
	resq@tmXBMinorValues	=ispan(1,23,1)
	resq@tmXBLabels 	= (/"00","03","06","09","12","15","18","21","24"/)
	resq@tiXAxisString 	= "Time (UTC)"
	resq@tiYAxisString 	= ""
	resq@trYMaxF= 15400
	resq@vpWidthF  = 1
	resq@vpHeightF = 0.2
	plot(4)=gsn_csm_contour(wks,qcf,resq)
 	if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then 
		 overlay(plot(4),plotd4) 
	end if
;================================================================================
;LEGENDS
;================================================================================
colors=(/"Black","Red","Blue"/)
dum=new(3,graphic)
xleg=(/0.65,  0.65, 0.65/)
yleg=(/0.7,  0.67, 0.64 /)
xtxt=(/0.7,  0.7, 0.7/)
ytxt=(/0.7,  0.67, 0.64/)
gsres=True
txres=True
labels=(/"First CBH","Second CBH","Third CBH"/)
do i=0,2
  mkres= True
  mkres@txFontHeightF = 0.01
  mkres@gsMarkerIndex = 1
  mkres@gsMarkerColor = colors(i)
  mkres@gsMarkerSizeF = 10.        ; Increase marker sizes.
  mkres@gsMarkerThicknessF=7
  gsn_polymarker_ndc(wks,xleg(i),yleg(i),mkres)
  gsn_text_ndc(wks,labels(i),xtxt(i),ytxt(i),mkres)
end do

yleg=(/0.5,  0.47, 0.44 /)
ytxt=(/0.5,  0.47, 0.44/)
gsres=True
txres=True
labels=(/"First BLH","Second BLH","Third BLH"/)
do i=0,2
  mkres= True
  mkres@txFontHeightF = 0.01
  mkres@gsMarkerIndex = 1
  mkres@gsMarkerColor = colors(i)
  mkres@gsMarkerSizeF = 10.        ; Increase marker sizes.
  mkres@gsMarkerThicknessF=7
  gsn_polymarker_ndc(wks,xleg(i),yleg(i),mkres)
  gsn_text_ndc(wks,labels(i),xtxt(i),ytxt(i),mkres)
end do

yleg=(/0.3,  0.27, 0.24 /)
ytxt=(/0.3,  0.27, 0.24/)
gsres=True
txres=True
labels=(/"First BLI","Second BLI","Third BLI"/)
do i=0,2
  mkres= True
  mkres@txFontHeightF = 0.01
  mkres@gsMarkerIndex = 1
  mkres@gsMarkerColor = colors(i)
  mkres@gsMarkerSizeF = 10.        ; Increase marker sizes.
  mkres@gsMarkerThicknessF=7
  gsn_polymarker_ndc(wks,xleg(i),yleg(i),mkres)
  gsn_text_ndc(wks,labels(i),xtxt(i),ytxt(i),mkres)
end do
;===============================================================================
;Pannel
;===============================================================================
	;res@trYMaxF= 3000
	; plot(1)=gsn_csm_contour(wks,bs,res)
	; if ((dndcount.gt.0).and.(dndcount.lt.tlen)) then 
;		 overlay(plot(1),plotd0) 
;	 end if
	resp= True
	resp@gsnDraw= True
	resp@gsnFrame= True
	resp@txString= title
	;res@trYMaxF= 300
	;plot(2)=gsn_csm_contour(wks,bs,res)
	gsn_panel(wks,plot,(/5,1/),resp)

end if
end

