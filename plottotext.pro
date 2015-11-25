PRO plottotext,imstr,filetosave,streakdump=streakdump

	imstr.oplplot(0) -> GetProperty, DATA=dat

	x=REFORM(dat(0,*)) & y=REFORM(dat(1,*))
	nx=N_ELEMENTS(x)

	FOR ii=1,imstr.nplots-1 DO BEGIN
		imstr.oplplot(ii) -> GetProperty, DATA=dat
		IF N_ELEMENTS(dat(0,*)) NE nx THEN BEGIN
			junk=DIALOG_MESSAGE("Number and value of x-elements must be equal for all plots")
			RETURN
		ENDIF
		y=[[y],[REFORM(dat(1,*))]]
	ENDFOR



	OPENW,unit,filetosave,/GET_LUN

	imstr.oplxaxis0 -> GetProperty, TITLE=oxtitle
	imstr.oplyaxis0 -> GetProperty, TITLE=oytitle
	imstr.oplxaxis1 -> GetProperty, TITLE=otitle

	oxtitle -> GetProperty, STRINGS=xtitl
	oytitle -> GetProperty, STRINGS=ytitl
	otitle -> GetProperty, STRINGS=titl

	 PRINTF,unit,imstr.file
	 PRINTF,unit,"Rotation (rads): "+STRCOMPRESS(STRING(imstr.lastrot),/REMOVE_ALL)
	 PRINTF,unit,"Shear: "+STRCOMPRESS(STRING(imstr.lastshear),/REMOVE_ALL)

IF KEYWORD_SET(streakdump) THEN BEGIN
   WIDGET_CONTROL,imstr.mag_id,GET_VALUE=micpx  ;mics/pixel
   WIDGET_CONTROL,imstr.mag0_id,GET_VALUE=d0    ;pixels
   WIDGET_CONTROL,imstr.fidpx_id,GET_VALUE=fidpx ;pixels
   WIDGET_CONTROL,imstr.fidtt_id,GET_VALUE=fidtt ;ps
   WIDGET_CONTROL,imstr.dx_id,GET_VALUE=dx      ;pixels
   WIDGET_CONTROL,imstr.dy_id,GET_VALUE=dy      ;pixels
   WIDGET_CONTROL,imstr.slit_id,GET_VALUE=slit  ;mic
   WIDGET_CONTROL,imstr.rho0_id,GET_VALUE=rho0  ;g/cc
   WIDGET_CONTROL,imstr.mu0_id,GET_VALUE=mu     ;mass attenuation coefficient, cm2/g
   WIDGET_CONTROL,imstr.eta_id,GET_VALUE=eta    ;rho/rho0 of shell
   WIDGET_CONTROL,imstr.R0_id,GET_VALUE=R0      ;(mics) outer shell edge position
   WIDGET_CONTROL,imstr.del_id,GET_VALUE=del    ;(mics) shell stdev thickness
   WIDGET_CONTROL,imstr.sloty0_id,GET_VALUE=sl_y0    ;(mics) hohl slot position (y0=0=centered on capsule)
   WIDGET_CONTROL,imstr.slotdy_id,GET_VALUE=sl_dy    ;(mics) hohl slot width

   PRINTF,unit,"Polynomial coefficients: (a1 -> a"+STRCOMPRESS(STRING(FIX(N_ELEMENTS(imstr.tpoly)-1)),/REMOVE_ALL)+")"
   PRINTF,unit,STRING(imstr.tpoly(1:N_ELEMENTS(imstr.tpoly)-1),/PRINT)
   PRINTF,unit,"Mag (mic/px):"+micpx
   PRINTF,unit,"x0 (pixel):"+d0
   PRINTF,unit,"Fidu (px):"+fidpx+" Fidu (ps):"+fidtt
   PRINTF,unit,"dx(pix):"+dx+" dy(pix):"+dy
   PRINTF,unit,"Slit size (mic):"+slit
   PRINTF,unit,"Initial density (g/cc):"+rho0
   PRINTF,unit,"Mass attenuation (cm2/g):"+mu
   PRINTF,unit,"First guess - Compression:"+eta
   PRINTF,unit,"First guess - Position (mic):"+R0
   PRINTF,unit,"First guess - Thickness (mic):"+del
   PRINTF,unit,"Masking slot position (mic):"+sl_y0
   PRINTF,unit,"Masking slot width (mic):"+sl_dy
   
   PRINTF,unit,"Plot ROI (xmin,xmax,ymin,ymax)"
   PRINTF,unit,STRCOMPRESS(STRING(REFORM(TRANSPOSE(imstr.xyroi),4)))
   PRINTF,unit,"Inversion ROI (xmin,xmax,ymin,ymax)"
   PRINTF,unit,STRCOMPRESS(STRING(REFORM(TRANSPOSE(imstr.phroi),4)))
   PRINTF,unit,"Intensity ROI (xmin,xmax,ymin,ymax)"
   PRINTF,unit,STRCOMPRESS(STRING(REFORM(TRANSPOSE(imstr.inroi),4)))

   PRINTF,unit,"Warp region (xmin,xmax,ymin,ymax)"
   PRINTF,unit,STRCOMPRESS(STRING([imstr.warp.xmin,imstr.warp.xmax,imstr.warp.ymin,imstr.warp.ymax]))
   PRINTF,unit,"Warp matrix"
   PRINTF,unit,STRCOMPRESS(STRING(imstr.warp.c))
ENDIF ELSE BEGIN
	 WIDGET_CONTROL,imstr.mag_id,GET_VALUE=mag
	 WIDGET_CONTROL,imstr.mag0_id,GET_VALUE=d0
	 WIDGET_CONTROL,imstr.fidpx_id,GET_VALUE=fidpx
	 WIDGET_CONTROL,imstr.fidtt_id,GET_VALUE=fidtt
	 WIDGET_CONTROL,imstr.fmin_id,GET_VALUE=fmin
	 WIDGET_CONTROL,imstr.fmax_id,GET_VALUE=fmax
	 WIDGET_CONTROL,imstr.vpf_id,GET_VALUE=vpf
	 WIDGET_CONTROL,imstr.foffset_id,GET_VALUE=foffset
	 WIDGET_CONTROL,imstr.bo_id,GET_VALUE=bo

	 ;PRINTF,unit,"a:"+a+" b:"+b+" c:"+c
	 PRINTF,unit,"Polynomial coefficients: (a1 -> a"+STRCOMPRESS(STRING(FIX(N_ELEMENTS(imstr.tpoly)-1)),/REMOVE_ALL)+")"
	 PRINTF,unit,STRING(imstr.tpoly(1:N_ELEMENTS(imstr.tpoly)-1),/PRINT)
	 PRINTF,unit,"Mag (mic/px):"+mag
	 PRINTF,unit,"d0 (pixel):"+d0
	 PRINTF,unit,"Fidu (px):"+fidpx+" Fidu (ps):"+fidtt
	 PRINTF,unit,"fmin: "+fmin+", fmax: "+fmax

	 PRINTF,unit,"Plot ROI (xmin,xmax,ymin,ymax)"
	 PRINTF,unit,STRCOMPRESS(STRING(REFORM(TRANSPOSE(imstr.xyroi),4)))
	 PRINTF,unit,"Phase ROI (xmin,xmax,ymin,ymax)"
	 PRINTF,unit,STRCOMPRESS(STRING(REFORM(TRANSPOSE(imstr.phroi),4)))
	 PRINTF,unit,"Intensity ROI (xmin,xmax,ymin,ymax)"
	 PRINTF,unit,STRCOMPRESS(STRING(REFORM(TRANSPOSE(imstr.inroi),4)))

	 PRINTF,unit,"Warp region (xmin,xmax,ymin,ymax)"
	 PRINTF,unit,STRCOMPRESS(STRING([imstr.warp.xmin,imstr.warp.xmax,imstr.warp.ymin,imstr.warp.ymax]))
	 PRINTF,unit,"Warp matrix"
	 PRINTF,unit,STRCOMPRESS(STRING(imstr.warp.c))
ENDELSE

	 IF imstr.plotlist(imstr.plotindex) EQ 'Velocity' THEN BEGIN
		PRINTF,unit,"VPF (mic/ns) = "+vpf
		PRINTF,unit,"Fringe offset = "+foffset
		PRINTF,unit,"Break out time (ps) = "+bo
	 ENDIF

	 PRINTF,unit,titl
	 PRINTF,unit,imstr.plotlist(imstr.plotindex)
	 PRINTF,unit,"IMAGE type: "+imstr.whichimage
 	 PRINTF,unit,'***'
 	 PRINTF,unit,xtitl+' / '+ytitl

	 PRINTF,unit,STRING([TRANSPOSE(x),TRANSPOSE(y)],FORMAT='(4(" ",G12.5))')
	 ;ysz=SIZE(yyy)
	 ;IF ysz(0) EQ 1 THEN BEGIN
	 ;	PRINTF,unit,[TRANSPOSE(REFORM(xx)),TRANSPOSE(REFORM(yy(*,0)))]
	 ;ENDIF ELSE BEGIN
	;	PRINTF,unit,[TRANSPOSE(REFORM(xx)),TRANSPOSE(REFORM(yyy(*,0))),$
	;				TRANSPOSE(REFORM(yyy(*,1))),TRANSPOSE(REFORM(yyy(*,2)))]
	 ;ENDELSE
	;ENDIF

	CLOSE,unit
	FREE_LUN,unit

END