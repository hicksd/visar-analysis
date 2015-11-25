PRO xvis_event, event


WIDGET_CONTROL, event.id, GET_UVALUE = eventval     ;find the user value
                   ;of the widget where
                   ;the event occured
IF N_ELEMENTS(eventval) EQ 0 THEN RETURN
CASE eventval OF
'THEMENU': BEGIN
    CASE event.value OF

    "Open":BEGIN
       xvisopen,event
       END
    "Copy Image":BEGIN;copy image to clipboard
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_copyimage
       ENDIF

       oimclipb=OBJ_NEW('IDLgrClipboard')
       oimclipb -> SetProperty, PALETTE=imstr.oimpalette

       oimclipb -> Draw, imstr.oimview, VECTOR=0

       break_copyimage:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       END
    "Copy Plot":BEGIN;copy plot to clipboard
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_copyplot
       ENDIF

       oimclipb=OBJ_NEW('IDLgrClipboard')

       imstr.oplwindow -> GetProperty, DIMENSIONS=dims ;IDLgrWindow object
       ;oimclipb -> GetProperty, DIMENSIONS=odims
       oimclipb -> SetProperty, DIMENSIONS=dims

       oimclipb -> Draw, imstr.oplview, VECTOR=0;,POSTSCRIPT=0,VECT_TEXT_RENDER_METHOD=1

       break_copyplot:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       END
    "TIF (no bin)":BEGIN
      savetif,event,bin=0
    END
    "TIF (binned)":BEGIN
      savetif,event,bin=1
    END
    "Raw binary (no bin)":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_save_binary
       ENDIF

       file=DIALOG_PICKFILE(PATH=imstr.pathsave,FILE="*.raw",$
         FILTER="*.raw",/WRITE,GET_PATH=path,GROUP=event.handler)

       IF file EQ '' THEN GOTO,break_save_binary

       imstr.pathsave=path
     OPENU,unit,imstr.pathfile,/GET_LUN
         PRINTF,unit,imstr.pathopen
         PRINTF,unit,imstr.pathsave
     CLOSE,unit
     FREE_LUN,unit

;print,file
;print,imstr.factor,imstr.xsize,imstr.ysize
       OPENW,unit,file,/GET_LUN
           isz=SIZE(imstr.image)
print,isz
           WRITEU,unit,UINT(isz(1)),UINT(isz(2))
           WRITEU,unit,imstr.image
       CLOSE,unit
       FREE_LUN,unit

       break_save_binary:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
        "Raw ROI":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_save_rawroi
       ENDIF

       file=DIALOG_PICKFILE(PATH=imstr.pathsave,FILE="*.raw",$
         FILTER="*.raw",/WRITE,GET_PATH=path,GROUP=event.handler)

       IF file EQ '' THEN GOTO,break_save_rawroi

       imstr.pathsave=path
     OPENU,unit,imstr.pathfile,/GET_LUN
         PRINTF,unit,imstr.pathopen
         PRINTF,unit,imstr.pathsave
     CLOSE,unit
     FREE_LUN,unit

       sz=SIZE(imstr.image)
       xmin=imstr.xyroi(0,0) & xmax=imstr.xyroi(0,1)
       ymin=imstr.xyroi(1,0) & ymax=imstr.xyroi(1,1)

       IF xmax-xmin LE 0 OR xmin LT 0 OR xmax LT 0 THEN BEGIN
         xmin=0 & xmax=sz(1)-1
       END
       IF ymax-ymin LE 0 OR ymin LT 0 OR ymax LT 0 THEN BEGIN
         ymin=0 & ymax=sz(2)-1
       END

       OPENW,unit,file,/GET_LUN
           isz=SIZE(imstr.image(xmin:xmax,ymin:ymax))
print,isz
           WRITEU,unit,UINT(isz(1)),UINT(isz(2))
           WRITEU,unit,imstr.image(xmin:xmax,ymin:ymax)
       CLOSE,unit
       FREE_LUN,unit

       break_save_rawroi:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Save Scaled Image":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_save_scaled_image
       ENDIF

       file=DIALOG_PICKFILE(PATH=imstr.pathsave,FILE="*.cgm",$
         FILTER="*.cgm",/WRITE,GET_PATH=path,GROUP=event.handler)

       IF file EQ '' THEN GOTO,break_save_scaled_image

       imstr.pathsave=path
     OPENU,unit,imstr.pathfile,/GET_LUN
         PRINTF,unit,imstr.pathopen
         PRINTF,unit,imstr.pathsave
     CLOSE,unit
     FREE_LUN,unit

       sz=SIZE(imstr.image)

       xmin=imstr.xyroi(0,0) & xmax=imstr.xyroi(0,1)
       ymin=imstr.xyroi(1,0) & ymax=imstr.xyroi(1,1)

       IF xmax-xmin LE 10 OR xmin LT 0 OR xmax LT 0 THEN BEGIN
         xmin=0 & xmax=sz(1)-1
       END
       IF ymax-ymin LE 10 OR ymin LT 0 OR ymax LT 0 THEN BEGIN
         ymin=0 & ymax=sz(2)-1
       END

       xsz=xmax-xmin+1 & ysz=ymax-ymin+1
print,xmin,xmax,ymin,ymax
       im=imstr.image(xmin:xmax,ymin:ymax)
        px=FINDGEN(xsz)+xmin & py=FINDGEN(ysz)+ymin
        tt=time(imstr,px,py,dd)

       IF imstr.factor GT 1. THEN BEGIN
            tt=CONGRID(tt,xsz/imstr.factor)
            dd=CONGRID(dd,ysz/imstr.factor)
            im=CONGRID(im,xsz/imstr.factor,ysz/imstr.factor)
       ENDIF

       cmin=MIN(im) & cmax=MAX(im)
       IF imstr.logplot EQ 1 THEN BEGIN
         im=(ALOG(im-cmin+1))/ALOG(cmax-cmin+2)
       ENDIF ELSE BEGIN
         im=(im-cmin)/(cmax-cmin)
       ENDELSE

       savscaleimage,file,im,tt,dd

       break_save_scaled_image:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Save Plot as Text":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF imstr.nplots LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("Insufficient plots available" )
         GOTO,break_saveplottotext
       ENDIF

       filetosave=DIALOG_PICKFILE(PATH=imstr.pathsave,GET_PATH=path,GROUP=event.handler)

       IF filetosave NE '' THEN BEGIN
         preexist=FINDFILE(filetosave)

         IF preexist(0) NE '' THEN BEGIN
          answer=DIALOG_MESSAGE("File already exists. Overwrite?",/QUESTION)
          IF answer EQ 'No' THEN GOTO,break_saveplottotext
         ENDIF
         plottotext,imstr,filetosave

         imstr.pathsave=path
           OPENU,unit,imstr.pathfile,/GET_LUN
            PRINTF,unit,imstr.pathopen
            PRINTF,unit,imstr.pathsave
         CLOSE,unit
         FREE_LUN,unit
       ENDIF

       break_saveplottotext:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Print Image":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_printimage
       ENDIF

       oprint = OBJ_NEW('IDLgrPrinter',COLOR_MODEL=1)
       oprint -> SetProperty, PALETTE=imstr.oimpalette, LANDSCAPE=1
       result=DIALOG_PRINTERSETUP(oprint)
       ;result=DIALOG_PRINTJOB(oprint)

       IF result NE 0 THEN BEGIN
         oprint -> Draw, imstr.oimview, VECTOR=0
         oprint -> NewPage
         oprint -> NewDocument
       END

       break_printimage:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Print Plot":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_printplot
       ENDIF

       oprint = OBJ_NEW('IDLgrPrinter',COLOR_MODEL=1)
       result=DIALOG_PRINTERSETUP(oprint)
       ;result=DIALOG_PRINTJOB(oprint)

       IF result NE 0 THEN BEGIN
         oprint -> Draw, imstr.oplview, VECTOR=1
         ;oprint -> NewPage
         oprint -> NewDocument
       END

       break_printplot:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Exit": BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       OBJ_DESTROY,imstr.oimwindow
       OBJ_DESTROY,imstr.oimview
       OBJ_DESTROY,imstr.oplwindow
       OBJ_DESTROY,imstr.oplview
     WIDGET_CONTROL, event.top, /DESTROY
       END
    "Show ROI":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_showroi
       ENDIF

       imstr.oimpolyxyroi -> SetProperty, HIDE=0
       imstr.oimwindow -> Draw, imstr.oimview

       break_showroi:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Log/Linear":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_log
       ENDIF

       IF imstr.logplot EQ 0 THEN imstr.logplot=1 ELSE imstr.logplot=0

       wshow_images,imstr,'IMAGE'

       break_log:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Refresh":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_ref
       ENDIF

       wshow_images,imstr,'IMAGE'
       imstr.whichimage='IMAGE'

       break_ref:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Color Table": BEGIN
     WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
     IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_ct
       ENDIF
       XLOADCT, GROUP = event.handler,/MODAL
       wshow_images,imstr,'IMAGE'
       help,imstr.image

       break_ct:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY

       END
    "Color Palette": BEGIN
     WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_pal
       ENDIF
       XPALETTE, GROUP = event.top;,/MODAL
       wshow_images,imstr,'IMAGE'

       break_pal:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY

       END
    "Linear Fit":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       imstr.oplplot(0) -> GetProperty, DATA=dat
       IF N_ELEMENTS(dat) EQ 0 THEN BEGIN
         junk=DIALOG_MESSAGE("A plot must exist before a fit can be performed")
         GOTO,break_lfit
       ENDIF

       x=REFORM(dat(0,*)) & y=REFORM(dat(1,*))

       IF imstr.nplots GT 1 THEN BEGIN
         imstr.oplplot(1) -> GetProperty, DATA=dat
         yerr=ABS(REFORM(dat(1,*))-y)
       ENDIF ELSE yerr=SQRT(y)
       ;IF N_ELEMENTS(dat) EQ 0 THEN BEGIN
       ; junk=DIALOG_MESSAGE("Data must contain errors to perform a fit")
       ; GOTO,break_lfit
       ;ENDIF

       c=POLY_FIT(x,y,2,SIGMA=sig,MEASURE_ERRORS=yerr)
       ;PRINT,"Y = ("+STRCOMPRESS(STRING(c(1)))+"+/-"+STRCOMPRESS(STRING(sig(1)))+")*X + "+STRCOMPRESS(STRING(c(0)))+"+/-"+STRCOMPRESS(STRING(sig(0)))
       PRINT,"Coeffs = ",REFORM(c)

       imstr.oplplot(imstr.nplots) = OBJ_NEW('IDLgrPlot')
       imstr.oplmodel -> Add, imstr.oplplot(imstr.nplots)

       daty=FLTARR(N_ELEMENTS(x))
       FOR i=0,N_ELEMENTS(c)-1 DO daty=daty+c(i)*x^i

       imstr.oplplot(0) -> GetProperty, XRANGE=xr0,YRANGE=yr0
       imstr.oplplot(imstr.nplots) -> SetProperty, DATAX=x,DATAY=daty,$;c(0)+c(1)*x, $
              MIN_VALUE=yr0(0),MAX_VALUE=yr0(1),LINESTYLE=2

       imstr.nplots=imstr.nplots+1

       imstr.oplwindow -> Draw, imstr.oplview

       break_lfit:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       END
    "Gaussian Fit":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       imstr.oplplot(0) -> GetProperty, DATA=dat
       IF N_ELEMENTS(dat) EQ 0 THEN BEGIN
         junk=DIALOG_MESSAGE("A plot must exist before a fit can be performed")
         GOTO,break_gfit
       ENDIF

       x=REFORM(dat(0,*)) & y=REFORM(dat(1,*))

       ;IF imstr.nplots GT 1 THEN BEGIN
       ; imstr.oplplot(1) -> GetProperty, DATA=dat
       ; yerr=ABS(REFORM(dat(1,*))-y)
       ;ENDIF ELSE yerr=SQRT(y)
       ;IF N_ELEMENTS(dat) EQ 0 THEN BEGIN
       ; junk=DIALOG_MESSAGE("Data must contain errors to perform a fit")
       ; GOTO,break_lfit
       ;ENDIF

       yfit = GAUSSFIT(x,y,a,NTERMS=3)
       PRINT,"Y = "+STRCOMPRESS(STRING(a(0)))+"exp((x-"+$
         STRCOMPRESS(STRING(a(1)))+")^2/2*"$
         +STRCOMPRESS(STRING(a(2)))+"^2)"

       print,a
       imstr.oplplot(imstr.nplots) = OBJ_NEW('IDLgrPlot')
       imstr.oplmodel -> Add, imstr.oplplot(imstr.nplots)

       imstr.oplplot(0) -> GetProperty, XRANGE=xr0,YRANGE=yr0
       imstr.oplplot(imstr.nplots) -> SetProperty, DATAX=x,DATAY=yfit, $
              MIN_VALUE=yr0(0),MAX_VALUE=yr0(1),LINESTYLE=2

       imstr.nplots=imstr.nplots+1

       imstr.oplwindow -> Draw, imstr.oplview

       break_gfit:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       END
    "Find Warp Matrix":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       WIDGET_CONTROL,/HOURGLASS

       phxmin=imstr.phroi(0,0) & phxmax=imstr.phroi(0,1)
       phymin=imstr.phroi(1,0) & phymax=imstr.phroi(1,1)

       xmin=imstr.xyroi(0,0) & xmax=imstr.xyroi(0,1)
       ymin=imstr.xyroi(1,0) & ymax=imstr.xyroi(1,1)

       sz=SIZE(imstr.image)
       IF xmin LT 0 OR xmax GT sz(1) OR ymin LT 0 OR ymax GT sz(2) OR $
          phxmin LT 0 OR phxmax GT sz(1) OR phymin LT 0 OR phymax GT sz(2)   THEN BEGIN
         junk=DIALOG_MESSAGE("Plot ROI or Phase ROI exceeds bounds of image")
         GOTO,break_findwarp
       ENDIF
       IF N_ELEMENTS(imstr.cumulphase) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("A phase image must be calculated before warping can be performed")
         GOTO,break_findwarp
       END

       IF xmin GE phxmin AND xmax LE phxmax AND $
         ymin GE phymin AND ymax LE phymax THEN BEGIN

         cmlphim=imstr.cumulphase(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin)

         sz=SIZE(cmlphim) & npx=sz(1) & npy=sz(2)
         px=INDGEN(npx) & py=INDGEN(npy)

         ;Generate transformation matrix elements, c(npolyX,npolyY)
         ;First, fit each column of data (y-direction) to a polynomial
         ;of order npolyY
         npolyY=imstr.warp.nPolyY
         cY=FLTARR(npx,npolyY+1)
         FOR ii=0,npx-1 DO cY(ii,*)=POLY_FIT(py,cmlphim(ii,*),npolyY)
         ;Now fit each coeffcient from the above fit to a polynomial
         ;of order npolyX.
         npolyX=imstr.warp.nPolyX
         c=FLTARR(npolyX+1,npolyY+1)
         FOR ii=0,npolyY DO c(*,ii)=POLY_FIT(px,cY(*,ii),npolyX)

         ;Now estimate the conversion from fringe shift to y-pixel shift
         ;by taking the very first column only and finding the average phase
         ;difference between each pixel. This is potentially the weakest point
         ;in this analysis.
         dphi=cumulative_phase(imstr.phase(xmin-phxmin,ymin-phymin:ymax-phymin),/RETURN_DPHI)
         dphi=REFORM(dphi)
         ;Find the average phase change per pixel at xmin in y-direction
         av=MEAN(dphi) & sig=STDDEV(dphi)
         norm=WHERE(ABS(dphi - av) LT 2.*sig)
         IF norm(0) NE -1 THEN dphi0=MEAN(dphi(norm)) ELSE dphi0=MEAN(dphi0)

         dF0=dphi0/2./!pi
         PRINT,"Average Pixels/Fringe, dY/dF = "+STRCOMPRESS(STRING(1/dF0))


       ENDIF ELSE BEGIN
         junk=DIALOG_MESSAGE("Plot ROI must be contained within Phase ROI")
         WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
         RETURN
       ENDELSE

       ;Convert the transformation matrix c (presently describing the fringe
       ;shift error dF) to a pixel shift error dY. dF0=fringes per pixel.
       c=c/dF0

       imstr.warp.c=c
       imstr.warp.xmin=xmin & imstr.warp.xmax=xmax
       imstr.warp.ymin=ymin & imstr.warp.ymax=ymax
       imstr.warp.exist=1
print,c
print,xmin,xmax,ymin,ymax
       break_findwarp:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       RETURN

    END
    "Apply Warp":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_applywarp
       ENDIF
       IF imstr.warp.exist EQ 0 THEN BEGIN
         junk=DIALOG_MESSAGE("No warp matrix exists")
         GOTO,break_applywarp
       ENDIF

       WIDGET_CONTROL,/HOURGLASS

       xmin=imstr.warp.xmin & xmax=imstr.warp.xmax
       ymin=imstr.warp.ymin & ymax=imstr.warp.ymax

       nPolyX=imstr.warp.nPolyX & nPolyY=imstr.warp.nPolyY

       c=imstr.warp.c

       ;Apply the dY shift transformation to each column of data
       sz=SIZE(imstr.image)

       IF xmin LT 0 OR xmax GT sz(1) OR ymin LT 0 OR ymax GT sz(2) THEN BEGIN
         junk=DIALOG_MESSAGE("Edges of warp region exceed current image size")
         GOTO,break_applywarp
       ENDIF

       oldim=imstr.image(xmin:xmax,ymin:ymax)

       y=FINDGEN(ymax-ymin+1)
       dy=FINDGEN(ymax-ymin+1)

       FOR ii=0,xmax-xmin DO BEGIN
         IF nPolyX EQ 1 AND nPolyY EQ 1 THEN BEGIN
          dy=(c(0,0)+c(1,0)*ii)+(c(0,1)+c(1,1)*ii)*y
         ENDIF ELSE BEGIN
          IF nPolyX EQ 2 AND nPolyY EQ 2 THEN BEGIN
            dy=(c(0,0)+c(1,0)*ii+c(2,0)*ii^2)+(c(0,1)+c(1,1)*ii+c(2,1)*ii^2)*y+$
            (c(0,2)+c(1,2)*ii+c(2,2)*ii^2)*y^2
          ENDIF ELSE BEGIN
              FOR jj=0,ymax-ymin DO BEGIN
                 dy(jj)=TOTAL(c##TRANSPOSE(REPLICATE(ii,npolyX+1)^INDGEN(npolyX+1))$
                   *y(jj)^INDGEN(npolyY+1) )
              ENDFOR
          ENDELSE
         ENDELSE
         imstr.image(xmin+ii,ymin:ymax)=INTERPOL(oldim(ii,*),y,y+dY)
       ENDFOR

       break_applywarp:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Camera Settings":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       result = CAMERA(imstr,event)
       IF result(0) EQ -1 THEN GOTO,break_cam

       WIDGET_CONTROL,imstr.rot_id,SET_VALUE=result(0)
       WIDGET_CONTROL,imstr.shr_id,SET_VALUE=result(1)
       WIDGET_CONTROL,imstr.mag_id,SET_VALUE=result(2)
       ;WIDGET_CONTROL,imstr.delay_id,SET_VALUE=result(3)
;       WIDGET_CONTROL,imstr.a_id,SET_VALUE=result(4)
;       WIDGET_CONTROL,imstr.b_id,SET_VALUE=result(5)
;       WIDGET_CONTROL,imstr.c_id,SET_VALUE=result(6)
;       WIDGET_CONTROL,imstr.fidpx_id,SET_VALUE=result(7)
;       WIDGET_CONTROL,imstr.fidtt_id,SET_VALUE=result(8)
      imstr.MinPix=result(3)
      imstr.MaxPix=result(4)
      FOR iii=1,N_ELEMENTS(imstr.tpoly)-1 DO imstr.tpoly(iii)=result(iii+5-1)
      FOR jjj=1,N_ELEMENTS(imstr.tpoly)-1 DO imstr.spoly(jjj-1)=imstr.tpoly(jjj)*jjj

       break_cam:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       END
    "Rotation":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_rot
       ENDIF
       WIDGET_CONTROL,imstr.rot_id,GET_VALUE=rads
       WIDGET_CONTROL,/HOURGLASS

       drad=FLOAT(rads(0))-imstr.lastrot
       imstr.image=ROT(imstr.image,FLOAT(drad)/!pi*180.,/INTERP)
       imstr.lastrot=FLOAT(rads(0))

       wshow_images,imstr,'IMAGE'
       break_rot:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    "Right 90":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_rotR
       ENDIF
       WIDGET_CONTROL,/HOURGLASS

       newimstr=RotateFlip(imstr,'Right 90')

       WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
       break_rotR:
    END
    "Left 90":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_rotL
       ENDIF
       WIDGET_CONTROL,/HOURGLASS

       newimstr=RotateFlip(imstr,'Left 90')

       WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
       break_rotL:
    END
    "Flip Horizontal":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_flipH
       ENDIF
       WIDGET_CONTROL,/HOURGLASS

       newimstr=RotateFlip(imstr,'Flip Horizontal')

       WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
       break_flipH:
    END
    "Flip Vertical":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_flipV
       ENDIF
       WIDGET_CONTROL,/HOURGLASS

       newimstr=RotateFlip(imstr,'Flip Vertical')

       WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
       break_flipV:
    END
    "Shear":BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
       IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         GOTO,break_shear
       ENDIF
       WIDGET_CONTROL,imstr.shr_id,GET_VALUE=shearr

       shr=FLOAT(shearr(0))-imstr.lastshear
       imstr.lastshear=FLOAT(shearr(0))

       nrows=N_ELEMENTS(imstr.image(0,*)-1)
       ncols=N_ELEMENTS(imstr.image(*,0)-1)

       WIDGET_CONTROL,/HOURGLASS
       FOR ii=0,nrows-1 DO BEGIN
         row=imstr.image(*,ii)
         dx=-shr*ii
         n=FIX(dx) & f=dx MOD 1
         imstr.image(*,ii)=(1-f)*SHIFT(row,n)+f*SHIFT(row,n+1)
       ENDFOR

       wshow_images,imstr,'IMAGE'
       break_shear:
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Median Filter':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_medfil
      ENDIF
    WIDGET_CONTROL,imstr.medfilval_id,GET_VALUE=med

    WIDGET_CONTROL,/HOURGLASS

    imstr.image=MEDIAN(imstr.image,FLOAT(med(0)))
    ;imstr.image=SMOOTH(imstr.image,FLOAT(med(0)))
    ;imstr.image=wvfilter(imstr.image,FLOAT(med(0)),FLOAT(med(0)))
    ;imstr.image=LEEFILT(imstr.image,FLOAT(med(0)))
    ;imstr.image = WV_DENOISE(imstr.image,"Daubechies",2,PERCENT=97,$;COEFFICIENTS=1024,$
    ;    DWT_FILT=dwt_filt,THRESHOLD=0,WPS_FILT=wps_filt)

    wshow_images,imstr,'IMAGE'
    break_medfil:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Subtract Image':BEGIN
        WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       GOTO,break_subbkg
      ENDIF ELSE WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY

    xvisopen,event,/BKG
    break_subbkg:
    END
    'Subtract DC offset':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_subDC
      ENDIF
    WIDGET_CONTROL,imstr.bkg_id,GET_VALUE=sbkg

    WIDGET_CONTROL,/HOURGLASS

    b=FIX(sbkg(0))
    lb=WHERE(imstr.image LT b)
    gb=WHERE(imstr.image GE b)
    IF lb(0) NE -1 THEN imstr.image(lb) = 0
    IF gb(0) GT -1 THEN imstr.image(gb) = imstr.image(gb) - b

    wshow_images,imstr,'IMAGE'
    break_subDC:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END

    'Remove Ghost Fringes':BEGIN
      WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY

      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_ghost
      ENDIF
      IF imstr.ghroi(0,0) EQ -1 THEN BEGIN
        junk=DIALOG_MESSAGE("Define Ghost ROI")
        GOTO,break_ghost
      ENDIF

    WIDGET_CONTROL,/HOURGLASS

    ghxmin=imstr.ghroi(0,0) & ghxmax=imstr.ghroi(0,1)
    ghymin=imstr.ghroi(1,0) & ghymax=imstr.ghroi(1,1)

    im=imstr.image(ghxmin:ghxmax,ghymin:ghymax)
    sz=SIZE(im) & nx=sz(1) & ny=sz(2)

Fim=FFT(im)
;    F0=Fim(0,0)
;    Fim(0,*)=0.5*(Fim(1,*)+Fim(nx-1,*)) ;get rid of Kx=0 (or set to adjacent Kx=1 values)
;    Fim(0,0)=F0
;Fim(0,*)=COMPLEX(0,1)*Fim(0,*)

;fmin=12
;fcol=Fim(0,*)
;infcol=COMPLEXARR(ny)
;infcol(0:fmin-1)=fcol(0:fmin-1)
;infcol(ny-1-(fmin-1):ny-1)=fcol(ny-1-(fmin-1):ny-1)
;Fim(0,*)=infcol

;eps=MinGhost(Fim)
;
F0=Fim(0,0) & med0=MEDIAN(im)
theta=!pi/2.;+eps
Fim(0,*)=Fim(0,*)*COMPLEX(COS(theta),SIN(theta))
Fim(0,0)=F0 ;set overall DC level to same as before ghost subtraction.
;Fim(0,*)=0. ;& Fim(1,*)=0. & Fim(nx-1,*)=0.

imf=(FFT(Fim,/INVERSE))

    imstr.image(ghxmin:ghxmax,ghymin:ghymax)=imf

    wshow_images,imstr,'IMAGE'

    break_ghost:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END

    'Flatfield':BEGIN
        WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       GOTO,break_flatfield
      ENDIF ELSE WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY

    xvisopen,event,/flatfield
    break_flatfield:
    END
    'Find Sweep':BEGIN
        WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
       GOTO,break_findsweep
      ENDIF ELSE WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY

    junk=DIALOG_MESSAGE("Find sweep procedure not yet implemented...suckah!")
    break_findsweep:
    END
    'Film Scale':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
      IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_filmscale
      ENDIF

    WIDGET_CONTROL,/HOURGLASS
    imstr.image=10.^(0.0016*imstr.image-2.66)

    wshow_images,imstr,'IMAGE'
    break_filmscale:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Image':BEGIN ;Display image
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY

   IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No image loaded")
         WIDGET_CONTROL,event.handler,SET_UVALUE=imstr,/NO_COPY
         RETURN
       ENDIF
       wshow_images,imstr,'IMAGE'

       imstr.whichimage = 'IMAGE'

       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Phase':BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
        IF N_ELEMENTS(imstr.phase) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No phase image exists")
         WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
              RETURN
        ENDIF

       wshow_images,imstr,'PHASE',origin=[imstr.phroi(0,0),imstr.phroi(1,0)]/imstr.factor

       imstr.whichimage = 'PHASE'

       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Fringe Unwrap':BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
        IF N_ELEMENTS(imstr.cumulphase) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No phase image exists")
         WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
              RETURN
        ENDIF
       wshow_images,imstr,'CUMULPHASE',origin=[imstr.phroi(0,0),imstr.phroi(1,0)]/imstr.factor

       imstr.whichimage = 'CUMULPHASE'

       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Amplitude':BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
        IF N_ELEMENTS(imstr.ampl) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No phase image exists")
         WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
              RETURN
        ENDIF
       wshow_images,imstr,'AMPL',origin=[imstr.phroi(0,0),imstr.phroi(1,0)]/imstr.factor

       imstr.whichimage = 'AMPL'

       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END
    'Normal Amplitude':BEGIN
       WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
        IF N_ELEMENTS(imstr.normalampl) LE 1 THEN BEGIN
         junk=DIALOG_MESSAGE("No phase image exists")
         WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
              RETURN
        ENDIF
       wshow_images,imstr,'NORMALAMPL',origin=[imstr.phroi(0,0),imstr.phroi(1,0)]/imstr.factor

       imstr.whichimage = 'NORMALAMPL'

       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
    END

    ELSE: BEGIN
     junk=DIALOG_MESSAGE("Program Error: Event User Value Not Found")
    ENDELSE
ENDCASE
END
'Roitype':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    break_roitype:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'XYTABLE':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE('Image must be loaded first')
       GOTO,break_xytable
    ENDIF

    IF event.type EQ 0 THEN BEGIN ;For end-of-line insertion events
       xcell=event.x & ycell=event.y
       oldvalue=imstr.xyvals(xcell,ycell)

       WIDGET_CONTROL,imstr.xytable,GET_VALUE=a
       imstr.xyvals=FLOAT(a)

       IF xcell NE 4 THEN BEGIN
         CASE xcell OF
         0:BEGIN
          imstr.xyroi(0,0)=imstr.xyvals(0,0) & imstr.xyroi(0,1)=imstr.xyvals(0,1)
          imstr.xyroi(1,0)=imstr.xyvals(0,2) & imstr.xyroi(1,1)=imstr.xyvals(0,3)
          roi=imstr.xyroi
           END
         1:BEGIN
          IF imstr.whichimage NE 'IMAGE' AND imstr.roilist(xcell) EQ 'Set Phase ROI' THEN BEGIN
              junk=DIALOG_MESSAGE("Changing of phase ROI can be performed in image view only")
              break
          ENDIF

          IF N_ELEMENTS(imstr.phase) GT 1 AND imstr.roilist(xcell) EQ 'Set Phase ROI' THEN BEGIN
              answer=DIALOG_MESSAGE("Changing the phase ROI will delete all existing phase data. Do you wish to continue?",/QUESTION)
              IF answer EQ 'Yes' THEN BEGIN
                 WIDGET_CONTROL,/HOURGLASS
                   imstr1  =imagetostructure(imstr,'PHASE',[-1])
                 imstr2  =imagetostructure(imstr1,'CUMULPHASE',[-1])
                 imstr3  =imagetostructure(imstr2,'NORMALAMPL',[-1])
                 newimstr=imagetostructure(imstr3,'AMPL',[-1])
                 WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
                 WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
              ENDIF ELSE BEGIN
                 imstr.xyvals(xcell,ycell)=oldvalue
                 WIDGET_CONTROL,imstr.xytable,SET_VALUE=imstr.xyvals
                 GOTO,break_xytable
              ENDELSE
          ENDIF

          imstr.phroi(0,0)=imstr.xyvals(1,0) & imstr.phroi(0,1)=imstr.xyvals(1,1)
          imstr.phroi(1,0)=imstr.xyvals(1,2) & imstr.phroi(1,1)=imstr.xyvals(1,3)
          roi=imstr.phroi
           END
         2:BEGIN
          imstr.inroi(0,0)=imstr.xyvals(2,0) & imstr.inroi(0,1)=imstr.xyvals(2,1)
          imstr.inroi(1,0)=imstr.xyvals(2,2) & imstr.inroi(1,1)=imstr.xyvals(2,3)
          roi=imstr.inroi
           END
         3:BEGIN
          imstr.ghroi(0,0)=imstr.xyvals(3,0) & imstr.ghroi(0,1)=imstr.xyvals(3,1)
          imstr.ghroi(1,0)=imstr.xyvals(3,2) & imstr.ghroi(1,1)=imstr.xyvals(3,3)
          roi=imstr.ghroi
           END
           
         ENDCASE
         set_roi,imstr,roi,xcell
       ENDIF ELSE BEGIN
         imstr.limindex=1
         WIDGET_CONTROL,imstr.limtype,SET_DROPLIST_SELECT=imstr.limindex
       ENDELSE

    END
    break_xytable:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'VImage Window':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY

    IF N_ELEMENTS(imstr.image) LE 1 THEN GOTO,break_fw

    IF (event.x) LT 0 OR (event.x GE imstr.xsize) THEN GOTO,break_fw
    IF (event.y) LT 0 OR (event.y GE imstr.ysize) THEN GOTO,break_fw

    WIDGET_CONTROL, imstr.vimageID, GET_DRAW_VIEW=drawview

    ;Find and display the cursor position & value in pixels and time & distance
       px=event.x*imstr.factor
       py=event.y*imstr.factor

       IF imstr.whichimage EQ 'IMAGE' THEN BEGIN
        zvalue=STRING(imstr.image(px,py))
       ENDIF ELSE BEGIN
        IF px GE imstr.phroi(0,0) AND px LE imstr.phroi(0,1) AND $
         py GE imstr.phroi(1,0) AND py LE imstr.phroi(1,1) THEN BEGIN
          CASE imstr.whichimage OF
                'PHASE':zvalue=STRING(imstr.phase(px-imstr.phroi(0,0),py-imstr.phroi(1,0)))
                'CUMULPHASE':zvalue=STRING(imstr.cumulphase(px-imstr.phroi(0,0),py-imstr.phroi(1,0)))
                'AMPL':zvalue=STRING(imstr.ampl(px-imstr.phroi(0,0),py-imstr.phroi(1,0)))
                'NORMALAMPL':zvalue=STRING(imstr.normalampl(px-imstr.phroi(0,0),py-imstr.phroi(1,0)))
          ENDCASE
        ENDIF ELSE zvalue=''
       ENDELSE
       xyz=STRING(px,FORMAT='(I)')+STRING(py,FORMAT='(I)')+ zvalue

       WIDGET_CONTROL,imstr.pixID,SET_VALUE='          [x,y,z]='+xyz

       tt=STRCOMPRESS(STRING(time(imstr,px,py,dst)))
       dst=STRCOMPRESS(STRING(dst))

       WIDGET_CONTROL,imstr.dtID,$
       SET_VALUE="                    d (mic) ="+dst(0)+"   t (ps) ="+tt(0)

       x=FIX(event.x*imstr.factor)
       y=FIX(event.y*imstr.factor)
       ;z=imstr.image(x,y)

       WIDGET_CONTROL, imstr.vimageID, SET_DRAW_VIEW=drawview

    ;If right button is pressed then print (x,y,z)
       IF event.press EQ 4 THEN BEGIN
         PRINT,x,y,imstr.image(x,y)
       ENDIF

    ;If left button is pressed then define ROI
       IF event.press EQ 1 THEN BEGIN ;start drag on button press
         imstr.drag = 1
         imstr.roi = [[x,y],[0,0]]
         imstr.oldphroi=imstr.phroi ;saves phase roi information in case changes need to be undone
       ENDIF

    ;If roi is still being dragged then plot the window
      IF imstr.drag EQ 1 THEN BEGIN
       imstr.roi = [[imstr.roi(0),imstr.roi(1)],[x,y]]
;       ;Makes sure the max and min are in the right order
;        xmin=MIN(imstr.roi(0,*)) & ymin=MIN(imstr.roi(1,*))
;        xmax=MAX(imstr.roi(0,*)) & ymax=MAX(imstr.roi(1,*))
;        imstr.roi=[[xmin,ymin],[xmax,ymax]]
       ;Dynamically shows ROI during re-sizing; also dynamically updates ROI table
       WIDGET_CONTROL,imstr.roitype,GET_VALUE=roiindex
       set_roi,imstr,imstr.roi,roiindex        
      ENDIF
      
    ;Only respond on left button release
       IF event.release EQ 1 AND imstr.drag EQ 1 THEN BEGIN

       imstr.roi = [[imstr.roi(0),imstr.roi(1)],[x,y]] ;[[x0,y0],[x1,y1]]
       imstr.drag = 0

       ;Makes sure the max and min are in the right order
        xmin=MIN(imstr.roi(0,*)) & ymin=MIN(imstr.roi(1,*))
        xmax=MAX(imstr.roi(0,*)) & ymax=MAX(imstr.roi(1,*))
       imstr.roi=[[xmin,ymin],[xmax,ymax]]

       WIDGET_CONTROL,imstr.roitype,GET_VALUE=roiindex

          ;Checks to see if phase ROI really does want to be changed, since this will
          ;erase all stored phase information.
          IF imstr.whichimage NE 'IMAGE' AND imstr.roilist(roiindex) EQ 'Set Phase ROI' THEN BEGIN
            junk=DIALOG_MESSAGE("Changing of phase ROI can be performed in image view only")
            set_roi,imstr,imstr.oldphroi,roiindex
            GOTO,break_fw
          ENDIF

          IF N_ELEMENTS(imstr.phase) GT 1 AND imstr.roilist(roiindex) EQ 'Set Phase ROI' THEN BEGIN
            answer=DIALOG_MESSAGE("Changing the phase ROI will delete all existing phase data. Do you wish to continue?",/QUESTION)
            IF answer EQ 'Yes' THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              imstr1  =imagetostructure(imstr,'PHASE',[-1])
              imstr2  =imagetostructure(imstr1,'CUMULPHASE',[-1])
              imstr3  =imagetostructure(imstr2,'NORMALAMPL',[-1])
              newimstr=imagetostructure(imstr3,'AMPL',[-1])
              WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
              WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
            ENDIF ELSE BEGIN
              set_roi,imstr,imstr.oldphroi,roiindex
              GOTO,break_fw
            ENDELSE
          ENDIF


       set_roi,imstr,imstr.roi,roiindex
       
       ENDIF ;execute only on button release

    break_fw:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'PhaseCalc':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    IF imstr.phroi(0,0) EQ -1 THEN BEGIN
       junk=DIALOG_MESSAGE("Define Phase Region")
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
           RETURN
    ENDIF

    IF imstr.xyroi(0,0) GE imstr.xyroi(0,1) OR $ ;xmin GE xmax
       imstr.xyroi(1,0) GE imstr.xyroi(1,1) OR $ ;ymin GE ymax
       imstr.phroi(0,0) GE imstr.phroi(0,1) OR $
       imstr.phroi(1,0) GE imstr.phroi(1,1)    $
       THEN BEGIN
       junk=DIALOG_MESSAGE("Make sure xmin < xmax and ymin < ymax for ROI's")
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
           RETURN
    ENDIF

    WIDGET_CONTROL,/HOURGLASS

    phxmin=imstr.phroi(0,0) & phxmax=imstr.phroi(0,1)
    phymin=imstr.phroi(1,0) & phymax=imstr.phroi(1,1)

    im=imstr.image(phxmin:phxmax,phymin:phymax)
    nx=phxmax-phxmin+1 & ny=phymax-phymin+1

    WIDGET_CONTROL,imstr.fmin_id,GET_VALUE=fmin
    WIDGET_CONTROL,imstr.fmax_id,GET_VALUE=fmax
    fmin=FLOAT(fmin(0))
    fmax=FLOAT(fmax(0))

    result=find_phase(im,nx,ny,fmin,fmax,coh=coh,incoh=incoh,phim=phim)

    IF result EQ -1 THEN BEGIN
       WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
           RETURN
    ENDIF

    ampl=coh+incoh

    normalampl=FLTARR(nx,ny)
    FOR yy=0,ny-1 DO BEGIN
       rowav=MEAN(ampl(*,yy))
       ;rowsig=STDDEV(ampl(*,yy))
       normalampl(*,yy)=(ampl(*,yy)-rowav)/rowav
    ENDFOR
    ;FOR xx=0,nx-1 DO BEGIN
    ;  colav=MEAN(normalampl(xx,*))
    ;  colsig=STDDEV(normalampl(xx,*))
    ;  normalampl(xx,*)=(normalampl(xx,*)-colav)/colsig
    ;ENDFOR

    IF imstr.negstate EQ 1 THEN negph=-1. ELSE negph=1.

    cmlphim=FLTARR(nx,ny)
    FOR kk=0,ny-1 DO BEGIN
       cmlphim(*,kk)= negph*cumulative_phase(phim(*,kk))
    ENDFOR
    cmlphim=-cmlphim/(2.*!pi)

    imstr1  =imagetostructure(imstr,'PHASE',phim/ABS(phim))
    imstr2  =imagetostructure(imstr1,'CUMULPHASE',cmlphim)
    imstr3  =imagetostructure(imstr2,'NORMALAMPL',normalampl)
    newimstr=imagetostructure(imstr3,'AMPL',ampl)

    break_phasecalc:
    WIDGET_CONTROL,event.handler, SET_UVALUE=newimstr, /NO_COPY
END
'PlotNow':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_plotnow
    ENDIF

;***************Check that max > min for all roi's
    IF imstr.xyroi(0,0) GE imstr.xyroi(0,1) OR $ ;xmin GE xmax
       imstr.xyroi(1,0) GE imstr.xyroi(1,1) OR $ ;ymin GE ymax
       imstr.phroi(0,0) GE imstr.phroi(0,1) OR $
       imstr.phroi(1,0) GE imstr.phroi(1,1) OR $
       imstr.inroi(0,0) GE imstr.inroi(0,1) OR $
       imstr.inroi(1,0) GE imstr.inroi(1,1) OR $
       imstr.ghroi(0,0) GE imstr.ghroi(0,1) OR $
       imstr.ghroi(1,0) GE imstr.ghroi(1,1) $       
       THEN BEGIN
       junk=DIALOG_MESSAGE('Make sure xmin < xmax and ymin < ymax for all ROI')
       GOTO,break_plotnow
    ENDIF

    IF imstr.xyroi(0,0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define Plot ROI Region")
         GOTO,break_plotnow
       ENDIF

    ;The following represent the x,y limits for the ROI to
    ;be used as the averaging region.
    xmin=imstr.xyroi(0,0) & ymin=imstr.xyroi(1,0)
    xmax=imstr.xyroi(0,1) & ymax=imstr.xyroi(1,1)

    ;The following are the x, y limits for the phase ROI
    phxmin=imstr.phroi(0,0) & phxmax=imstr.phroi(0,1)
    phymin=imstr.phroi(1,0) & phymax=imstr.phroi(1,1)


       IF imstr.whichimage EQ 'IMAGE' THEN BEGIN
          IF xmin LT 0 OR xmax GE imstr.xsize*imstr.factor OR $
            ymin LT 0 OR ymax GE imstr.ysize*imstr.factor THEN BEGIN
            junk=DIALOG_MESSAGE('Make sure (xmin,xmax,ymin,ymax) for Plot ROI are within image')
            GOTO,break_plotnow
          ENDIF  
         im=imstr.image(xmin:xmax,ymin:ymax)
       ENDIF ELSE BEGIN
         IF xmin GE phxmin AND xmax LE phxmax AND $
          ymin GE phymin AND ymax LE phymax THEN BEGIN
          CASE imstr.whichimage OF
           'PHASE':im=imstr.phase(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin)
           'CUMULPHASE':im=imstr.cumulphase(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin)
           'AMPL':im=imstr.ampl(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin)
           'NORMALAMPL':im=imstr.normalampl(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin)
          ENDCASE
         ENDIF ELSE BEGIN
          junk=DIALOG_MESSAGE("Plot ROI must be contained within Phase ROI")
          GOTO,break_plotnow
         ENDELSE
       ENDELSE


    CASE imstr.plotlist(imstr.plotindex) OF
    'Row Profile':BEGIN;Plot row

        ;WIDGET_CONTROL,imstr.vimageID,GET_DRAW_VIEW=pt
       ;vxmin=pt(0)*imstr.factor
       ;vymin=pt(1)*imstr.factor

       ;IF xmin EQ xmax THEN BEGIN
       ; sz=SIZE(imstr.image)
       ; xmin=0 & xmax=sz(1)-1
       ;ENDIF
       avrow=REBIN(FLOAT(im),xmax-xmin+1,1)

       sigrow=FLTARR(xmax-xmin+1)
       FOR ii=0,xmax-xmin DO BEGIN
         sigrow(ii)=STDDEV(FLOAT(im(ii,*)))
       ENDFOR

       IF imstr.ylog EQ 1 THEN BEGIN
         avrow=ALOG10(avrow)
         sigrow=(ALOG10(avrow+sigrow)-ALOG10(avrow-sigrow))/2.
       w=WHERE(~FINITE(avrow)) & IF w(0) NE -1 THEN avrow(w)=1E-1
       w=WHERE(~FINITE(sigrow)) & IF w(0) NE -1 THEN sigrow(w)=1E-1
       ENDIF

       OUTPLOT,time(imstr,xmin+INDGEN(xmax-xmin+1)),avrow,imstr,$
         ERROR=sigrow,HIST=0,$
         XTITL="Time (ps)",YTITL="Intensity",$
         TITL="X="+STRCOMPRESS(STRING(xmin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(xmax),/REMOVE_ALL)+$
          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)

    END
    'Column Profile':BEGIN;Plot column

       ;WIDGET_CONTROL,imstr.vimageID,GET_DRAW_VIEW=pt

       ;vxmin=pt(0)*imstr.factor
       ;vymin=pt(1)*imstr.factor

       ;IF ymin EQ ymax THEN BEGIN
       ; sz=SIZE(imstr.image)
       ; ymin=0 & ymax=sz(2)-1
       ;ENDIF

       avcol=REFORM(REBIN(FLOAT(im),1,ymax-ymin+1))
       sigcol=FLTARR(ymax-ymin+1)
       FOR ii=0,ymax-ymin DO BEGIN
         sigcol(ii)=STDDEV(FLOAT(im(*,ii)))
       ENDFOR

       IF imstr.ylog EQ 1 THEN BEGIN
         avcol=ALOG10(avcol)
         sigcol=(ALOG10(avcol+sigcol)-ALOG10(avcol-sigcol))/2.
       w=WHERE(~FINITE(avcol)) & IF w(0) NE -1 THEN avcol(w)=1E-1
       w=WHERE(~FINITE(sigcol)) & IF w(0) NE -1 THEN sigcol(w)=1E-1
       ENDIF


       t=time(imstr,xmin+INDGEN(xmax-xmin+1),ymin+INDGEN(ymax-ymin+1),dist)

       OUTPLOT,dist,avcol,imstr,ERROR=sigcol,HIST=0,$
         XTITL="Distance (mics)",YTITL="Intensity",$
         TITL="X="+STRCOMPRESS(STRING(xmin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(xmax),/REMOVE_ALL)+$
          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)

    END
;    'Time derivative':BEGIN ;Plot time derivative, with time delay given by time delay
;
;       WIDGET_CONTROL,imstr.vimageID,GET_DRAW_VIEW=pt
;
;       WIDGET_CONTROL,imstr.delay_id,GET_VALUE=tau_ps
;       WIDGET_CONTROL,imstr.a_id,GET_VALUE=a
;
;       tau_pix=FLOAT(tau_ps(0))/FLOAT(a(0))
;       IF tau_pix LT 2 THEN tau_pix=2.
;       IF tau_pix GE 3 THEN BEGIN
;         FOR ii=0,ymax-ymin DO im(*,ii)=SMOOTH(im(*,ii),tau_pix)
;       END
;
;       FOR ii=0,ymax-ymin DO BEGIN
;         im(*,ii)=SHIFT(im(*,ii),tau_pix/2.)-SHIFT(im(*,ii),-tau_pix/2.)
;         im(0:tau_pix/2.,ii)=im(tau_pix/2.+1)
;         im(xmax-xmin-tau_pix/2.:xmax-xmin,ii)=im(xmax-xmin-tau_pix/2.-1,ii)
;       ENDFOR
;
;       ac=REBIN(FLOAT(im),xmax-xmin+1,1)
;
;       sigac=FLTARR(xmax-xmin+1)
;       FOR ii=0,xmax-xmin DO BEGIN
;         sigac(ii)=STDDEV(FLOAT(im(ii,*)))
;       ENDFOR
;
;       OUTPLOT,time(imstr,xmin+INDGEN(xmax-xmin+1)),ac,imstr,$
;         ERROR=sigac,hist=1,$
;         XTITL='Time (ps)',YTITL='F(t-!4s!3/2)-F(t+!4s!3/2)',$
;         TITL="X="+STRCOMPRESS(STRING(xmin),/REMOVE_ALL)+$
;          ":"+STRCOMPRESS(STRING(xmax),/REMOVE_ALL)+$
;          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
;          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)
;
;    END
;    'Edge Find':BEGIN
;       WIDGET_CONTROL,imstr.vimageID,GET_DRAW_VIEW=pt
;
;       WIDGET_CONTROL,imstr.delay_id,GET_VALUE=tau_ps
;       WIDGET_CONTROL,imstr.a_id,GET_VALUE=a
;
;       tau_pix=FLOAT(tau_ps(0))/FLOAT(a(0))
;       IF tau_pix LT 2 THEN tau_pix=2.
;       IF tau_pix GE 3 THEN BEGIN
;         FOR ii=0,ymax-ymin DO im(*,ii)=SMOOTH(im(*,ii),tau_pix)
;       END
;
;       edge=FLTARR(ymax-ymin+1)
;       err=FLTARR(ymax-ymin+1)
;       FOR ii=0,ymax-ymin DO BEGIN
;         im(*,ii)=ABS(SHIFT(im(*,ii),tau_pix/2.)-SHIFT(im(*,ii),-tau_pix/2.)  )
;         im(0:tau_pix/2.,ii)=im(tau_pix/2.+1)
;         im(xmax-xmin-tau_pix/2.:xmax-xmin,ii)=im(xmax-xmin-tau_pix/2.-1,ii)
;         ;edge(ii)=TOTAL(im(*,ii)*(FINDGEN(xmax-xmin+1)+xmin))/TOTAL(im(*,ii))
;         edge(ii)=MEAN(WHERE(im(*,ii) EQ MAX(im(*,ii))))+xmin
;         ;err(ii)=
;       ENDFOR
;
;       tt=time(imstr,edge,ymin+INDGEN(ymax-ymin+1),dist)
;       OUTPLOT,dist,edge,imstr,HIST=0,$
;         ;PSY=3,$
;         YTITL='Pixel #',XTITL='Distance (mic)',$
;         TITL="X="+STRCOMPRESS(STRING(xmin),/REMOVE_ALL)+$
;          ":"+STRCOMPRESS(STRING(xmax),/REMOVE_ALL)+$
;          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
;          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)
;    END
    'Freq spectrum':BEGIN;Find Freq spectrum for a given x value within ROI
         ;specified ymin -> ymax

       IF imstr.phroi(0,0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define Phase Region")
         GOTO,break_freq
       ENDIF

       WIDGET_CONTROL,/HOURGLASS

       phxmin=imstr.phroi(0,0) & phxmax=imstr.phroi(0,1)
       phymin=imstr.phroi(1,0) & phymax=imstr.phroi(1,1)

       IF xmin GE phxmin AND xmax LE phxmax THEN BEGIN

         fim=COMPLEXARR(xmax-xmin+1,phymax-phymin+1)
         FOR ff=xmin,xmax DO BEGIN
          fim(ff-xmin,*)=FFT(imstr.image(ff,phymin:phymax)$
                        *HANNING(phymax-phymin+1))
         ENDFOR

         fim=REFORM(REBIN(ABS(fim),1,phymax-phymin+1))

         fim(0) = 0 ;suppress DC component to simplify y-axis scaling
         fim(1) = 0 ;suppress single wavelength mode to simplify y-axis scaling
         fim(N_ELEMENTS(fim)-1) = 0
         fim=fim(0:40)

         OUTPLOT,INDGEN(N_ELEMENTS(fim)),fim,imstr,$
          XTITL="# fringes",YTITL="Amplitude",$
          TITL="X="+STRCOMPRESS(STRING(xmin),/REMOVE_ALL)+':'+$
                   STRCOMPRESS(STRING(xmax),/REMOVE_ALL)

       ENDIF ELSE junk=DIALOG_MESSAGE("Plot ROI x-values must be contained within Phase ROI x-values")

       break_freq:
    END


    'Phase Unwrap':BEGIN ;Plot phase versus x

           IF N_ELEMENTS(imstr.cumulphase) LE 1 THEN BEGIN
          junk=DIALOG_MESSAGE("No Phase Calculation Exists")
                GOTO,break_phase
               ENDIF

          WIDGET_CONTROL,/HOURGLASS

          phxmin=imstr.phroi(0,0) & phxmax=imstr.phroi(0,1)
          phymin=imstr.phroi(1,0) & phymax=imstr.phroi(1,1)

          IF ymin GE phymin AND ymax LE phymax $
           AND xmin GE phxmin AND xmax LE phxmax THEN BEGIN

         ;im=imstr.image(phxmin:phxmax,phymin:phymax)
         ;nx=phxmax-phxmin+1 & ny=phymax-phymin+1
         ;;phim=FLTARR(nx,ny)

         ;WIDGET_CONTROL,imstr.fmin_id,GET_VALUE=fmin
         ;WIDGET_CONTROL,imstr.fmax_id,GET_VALUE=fmax
         ;fmin=FLOAT(fmin(0))
         ;fmax=FLOAT(fmax(0))

         ;result=find_phase(im,nx,ny,fmin,fmax,phim=phim)

         ;IF result EQ -1 THEN GOTO,break_phase

         ;cmlphim=FLTARR(nx,ymax-ymin+1)
         ;FOR kk=0,ymax-ymin DO BEGIN
         ;    cmlphim(*,kk)=cumulative_phase(phim(*,ymin+kk-phymin))
         ;ENDFOR
         ;;cumulfringe=-REBIN(cmlphim,nx,1)/(2.*!pi)
         cumulfringe=modaverage($
          imstr.cumulphase(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin),sigma=sig)

         OUTPLOT,time(imstr,xmin+INDGEN(xmax-xmin+1)),$
          cumulfringe,imstr,$
          error=sig,$
          XTITL="Time (ps)",YTITL="Fringe position",$
          TITL="X="+STRCOMPRESS(STRING(phxmin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(phxmax),/REMOVE_ALL)+$
          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)

          ENDIF ELSE junk=DIALOG_MESSAGE('Plot ROI must be contained within Phase ROI')

       break_phase:
    END
    'Velocity':BEGIN ;Plot velocity given by fringe shift

           IF N_ELEMENTS(imstr.cumulphase) LE 1 THEN BEGIN
          junk=DIALOG_MESSAGE("No Phase Calculation Exists")
                GOTO,break_velocity
               ENDIF

         WIDGET_CONTROL,imstr.vpf_id,GET_VALUE=vpf
         WIDGET_CONTROL,imstr.foffset_id,GET_VALUE=foffset
         WIDGET_CONTROL,imstr.bo_id,GET_VALUE=bo

         vpf=FLOAT(vpf(0))
         foffset=FLOAT(foffset(0))
         bo=FLOAT(bo(0))

          WIDGET_CONTROL,/HOURGLASS

          phxmin=imstr.phroi(0,0) & phxmax=imstr.phroi(0,1)
          phymin=imstr.phroi(1,0) & phymax=imstr.phroi(1,1)

          IF ymin GE phymin AND ymax LE phymax $
           AND xmin GE phxmin AND xmax LE phxmax THEN BEGIN

         cumulfringe=modaverage($
          imstr.cumulphase(xmin-phxmin:xmax-phxmin,ymin-phymin:ymax-phymin),sigma=sig)

         tt=time(imstr,xmin+INDGEN(xmax-xmin+1))

         preshock=WHERE(tt LT bo)
         postshock=WHERE(tt GE bo)

         IF preshock(0) EQ -1 THEN vel=vpf*(cumulfringe+foffset)
         IF postshock(0) EQ -1 THEN vel=vpf*cumulfringe
         IF preshock(0) NE -1 AND postshock(0) NE -1 THEN $
           vel=[vpf*cumulfringe(preshock),vpf*(cumulfringe(postshock)+foffset)]

         velsig=vpf*sig

         OUTPLOT,tt,vel,imstr,error=velsig,$
          XTITL="Time (ps)",YTITL="Velocity (mic/ns)",$
          TITL="X="+STRCOMPRESS(STRING(phxmin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(phxmax),/REMOVE_ALL)+$
          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)

          ENDIF ELSE junk=DIALOG_MESSAGE('Plot ROI must be contained within Phase ROI')

       break_velocity:
    END
    'Sweep Rate':BEGIN

        MinPix=imstr.MinPix & MaxPix=imstr.MaxPix
       cff=FLOAT(imstr.tpoly) & scff=FLOAT(imstr.spoly)

        px=FINDGEN(xmax-xmin+1)+xmin

       sweeprate=POLY(px,scff) ;Create the ps/pix vector

       wlo=WHERE(px LT MinPix)   & whi=WHERE(px GT MaxPix)
       IF wlo(0) NE -1 THEN BEGIN
         sweeprate(wlo)=POLY(MinPix,scff)
       ENDIF
       IF whi(0) NE -1 THEN BEGIN
         sweeprate(whi)=POLY(MaxPix,scff)
       ENDIF

;     t=time(imstr,px) & sweeprate=DERIV(px,t)

       OUTPLOT,px,sweeprate,imstr,$
        XTITL="Pixel",YTITL="Sweep Rate (ps/pix)",$
         TITL="Sweep Rate"

       break_sweep:
    END
    'Reflectivity':BEGIN
       IF imstr.inroi(0,0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define Intensity ROI")
         GOTO,break_refl
       ENDIF

           WIDGET_CONTROL,imstr.vimageID,GET_DRAW_VIEW=pt
       vxmin=pt(0)*imstr.factor
       vymin=pt(1)*imstr.factor

       IF xmin EQ xmax THEN BEGIN
         sz=SIZE(imstr.image)
         xmin=0 & xmax=sz(1)-1
       ENDIF

       signal=REBIN(FLOAT(imstr.image(xmin:xmax,ymin:ymax)),$
          xmax-xmin+1,1)
       normal=intensity(xmin,xmax,imstr);,offset=-600);insert offset

       ;no error is calculated here yet. need to fix

       refl=[[signal/normal],[signal],[normal]]


       OUTPLOT,time(imstr,xmin+INDGEN(xmax-xmin+1)),refl,imstr,$
        ERROR=FLTARR(N_ELEMENTS(refl)),$
         XTITL="Time (ps)",YTITL="Reflectivity",$
         TITL="X="+STRCOMPRESS(STRING(xmin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(xmax),/REMOVE_ALL)+$
          ", Y="+STRCOMPRESS(STRING(ymin),/REMOVE_ALL)+$
          ":"+STRCOMPRESS(STRING(ymax),/REMOVE_ALL)

       break_refl:
    END

    ELSE:

    ENDCASE

    break_plotnow:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END

'Plus':BEGIN
WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_plus
    ENDIF
IF imstr.factor GT 1 THEN BEGIN
    image_size=SIZE(imstr.image)
    imstr.factor=imstr.factor-1.

    imstr.xsize=image_size(1)/imstr.factor
    imstr.ysize=image_size(2)/imstr.factor

    WIDGET_CONTROL,imstr.vimageID,DRAW_XSIZE=image_size(1)/imstr.factor
    WIDGET_CONTROL,imstr.vimageID,DRAW_YSIZE=image_size(2)/imstr.factor
ENDIF

wshow_images,imstr,'IMAGE'

set_roi,imstr,imstr.xyroi,0
set_roi,imstr,imstr.phroi,1
set_roi,imstr,imstr.inroi,2
set_roi,imstr,imstr.ghroi,3

break_plus:
WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END

'Minus':BEGIN
WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       GOTO,break_minus
    ENDIF
IF imstr.factor LT 10 THEN BEGIN
    image_size=SIZE(imstr.image)
    imstr.factor=imstr.factor+1.

    imstr.xsize=image_size(1)/imstr.factor
    imstr.ysize=image_size(2)/imstr.factor

    WIDGET_CONTROL,imstr.vimageID,DRAW_XSIZE=image_size(1)/imstr.factor
    WIDGET_CONTROL,imstr.vimageID,DRAW_YSIZE=image_size(2)/imstr.factor
ENDIF

wshow_images,imstr,'IMAGE'

set_roi,imstr,imstr.xyroi,0
set_roi,imstr,imstr.phroi,1
set_roi,imstr,imstr.inroi,2
set_roi,imstr,imstr.ghroi,3

break_minus:
WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
;'Gridbut':BEGIN
;  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
;    imstr.gridstate=event.select
;  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
;END
;'Refbut':BEGIN
;  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
;    imstr.refstate=event.select
;  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
;END
;"Bkg only":BEGIN
;  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
;    imstr.bkgonly=event.select
;  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
;END
'Dattype':BEGIN
    WIDGET_CONTROL,event.handler,GET_UVALUE=imstr, /NO_COPY
    ;imstr.datlist=['Data','Reference','Grid','Bkg only']
    imstr.whichdat=imstr.datlist(event.value)
    break_dattype:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'No bkg':BEGIN
  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    imstr.bkgstate=event.select
  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END

'Negphase':BEGIN
  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    oldstate=imstr.negstate
    IF event.select NE oldstate THEN BEGIN
       IF N_ELEMENTS(imstr.cumulphase) GT 1 THEN $
         imstr.cumulphase= -imstr.cumulphase

       imstr.negstate=event.select
    ENDIF
  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'ClrRoi':BEGIN
  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    oldstate=imstr.roiclr
    IF event.select NE oldstate THEN BEGIN
       imstr.roiclr=event.select
    ENDIF
  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'Ylog':BEGIN
  WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    oldstate=imstr.ylog
    IF event.select NE oldstate THEN BEGIN
       imstr.ylog=event.select
    ENDIF
  WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'Plottype':BEGIN
WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
;   IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
;     junk=DIALOG_MESSAGE("No image loaded")
;     WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
;     GOTO,break_plottype
;   ENDIF

CASE imstr.plotlist(imstr.plotindex) OF
;    'Time derivative':BEGIN;autocorrelate
;       WIDGET_CONTROL,imstr.a_id,GET_VALUE=a
;       IF FLOAT(a(0)) EQ 0 THEN BEGIN
;         junk=DIALOG_MESSAGE("Time scale required")
;         WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
;         imstr.plotindex=0
;         GOTO,break_plottype
;       ENDIF
;    END
    'Freq spectrum' :BEGIN;freq spectrum
       IF imstr.phroi(0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define an ROI first")
         WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
         imstr.plotindex=0
         GOTO,break_plottype
       ENDIF
    END
    'Phase Image':BEGIN;phase image
       IF imstr.phroi(0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define an ROI first")
         WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
         imstr.plotindex=0
         GOTO,break_plottype
       ENDIF
    END
    'Phase Unwrap':BEGIN;phase unwrap
       IF imstr.phroi(0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define a Phase ROI first")
         WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
         imstr.plotindex=0
         GOTO,break_plottype
       ENDIF
    END
    'Reflectivity':BEGIN
       IF imstr.inroi(0) EQ -1 THEN BEGIN
         junk=DIALOG_MESSAGE("Define phase and inten. ROI first")
         WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
         imstr.plotindex=0
         GOTO,break_plottype
       ENDIF
    END
    ELSE:
ENDCASE

imstr.plotindex=event.index

break_plottype:
WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'Overtype':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY
    IF N_ELEMENTS(imstr.image) LE 1 THEN BEGIN
       junk=DIALOG_MESSAGE("No image loaded")
       WIDGET_CONTROL,event.id,SET_DROPLIST_SELECT=0
       GOTO,break_overtype
    ENDIF

    imstr.overindex=event.index

    break_overtype:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END
'Limtype':BEGIN
    WIDGET_CONTROL, event.handler, GET_UVALUE=imstr, /NO_COPY

    imstr.limindex=event.index

    break_limtype:
    WIDGET_CONTROL,event.handler, SET_UVALUE=imstr, /NO_COPY
END

ELSE: junk=DIALOG_MESSAGE("Program Error: Event User Value Not Found")
ENDCASE



END
