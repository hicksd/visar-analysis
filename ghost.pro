
;FUNCTION gaussianfilter,Fy,k0,sig,notch=notch
;;Takes an FFT
;
;RETURN,gFy
;END

FUNCTION testimage,ghost
;creates a test image

nx=128 & ny=128 & im=FLTARR(nx,ny) & ghost=FLTARR(nx,ny)

bo1=nx/8*2.

;Stationary fringes
numph=!pi*2*ny/10.0
phase = FINDGEN(ny)/(ny-1)*numph

;x-dependent shift
shph1 = !pi*2*5.*1.
phx = (FINDGEN(nx)/(nx-1))^1*shph1 ;+ !pi/1.
phx(0:bo1)=0

amp0=1.
maxamp=0.
ampl=FINDGEN(nx)/(nx-1)*maxamp+amp0

FOR i=0,nx-1 DO im(i,*)=ampl(i)*COS(phase-phx(i)*1.)
FOR i=0,nx-1 DO ghost(i,*)=ampl(i)*COS(phase)

im=im*1.+ghost

RETURN,im
END

PRO testghost,im,imf
;attempts to get rid of ghost fringes
RED=255 & GREEN=255*256L & BLUE=255*256L*256L & YELLOW=RED+GREEN & PINK=RED+BLUE & LBLUE=GREEN+BLUE
ORANGE=255+126*256L & PURPLE=126+BLUE & PLAIN=RED+GREEN+BLUE & BACKG=0 & BROWN=174+71*256L
clr=[PLAIN,RED,GREEN,BLUE,BROWN,PINK,LBLUE,ORANGE,PURPLE,YELLOW]


im=testimage(ghost)
;im=omegahdf('c:\hicks\omega\data\0708\asbo2_s48445.hdf',bkg,dat)
;im=read_raw('c:\hicks\omega\data\0708\asbo1_48447.raw')
;im=read_raw('c:\hicks\omega\data\0708\asbo2_48447.raw')
;im=read_raw('c:\hicks\omega\data\0706\asbo2_47932.raw')

sz=SIZE(im) & nx=sz(1) & ny=sz(2)
Fim=FFT(im)

;Fim=Fim-FFT(ghost)

!P.MULTI=[0,3,2]

PLOT,FINDGEN(nx),FINDGEN(ny),/NODATA,XTITLE="x",YTITLE="y",TITLE="Unfiltered",$
  TICKLEN=-0.02,/XSTYLE,/YSTYLE;,COLOR=PLAIN,CHARSIZE=1.3,XTHICK=1.5,YTHICK=1.5,charthick=1.5
;Get size of plot window in device pixels.
PX = !X.WINDOW * !D.X_VSIZE & PY = !Y.WINDOW * !D.Y_VSIZE
;Desired size of image in pixels.
SX = PX[1] - PX[0] + 1 & SY = PY[1] - PY[0] + 1
TVSCL,CONGRID(im, SX, SY,INTERP=0), PX[0], PY[0] ;  TVSCL,im,XSIZE=SX,YSIZE=SY, PX[0], PY[0] ;for EPS or CGM

PLOT,FINDGEN(nx)-nx/2,FINDGEN(ny)-ny/2,/NODATA,XTITLE="Kx",YTITLE="Ky",TITLE="Power Spectrum",$
  TICKLEN=-0.02,/XSTYLE,/YSTYLE
;Get size of plot window in device pixels.
PX = !X.WINDOW * !D.X_VSIZE & PY = !Y.WINDOW * !D.Y_VSIZE
;Desired size of image in pixels.
SX = PX[1] - PX[0] + 1 & SY = PY[1] - PY[0] + 1
newim=ALOG(ABS(SHIFT(Fim,nx/2,ny/2)))
TVSCL,CONGRID(newim, SX, SY,INTERP=0), PX[0], PY[0] ;  TVSCL,im,XSIZE=SX,YSIZE=SY, PX[0], PY[0] ;for EPS or CGM

PLOT,FINDGEN(ny)-ny/2,SHIFT(ABS(Fim(0,*)),ny/2),PSYM=10,YLOG=1,/XSTYLE,XTITLE="Ky",YTITLE="Fy";,XRANGE=[200,250]
FOR i=1,10 DO OPLOT,FINDGEN(ny)-ny/2,SHIFT(ABS(fim(i*1,*)),ny/2),COLOR=clr(i MOD 10),PSYM=10

PLOT,FINDGEN(nx)-nx/2,FINDGEN(ny)-ny/2,/NODATA,XTITLE="Kx",YTITLE="Ky",TITLE="Angle",$
  TICKLEN=-0.02,/XSTYLE,/YSTYLE
PX = !X.WINDOW * !D.X_VSIZE & PY = !Y.WINDOW * !D.Y_VSIZE
SX = PX[1] - PX[0] + 1 & SY = PY[1] - PY[0] + 1
newim=SHIFT(ATAN(Fim,/PHASE),nx/2,ny/2)
TVSCL,CONGRID(newim, SX, SY,INTERP=0), PX[0], PY[0] ;  TVSCL,im,XSIZE=SX,YSIZE=SY, PX[0], PY[0] ;for EPS or CGM

PLOT,FINDGEN(ny)-ny/2,SHIFT(ATAN(Fim(0,*),/PHASE),ny/2),PSYM=10,YLOG=0,/XSTYLE,XTITLE="Ky",YTITLE="Fy";,XRANGE=[200,250]
FOR i=1,9 DO OPLOT,FINDGEN(ny)-ny/2,SHIFT(ATAN(Fim(i*1,*),/PHASE),ny/2),COLOR=clr(i MOD 10),PSYM=10



;eps=0.0
;Time direction
;Fim(0,*)=RANDOMN(1,ny)*Fim(0,*)
;Fim(0,*)=MEDIAN(REFORM(Fim(0,*)),5)

;Fim(0,*)=0.5*(Fim(1,*)+Fim(nx-1,*))
;Fim(0,*)=COMPLEX(REAL_PART(Fim(0,*)),0)
;Fim(0,*)=COMPLEX(0,IMAGINARY(Fim(0,*)))
;Fim(0,*)=COMPLEX(0,1)*Fim(0,*)
;Fim(0,*)=CONJ(Fim(0,*))-Fim(0,*)

FOR eps=-0.5,0.5,0.1 DO BEGIN
    theta=!pi/2.+eps
    ffim=Fim
    ffim(0,*)=Fim(0,*)*COMPLEX(COS(theta),SIN(theta))
    imff=REAL_PART(FFT(ffim,/INVERSE))
    PRINT,MEAN(imff)
ENDFOR

theta=!pi/2.
;Fim(0,*)=Fim(0,*)*COMPLEX(COS(theta),SIN(theta))

Fim=Fim*COMPLEX(0,1)

;xfmax=1. ;2*sigma
;Fim(0:xfmax,*)=0.
;FOR i=1,xfmax+1 DO BEGIN
;    Fim(i,*)=Fim(i,*)*(1-EXP(-i^2/(2.*(xfmax/2)^2)))
;    Fim(nx-i,*)=Fim(nx-i,*)*(1-EXP(-i^2/(2.*(xfmax/2)^2)))
;ENDFOR
;Fringe direction
;yfmax=5.
;IF yfmax GT 0 THEN BEGIN
;    Fim(*,0:yfmax)=eps & Fim(*,ny-yfmax:ny-1)=eps
;ENDIF ELSE Fim(*,0)=eps

imf=REAL_PART(FFT(Fim,/INVERSE))

PLOT,FINDGEN(nx),FINDGEN(ny),/NODATA,XTITLE="x",YTITLE="y",TITLE="Filtered",$
  TICKLEN=-0.02,/XSTYLE,/YSTYLE;,COLOR=PLAIN,CHARSIZE=1.3,XTHICK=1.5,YTHICK=1.5,charthick=1.5
;Get size of plot window in device pixels.
PX = !X.WINDOW * !D.X_VSIZE & PY = !Y.WINDOW * !D.Y_VSIZE
;Desired size of image in pixels.
SX = PX[1] - PX[0] + 1 & SY = PY[1] - PY[0] + 1
TVSCL,CONGRID(imf, SX, SY,/INTERP), PX[0], PY[0] ;  TVSCL,im,XSIZE=SX,YSIZE=SY, PX[0], PY[0] ;for EPS or CGM

;     OPENW,unit,'c:\hicks\omega\data\0706\asbo2_47932f.raw',/GET_LUN
;;       OPENW,unit,'c:\hicks\omega\data\0708\\asbo2_48447f.raw',/GET_LUN
;       OPENW,unit,'c:\hicks\visar\testimages\ghost1.raw',/GET_LUN
;           isz=SIZE(imf)
;           WRITEU,unit,UINT(isz(1)),UINT(isz(2))
;           WRITEU,unit,im
;       CLOSE,unit
;       FREE_LUN,unit


;TVSCL,CONGRID(im,256,256),0
;TVSCL,CONGRID(FFT((ABS(fim))^2,/INVERSE),256,256),1
!P.MULTI=0
RETURN
END