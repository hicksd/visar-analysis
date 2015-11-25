
;data=readxvisdat('c:\hicks\visar\BlipPlot.txt',x,y,ysig)

FUNCTION findsweep,x,y,psblip,order,NOPLOT=NOPLOT,FILE=FILE
;Takes a vector containing an intensity profile of fidu blips, y,
;versus position, x, and determines the peak fidu positions to a fraction of a pixel using
;a Fourier transform (to select the carrier freq and a suitable interval around it), and taking the
;centroid of the peaks.
;psblip is the number of ps per blip.
;order is the order of the polynomial fit

RED=255 & GREEN=255*256L & BLUE=255*256L*256L & YELLOW=RED+GREEN & PINK=RED+BLUE & LBLUE=GREEN+BLUE
ORANGE=255+126*256L & PURPLE=126+BLUE & PLAIN=RED+GREEN+BLUE & BACKG=0 & BROWN=174+71*256L
clr=[PLAIN,RED,GREEN,BLUE,BROWN,PINK,LBLUE,ORANGE,PURPLE,YELLOW]

IF KEYWORD_SET(file) THEN a=readxvisdat(file,x,y,ysig)

xmin=MIN(x) ;gives starting pixel of vector.

f0=1 ;max f up to which is considered "DC"

    y=[y,y,y] & f0=f0*3. ;pad to form periodic array to minimize edge effects

ny=N_ELEMENTS(y)
fwindow=1.;HANNING(ny,alpha=0.54)

Fy=FFT(y*fwindow)
Fy(0:f0)=0 ;Take FFT and set 0:f0 values to zero
Fy((ny-1)/2+1:(ny-1))=0 ;Set k<0 values to zero

ph=COMPLEXARR(ny)
Fph=COMPLEXARR(ny)

fpeak=MIN(WHERE(ABS(Fy) EQ MAX(ABS(Fy))))
fmin=0.7*fpeak & fmax=1.3*fpeak

Fph(fmin:fmax)=Fy(fmin:fmax)
ph=FFT(Fph,/INVERSE)

   ny=ny/3.
   ph=ph(ny:(2.*ny-1));recover original array by removing padding


ph=REAL_PART(ph)/ABS(ph)
edge=0.05   ;Don't use outer edge% of phase array (which may be distorted by FFT).
ph(0:edge*ny)=0. & ph((1-edge)*(ny-1):ny-1)=0

b=FLTARR(ny)
w=WHERE(ph GT 0.7) & b(w)=1. ;Find pixels that are above a given threshold and set them to 1
bw=LABEL_REGION(b) ;groups together regions that are above set threshold (i.e. wave crests)

bpix=FLTARR(MAX(bw)) ;create array to contain positions of wave crests.
FOR i=0,N_ELEMENTS(bpix)-1 DO BEGIN
    wi=WHERE(bw EQ i+1)
    bpix(i)=TOTAL(ph(wi)*wi)/TOTAL(ph(wi)) ;calculate centroid of crest.
ENDFOR
bpix=bpix+xmin ;Convert to actual x-positions of image

blip=FINDGEN(N_ELEMENTS(bpix)) ;blips are labelled sequentially
cff=REFORM(POLY_FIT(bpix,blip,order)) ;fits blip labels to pixel positions

bmin=ROUND(MIN(bpix)) & bmax=ROUND(MAX(bpix))
;xpix=FINDGEN(bmax-bmin+1)+bmin & bfit=POLY(xpix,cff) ;Use polynomial fit to create a continuous blip (i.e. time) versus pixel vector.
xpix=bpix & bfit=POLY(xpix,cff) ;Use polynomial fit to create a continuous blip (i.e. time) versus pixel vector.

scff=FLTARR(N_ELEMENTS(cff)-1) ;Calculate coefficients for polynomial fit of sweep rate
FOR i=1,N_ELEMENTS(cff)-1 DO scff(i-1)=cff(i)*i
bperpix=POLY(xpix,scff) ;Create the blip per pixel vector (i.e. ps/pix vector) for the whole y-vector.

;When pixels are outside the range where blips exist, make bperpix equal to the the nearest element where blips do exist
;bmin=MIN(bpix) & bmax=MAX(bpix)
;wmin=WHERE(xpix LE bmin) & IF wmin(0) NE -1 THEN bperpix(wmin)=bperpix(bmin-xmin)
;wmax=WHERE(xpix GE bmax) & IF wmax(0) NE -1 THEN bperpix(wmax)=bperpix(bmax-xmin)
;Integrate this edge-corrected sweep rate to give the final blips versus time fit


IF NOT(KEYWORD_SET(NOPLOT)) THEN BEGIN
!P.MULTI=[0,2,2]

PLOT,ABS(Fy(0:200)),XTITLE="Frequency (per pixel)",YTITLE="Power Spectrum"
OPLOT,[fmin,fmin],[0,1000],LINESTYLE=1
OPLOT,[fmax,fmax],[0,1000],LINESTYLE=1

PLOT,x,y
OPLOT,xpix,y(xpix-xmin),PSYM=2,COLOR=RED

PLOT,xpix,bfit*psblip,XSTYLE=16,XTITLE="Pixel",YTITLE="Time (ps)"
OPLOT,bpix,blip*psblip,PSYM=4

PLOT,[xpix,bpix],[bperpix*psblip,DERIV(bpix,blip)*psblip],/NODATA,$
    YSTYLE=16,XSTYLE=16,XTITLE="Pixel",YTITLE="Sweep (ps per pix)"
OPLOT,xpix,bperpix*psblip
OPLOT,bpix,DERIV(bpix,blip)*psblip,PSYM=4

!P.MULTI=0
ENDIF

;Returns a structure containing the sweep rate at each pixel along y, the blip positions,
;and the sweep rate at each blip position.
sweep={x:xpix,dtdp:bperpix*psblip,bx:bpix,bdtdp:DERIV(bpix,blip)*psblip,$
       cff:cff,scff:scff}

RETURN,sweep
END

FUNCTION compositesweep,files,psblip,order,noplot=noplot
;Takes several files containing fidu/comb data and combines them to produce a composite
;sweep structure
;psblip should be a vector containing the same number of elements as there are files. If
;psblip is single-valued it is replicated to the number of files there are.
RED=255 & GREEN=255*256L & BLUE=255*256L*256L & YELLOW=RED+GREEN & PINK=RED+BLUE & LBLUE=GREEN+BLUE
ORANGE=255+126*256L & PURPLE=126+BLUE & PLAIN=RED+GREEN+BLUE & BACKG=0 & BROWN=174+71*256L
clr=[PLAIN,RED,GREEN,BLUE,BROWN,PINK,LBLUE,ORANGE,PURPLE,YELLOW,PLAIN,RED,GREEN,BLUE,BROWN,PINK,LBLUE,ORANGE,PURPLE,YELLOW]


Nf=N_ELEMENTS(files)
IF N_ELEMENTS(psblip) LT Nf THEN psblip=REPLICATE(psblip(0),Nf)

nlist=INDGEN(Nf) ;a vector to contain starting elements of each set of sweep rate fits
blist=INDGEN(Nf) ;a vector to contain starting elements of each set of sweep rate fidus

FOR i=0,Nf-1 DO BEGIN
    a=readxvisdat(files(i),xx,yy,ysig)
    sweep=findsweep(xx,yy,psblip(i),order,NOPLOT=0) & wait,1.0
    IF i GT 0 THEN BEGIN
        x=[x,sweep.x] & dtdp=[dtdp,sweep.dtdp]
        bx=[bx,sweep.bx] & bdtdp=[bdtdp,sweep.bdtdp]
        IF i LT Nf-1 THEN BEGIN
            nlist(i+1)=nlist(i)+N_ELEMENTS(sweep.x)
            blist(i+1)=blist(i)+N_ELEMENTS(sweep.bx)
        ENDIF
    ENDIF ELSE BEGIN
       x=sweep.x & dtdp=sweep.dtdp
       bx=sweep.bx & bdtdp=sweep.bdtdp
       nlist(0)=0 & nlist(1)=N_ELEMENTS(sweep.x)
       blist(0)=0 & blist(1)=N_ELEMENTS(sweep.bx)
    ENDELSE
ENDFOR

PLOT,[x,bx],[dtdp,bdtdp],/NODATA,XSTYLE=16,YSTYLE=16,TICKLEN=1.0,$
    XTITLE="Pixel #",YTITLE="Sweep rate (ps/pix)"
FOR i=0,Nf-1 DO BEGIN
    nmin=nlist(i)
    IF i NE Nf-1 THEN nmax=nlist(i+1)-1 ELSE nmax=N_ELEMENTS(x)-1
    bmin=blist(i)
    IF i NE Nf-1 THEN bmax=blist(i+1)-1 ELSE bmax=N_ELEMENTS(bx)-1
    OPLOT,x(nmin:nmax),dtdp(nmin:nmax),COLOR=clr(i)
    OPLOT,bx(bmin:bmax),bdtdp(bmin:bmax),PSYM=4,COLOR=clr(i)
ENDFOR

scff=REFORM(POLY_FIT(x,dtdp,order-1))
xmin=ROUND(MIN(x)) & xmax=ROUND(MAX(x))
xx=FINDGEN(xmax-xmin)+xmin

OPLOT,xx,POLY(xx,scff),THICK=2

cff=FLTARR(N_ELEMENTS(scff)+1)
FOR i=1,N_ELEMENTS(cff)-1 DO cff(i)=(1./i)*scff(i-1)

sweep={x:x,dtdp:dtdp,bx:bx,bdtdp:bdtdp,scff:scff,cff:cff}

RETURN,sweep
END


PRO loadsweepfiles

;files = [$
;       'c:\hicks\omega\data\0708\sweep\swp2_48440.txt'$		;15ns asbo2
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48440r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48442.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48442r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48443.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48443r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48445.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48445r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48447.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp2_48447r.txt' ]
;psblip=REPLICATE(500.,N_ELEMENTS(files))
;order=7 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0709\sweep\swp2_48880.txt'$		;5ns asbo2
;       ,'c:\hicks\omega\data\0709\sweep\swp2_48880r.txt'$
;       ,'c:\hicks\omega\data\0709\sweep\swp2_48882.txt'$
;       ,'c:\hicks\omega\data\0709\sweep\swp2_48882r.txt' ]
;psblip=REPLICATE(500.,N_ELEMENTS(files))
;order=7 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0708\sweep\swp1_48440.txt'$		;9ns asbo1
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48440r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48442.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48442r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48443.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48443r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48445.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48445r.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48447.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\swp1_48447r.txt'$
;       ]
;
;psblip=REPLICATE(500.,N_ELEMENTS(files))
;order=8 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0706\sweep\sscA_47923.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47924.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47926.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47927.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47928.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47929.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47930.txt'$
;       ,'c:\hicks\omega\data\0706\sweep\sscA_47931.txt'$
;       ]
;
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=4 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0702\sweep\sop_46577.txt'$		;7ns SOP
;       ,'c:\hicks\omega\data\0702\sweep\sop_46578.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46580.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46581.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46582.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46583.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46588.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46590.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46592.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46593.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46594.txt'$
;       ,'c:\hicks\omega\data\0702\sweep\sop_46595.txt'$
;       ]
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=3 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0708\sweep\sop_48448.txt'$		;16ns SOP
;       ,'c:\hicks\omega\data\0708\sweep\sop_48448b.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48447.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48447b.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48442.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48442b.txt'$
;       ]
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=3 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0708\sweep\sop_48443.txt'$		;7ns SOP
;       ,'c:\hicks\omega\data\0708\sweep\sop_48443b.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48445.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48445b.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48440.txt'$
;       ,'c:\hicks\omega\data\0708\sweep\sop_48440b.txt'$
;       ]
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=3 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0711\sweep\swp2_49609r.txt',$ ;14ns Asbo2, Nov 2007
;       'c:\hicks\omega\data\0711\sweep\swp2_49610r.txt',$
;       'c:\hicks\omega\data\0711\sweep\swp2_49613r.txt',$
;       'c:\hicks\omega\data\0711\sweep\swp2_49616r.txt'$
;       ]
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=4 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0711\sweep\swp2_49447r.txt',$ ;14ns Asbo2, Nov 2007
;       'c:\hicks\omega\data\0711\sweep\swp2_49450r.txt',$
;       'c:\hicks\omega\data\0711\sweep\swp2_49450.txt',$
;       'c:\hicks\omega\data\0711\sweep\swp2_49451r.txt',$
;       'c:\hicks\omega\data\0711\sweep\swp2_49451.txt'$
;       ]
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=3 ;polynomial order
;
;files = [$
;       'c:\hicks\omega\data\0801\sweep\sop_49967.txt',$ ;50ns SOP, Jan 2008
;       'c:\hicks\omega\data\0801\sweep\sop_49967b.txt'$
;       ]
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=1 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0801\sweep\swp1_49967.txt',$ ;50ns asbo1, Jan 2008
;       'c:\hicks\omega\data\0801\sweep\swp1_49967r.txt'$
;       ]
;psblip=REPLICATE(2000.,N_ELEMENTS(files))
;order=4 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0801\sweep\swp2_49967.txt',$ ;50ns asbo2, Jan 2008
;       'c:\hicks\omega\data\0801\sweep\swp2_49967r.txt'$
;       ]
;psblip=REPLICATE(2000.,N_ELEMENTS(files))
;order=4 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0802\sweep\swp1_50364.txt',$ ;27ns Asbo1, FEb 2008
;       'c:\hicks\omega\data\0802\sweep\swp1_50364r.txt'$
;       ]
;psblip=REPLICATE(500.,N_ELEMENTS(files))
;order=4 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0712\sweep calib\sscA_49772.txt'$ ;4ns sscA - Dec 2007
;       ,'c:\hicks\omega\data\0712\sweep calib\sscA_49773.txt'$
;       ,'c:\hicks\omega\data\0712\sweep calib\sscA_49788.txt'$
;       ]
;
;psblip=REPLICATE(548.261,N_ELEMENTS(files))
;order=3 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0809\sweep\swp2_52113.txt',$ ;15ns Asbo2, Sept 3, 2008
;       'c:\hicks\omega\data\0809\sweep\swp2_52113r.txt'$
;       ]
;psblip=REPLICATE(500.,N_ELEMENTS(files))
;order=8 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0904\sweep\swp1_54123.txt'$ ;9ns Asbo1, Apr 8, 2009
;       ,'c:\hicks\omega\data\0904\sweep\swp1_54123r.txt'$ ;9ns Asbo1, Apr 8, 2009
;       ,'c:\hicks\omega\data\0904\sweep\swp1_54125.txt'$ ;9ns Asbo1, Apr 8, 2009
;       ,'c:\hicks\omega\data\0904\sweep\swp1_54125r.txt'$ ;9ns Asbo1, Apr 8, 2009
;       ]
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=8 ;polynomial order

;files = [$
;       'c:\hicks\omega\data\0904\sweep\swp2_54123.txt'$ ;15ns Asbo2, Apr 8, 2009
;       ,'c:\hicks\omega\data\0904\sweep\swp2_54123r.txt'$ ;15ns Asbo2, Apr 8, 2009
;       ,'c:\hicks\omega\data\0904\sweep\swp2_54125.txt'$ ;15ns Asbo2, Apr 8, 2009
;       ,'c:\hicks\omega\data\0904\sweep\swp2_54125r.txt'$ ;15ns Asbo2, Apr 8, 2009
;       ]
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=8 ;polynomial order
       
;files = [$
;        'c:\hicks\omega\data\0909\sweep\swp1_55502.txt'$ ;9ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55503.txt'$ ;9ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55505.txt'$ ;9ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55506.txt'$ ;9ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55507.txt'$ ;9ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55508.txt'$ ;9ns Asbo1, Sept 15, 2009
;       ]      
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=8 ;polynomial order
        
;files = [$
;        'c:\hicks\omega\data\0909\sweep\swp1_55510.txt'$ ;27ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55511.txt'$ ;27ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55512.txt'$ ;27ns Asbo1, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp1_55514.txt'$ ;27ns Asbo1, Sept 15, 2009
;       ]       
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=9 ;polynomial order

;files = [$
;        'c:\hicks\omega\data\0909\sweep\swp2_55501.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp2_55502.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp2_55503.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp2_55505.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp2_55506.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp2_55507.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ,'c:\hicks\omega\data\0909\sweep\swp2_55508.txt'$ ;15ns Asbo2, Sept 15, 2009
;       ]      
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=8 ;polynomial order

;files = [$
;        'c:\hicks\omega\data\1007\sweep\swp1_58613.txt'$ ;9ns Asbo1, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swp1R_58613.txt'$ ;9ns Asbo1, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swp1_58614.txt'$ ;9ns Asbo1, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swp1R_58614.txt'$ ;9ns Asbo1, July 7, 2010
;       ]      
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=8 ;polynomial order

;files = [$
;        'c:\hicks\omega\data\1007\sweep\swp2_58613.txt'$ ;15ns Asbo2, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swp2R_58613.txt'$ ;15ns Asbo2, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swp2_58614.txt'$ ;15ns Asbo2, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swp2R_58614.txt'$ ;15ns Asbo2, July 7, 2010
;       ]      
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=9 ;polynomial order

;files = [$
;        'c:\hicks\omega\data\1007\sweep\swpSOP_58613.txt'$ ;7ns SOP, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swpSOP_58614.txt'$ ;7ns SOP, July 7, 2010
;       ,'c:\hicks\omega\data\1007\sweep\swpSOPr_58614.txt'$ ;7ns SOP, July 7, 2010
;       ]      
;psblip=REPLICATE(506.10,N_ELEMENTS(files))
;order=6 ;polynomial order

;files = [$
;        'c:\Hicks\NIF\ShockTiming\Sim-VisLegArev8ycr_fidu1.txt'$ ;27ns Simulated NIF VISAR A, Aug 19, 2010
;       ,'c:\Hicks\NIF\ShockTiming\Sim-VisLegArev8ycr_fidu2.txt'$ ;27ns Simulated NIF VISAR B, Aug 19, 2010
;       ]      
;psblip=REPLICATE(1000.,N_ELEMENTS(files))
;order=1 ;polynomial order

files = [$
        'c:\Hicks\NIF\ShockTiming\Sim-VisLegBrev8ycr_fidu1.txt'$ ;27ns Simulated NIF VISAR A, Aug 19, 2010
       ,'c:\Hicks\NIF\ShockTiming\Sim-VisLegBrev8ycr_fidu2.txt'$ ;27ns Simulated NIF VISAR B, Aug 19, 2010
       ]      
psblip=REPLICATE(333.33,N_ELEMENTS(files))
order=1 ;polynomial order
;************
sweep=compositesweep(files,psblip,order)

PRINT,MIN(sweep.x),MAX(sweep.x)
PRINT,''
PRINT,TRANSPOSE([[FINDGEN(N_ELEMENTS(sweep.cff))],[sweep.cff]])

RETURN
END