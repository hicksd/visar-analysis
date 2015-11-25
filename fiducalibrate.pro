FUNCTION scanimage,file,shot,threshold=threshold,xr=xr,yr=yr,spin=spin,ref=ref

image=omegahdf(file,ref=ref)

IF KEYWORD_SET(spin) THEN image=ROT(image,FLOAT(spin)/!pi*180.,/INTERP)

sz=SIZE(image)
nx=sz(1)
IF KEYWORD_SET(xr) THEN BEGIN
	xmin=xr(0) & xmax=xr(1)
ENDIF ELSE BEGIN
	xmin=0 & xmax=sz(1)-1
ENDELSE

line=REFORM(REBIN(image(*,yr(0):yr(1)),nx,1))

;Find approximate baseline
h=HISTOGRAM(line(xmin:xmax),MIN=0)
w=WHERE(h EQ MAX(h(1:N_ELEMENTS(h)-1)))
w=w(0)+MIN(h)

;Subtract baseline and set negative values to zero
line=line-w & line=line*(line GT 0)
;Smooth and renomalize data to range [0,1]
line=SMOOTH(FLOAT(line),3)
range=(MAX(line)-MIN(line))
line=(line-MIN(line))/range
;Set all points below a threshold to zero.
line=line*(line GT threshold)
line=FIX(line*range)

titl=STRMID(file,STRPOS(file,'\',/REVERSE_SEARCH)+1,12)
PLOT,line,TITLE=titl,/XSTYLE,PSYM=3

shot=FIX(STRMID(titl,STRPOS(titl,'_',/REVERSE_SEARCH)+2,5))
RETURN,line
END

FUNCTION findfidu,data,peaks,xlim=xlim

a=LABEL_REGION(data)
; Get population and members of each blob:
h = HISTOGRAM(a, REVERSE_INDICES=r)
IF N_ELEMENTS(h) LT 2 THEN RETURN,-1

;h=h(1:N_ELEMENTS(h)-1)
;h=h(WHERE(ABS((h-MEAN(h))/MEAN(h)) LT 10.))

fidus=FLTARR(N_ELEMENTS(h)-1)
peaks=FLTARR(N_ELEMENTS(h)-1)

; Each region
FOR i=1, N_ELEMENTS(h)-1 DO BEGIN
   ;Find subscripts of members of region i.
   p = r(r[i]:r[i+1]-1)
   ; Pixels of region i
   q = data[p]

   fidus(i-1)=TOTAL(q*FLOAT(p))/TOTAL(q)
   peaks(i-1)=MAX(q)
ENDFOR

;Only use fidus that are greater than a
;certain number of pixels
h=h(1:N_ELEMENTS(h)-1)
ok=WHERE(h GT 5.)
fidus=fidus(ok)
peaks=peaks(ok)


IF KEYWORD_SET(xlim) THEN BEGIN
	valid=WHERE(fidus GT xlim(0) AND fidus LT xlim(1),count)
	IF count GT 0 THEN BEGIN
		fidus=fidus(valid)
		peaks=peaks(valid)
	ENDIF ELSE fidus=-1.
ENDIF

RETURN,fidus
END

PRO delfidu,f,del,p

n=N_ELEMENTS(f)

IF n GE 2 THEN BEGIN
	del=FLTARR(n-1)
	p=FLTARR(n-1)

	FOR i=0, n-2 DO BEGIN
		del(i)=f(i+1)-f(i)
		p(i)=(f(i+1)+f(i))/2.
	ENDFOR

ENDIF ELSE del=0.

RETURN
END

FUNCTION scanasbo,files,threshold,xr,yr,asbo,card,spin=spin

dp=-1.
p=-1.
shots=''

!P.MULTI=[0,5,5]

nfiles=N_ELEMENTS(files)
FOR i=0,nfiles-1 DO BEGIN
	line=scanimage(files(i),shot,threshold=threshold,xr=xr,yr=yr,spin=spin)
	fidus=findfidu(line,peaks,xlim=[xr(0),xr(1)])

	OPLOT,fidus,peaks,PSYM=7,COLOR=255

	delfidu,fidus,dx,x

	IF dx(0) GT 0. THEN BEGIN
		IF dp(0) NE -1 THEN BEGIN
			dp=[dp,dx]
			p=[p,x]
			shots=[shots,REPLICATE(shot,N_ELEMENTS(dx))]
		ENDIF ELSE BEGIN
			dp=dx
			p=x
			shots=REPLICATE(shot,N_ELEMENTS(dx))
		ENDELSE
	ENDIF

ENDFOR

PLOT,p,dp,XRANGE=[xr(0),xr(1)],YSTYLE=16,PSYM=3

!P.MULTI=0

result={asbo:asbo,card:card,p:p,dp:dp,shots:shots}

RETURN,result
END

PRO asbo1_3x,result

asbo=1
card=3
threshold=0.15
spin=0.0058

path="C:\Hicks\OMEGA\Data\0210\"
files=FINDFILE(path+"asbo1*.hdf")

xmin=290 & xmax=1230 ;fidu centres outside this range are eliminated
ymin=870 & ymax=927

xr=[xmin,xmax]
yr=[ymin,ymax]

result=scanasbo(files,threshold,xr,yr,asbo,card,spin=spin)

SAVE,result,FILENAME="Asbo1-3ns-0210.sav"

RETURN
END

PRO asbo2_3x,result

asbo=2
card=3
threshold=0.15
spin=-0.004

path="C:\Hicks\OMEGA\Data\0210\"
files=FINDFILE(path+"asbo2*.hdf")

xmin=255 & xmax=1278 ;fidu centres outside this range are eliminated
ymin=900 & ymax=945

xr=[xmin,xmax]
yr=[ymin,ymax]

result=scanasbo(files,threshold,xr,yr,asbo,card,spin=spin)

SAVE,result,FILENAME="Asbo2-3ns-0210.sav"

RETURN
END

PRO asbo2_10x,result

asbo=2
card=10
threshold=0.15
spin=-0.0027

path="C:\Hicks\OMEGA\Data\0207\"
files=FINDFILE(path+"asbo2*.hdf")

xmin=255 & xmax=1278 ;fidu centres outside this range are eliminated
ymin=850 & ymax=950

xr=[xmin,xmax]
yr=[ymin,ymax]

result=scanasbo(files,threshold,xr,yr,asbo,card,spin=spin)

SAVE,result,FILENAME="Asbo2-10ns-0207.sav"

RETURN
END

PRO shiftP0,x0,a,s,aa,ss
;This takes a polynomial of order "order" and coefficients
;"a" with errors "s" that is centered about x=x0
;(y=A+B(x-xm)+C(x-xm)^2...) and determines the coefficients and errors
;for an identical polynomial that is instead centered
;about x=0 (y=A'+B'x+C'x^2...)

order=N_ELEMENTS(a)-1

aa=FLTARR(order+1)
ss=FLTARR(order+1)

FOR j=0,order DO BEGIN
	FOR i=j,order DO BEGIN
		IF j GE 1 THEN BEGIN
			h=i  &	hi=h
			FOR k=1,j-1 DO BEGIN
				h=h-1. &  hi=hi*h
			ENDFOR
		ENDIF ELSE hi=1.

		l=1. & lo=l
		WHILE l LT j DO BEGIN
			l=l+1. & lo=lo*l
		ENDWHILE
		aa(j)=aa(j)+(hi/lo)*a(i)*(-FLOAT(x0))^(i-j)
		ss(j)=ss(j)+(s(i)*(hi/lo)*(-FLOAT(x0))^(i-j))^2
	ENDFOR
ENDFOR
ss=SQRT(ss)

RETURN
END

PRO plotfit,x,y,order

xm=MEAN(x) & x=x-xm
a=REFORM(POLY_FIT(x,y,order,CHISQ=q,SIGMA=s,YFIT=yf,YBAND=yb))

shiftp0,xm,a,s,aaa,sss
aa =FLTARR(order+1)
aap=STRARR(order+1)

ap =FLTARR(order+1)
sp =FLTARR(order+1)
as =STRARR(order+1)
asp=STRARR(order+1)

p0="p0 = "+STRCOMPRESS(STRING(xm))

FOR i=0,order DO BEGIN
	aa(i)=aaa(i)/(i+1.)

	ap(i)=a(i)/(i+1.)
	sp(i)=s(i)/(i+1.)

	IF i EQ 0 THEN as(i)='dt/dp = '
	as(i)=as(i)+'('+STRING(a(i),FORMAT='(G0.5)')+' +/-'+STRING(s(i),FORMAT='(G0.5)')+')'+'(p-p0)^'+STRING(i,FORMAT='(I1)')
	IF i NE order THEN as(i)=as(i)+'+!C!C'

	IF i EQ 0 THEN asp(i)='t = '
	asp(i)=asp(i)+'('+STRING(ap(i),FORMAT='(G0.5)')+' +/-'+STRING(sp(i),FORMAT='(G0.5)')+')'+'(p-p0)^'+STRING(i+1,FORMAT='(I1)')
	IF i NE order THEN asp(i)=asp(i)+'+!C!C'

	IF i EQ 0 THEN aap(i)='!c!c!c t = '
	aap(i)=aap(i)+STRING(aa(i),FORMAT='(G0.5)')+'p^'+STRING(i+1,FORMAT='(I1)')
	IF i NE order THEN aap(i)=aap(i)+'+'

ENDFOR

srt=SORT(x)
x=x(srt) & yf=yf(srt) & yb=yb(srt)

nn=100
xx=FINDGEN(nn+1)/nn*(MAX(x)-MIN(x))+MIN(x)
yyf=FLTARR(nn+1) & yyb=FLTARR(nn+1)

FOR j=0,order DO BEGIN
	yyf=yyf+a(j)*xx^j
	yyb=yyb+xx^(2*j)*s(j)^2
ENDFOR
yyb=SQRT(yyb)

OPLOT,xx+xm,yyf & OPLOT,xx+xm,yyf+yyb & OPLOT,xx+xm,yyf-yyb

xx=FINDGEN(nn+1)/nn*(MAX(x)-MIN(x))+MIN(x)+xm
yyf=FLTARR(nn+1) & yyb=FLTARR(nn+1)

;shiftP0,xm,a,s,aa,ss

FOR j=0,order DO BEGIN
	yyf=yyf+aaa(j)*xx^j
	yyb=yyb+xx^(2*j)*sss(j)^2
ENDFOR
yyb=SQRT(yyb)

;oplot,xx,yyf,color=255 & oplot,xx,yyf+yyb,color=255 & oplot,xx,yyf-yyb,color=255
;OPLOTERR,x+xm,yf,yb,3
;POLYFILL,[xx+xm,REVERSE(xx)+xm],[yyf+yyb,REVERSE(yyf)-REVERSE(yyb)],ORIENTATION=-45,SPACING=0.1,COLOR=255
print,aaa

print,p0
print,as
print,asp
print,aap

x0=!x.crange(0)+(!x.crange(1)-!x.crange(0))*0.05
y0=!y.crange(0)+(!y.crange(1)-!y.crange(0))*0.9
XYOUTS,x0,y0,STRCOMPRESS(p0+'!c!c'+STRJOIN(asp)+STRJOIN(aap),/REMOVE),CHARSIZE=0.8

RETURN
END

PRO findfit,ps=ps,eps=eps,hgl=hgl

IF KEYWORD_SET(ps) THEN BEGIN
SET_PLOT,'PS'
;DEVICE,FILENAME="C:\WINDOWS\DESKTOP\IDL.PS",/LANDSCAPE
DEVICE,FILENAME="C:\Documents and Settings\All Users\DESKTOP\IDL.ps"
ENDIF
IF KEYWORD_SET(hgl) THEN BEGIN
SET_PLOT,'hp'
;DEVICE,FILENAME="C:\WINDOWS\DESKTOP\IDL.hgl",/LANDSCAPE
DEVICE,FILENAME="C:\Documents and Settings\All Users\DESKTOP\IDL.HGL"
ENDIF
IF KEYWORD_SET(eps) THEN BEGIN
SET_PLOT,'PS'
;DEVICE,FILENAME="C:\WINDOWS\DESKTOP\IDL.PS",/LANDSCAPE
DEVICE,/ENCAPSULATED,LANDSCAPE=0,$
	FILENAME="C:\Documents and Settings\All Users\DESKTOP\IDL.eps"
ENDIF

dt=548.261 ;ps

RESTORE,FILENAME="c:\hicks\visar\Asbo1-3ns-0210.sav"
x1=result
RESTORE,FILENAME="c:\hicks\visar\Asbo2-3ns-0210.sav"
x2=result

!P.MULTI=[0,1,2]

;*********************
;Asbo 1
;PLOT,x1.p,x1.dp,$
;	TITLE="Asbo 1: 3 ns",XTITLE="Pixel #",YTITLE="Fidu separation (pix)",$
;	PSYM=4,YRANGE=[160,195],/YSTYLE

x=x1.p & y=dt/x1.dp
PLOT,x,y,$
	TITLE="Asbo 1: 3 ns",XTITLE="Pixel #",YTITLE="dt/dp (ps/pix)",$
	PSYM=4,YSTYLE=16
plotfit,x,y,1
;oplot,findgen(1536),2.7118+(findgen(1536))*0.0002037*2,psym=5,color=255

;*********************
;Asbo 2
;PLOT,x2.p,x2.dp,$
;	TITLE="Asbo 2: 3 ns",XTITLE="Pixel #",YTITLE="Fidu separation (pix)",$
;	PSYM=4,YRANGE=[160,195],/YSTYLE

x=x2.p & y=dt/x2.dp
PLOT,x,y,$
	TITLE="Asbo 2: 3 ns",XTITLE="Pixel #",YTITLE="dt/dp (ps/pix)",$
	PSYM=4,YSTYLE=16
plotfit,x,y,2
;x=findgen(1536)
;y=3.2144-2*0.0004949*x+3*3.015E-07*x^2
;oplot,x,y,color=255

;*********************

!P.MULTI=0

IF KEYWORD_SET(ps) OR KEYWORD_SET(hgl) OR KEYWORD_SET(eps) THEN BEGIN
	DEVICE,/CLOSE
	SET_PLOT,'WIN'
ENDIF

RETURN
END