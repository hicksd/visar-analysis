PRO warpimage

; Create the original image:

n=256
f=8
phi=FINDGEN(n)/(n-1)*2.*!pi*f

im=REBIN(COS(phi),n,n)
im=TRANSPOSE(TEMPORARY(im))

; Now set up the Xi's and Yi's:
;XI = [24, 35, 102, 92]
;YI = [81, 24, 25, 92]

; Enter the Xo's and Yo's:
;XO = [61, 62, 143, 133]
;YO = [89, 34, 38, 105]

x0=REBIN(FINDGEN(n),n,n)
y0=TRANSPOSE(x0)

x1=x0

a=-2.*(y0-n/2)/(0.15*n)*EXP(-((y0-n/2)/(0.15*n))^2) * 5.

y1=y0+a*(1-(y0/(n/2)-1)^2)

ndegree=6
coeff=POLY_FIT(y0,y1,ndegree,yfit)

; Run POLYWARP to obtain a Kx and Ky:
;POLYWARP,x1,y1,x0,y0,3, KX, KY

Kx=FLTARR(ndegree+1,ndegree+1) & Kx(0,1)=1.
Ky=FLTARR(ndegree+1,ndegree+1) & Ky(*,0)=coeff

; Create a warped image based on Kx and Ky with POLY_2D:
result = POLY_2D(im, KX, KY, /CUBIC)
print,kx
print,ky

WINDOW,/FREE,XSIZE=2.*n,YSIZE=2*n
!p.multi=[0,1,2]
PLOT,y0,y1-y0
OPLOT,y0,yfit-y0,LINESTYLE=1

TVSCL,im,0,0
TVSCL,result,n,0

!P.MULTI=0
RETURN
END

PRO warpimage2

; Create the original image:
n=256
f=8
phi=FINDGEN(n)/(n-1)*2.*!pi*f

im=REBIN(COS(phi),n,n)
im=TRANSPOSE(TEMPORARY(im))

x0=REBIN(FINDGEN(n),n,n)
y0=TRANSPOSE(x0)

x1=x0

a=-2.*(y0-n/2)/(0.15*n)*EXP(-((y0-n/2)/(0.15*n))^2) * 5.

y1=y0+a*(1-(y0/(n/2)-1)^2)

im_new=FLTARR(n,n)
FOR ii=0,n-1 DO BEGIN
	im_new(ii,*)=INTERPOL(im(ii,*),y0(ii,*),y1(ii,*))
ENDFOR

WINDOW,/FREE,XSIZE=2.*n,YSIZE=2*n
!p.multi=[0,1,2]
PLOT,REFORM(y0(0,*)),REFORM(im(0,*))
OPLOT,REFORM(y0(0,*)),REFORM(im_new(0,*)),LINESTYLE=1

TVSCL,im,0,0
TVSCL,im_new,n,0

!P.MULTI=0
RETURN
END



PRO triwarp,result

n=256
f=8
phi=FINDGEN(n)/(n-1)*2.*!pi*f

im=REBIN(COS(phi),n,n)
im=TRANSPOSE(TEMPORARY(im))

x0=REBIN(FINDGEN(n),n,n)
y0=TRANSPOSE(x0)

x1=x0

a=-2.*(y0-n/2)/(0.15*n)*EXP(-((y0-n/2)/(0.15*n))^2) * 5.

y1=y0+a*(1-(y0/(n/2)-1)^2)

result=WARP_TRI(x0,y0,x1,y1,im)


WINDOW,/FREE,XSIZE=2.*n,YSIZE=n
TVSCL,im,0,0
TVSCL,result,n,0

RETURN
END

PRO cosfit,x,k,y,pder

phi=k(0)+(k(1)+k(2)*x)*x
y=COS(phi)

IF N_PARAMS() GE 4 THEN $
    pder = [[-SIN(phi)],[-SIN(phi)*x],[-SIN(phi)*x^2]]

END

FUNCTION lmcosfit,x,k
	phi=k(0)+(k(1)+k(2)*x)*x
	RETURN,[[COS(phi)],[-SIN(phi)],[-SIN(phi)*x],[-SIN(phi)*x^2]]
END

PRO trycosfit,x,k


n=1023
x=FINDGEN(n+1)/n*2.*!PI
phi0=0.7
k1=15.
k2=0.0


phi=phi0+k1*x+k2*x^2
y=COS(phi)

k=[0.,14.8,0.]

weights=1/y;REPLICATE(1.0,n+1)
;yfit=CURVEFIT(x,y, weights, k, SIGMA, FUNCTION_NAME='cosfit',ITMAX=10000,/DOUBLE)
yfit=LMFIT(x,y,k,/DOUBLE,SIGMA=sigma,FUNCTION_NAME='lmcosfit')

print,k,SIGMA
PLOT,x,y
OPLOT,x,yfit,LINESTYLE=1

END


