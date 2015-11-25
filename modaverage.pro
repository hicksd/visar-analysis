FUNCTION ModAverage,a,sigma=sigma
;This takes an array, a[nx,ny], and averages columns
;to produce a vector , modav[nx]. The averaging attempts
;to take into account the fact that numbers approximately
;an integer apart require averaging without this integer
;separation.

sz=SIZE(a)

nx=sz(1) & ny=sz(2)

modav=FLTARR(nx)
sigma=FLTARR(nx)

FOR ii=0,nx-1 DO BEGIN

	s=a(ii,*)
	mid=MEDIAN(s)
	del=s-mid

	off=WHERE(ABS(del) GT 0.5)
	IF off(0) NE -1 THEN BEGIN
		del(off)=del(off)-ROUND(del(off))
	ENDIF

	modav(ii)=MEAN(del)+mid
	sigma(ii)=STDDEV(del)
ENDFOR

RETURN,modav
END