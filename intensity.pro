FUNCTION intensity,xmin,xmax,imstr,offset=offset
;Returns the averaged intensity fidu strength over the requested
;range of x.

IF KEYWORD_SET(offset) THEN BEGIN
;If the fiducial is offset by a particular time, must
;resample the intensity fiducial with the corrected time offset

sz=SIZE(imstr.image)
xinten=REBIN(FLOAT(imstr.image(*,imstr.inroi(1,0):imstr.inroi(1,1))),$
		sz(1),1)

tt=time(imstr,INDGEN(sz(1)))
ttnew=tt+offset
xinten=INTERPOL(xinten,ttnew,tt,/SPLINE)

xinten=xinten(xmin:xmax)
RETURN,xinten
ENDIF

xinten=REBIN(FLOAT(imstr.image(xmin:xmax,imstr.inroi(1,0):imstr.inroi(1,1))),$
		xmax-xmin+1,1)

RETURN,xinten
END