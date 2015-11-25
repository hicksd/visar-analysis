FUNCTION find_phase,im,nx,ny,fmin,fmax,phim=phim,coh=coh,incoh=incoh

fwindow=HANNING(ny,alpha=0.54)

phim=COMPLEXARR(nx,ny)
coh=FLTARR(nx,ny)
incoh=FLTARR(nx,ny)

IF fmin GE 0 AND fmax LE ny/2.-1 AND fmax GT fmin THEN BEGIN

	FOR ii=0,nx-1 DO BEGIN
		col=im(ii,*)
		fcol=FFT(col*fwindow)

		;IF KEYWORD_SET(incoh) THEN BEGIN
			infcol=COMPLEXARR(ny)
			infcol(0:fmin-1)=fcol(0:fmin-1)
			infcol(ny-1-(fmin-1):ny-1)=fcol(ny-1-(fmin-1):ny-1)
			incol=FFT(infcol,/INVERSE)
			incoh(ii,*)=FLOAT(incol)/fwindow
		;ENDIF
		;IF KEYWORD_SET(phim) THEN BEGIN
			zfcol=COMPLEXARR(ny)
			zfcol(fmin:fmax)=fcol(fmin:fmax)
			zcol=FFT(zfcol,/INVERSE)
			;phim(ii,*)=ATAN(IMAGINARY(zcol),FLOAT(zcol))
			phim(ii,*)=zcol;/ABS(zcol)
		;ENDIF
		;IF KEYWORD_SET(coh) THEN BEGIN
			;zfcol=COMPLEXARR(ny)
			;zfcol(fmin:fmax)=fcol(fmin:fmax)
			;zcol=FFT(zfcol,/INVERSE)
			coh(ii,*)=2.*ABS(zcol)/fwindow
		;ENDIF

	ENDFOR

ENDIF ELSE BEGIN
 junk=DIALOG_MESSAGE('0 < fmin < fmax AND fmin < fmax < ny/2-1')
 RETURN,-1
ENDELSE

RETURN,1
END

FUNCTION find_phase_OLD,im,nx,ny,fmin,fmax,phim=phim,coh=coh,incoh=incoh

fwindow=HANNING(ny,alpha=0.54)

phim=FLTARR(nx,ny)
coh=FLTARR(nx,ny)
incoh=FLTARR(nx,ny)

IF fmin GE 0 AND fmax LE ny/2.-1 AND fmax GT fmin THEN BEGIN

	FOR ii=0,nx-1 DO BEGIN
		col=im(ii,*)
		fcol=FFT(col*fwindow)

		;IF KEYWORD_SET(incoh) THEN BEGIN
			infcol=COMPLEXARR(ny)
			infcol(0:fmin-1)=fcol(0:fmin-1)
			infcol(ny-1-(fmin-1):ny-1)=fcol(ny-1-(fmin-1):ny-1)
			incol=FFT(infcol,/INVERSE)
			incoh(ii,*)=FLOAT(incol)/fwindow
		;ENDIF
		;IF KEYWORD_SET(phim) THEN BEGIN
			zfcol=COMPLEXARR(ny)
			zfcol(fmin:fmax)=fcol(fmin:fmax)
			zcol=FFT(zfcol,/INVERSE)
			phim(ii,*)=ATAN(IMAGINARY(zcol),FLOAT(zcol))
			;phim(ii,*)=zcol;/ABS(zcol)
		;ENDIF
		;IF KEYWORD_SET(coh) THEN BEGIN
			;zfcol=COMPLEXARR(ny)
			;zfcol(fmin:fmax)=fcol(fmin:fmax)
			;zcol=FFT(zfcol,/INVERSE)
			coh(ii,*)=2.*ABS(zcol)/fwindow
		;ENDIF

	ENDFOR

ENDIF ELSE BEGIN
 junk=DIALOG_MESSAGE('0 < fmin < fmax AND fmin < fmax < ny/2-1')
 RETURN,-1
ENDELSE

RETURN,1
END