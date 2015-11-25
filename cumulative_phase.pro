FUNCTION cumulative_phase,cplx,return_dphi=return_dphi
;Takes a linear array of complex numbers, 'cplx', and determines
;the phase difference between each successive element. Returns
;the cumulative phase difference of each element relative to the
;first.

;THE TECHNIQUE OF USING ONLY PHASE DIFFERENCES
;AVOIDS HAVING TO CHECK FOR LARGE (~ PI) PHASE
;JUMPS INHERENT IN USING THE TOTAL PHASE (SEE
;OLD TECHNIQUE BELOW)

;take the complex ratio of successive elements to
;calculate the phase difference between each element.
ratio=SHIFT(cplx,-1)/cplx
dphi=ATAN(IMAGINARY(ratio),FLOAT(ratio))
dphi=SHIFT(dphi,1)
dphi(0)=0.

IF KEYWORD_SET(return_dphi) THEN RETURN,dphi

cumul=FLTARR(N_ELEMENTS(dphi))
FOR ii=0,N_ELEMENTS(cumul)-1 DO BEGIN
	cumul(ii)=TOTAL(dphi(0:ii))
ENDFOR

RETURN,cumul
END



FUNCTION cumulative_phase_OLD,phase
;Takes a linear array of phase angles,"phase", finds the
;sequential phase difference, delta, between successive
;elements, and adds a 2*pi factor whenever the phase jump is
;more than 1.0*pi. Returns the cumulative phase difference of
;each element relative to the first.
delta=SHIFT(phase,-1)-phase
sign=(delta GT 0.)*2.-1
newcycle=WHERE(ABS(delta) GT !pi*1.0)
IF newcycle(0) NE -1 THEN $
delta(newcycle) = delta(newcycle) - 2.*!pi*sign(newcycle)
;The final (Nth) element in 'delta' makes no sense since it is just
;the phase diff between the first element and the last (due
;to the wrap around property of the SHIFT function). The cumulative
;phase difference at each element is the sum of all previous elements
;of delta
cumul=FLTARR(N_ELEMENTS(delta))
FOR ii=0,N_ELEMENTS(cumul)-1 DO BEGIN
	cumul(ii)=TOTAL(delta(0:ii))
ENDFOR

;Shift elements up one place and set the first element
;equal to zero.
cumul=SHIFT(cumul,1)
cumul(0)=0.

RETURN,cumul
END