FUNCTION get_fom,eps
;Calculates the figure of merit, determining how well the ghost fringes minimize the image
COMMON ghostblock,FFim

theta=!pi/2.+eps(0)
FFim(0,*)=FFim(0,*)*COMPLEX(COS(theta),SIN(theta))
imff=REAL_PART(FFT(FFim,/INVERSE))
fom=MEAN(ABS(imFF))


RETURN,fom
END

FUNCTION minghost,Fim
;Takes the FFT of an image, Fim, and calculates the optimal amount to reduce Fim(0,*)
;in order to minimize the effect of ghost fringes. Fim(0,*) is reduced by rotating in complex
;space by an angle close to !pi/2. The angle eps is returned, corresponding to the deviation of
;the rotation from the ideal (!pi/2).
COMMON ghostblock

FFim=Fim

;epsmin=IMSL_FMIN('get_fom',-!pi/3,!pi/3,STEP=1.0,XGUESS=0,MAX_EVALS=20)
;print,epsmin

eps0=!pi/3.
eps=FINDGEN(11)/10*eps0*2-eps0
fom=FLTARR(N_ELEMENTS(eps))
FOR i=0,N_ELEMENTS(eps)-1 DO BEGIN
        theta=!pi/2.+eps(i)
        ffim=Fim
        ffim(0,*)=Fim(0,*)*COMPLEX(COS(theta),SIN(theta))
        imff=REAL_PART(FFT(ffim,/INVERSE))
        fom(i)=MEAN(ABS(imff))
        print,eps(i),fom(i)
ENDFOR
minfom=MIN(fom,w)
print,eps(w)
epsmin=eps(w)

RETURN,epsmin
END