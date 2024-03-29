FUNCTION imagetostructure,q,tagname,image

;The following inserts the new image, 'image', into the
;structure 'q' at q.tagname. 'image' does not need to be
;the same size or type as that existinag as q.tagname.
	names=TAG_NAMES(q)
	FOR ii=0,N_TAGS(q)-1 DO BEGIN
		IF names(ii) NE tagname THEN BEGIN
		  IF N_ELEMENTS(qnew) NE 0 THEN $
			qnew=CREATE_STRUCT(qnew,names(ii),q.(ii)) $
		  ELSE qnew=CREATE_STRUCT(names(ii),q.(ii))
		ENDIF
	ENDFOR
	qnew=CREATE_STRUCT(qnew,tagname,image)
	q=0

RETURN,qnew
END