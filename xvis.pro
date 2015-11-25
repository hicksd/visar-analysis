; To create a standalone xvis.sav file (e.g to run with IDL Virtual Machine)
; execute the following commands:
; 
; .FULL_RESET_SESSION 
; .r xvis
; .r xvis_event
; RESOLVE_ALL 
; SAVE, /ROUTINES, FILENAME = 'c:\hicks\visar\xvis.sav' 
;
PRO xvis, image

  factor=FLOAT(3) ;reduction factor of image

  image=BYTARR(1)
  ;image_size=BYTARR(1)
  xsize = 1536/factor ;1536/factor
  ysize = 1110/factor ;1024/factor

  xvisible=xsize;/factor
  yvisible=ysize;/factor

  phase_image=COMPLEXARR(1)-1
  cumul_image=FLTARR(1)-1
  ampl_image=FLTARR(1)-1
  nampl_image=FLTARR(1)-1

  ;show_full = 1
  ;order = 0
  ;use_congrid = 1
  title='xVIS: v6.95'
  block=0

  fonttype='';'HELVETICA*BOLD*14'

  base   = WIDGET_BASE(title=title, /COLUMN, MBAR=bar)
  cmbase = WIDGET_BASE(base,/ROW)
  ;frbase = WIDGET_BASE(base,/ROW)
  fbase  = WIDGET_BASE(base,/ROW)
  ibase  = WIDGET_BASE(base, /ROW)
  bbase  = WIDGET_BASE(base, /ROW)

  menu = CW_PDMENU(bar, /RETURN_NAME, /MBAR, $
                 ['1\File','0\Open','1\Copy','0\Copy Image','2\Copy Plot',$
                 '1\Save Image','0\TIF (no bin)','0\TIF (binned)','2\Raw binary (no bin)',$
                 '1\Save Plot ROI','0\Raw ROI','2\Save Scaled Image',$
                 '0\Save Plot as Text',$
                 '1\Print','0\Print Image','2\Print Plot',$
                 '2\Exit',$
                  '1\View','0\Log/Linear','0\Refresh',$
                  '0\Color Table','2\Color Palette',$
                  '1\Analysis','0\Linear Fit','0\Gaussian Fit','0\Find Warp Matrix','2\Apply Warp',$
                  '1\Tools','0\Camera Settings','0\Rotation',$
                  '1\Rotate or Flip','0\Right 90','0\Left 90','0\Flip Horizontal','2\Flip Vertical',$
                  '0\Shear','0\Median Filter','1\Subtract Background','0\Subtract Image',$
                  '2\Subtract DC offset',$
                  '0\Remove Ghost Fringes','0\Flatfield','0\Find Sweep','2\Film Scale',$
                  '1\Window','0\Image','0\Phase','0\Fringe Unwrap',$
                  '0\Amplitude','2\Normal Amplitude'], $
                  UVALUE='THEMENU')

  rot_id =  CW_FIELD(cmbase, TITLE='Rot (rad)',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=0)
  shr_id =  CW_FIELD(cmbase, TITLE='Shear',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=0)
  mag_id =  CW_FIELD(cmbase, TITLE='Mag. (mic/px)',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=1)
  mag0_id = CW_FIELD(cmbase, TITLE='d=0 (px)',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=0)
  medfilval_id = CW_FIELD(cmbase, TITLE='Median Filt(px)',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=5)

;  a_id =    CW_FIELD(cmbase, TITLE='A" (ps/px)',$
;            FONT="HELVETICA*BOLD*14",/COLUMN,VALUE=1)
;  b_id =    CW_FIELD(cmbase, TITLE='B" (ps/px^2)',$
;            FONT="HELVETICA*BOLD*14",/COLUMN,VALUE=0)
;  c_id =    CW_FIELD(cmbase, TITLE='C" (ps/px^3)',$
;            FONT="HELVETICA*BOLD*14",/COLUMN,VALUE=0)

  fidpx_id = CW_FIELD(cmbase, TITLE='Fidu (px)',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=0)
  fidtt_id = CW_FIELD(cmbase, TITLE='Fidu (ps)',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=0)
  fmin_id = CW_FIELD(cmbase, TITLE='f_min',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=5)
  fmax_id = CW_FIELD(cmbase, TITLE='f_max',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=200)
  bkg_id = CW_FIELD(cmbase, TITLE='Background',XSIZE=10,$
            FONT=fonttype,/COLUMN,VALUE=0)


;  delay_id= CW_FIELD(frbase, TITLE='dt (ps)',$
;            FONT="HELVETICA*BOLD*14",/COLUMN,VALUE=35.0)


  fname_id = WIDGET_LABEL(Fbase,/ALIGN_LEFT,/DYNAMIC_RESIZE,$
            VALUE='File: ')

  pix = WIDGET_LABEL(Fbase,/ALIGN_LEFT,/DYNAMIC_RESIZE,$
            VALUE="          [x,y,z]=")
  dt = WIDGET_LABEL(Fbase,/ALIGN_LEFT,/DYNAMIC_RESIZE,$
            VALUE="                   [ d (mic), t (ps) ]=")

  rbase   = WIDGET_BASE(base,/ROW)
  c1rbase = WIDGET_BASE(rbase,/COLUMN)
  c2rbase = WIDGET_BASE(rbase,/COLUMN)
  c3rbase = WIDGET_BASE(rbase,/COLUMN)
  c4rbase = WIDGET_BASE(rbase,/COLUMN)
  c5rbase = WIDGET_BASE(rbase,/COLUMN)

  plus=WIDGET_BUTTON(c1rbase,VALUE='Zoom In',UVALUE='Plus',FONT=fonttype)
  minus=WIDGET_BUTTON(c1rbase,VALUE='Zoom Out',UVALUE='Minus',FONT=fonttype)

;  c1rbaseA = WIDGET_BASE(c1rbase,/COLUMN,/EXCLUSIVE)
;  datbut = WIDGET_BUTTON(c1rbaseA,VALUE="Data",UVALUE="Datbut")
;  refbut = WIDGET_BUTTON(c1rbaseA,VALUE="Reference",UVALUE="Refbut")
;  gridbut = WIDGET_BUTTON(c1rbaseA,VALUE="Grid",UVALUE="Gridbut")
;  bkgonly= WIDGET_BUTTON(c1rbaseA,VALUE="Bkg only",UVALUE="Bkg only")
  datlist=['Data','Reference','Grid','Bkg only']
  dattype=CW_BGROUP(c1rbase,datlist,ROW=4,/EXCLUSIVE,UVALUE='Dattype')
  WIDGET_CONTROL,dattype,SET_VALUE=0
  
  plotindex=0
  plotlist=['Select Plot Type','Row Profile','Column Profile',$
         ;'Time derivative','Edge Find',$
         'Freq spectrum',$
         'Phase Unwrap','Velocity','Sweep Rate','Reflectivity']
  plottype=WIDGET_DROPLIST(c2rbase,VALUE=plotlist,$
         FONT=fonttype,UVALUE='Plottype',UNITS=1,XSIZE=1.2)

  overindex=0
  overlist=['New Plot','Overplot']
  overtype=WIDGET_DROPLIST(c2rbase,VALUE=overlist,$
         FONT=fonttype,UVALUE='Overtype',UNITS=1,XSIZE=1.2)

  limindex=0
  limlist=['Auto Limits','Manual Limits']
  limtype=WIDGET_DROPLIST(c2rbase,VALUE=limlist,$
         FONT=fonttype,UVALUE='Limtype',UNITS=1,XSIZE=1.2)

  c2rbaseA = WIDGET_BASE(c2rbase,/COLUMN,/NONEXCLUSIVE)
  negbut = WIDGET_BUTTON(c2rbaseA,VALUE="-ve phase",UVALUE="Negphase")
  Ylogbut = WIDGET_BUTTON(c2rbaseA,VALUE="Y-axis log",UVALUE="Ylog")
  roiclrbut = WIDGET_BUTTON(c2rbaseA,VALUE="Clr from inROI",UVALUE="ClrRoi")
  bkgbut = WIDGET_BUTTON(c2rbaseA,VALUE="No bkg sub",UVALUE="No bkg")
  ;deviceindex=0
  ;devlist=['Screen','PS','HGL','ASCII']
  ;plotdevice=WIDGET_DROPLIST(c2rbase,VALUE=devlist,$
  ;        FONT="HELVETICA*BOLD*14",UVALUE='Plotdevice',UNITS=1,XSIZE=1.2)


  phasecalc = WIDGET_BUTTON(c3rbase,VALUE='Calculate Phase',UVALUE='PhaseCalc')
  plotnow = WIDGET_BUTTON(c3rbase,VALUE='Plot Now',UVALUE='PlotNow')

  roilist=['Set Plot ROI','Set Phase ROI','Set Intensity ROI','Set Ghost ROI']
  roitype=CW_BGROUP(c3rbase,roilist,ROW=4,/EXCLUSIVE,UVALUE='Roitype')
  WIDGET_CONTROL,roitype,SET_VALUE=0

  tcol=['Plot ROI','Phase ROI','Intensity ROI','Ghost ROI','Plot Limits']
  trow=['Xmin','Xmax','Ymin','Ymax']
  xyvals=FLTARR(5,4)

  xytable = WIDGET_TABLE(c4rbase,COLUMN_LABELS=tcol,$
         ROW_LABELS=trow,ALIGNMENT=1,/EDITABLE,X_SCROLL_SIZE=4,$
         VALUE=xyvals,UVALUE='XYTABLE')

  VPF_id= CW_FIELD(c5rbase, TITLE='VPF',XSIZE=13,$
            FONT=fonttype,/COLUMN,VALUE=1.0)
  foffset_id= CW_FIELD(c5rbase, TITLE='Fringe offset',XSIZE=13,$
            FONT=fonttype,/COLUMN,VALUE=0)
  bo_id= CW_FIELD(c5rbase, TITLE='Break Out time',XSIZE=13,$
            FONT=fonttype,/COLUMN,VALUE=0.0)


  logplot=0 ; 0=Linear image, 1=Log image

  lastrot=0. ;Last rotation
  lastshear=0. ;Last shear

  ; Setting the managed attribute indicates our intention to put this app
  ; under the control of XMANAGER, and prevents our draw widgets from
  ; becoming candidates for becoming the default window on WSET, -1. XMANAGER
  ; sets this, but doing it here prevents our own WSETs at startup from
  ; having that problem.
  WIDGET_CONTROL, /MANAGED, base

    ;Create draw widget for image display with object graphics
    ;and index (ie. not rgb) color
    vimageID = WIDGET_DRAW(ibase, RETAIN=2, xsize=xsize, ysize=ysize, $
       /frame, /scroll, x_scroll_size=xvisible, y_scroll_size=yvisible,$
       GRAPHICS_LEVEL=2,COLOR_MODEL=0,/button_events,/motion_events,$
       EXPOSE_EVENTS=0,UVALUE='VImage Window')

    oimview=OBJ_NEW('IDLgrView')
    oimmodel=OBJ_NEW('IDLgrModel')
    oimimage=OBJ_NEW('IDLgrImage',BYTARR(10,10)) ;insert dummy 10x10 image

    r=(g=(b=INDGEN(256)));Load palette initially with grey scale (this will be changed
    oimpalette=OBJ_NEW('IDLgrPalette',r,g,b) ;when the first image is loaded.
    ;Create PLOT roi polyline object
    oimpolyxyroi=OBJ_NEW('IDLgrPolyline',INTARR(2,2));polyline object w/ initial dummy roi line
    oimmodelxyroi=OBJ_NEW('IDLgrModel')
    ;Create PHASE roi polyline object
    oimpolyphroi=OBJ_NEW('IDLgrPolyline',INTARR(2,2));polyline object w/ initial dummy roi line
    oimmodelphroi=OBJ_NEW('IDLgrModel')
    ;Create INTENSITY roi polyline object
    oimpolyinroi=OBJ_NEW('IDLgrPolyline',INTARR(2,2));polyline object w/ initial dummy roi line
    oimmodelinroi=OBJ_NEW('IDLgrModel')
    ;Create GHOST roi polyline object
    oimpolyghroi=OBJ_NEW('IDLgrPolyline',INTARR(2,2));polyline object w/ initial dummy roi line
    oimmodelghroi=OBJ_NEW('IDLgrModel')
    
    ;load image and polyline roi objects into corresponding models
     oimmodel -> Add, oimimage
     oimmodelxyroi -> Add,oimpolyxyroi
     oimmodelphroi -> Add,oimpolyphroi
     oimmodelinroi -> Add,oimpolyinroi
     oimmodelghroi -> Add,oimpolyghroi
    ;load image and polyline models into view object
     oimview -> Add, oimmodel
     oimview -> Add, oimmodelxyroi
     oimview -> Add, oimmodelphroi
     oimview -> Add, oimmodelinroi
     oimview -> Add, oimmodelghroi

    ;Create a draw widget for plot displays with object graphics
    plotID = WIDGET_DRAW(ibase,RETAIN=2,XSIZE=387,YSIZE=YSIZE,/FRAME,$
          GRAPHICS_LEVEL=2,EXPOSE_EVENTS=0)

    oplview =OBJ_NEW('IDLgrView',VIEWPLANE_RECT=[-0.3,-0.2,1.4,1.3]);[-1.0, -1.0, 2.0, 2.0]
    oplmodel=OBJ_NEW('IDLgrModel')

    oplxaxistitle=OBJ_NEW('IDLgrText','X-Axis',/ENABLE_FORMATTING,RENDER_METHOD=0,FILL_BACKGROUND=0)
    oplyaxistitle=OBJ_NEW('IDLgrText','Y-Axis',/ENABLE_FORMATTING,RENDER_METHOD=0,FILL_BACKGROUND=0)
    opltitle = OBJ_NEW('IDLgrText','Title',/ENABLE_FORMATTING,RENDER_METHOD=0,FILL_BACKGROUND=0)

;    opaxisfont = OBJ_NEW('IDLgrFont','Helvetica',SIZE=15) ;Font size for axes and titles.
;
;    oplxaxistitle -> SetProperty, FONT=opaxisfont
;    oplyaxistitle -> SetProperty, FONT=opaxisfont
;    opltitle -> SetProperty, FONT=opaxisfont

    oplxaxis0=OBJ_NEW('IDLgrAxis',0,EXACT=1,TITLE=oplxaxistitle)
    oplxaxis0 -> GetProperty, TICKTEXT=oXTickText
    oXTickText -> SetProperty, FONT=opaxisfont

    oplyaxis0=OBJ_NEW('IDLgrAxis',1,EXACT=1,TITLE=oplyaxistitle)
    oplyaxis0 -> GetProperty, TICKTEXT=oYTickText
    oYTickText -> SetProperty, FONT=opaxisfont

    oplxaxis1=OBJ_NEW('IDLgrAxis',0,EXACT=1,NOTEXT=1,TICKDIR=1,TEXTPOS=1,TITLE=opltitle)
    oplxaxis1 -> GetProperty, TICKTEXT=ox1TickText
    ox1TickText -> SetProperty, FONT=opaxisfont

    oplyaxis1=OBJ_NEW('IDLgrAxis',1,EXACT=1,NOTEXT=1,TICKDIR=1)


    oplplot =OBJ_NEW('IDLgrPlot')

    oplmodel -> Add,oplplot
    oplmodel -> Add,oplxaxis0 & oplmodel -> Add,oplxaxis1
    oplmodel -> Add,oplyaxis0 &    oplmodel -> Add,oplyaxis1

    oplview -> Add,oplmodel

    WIDGET_CONTROL, /REALIZE, base

    WIDGET_CONTROL, vimageID, GET_VALUE=oimwindow
    WIDGET_CONTROL, vimageID, SET_DRAW_VIEW=[0,0]

    WIDGET_CONTROL,plotID,GET_VALUE=oplwindow

  nPolyX=1
  nPolyY=1

  imstr={image:image,factor:factor,xsize:xsize,ysize:ysize,menu_id:menu,file:'',$
       xvisible:xvisible,yvisible:yvisible,$
     oimwindow:oimwindow,oimview:oimview,oimmodel:oimmodel,$
     oimpalette:oimpalette,oimimage:oimimage,$
     oimpolyxyroi:oimpolyxyroi,oimmodelxyroi:oimmodelxyroi,$
     oimpolyphroi:oimpolyphroi,oimmodelphroi:oimmodelphroi,$
     oimpolyinroi:oimpolyinroi,oimmodelinroi:oimmodelinroi,$
     oimpolyghroi:oimpolyghroi,oimmodelghroi:oimmodelghroi,$
     oplwindow:oplwindow,oplview:oplview,oplmodel:oplmodel,$
     oplxaxis0:oplxaxis0,oplxaxis1:oplxaxis1,$
      oplyaxis0:oplyaxis0,oplyaxis1:oplyaxis1,$
     oplplot:REPLICATE(oplplot,24),nplots:0,$ ;save room for up to 24 plots
     pixID:pix,dtID:dt,rot_id:rot_id,shr_id:shr_id,$
     lastrot:lastrot,lastshear:lastshear,$
     mag_id:mag_id,mag0_id:mag0_id,fidpx_id:fidpx_id,fidtt_id:fidtt_id,$
     ;a_id:a_id,b_id:b_id,c_id:c_id,$
     ;delay_id:delay_id,$
     MinPix:0.,MaxPix:2048.,$;Min and Max pixels between which a time sweep exists (starting point is whole image(or greater))
     tpoly:FLTARR(10+1),$ ;Polynomial coefficients for time sweep
     spoly:FLTARR(9+1),$ ;Polynomial coefficients for sweep rate
     fmin_id:fmin_id,fmax_id:fmax_id,$
     medfilval_id:medfilval_id,bkg_id:bkg_id,$
       fname_id:fname_id,vimageID:vimageID,plotID:plotID,$
       plotindex:plotindex,plottype:plottype,plotlist:plotlist,$
       overindex:overindex,overtype:overtype,overlist:overlist,$
       limindex:limindex,limtype:limtype,limlist:limlist,$
       roilist:roilist,roitype:roitype,$
       datlist:datlist,whichdat:'Data',bkgstate:0,$
;       datstate:0,refstate:0,gridstate:0,bkgonly:0,$
       negbut:negbut,negstate:0,roiclrbut:roiclrbut,roiclr:0,Ylogbut:Ylogbut,ylog:0,$
     ;deviceindex:deviceindex,plotdevice:plotdevice,devlist:devlist,$
     xytable:xytable,xyvals:xyvals,vpf_id:vpf_id,foffset_id:foffset_id,bo_id:bo_id,$
     logplot:logplot,$
     phase:phase_image,cumulphase:cumul_image,ampl:ampl_image,$
     normalampl:nampl_image,whichimage:'IMAGE',$
     k:[-1.],$;warp correction parameter
     roi:[[-1,-1],[0,0]],$ ;temporary roi for mouse click and drag
     xyroi:[[-1,-1],[0,0]],$;roi for plot region [[xmin,ymin],[xmax,ymax]]
     phroi:[[-1,-1],[0,0]],$;roi for phase analysis
     inroi:[[-1,-1],[0,0]],$;roi for intensity fidu
     ghroi:[[-1,-1],[0,0]],$;roi for ghost fringes
     oldphroi:[[-1,-1],[0,0]],$;stores old phase roi so that change can be undone.
     warp:{nPolyX:nPolyX,nPolyY:nPolyY,c:FLTARR(nPolyX+1,nPolyY+1),exist:0,$
            xmin:-1.,xmax:-1.,ymin:-1.,ymax:-1},$;
       drag:0,$
     pathopen:'',pathsave:'',xpath:'',pathfile:'',camerafile:'xvis_sc_settings.txt'}

  imstr.tpoly(1)=1.
  imstr.spoly(0)=1.

  temp=''
  imstr.xpath=STRSPLIT(FILE_WHICH('xvis.sav',/INCLUDE_CURRENT_DIR),'xvis.sav',/REGEX,/EXTRACT) ;Finds xvis.sav pathname
  imstr.pathfile=imstr.xpath+'xvisinfo.txt'
  camerafile=FILE_WHICH(imstr.xpath,imstr.camerafile) ;gets full path + filename information for
  
  IF FILE_TEST(camerafile) THEN BEGIN
    imstr.camerafile=camerafile(0)          ;camera settings stuff
  ENDIF ELSE BEGIN
    junk=DIALOG_MESSAGE("Warning! Camera file not found."+imstr.xpath+"!!")
  ENDELSE
  
  IF FILE_TEST(imstr.pathfile) THEN BEGIN
    OPENU,unit,imstr.pathfile,/GET_LUN
    IF NOT(EOF(unit)) THEN BEGIN
     READF,unit,temp
     imstr.pathopen=temp
     READF,unit,temp
     imstr.pathsave=temp
    ENDIF ELSE BEGIN
       PRINTF,unit,FILEPATH('')
       PRINTF,unit,FILEPATH('')
       imstr.pathopen=FILEPATH('')
       imstr.pathsave=FILEPATH('')
    ENDELSE
    CLOSE,unit
    FREE_LUN,unit
  ENDIF ELSE BEGIN
    OPENW,unit,imstr.pathfile,/GET_LUN
    PRINTF,unit,FILEPATH('')
    PRINTF,unit,FILEPATH('')
    imstr.pathopen=FILEPATH('')
    imstr.pathsave=FILEPATH('')
    CLOSE,unit
    FREE_LUN,unit
  ENDELSE

  ;DEVICE,DECOMPOSED=0
; Show the image(s) if one is present
  ;IF (image_size[0] NE 0) THEN wshow_images,imstr

  IF (N_ELEMENTS(group) EQ 0) THEN group=base
  ;WSET, SWIN

  WIDGET_CONTROL,base,SET_UVALUE=imstr,/NO_COPY

  XMANAGER, 'xvis', base, event='xvis_event', $
    GROUP_LEADER = GROUP, NO_BLOCK=(NOT(FLOAT(block)))

END
