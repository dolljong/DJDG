;****************************************************************************
; Program : ALLPLOTN
;           ALL PLOT using Non Block
;           By Suk-Jong Yi
;           2004/05/10
;****************************************************************************
; 주어진 조건을 이용해 Border를 출력 (1파일당 1도면)
; 주로 plot point점을 찾아서 출력하는 용도임
; *.set파일 참조
; --  *.set 파일 내용 --
; HP LaserJet 4V    -- print명
; A3 297 x 420 mm   -- 종이크기
; acad.ctb          -- ctb파일
; 0,0               -- offset
; LAYER=            -- Layer이름
; COLOR=            -- Color번호
; ENTITY=           -- Entity type[pline/line/point/blockname]
;****************************************************************************
(defun C:ALLPLOTN( /
                    fplot wpnts  low_left up_right temp 
                 )


  
  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (read-set)      ;read setting file

  (setq wpnts (get_wp layern colorn entityty))  ;get window points
  (setq low_left (car wpnts)
	up_right (cadr wpnts))
  
  (setq dwgn (dwg_name))                                ;파일 이름

  (setq pltn (strcat (getvar "DWGPREFIX") dwgn))      ; dwg방에 생김

  (if (> (car low_left) (car up_right))
    (setq temp low_left
          low_left up_right
          up_right temp)
  ); if

  (setq pltn (strcat pltn ".plt"))

  (commandplot printer_name printer_papersize low_left up_right offset style_name fplot pltn)

  (princ)
) ;of defun

;--------------------------------------------
; function : commandplot
;            plot
;            Yi Suk Jong
;            04/05/11
;--------------------------------------------
; input : printer_name(string) printer_papersize(sring)
;         low_left(point) up_right(point)
;         offset(tsring) style_name(string)
;         fplot("Y"/"N") pltn(string)
;--------------------------------------------
(defun commandplot ( printer_name printer_papersize low_left up_right offset style_name fplot pltn / )
  (command "PLOT"
    "y"                 ;Detailed plot configuration? [Yes/No] <No>: y
    ""                  ;Enter a layout name or [?] <Model>:
    printer_name        ;Enter an output device name or [?] <HP LaserJet 4V>:	 
    printer_papersize   ;Enter paper size or [?] <A3 297 x 420 mm>:	 
    "m"                 ;Enter paper units [Inches/Millimeters] <Inches>: m
    "Landscape"         ;Enter drawing orientation [Portrait/Landscape] <Landscape>:
    "n"                 ;Plot upside down? [Yes/No] <No>:
    "w"                 ;Enter plot area [Display/Extents/Limits/View/Window] <Display>: w
    low_left            ;Enter lower left corner of window <0.000000,0.000000>:
    up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
    "fit"               ;Enter plot scale (Plotted Millimeters=Drawing Units) or [Fit] <Fit>:
    offset	            ;Enter plot offset (x,y) or [Center] <0.00,0.00>: center
    "Y"                 ;Plot with plot styles? [Yes/No] <Yes>:
    style_name         ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:	 
    "Y"               ;Plot with lineweights? [Yes/No] <Yes>:
    "N"                ;Remove hidden lines? [Yes/No] <No>:
    fplot               ;Write the plot to a file [Yes/No] <N>:
  );command	      

  (if (= fplot "Y") (command pltn))  ;(Enter file name <C:\Program Files\ACAD2000\djdg\sample>:

  (command  "N")                ;Save changes to model tab [Yes/No]? <N>
  (command "y")                 ;Proceed with plot [Yes/No] <Y>: y
);defun

;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open 명령으로 파일을 불러올 경우 DWGNAME이 full path명이 되므로
; full path명중 파일명 부분을 추출해냄
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;파일이름 인식
  (setq ls (strlen dn))                                 ;string 길이
  (setq count ls)                                       ;마지막 string부터
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg에서 '.dwg'제거

) ;of defun



;---------------------------------------
; function : read-set
;            read setting file
;            yi suk jong
;            04/05/10
;---------------------------------------
; setting file을 읽어임
;---------------------------------------
(defun read-set( / setfn fn opf )
  (setq setfn "allplotn.set")
  (setq fn (strcat (prefix) "djdg/" setfn))
  (setq opf (open fn "r"))                           ;open file
  (setq printer_name (read-line opf))
  (setq printer_papersize (read-line opf))
  (setq style_name (read-line opf))
  (setq offset (read-line opf))
  (setq layern (read-line opf))   ;layer name
  (setq colorn  (read-line opf)) ;color number
  (setq entityty (read-line opf)) ;entity type
  (close opf)
  (setq layern (cadr (divide_str layern "="))
	colorn (cadr (divide_str colorn "="))
	entityty (cadr (divide_str entityty "=")))
  (if (= layern "*") (setq layern nil))
  (if (= colorn "*") (setq colorn nil) (setq colorn (atoi colorn)))
  (if (= entityty "*") (setq entityty nil))
;  (list layern colorn entityty)
);defun


;----------------------------------------
; function : get_wp
;            get window point
;            Yi Suk Jong
;            04/05/11
;----------------------------------------
; window point 얻냄
; input: 조건(layer, color, entityty)
; pitput: 양쪽 끝 entity(window point)
;----------------------------------------
(defun get_wp( layer color entity
	      / layer color entity filter ss nss minx minxe maxx maxxe index ename minx11 epoint)
  (setq filter nil)
  (if (/= nil entity) (setq filter (append filter (list (cons 0 entity)))))
  (if (/= nil layer) (setq filter (append filter (list (cons 8 layer)))))
  (if (/= nil color) (setq filter (append filter (list (cons 62 color)))))
  (setq ss (ssget "X" filter))
  (setq nss (sslength ss))          ;number of sset
  (cond
    ;--------------------- LINE Entity -----------------
    ((= (strcase entity) "LINE")
      (setq minxpnt (cdr (assoc 10 (entget (ssname ss 0))))
	    maxxpnt minxpnt)
      (setq minx (car minxpnt )   ;x of first line
      	    minxe (ssname ss 0))                                 ;minx entity name
      (setq maxx (car maxxpnt)   ;x of first line
	    maxxe (ssname ss 0))                                 ;minx entity name
      (setq index 1)
      (repeat (1- nss)
        (setq ename (ssname ss index))    ;entity name
        (setq x (car (cdr (assoc 10 (entget ename)))))   ;x value
        (if (< x minx)                    ;check min x
	  (setq minx x
	        minxe ename)
        );if
        (if (> x maxx)			;check max x
	  (setq maxx x
	        maxxe ename)
        );if
	(setq index (1+ index))
      ); repeat
      (setq minx11 (car (cdr (assoc 11 (entget minxe)))))
      (if (> minx11 minx)
	(setq minxpnt (cdr (assoc 11 (entget minxe))))
	(setq minxpnt (cdr (assoc 10 (entget minxe))))
      );if
     
      (setq maxx11 (car (cdr (assoc 11 (entget maxxe)))))     
      (if (< maxx11 maxx)
	(setq maxxpnt (cdr (assoc 11 (entget maxxe))))
	(setq maxxpnt (cdr (assoc 10 (entget maxxe))))
      );if

 ;    (print minxpnt)(command "circle" minxpnt "1")
 ;    (print maxxpnt)(command "circle" maxxpnt "2")
    );subcond
    ;-------------- Point entity -----------------------
    ((or (= (strcase entity) "POINT") (= (strcase entity) "CIRCLE"))
      (setq minxpnt (cdr (assoc 10 (entget (ssname ss 0))))
	    maxxpnt minxpnt)
      (setq minx (car minxpnt )   ;x of first line
      	    minxe (ssname ss 0))                                 ;minx entity name
      (setq maxx (car maxxpnt)   ;x of first line
	    maxxe (ssname ss 0))                                 ;minx entity name
      (setq index 1)
      (repeat (1- nss)
        (setq ename (ssname ss index))    ;entity name
	(setq epoint (cdr (assoc 10 (entget ename))))
        (setq x (car epoint))   ;x value
        (if (< x minx)                    ;check min x
	  (setq minx x
		minxpnt epoint)
        );if
        (if (> x maxx)			;check max x
	  (setq maxx x
		maxxpnt epoint)
        );if
	(setq index (1+ index))
      ); repeat
    );subcond
    
    ;-------------- LWPLINE entity -----------------------
    ((= (strcase entity) "LWPOLYLINE")
      (setq minxpnt  (getlwvert (entget (ssname ss 0)) 1)
	    maxxpnt minxpnt)
      (setq minx (car minxpnt))   ;x of first line
      (setq maxx (car maxxpnt))   ;x of first line

      (setq index 1)
      (repeat (1- nss)
        (setq ename (ssname ss index))    ;entity name
	(setq epoint (getlwvert (entget ename) 1))
        (setq x (car epoint))   ;x value
        (if (< x minx)                    ;check min x
	  (setq minx x
		minxpnt epoint)
        );if
        (if (> x maxx)			;check max x
	  (setq maxx x
		maxxpnt epoint)
        );if
	(setq index (1+ index))
      ); repeat
;     (print minxpnt)(command "circle" minxpnt "1")
;     (print maxxpnt)(command "circle" maxxpnt "2")
    );subcond  
  );cond
  (list minxpnt maxxpnt)
);defun