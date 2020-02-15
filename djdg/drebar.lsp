;-------------------------
; Program : DREBAR
;           Dot Rebar
;           Yi Suk Jong
;           04/04/02
;-------------------------
; Draw Dot Rebars using dimension
;--------------------------------

;------------------------
; Program : CDIM
;           Copy referencing DIMension
;           Yi Suk Jong
;           04/08/23
;-------------------------

;------------------------
; Program : D2L
;           Dot to Line
;           Yi Suk Jong
;           04/08/30
;-------------------------



(defun c:drebar(
		/ gapld dimss bln side pbnt1 bpnt2 ndim plist index dment
		  ass01 ass10 ass13 ass14 ass50 elang new13 cpnt cpang
		  ang213 dtlist ldtlist gap pnt14 pnt13 num inum pntin pnt 
		)
  (setq gapld 30)         ;gap between line and donut

  (princ "\nSelect Dimensions: ")
  (setq dimss (ssget '((0 . "DIMENSION"))))
  (setq bln (entget (car (entsel "\nSelect base line: "))))
  (setq side (getpoint "\npick Side: "))

  (setq bpnt1 (cdr (assoc 10 bln))    ;start point of base line
	bpnt2 (cdr (assoc 11 bln)))   ;end point of base line
  
  (setq ndim (sslength dimss))
  
  (setq plist nil)
  (setq index 0)
  (repeat ndim
    (setq dment (entget (ssname dimss index)))
    (setq ass01 (cdr (assoc 1 dment))
          ass10 (cdr (assoc 10 dment))
	  ass13 (cdr (assoc 13 dment))
	  ass14 (cdr (assoc 14 dment))
	  ass50 (cdr (assoc 50 dment)))
    
    (setq elang (angle ass10 ass14))          ;ang of extention line
    (setq new13 (inters ass13 (polar ass13 elang 100) ass14 (polar ass14 ass50 100) nil))
    (setq cpnt (inters bpnt1 bpnt2 side (polar side elang 100)  nil))
    (setq cpang (angle cpnt side))     ;angle to side point
    
    (setq ang213 (angle ass14 new13))
    (setq dtlist (div_dimtxt ass01))
    (setq ldtlist (length dtlist));length of dim text list
    (cond
      ((= ldtlist 1)
        (setq gap (atof (car dtlist)))
        (setq pnt14 (polar (inters ass10 ass14 bpnt1 bpnt2 nil) elang gapld))
        (setq pnt13 (polar (inters new13 (polar new13 elang 100) bpnt1 bpnt2 nil) elang gapld))
        (command "donut" "0" "20" pnt14 pnt13 "")
        ;(command "circle" pnt14 "20")
        ;(command "circle" pnt13 "20")
       );sub cond
      ((or (= ldtlist 2) (= ldtlist 3))
        (setq num (atoi (car dtlist))) 
        (setq gap (atof (cadr dtlist)))
        (setq pnt14 (polar (inters ass10 ass14 bpnt1 bpnt2 nil) elang gapld))
        (setq pnt13 (polar (inters new13 (polar new13 elang 100) bpnt1 bpnt2 nil) elang gapld))
        (command "donut" "0" "20" pnt14 pnt13 )
        ;(command "circle" pnt14 "20")
        ;(command "circle" pnt13 "20")
        (setq inum 1)
        (repeat (1- num)   ;except last point(ass13)
	  (setq pnt (polar ass14 ang213 (* gap inum)))
          (setq pntin (polar (inters pnt (polar pnt elang 100) bpnt1 bpnt2 nil) elang gapld))	  ;point inner
          (command pntin)
	  ;(command "circle" pntin "20")
	  (setq inum (1+ inum))
	);repeat
        (command)
       );sub cond
    );cond
    (setq index (1+ index))
  );repeat  
)  ;defun

;-----------------------------------
;function : div_dimtxt
;           Yi Suk JOng
;           00/7/15
;----------------------------------
; str1 : dim string
;다양한 형태의 dimension text를 잘라서 list로 만들어준다. 
;단순한 1.000치수인 경우에는 1.000을 리턴, 10@100인경우에
;는 (10 100)을 리턴,10@100=1.000 인경우엔 (10 100 1.000)을
;리턴해준다
;----------------------------------------------------------
(defun div_dimtxt(dimstr / dimstrlen golpos eqpos)
  (setq dimstrlen (strlen dimstr)
        golpos (str_position dimstr "@")
	eqpos (str_position dimstr "="))
  
  (cond
    ((and (= golpos nil) (= eqpos nil))   ;1.000
      (list dimstr)
    )
    ((and (/= golpos nil) (= eqpos nil))  ;10@100
     (list (substr dimstr 1 (1- golpos))
	   (substr dimstr (1+ golpos) (- dimstrlen golpos)))
    )
    ((and (/= golpos nil) (/= eqpos nil))  ;10@100=1.000
     (list (substr dimstr 1 (1- golpos))
	   (substr dimstr (1+ golpos) (- eqpos golpos 1))
           (substr dimstr (1+ eqpos) (- dimstrlen eqpos )))
    ) 
    ((and (= golpos nil) (/= eqpos nil))  ;=1.000
     (list dimstr)
    ) 
    
  );cond  
  
);defun div_dimtxt



;------------------------
; Program : CPDIM
;           CoPy referencing DIMension
;           Yi Suk Jong
;           04/08/23
;-------------------------

(defun c:cpdim(
		/ dimss cpess bp dnim plist index ndim dment ass01 ass10 ass13 ass14 ass50
	          elang dtlist ldtlist angdl pnt14 pnt13 num gap inum pnt pntin pnum
		)

  (princ "\nSelect Dimensions: ")
  (setq dimss (ssget '((0 . "DIMENSION"))))
  (princ "\nSelect entities: ")  
  (setq cpess (ssget ))   ;entities to be copied

  (setq bp (cdr (assoc 10 (entget (ssname cpess 0))))) ;base point for copy

  
  (setq ndim (sslength dimss))
  
  (setq plist nil)
  (setq index 0)
  (repeat ndim
    (setq dment (entget (ssname dimss index)))
    (setq ass01 (cdr (assoc 1 dment))    ;text
          ass10 (cdr (assoc 10 dment))   ;dim line position
	  ass13 (cdr (assoc 13 dment))   ;first node
	  ass14 (cdr (assoc 14 dment))   ;second node
	  ass50 (cdr (assoc 50 dment)))  ;angle of dim line
    
    (setq elang (angle ass10 ass14))          ;ang of extention line
    
    (setq dtlist (div_dimtxt ass01))
    (setq ldtlist (length dtlist));length of dim text list
    (setq angdl ass50)
    (cond
      ((= ldtlist 1)         ;case : only two dot
        (setq pnt14 (inters bp (polar bp angdl 10) ass14 (polar ass14 elang 10) nil))
        (setq pnt13 (inters bp (polar bp angdl 10) ass13 (polar ass13 elang 10) nil))
        (if (not (djdg_issamepnt pnt14 plist))
	  (setq plist (append plist (list pnt14)))
	);if  
        (if (not (djdg_issamepnt pnt13 plist))
	  (setq plist (append plist (list pnt13)))
	);if  
       );sub cond
      ((or (= ldtlist 2) (= ldtlist 3))  ;case ldtlist=2 : 2@100, ldtlist=3 2@100=200
        (setq num (atoi (car dtlist))) 
        (setq gap (atof (cadr dtlist)))
        (setq pnt14 (inters bp (polar bp angdl 10) ass14 (polar ass14 elang 10) nil))
        (setq pnt13 (inters bp (polar bp angdl 10) ass13 (polar ass13 elang 10) nil))
        (if (not (djdg_issamepnt pnt14 plist))    ;if there is not same point in point list
	  (setq plist (append plist (list pnt14)))
	);if  
        (if (not (djdg_issamepnt pnt13 plist))
	  (setq plist (append plist (list pnt13)))
	);if  
        (setq inum 1)      ;from second
        (repeat (1- num)   ;except last point(ass13)
	  (setq pnt (polar ass13 angdl (* gap inum)))
          (setq pntin (inters bp (polar bp angdl 10) pnt (polar pnt elang 10) nil))	  ;destination point
          (if (not (djdg_issamepnt pntin plist))
  	    (setq plist (append plist (list pntin)))
	  );if  
	  (setq inum (1+ inum))
	);repeat
       );sub cond
    );cond
    (setq index (1+ index))
  );repeat

  
  (push-os)
  (command "copy" cpess "" "M" bp)  
  (setq index 0                     ;from first point
	pnum (length plist))        ;number of points
  (repeat pnum
    (command (nth index plist))
    (setq index (1+ index))
  );repeat  
  (command)
  (pop-os)
  
)  ;defun cpdim


;------------------------
; Program : D2L
;           Dot to Line
;           Yi Suk Jong
;           04/08/30
;-------------------------
(defun c:d2l( / ssd be1 be2 be110 be111 be210 be211 lne ass10 ass11
	        lang lang90 nssd plst index pnt cr1 cr2 )
  (princ "\nSelect dots: ")
  (setq ssd (ssget))         ;ss dot

  (setq be1 (entget (car (entsel "\nSelect boundary element-1: "))))
  (setq be2 (entget (car (entsel "\nSelect boundary element-2: "))))

  (setq be110 (cdr (assoc 10 be1))
	be111 (cdr (assoc 11 be1)))
  (setq be210 (cdr (assoc 10 be2))
	be211 (cdr (assoc 11 be2)))

  (setq lne (entget (car (entsel "\nSelect line: ")))) ;;line entity

  (setq ass10 (cdr (assoc 10 lne))        ; start point of line
	ass11 (cdr (assoc 11 lne)))       ; end point of line

  (setq lang (angle ass10 ass11))         ;angle of line(10-->11)
  (setq lang90 (+ (* 0.5 pi) lang))       ;angle + 90 degree

  (setq nssd (sslength ssd))   ;number of dot
  (setq plst (djdg_getpoints ssd nil))  ;point lists of dots nil: line인경우 중심점

  (setq index 0)
  (repeat nssd
    (setq pnt (nth index plst))   ;point
    (setq cr1 (inters pnt (polar pnt lang 10 ) be110 be111 nil)
	  cr2 (inters pnt (polar pnt lang 10 ) be210 be211 nil))
    
    (push-os)(command "line" cr1 cr2 "")(pop-os) 
    
    (setq index (1+ index))   ;next point
  );repeat  

);defun