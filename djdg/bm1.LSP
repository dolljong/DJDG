;********************************    
;   Program : BM1    
;             Bar Marking-1    
;             By Jong-Suk Yi    
;             95/3/2, 96/5/7    
;********************************    
; 철근마킹중 아래의 모양을 만들어준다.    
;       ①    
;       |    
;       |    
;********************************************    
    
(defun C:BM1(/    
rc sp ep ang dst cp    
) ;variable    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
  (setvar "CMDECHO" 0)    
  (setvar "BLIPMODE" 0)    
    
  (setq ds (getvar "DIMSCALE")   ;스케일값    
        th (getvar "DIMTXT"))    ;text크기    
    
  (setq rc (* ds 4.5))          ;마킹원 반지름  
  (setq th (* ds th))          ;text높이    
    
  (setq sp (getpoint "\nPick start point: "))    
  (setq ep (getpoint sp "\nPick end point: "))    
    
  (setq ang (angle sp ep))    
  (setq dst (distance sp ep))    
  (setq cp (polar sp ang (+ dst rc)))    
  (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 4 ds)) 0.0))    
    
  
  (cecolor "1")
  (command "LINE" sp ep "")    
  (command "CIRCLE" cp rc)    
  (popcolor)
    
    
  
  (setq mk (getstring "\nEnter Marking: "))    
;  (command "TEXT" "J" "M" cp th "0" (strcase mk))    
  (txtinc mk cp 0.0)    
  (setq dia (getstring "\nEnter Rebar Dia: "))
  (cecolor "bylayer")
  (command "TEXT" diaxy th "" (strcase dia))
  (popcolor)
  
    
  (setq *error* oer seterr nil)    
  (princ)    
) ;of defun    
    
;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; 
; 
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ds)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE" ))               ;text

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
	(progn
	  (cecolor "bylayer")
          (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
	  (popcolor)
	);progn
        (progn
	  (cecolor "bylayer")
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
	  (poplayer)
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (progn
      (cecolor "bylayer")
      (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
      (popcolor)
    );progn
  ) ;of IF
) ;of DEFUN
    
