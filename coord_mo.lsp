;***********************************
; Program ; LEAD
;           LEADer line
;           Yi -Suk-Jong
;           96/6/8
;***********************************
; coord.lsp에서의 x와 y좌표가 뒤집혀 들어가는 문제 수정
; 유신 구조부  -최우성- 99/11/3
;***********************************

(defun C:crd(/
ds p1 p2 ang w4 ys tp tbox tl p3                            
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                
  (setq ds (getvar "DIMSCALE"))                             

  (setq p1 (getpoint "\nFirst point: "))  !                  
  (setq py (car p1))
  (setq px (cadr p1))
  (setq pxt (strcat "X=" (rtos px 2 (getvar "luprec")))) 
  (setq pyt (strcat " Y=" (rtos py 2 (getvar "luprec"))))
  (setq coord (strcat pxt pyt)) 
  (setq p2 (getpoint p1 "\nSecond point: "))                
  (setq ang (angle p1 p2)                                   
        w4  (which4 ang))                                   
  (if (or (= w4 1) (= w4 4))                                
    (setq ys (* 1 ds))                                             
    (setq ys (* -1 ds))                                            
  ) ;of if
  (command "INSERT" (strcat (prefix) "blocks/arw1") p1 ds ys (rtod ang))
  (setvar "CECOLOR" "RED")
  (command "LINE" p1 p2 "")                                 
  (setvar "CECOLOR" "WHITE")

  (if (or (= w4 1) (= w4 4))
    (progn                                                  
      (setq tp (list (+ (car p2) (* ds 2.5)) (+ (cadr p2) (* ds 1.25)) 0.0))
      (command "TEXT" tp (* ds 2.5) "0.0" coord "")
    ) ;of progn
    (progn                                                  
      (setq tp (list (- (car p2) (* ds 2.5)) (+ (cadr p2) (* ds 1.25)) 0.0))
      (command "TEXT" "R" tp (* ds 2.5) "0.0" coord "")
    ) ;progn
  ) ;of if

  (setq tbox (textbox (entget (entlast))))                  
  (setq tl (- (car (nth 1 tbox)) (car (nth 0 tbox))))       
  (setq p3 (list (+ (car p2) (* 5.0 ys) (* tl (/ (abs ys) ys)))     
                 (cadr p2) 0.0))

  (setvar "CECOLOR" "RED")                                  
  (command "LINE" p2 p3 "")                                 
  (setvar "CECOLOR" "WHITE")                                

  (pop-env)                                                 
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
