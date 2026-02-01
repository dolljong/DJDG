;**********************************************  
; Program : PARA
;           Draw Parabola
;           Suk-Jong Yi  
;           2000. 4. 21
;**********************************************  
;포물선 그리기
;**********************************************  
  
(defun C:para(/  
               ds   lmin  
)  
  
  ;;;  
  ;;;define internal error routine
  ;;;  
  (defun SETERR(s)  
  ;If an error (CTRL-C) occurs when this command is active.  
    (if (/= s "Function cancelled")  
      (if (= s "quit / exit abort")  
        (princ "*cancel*")  
        (princ (strcat "\nError " s))  
      ) ;of if  
    ); of If  
  ;Restore previous error handler  
    (setq *error* oer seterr nil)  
    (princ)  
  ); of SETERR  
  
; (setq oer *error* *error* seterr)                ;run error handler
  
  (push-env)                                       ;puch environment variable
  
  (setq #cap "1")                             
  (setq #both "0")  

  (setq epnt (getpoint "\nPick End point: ")
	mpnt (getpoint epnt "\nPick Mid point: "))

  (if (= #exp nil) (setq #exp 2))
  (setq ex (car epnt)
        ey (cadr epnt)
	mx (car mpnt)
	my (cadr mpnt)
	h (- ey my)
	l2 (- ex mx)
;	dx (/ l2 n)
	#a (/ h (expt l2 2)))

;  (princ "aldskjflkadsf")
	 
  (para-dialog)                                    ;run dialog box

;  (alert (rtos #exp 2 0))
    
  (setq n #div)
  
  (draw_para epnt mpnt #exp n #caplen #both)

  
  
  (print n)
;  (print #cap)
;  (print #both)
  
;  (setq *error* oer seterr nil)         ;end error handler
  
  (princ)  
  
) ;of defun 
  
;;  
;; input with dialog box
;;  
(defun para-dialog(/  
                  dcl_id )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "para" dcl_id)) (exit))         ;ddscl.dcl 안의 scl빨몹  
  
  (cond
    ((= #exp 2) (set_tile "exp2" "1"))
    ((= #exp 3) (set_tile "exp3" "1"))    
    ((= #exp nil)
     (set_tile "exp2" "1")
     (setq #exp 2))
  );cond  
  (set_tile "div" (rtos 10 2 0))
  (if (= #exp 2)
    (set_tile "prop" (strcat "Y = aX^2 = " (rtos #a 1 6) " X^2"))
    (set_tile "prop" (strcat "Y = aX^3 = " (rtos #a 1 6) " X^3"))  
  );if  
  (set_tile "cap" #cap)  
  (set_tile "both" #both)
  (set_tile "length"  (strcat "L= " (rtos h 2 3)))
  (set_tile "height"  (strcat "H= " (rtos l2 2 3)))


  (action_tile "exp2" "(setexp 2)")               ;user 럼력 box
  (action_tile "exp3" "(setexp 3)")               ;user 럼력 box    
  (action_tile "div" "(set-div)")               ;user 럼력 box  
  (action_tile "cap" "(set-cap)")                     ;grid ON/OFF toggle  
  (action_tile "cap_len" "(set-caplen)")                     ;grid ON/OFF toggle  
  (action_tile "both" "(set-both)")                     ;grid ON/OFF toggle  
  (action_tile "cancel" "(do-cancel)")                ;CALCEL button
  (action_tile "accept" "(do-accept)")                ;CALCEL button
  
  (mode_tile "div" 2)
  (if (= #cap "1")
    (mode_tile "cap_len" 0)
    (mode_tile "cap_len" 1)
  );if  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun para-DIALOG  
  

;;  
;; get exponent
;;  
(defun setexp(nexp / nexp in )  
  (if (= nexp 2)  
    (progn
      (setq #exp 2)
;      (do-error)
    ) ;of THEN  
    (progn  
      (setq #exp 3)  
    ) ;of ELSE  
  ) ;of IF
  (setq #a (/ h (expt l2 nexp)))
  (if (= #exp 2)
    (set_tile "prop" (strcat "Y = aX^2 = " (rtos #a 1 6) " X^2"))
    (set_tile "prop" (strcat "Y = aX^3 = " (rtos #a 1 6) " X^3"))  
  );if  
    
) ;of defun SET-exp



;;  
;; get divide number
;;  
(defun set-div( / in )  
  (setq in (get_tile "div"))  
  (if (<= (atoi in) 2)  
    (progn  
      (do-error)  
      (mode_tile "div" 2)  
      nil  
    ) ;of THEN  
    (progn  
      (setq #div (atoi in))  
      (set_tile "error" "")  
      T  
    ) ;of ELSE  
  ) ;of IF  
) ;of defun SET-USER  
  

;;  
;; get cap length
;;  
(defun set-caplen( / in )  
  (setq in (get_tile "cap_len"))  
  (if (<= (atof in) 0)  
    (progn  
      (do-error)  
      (mode_tile "cap_len" 2)  
      nil  
    ) ;of THEN  
    (progn  
      (setq #caplen (atof in))
      (set_tile "error" "")  
      T  
    ) ;of ELSE  
  ) ;of IF  
) ;of defun SET-USER  

  
;;  
;;cap택했을 경우 
;;  
(defun set-cap( / in )  
  (mode_tile "cap" 2)  
  (setq in (get_tile "cap"))  
  (setq #cap in)
  (if (= #cap "1")
    (mode_tile "cap_len" 0)
    (mode_tile "cap_len" 1)
  );if  
) ;of defun SET-cap  



;;  
;;both택했을 경우 
;;  
(defun set-both( / in )  
  (mode_tile "both" 2)  
  (setq in (get_tile "both"))  
  (setq #both in)  
) ;of defun SET-cap  


;;  
;;ok를 у택했을 경우  
;;  
(defun do-accept()
  (done_dialog)  
;  (if (set-user) (done_dialog))  
) ;of defun DO-ACCEPT

(defun set-user()
   (if (and (set-div) (set-caplen) (set-cap) (set-both)) T nil)
)  
  
;; 
;;error 발생   수행되는 함수 (call back 함수 아님)
(defun do-error()  
  (set_tile "error" "Invalid Number.")  
  (mode_tile "scl_f" 2)  
) ;of DO-ERROR  
  
;;  
;;cancel을 у택했을 경우  
;;  
(defun do-cancel()  
  (done_dialog)  
  (exit)  
) ;of DO-CALCEL  
  


(defun draw_para(epnt mpnt nexp n caplen both / epnt mpnt nexp n caplen both )

;  (setq n 10)
;  (setq both T)
  
  (setq ex (car epnt)	
        ey (cadr epnt)
	mx (car mpnt)
	my (cadr mpnt)
	h (- ey my)
	l2 (- ex mx)
	dx (/ l2 n)
        a  (/ h (expt l2 nexp)))
    

  (setq epnt1 (list (- mx l2) ey))
  
  (setq fromx l2
        tox   (- 0 l2)
        x fromx)

  (command "PLINE")
  (setq index 0)
  (repeat (1+ n) ;(+ (* n 2) 1))
    (setq x (* (/ l2 n) (- n index))
          y (* a  (expt x nexp)))
    (setq x1 (+ mx x)
  	  y1 (+ my y))
    (setq xy (list x1 y1))
;  (print xy)
    (command xy)
    (setq index (1+ index))
  ) ;repeat  

;(setq index (+ (* n 2) 1))  
  (if (= both "1") 
    (repeat n
      (setq x (* (/ l2 n) (- n index))
  	    yx (* (/ l2 n) (- index n ))
            y (* a (expt yx nexp)))
      (setq x1 (+ mx x)
    	    y1 (+ my y))
      (setq xy (list x1 y1))
      (command xy)
      (setq index (1+ index))
    ) ;repeat  
  );endif
  
  (command "")

  (if (> caplen 0)
    (progn
      (setq 2ax1 (* nexp a (expt fromx (1- nexp)))
;	  2ax2 (* nexp a (expt tox (1- nexp)))
            ang1 (atan 2ax1)
;	  ang2 (atan 2ax2)
  	    caplen2 (/ caplen 2)
	    pi2 (/ pi 2.0)
	    cap11 (polar epnt (+ ang1 pi2) caplen2)
            cap12 (polar epnt (- ang1 pi2) caplen2)
;	  cap21 (polar epnt1 (+ ang2 pi2) caplen2)
            cap22 (polar epnt1 (- ang2 pi2) caplen2))
      (command "line" cap11 cap12 "")
      (if (= both "1")
        (progn
	  (setq cap11 (list (+ mx (-  mx (car cap11)))  (cadr cap11))
                cap12 (list (+ mx (-  mx (car cap12)))  (cadr cap12)))
          (command "line" cap11 cap12 "")
        );progn	
      );if  
    );progn	  
  );if  	  

);defun



