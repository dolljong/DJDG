;-------------------------------------
; Program : LSTIFF
;           Longitudinal STIFFner
;           Yi Suk Jong
;           04/06/15
;-------------------------------------
(defun c:lstiff(
		/
	       )

  ;------------------------------
  ; Sub function : LSTIFF_DIA
  ;                Yi Suk Jong
  ;------------------------------
  
  (defun lstiff_dia(   / dcl_id 
                   )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "LSTIFF" dcl_id)) (exit))

;    (start_image "anchimage")                                  ;image 보이기
;    (slide_image  0 0
;                  (dimx_tile "anchimage") (dimy_tile "anchimage")   ;182,37
;                  "djdg(fanchfv)")
;    (end_image)


;-------------------
; 초기값설정
;-------------------
  
  (set_tile "length" (strcat "Distance: " (rtos #dst 2 0)))
  (if (/= #num nil) (set_tile "num" (rtos #num 2 0)))  
  (if (/= #lstiff nil) (set_tile "brib" (rtos #lstiff 2 0)))  
  (if (/= #tstiff nil) (set_tile "thick" (rtos #tstiff 2 0)))
    
;---------------------------
; dialog box 초기화
;---------------------------
   (action_tile "num" "(set_num)")
   (action_tile "brib" "(set_num)") 
   (action_tile "thick" "(set_num)")
   (action_tile "check" "(set_num)") 
    
   (action_tile "accept" "(do-accept)")

   (action_tile "cancel"  "(exit)")

    ;   (mode_tile "fl_weld" 2)

    (start_dialog)

    (unload_dialog dcl_id)
  ) ;of sub defun FANCH_DIA

  ;----------------------------------
  ; sub routine : set_num
  ;----------------------------------
  (defun set_num( / v vf r result1s result2s )
    (setq #n (atof (get_tile "num")))
    (setq #lstiff (atof (get_tile "brib")))
    (setq #tstiff (atof (get_tile "thick")))
      
;    (setq n 8)
    (setq v (/ #dst #n))
    (setq vf (fix (/ #dst #n)))
    (setq r (- v vf))

    (setq result1 (djdg_divide #dst #n nil))
    (setq result2 (djdg_divide #dst #n T))
    (if (= (length result1) 2)
      (progn
	(mode_tile "result2" 1)
	(mode_tile "radio2" 1)
	(setq result1s (strcat "Result-1: " (rtos (nth 0 result1) 2 0) "@" (rtos (nth 1 result1) 2 0)))
	
      );rpogn
      (progn
        (mode_tile "result2" 0)
	(mode_tile "radio2" 0)
	(setq result1s (strcat "Result-1: "    (rtos (nth 0 result1) 2 0) "@" (rtos (nth 1 result1) 2 0)
			      "+"  (rtos (nth 2 result1) 2 0) "@" (rtos (nth 3 result1) 2 0)
			      "+"  (rtos (nth 0 result1) 2 0) "@" (rtos (nth 1 result1) 2 0)))
	(setq result2s (strcat "Result-2: "    (rtos (nth 0 result2) 2 0) "@" (rtos (nth 1 result2) 2 0)
			      "+"  (rtos (nth 2 result2) 2 0) "@" (rtos (nth 3 result2) 2 0)
			      "+"  (rtos (nth 0 result2) 2 0) "@" (rtos (nth 1 result2) 2 0)))
	(set_tile "result1" result1s)
	(set_tile "result2" result2s)
      );progn	
    );if
  );defun  
  
  (defun do-accept( / )
    (if (= (get_tile "radio1") "1")
      (setq resultc 1)
      (setq resultc 2)
     );
    (done_dialog)
  );defun  
  
  ;--------------------------------------------------
  ; Main Routine
  ;--------------------------------------------------
  (setq p1 (getpoint "\nPick first point: "))
  (setq p2 (getpoint p1 "\nPick second point: "))
  (setq p3 (getpoint "\nPick side: "))

  (setq #dst (distance p1 p2))
  (if (> (rem #dst 1) 0)
    (progn
      (alert "Distance must be integer")
      (exit)
    );progn  
  );if
  
  (setq ang (angle p1 p2))
  (setq vang (v_angle p1 p2 p3))

  (lstiff_dia)

  (if (= resultc 1)
    (setq resultc result1)
    (setq resultc result2)
  );if
  
  (if (= (length resultc) 2)
    (progn
      (setq index 1)
      (repeat (1- (nth 0 resultc))
	(setq p (polar p1 ang (* index (nth 1 resultc))))
        (push-os)(djdg_rect p vang #lstiff #tstiff 0)(pop-os)
	(setq index (1+ index))
      );repeat
    );progn
    (progn
      ; --------- left
      (setq index 1)
      (repeat (fix (nth 0 resultc))
	(setq p (polar p1 ang (* index (nth 1 resultc))))
        (push-os)(djdg_rect p vang #lstiff #tstiff 0)(pop-os)
	(setq index (1+ index))
      );repeat
      (setq plast p)
      ; --------- middle
      (setq index 1)
      (repeat (fix (nth 2 resultc))
	(setq p (polar plast ang (* index (nth 3 resultc))))
        (push-os)(djdg_rect p vang #lstiff #tstiff 0)(pop-os)
	(setq index (1+ index))
      );repeat
      (setq plast p)
      (setq index 1)
      (repeat (fix (1- (nth 0 resultc)))
	(setq p (polar plast ang (* index (nth 1 resultc))))
        (push-os)(djdg_rect p vang #lstiff #tstiff 0)(pop-os)
	(setq index (1+ index))
      );repeat
    );progn
  );if  
  
);defun  


;-------------------------------------------
; function : djdg_divide( dst n one)
;            make divide
;            Yi Suk Jong
;            04/06/19
;-------------------------------------------
; argument :
;   dst : distance
;   n   : number of divide
;   one : T   > remain to one
;         nil > divide ramain to be 1mm
;-------------------------------------------
(defun djdg_divide(dst n one / dst n one  q qf qf1 rm rm1 rm2 g1  g2 ng1)
  (setq q (/ dst n)
	qf (fix q)
	qf1 (1+ qf)
	rm (- dst (* n qf))
	rm1 (- dst (* n qf1)));
  (if (< (abs rm) (abs rm1))
    (setq rm2 rm
	  g1 qf)
    (setq rm2 rm1
	  g1 qf1)
  );
  (if (= rm 0)
    (progn
      (list n q)
    );progn  
    (progn
      (if (=  (rem rm2 2) 0)
        (progn
	  (if one
	    (progn						;arrange one
	      (setq ng1 1)
	      (setq g2 (+ g1 (/ rm2 2)))
	    );progn  
	    (progn
              (setq ng1 (abs (/ rm2 2)))                                 ;even number
              (setq g2 (+ g1 (/ rm2 (* 2 ng1))))
	    );progn  
	  );if
	  (list ng1 g2 (- n (* 2 ng1)) g1)	  
        );progn  
        (progn  							;odd numver
	  (if one
	    (progn						;arrange one
	      (setq ng1 (/ (- n 1) 2))
	      (setq g2 g1)
	      (setq g1 (+ g2 rm2))
	    );progn  
	    (progn						
	      (setq ng1 (/ (- n (abs rm2)) 2))
	      (setq g2 g1)
	      (setq g1 (+ g2 (/ rm2 (abs rm2))))
	    );progn  
	  );if
	  (list ng1 g2 (- n ng1 ng1) g1)
	);progn  
     );if
    );progn
  );if  
);defun  