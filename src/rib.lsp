;-------------------------
; Program : rib
;           draw open rib
;           Yi Suk Jong
;           2004/04/02
;-------------------------
(defun c:rib(
              / ls p1 p2 rstxt rsdim b thick lent
	        spnt epnt ang vang pp1 pp2 pp3 pp4
	     )
  (setq ls (entsel "\nSelect a line: "))
  (setq p1 (getpoint "\nPick insert point: "))
  (setq p2 (getpoint "\nPick Side: "))

  (RIB_DIA)                 ;call rib_dia routine

  (setq rstxt (strcat sclength "*" scthick))
;  (setq rstxt (getstring "\nRib Size<200*20>: "))
;  (if (= rstxt "") (setq rstxt  "200*20"))
  
  (setq rsdim (rib_dim rstxt))
  (setq  b (car rsdim)
         thick  (cadr rsdim))
  (setq lent (entget (car ls)))
  (setq spnt (cdr (assoc 10 lent))    ;start point  
	epnt (cdr (assoc 11 lent)))   ;ent point
  (setq ang (angle spnt epnt))
  (setq vang (v_angle spnt epnt p2))
  (setq pp1 (polar p1 ang (* 0.5 thick))
	pp2 (polar pp1 vang b)
	pp3 (polar p1 (+ ang pi) (* 0.5 thick))
	pp4 (polar pp3 vang b))
  
  (push-os)
  (command "pline" pp1 pp2 pp4 pp3 "")
;  (command "circle" pp3 "10")

  (cond
    ((= sctype 1)
     (f_scallop1 p1 (polar p1 vang b) thick)
    );sub cond
    ((= sctype 2)
     (f_scallop2 pp1 pp2 thick)
    );sub cond
  );cond  
  (pop-os)    
);defun


  ;;
  ;; Function: RIB_DIA (Dialog box로 입력받기)
  ;;
  (defun RIB_DIA (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "SCALLOP" dcl_id)) (exit))

    (start_image "sc_none")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "sc_none") (dimy_tile "sc_none")
                  "djdg(sc_none)")
    (end_image)

    (start_image "sc_tensile")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "sc_tensile") (dimy_tile "sc_tensile")
                  "djdg(sc_tensile)")
    (end_image)

    (start_image "sc_compress")                                  ;image 보이기
    (slide_image  0 0
                   (dimx_tile "sc_compress") (dimy_tile "sc_compress")
                  "djdg(sc_compress)")
    (end_image)


;-------------------
; 초기값설정
;-------------------


  (if (= sclength nil) (setq sclength "200"))
  (if (= scthick nil) (setq scthick "12"))
  (if (= sctype nil) (setq sctype 1))
    
;---------------------------
; dialog box 초기화
;---------------------------
   (set_tile "sc_length" sclength)
   (set_tile "sc_thick"  scthick)

   (action_tile "sc_none" "(setq sctype 0)")        
   (action_tile "sc_tensile" "(setq sctype 1)")      
   (action_tile "sc_compress" "(setq sctype 2)")
    
   (action_tile "sc_length"   "(setq sclength $VALUE)")
   (action_tile "sc_thick"   "(setq scthick $VALUE)")

    (action_tile "accept"  "(done_dialog)")
   (action_tile "cancel"  "(exit)")
;   (mode_tile "fl_weld" 2)

    (start_dialog)

    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG


;---------------------------------
; Program : Scallop1
;           Draw Scallop (open rib tension)
;           Yi Suk Jong
;           04/04/26
;----------------------------------
(defun c:scallop1(
		  /
		  )
  (setq p1 (getpoint "\nPick start point: "))
  (setq p2 (getpoint "\nPick end point: "))
  (setq thick (getreal "\nThickness of Rib: "))
  
  (f_scallop1 p1 p2 thick)
  	
);defun


(defun f_scallop1( p1 p2 thick / p1 p2 thick)
  (setq l (distance p1 p2))
  (cond
    ((<= thick 12) (setq A 70 B 50 R 20))
    ((and (> thick 12) (<= thick 22)) (setq A 80 B 50 R 20))
    ((and (> thick 22) (<= thick 32)) (setq A 90 B 65 R 25))
  );cond
  (setq l1 (expt (+ (expt (/ A 2.0) 2)  (expt l 2)) 0.5))
  (setq ang1 (+ (asin (/ (/ A 2.0) l1))  (* 0.5 pi) (* -1 (asin (/ R l1)))))
  (setq ang1 (- pi ang1))
  (setq ang (angle p1 p2))
  (setq p2l (polar p2 (+ ang ang1) R)
	p2r (polar p2 (- ang ang1) R))
  (setq p1l (polar p1 (+ ang (* 0.5 pi)) (* 0.5 A))
	p1r (polar p1 (- ang (* 0.5 pi)) (* 0.5 A)))
  
  (push-os)
  (command "pline" p1r p2r "a" "r" r p2l "l" p1l "")
  (pop-os)
  
)  ;defun
(defun c:scallop2(
		  /
		  )
  (setq p1 (getpoint "\nPick start point: "))
  (setq p2 (getpoint "\nPick end point: "))
  (setq thick (getreal "\nThickness of Rib: "))

  (f_scallop2 p1 p2 thick)
  	
);defun


(defun f_scallop2( p1 p2 thick
		  / p1 p2 thick l )
    (setq l (distance p1 p2))
  (cond
    ((<= thick 12) (setq A 70 B 50 R 20))
    ((and (> thick 12) (<= thick 22)) (setq A 80 B 50 R 20))
    ((and (> thick 22) (<= thick 32)) (setq A 90 B 65 R 25))
  );cond
  (setq R 35)
  (setq l1 (expt (+ (expt B 2)  (expt l 2)) 0.5))
  (setq ang1 (+ (asin (/ B l1))  (* 0.5 pi) (* -1 (asin (/ R l1)))))
  (setq ang1 (- pi ang1))
  (setq ang (angle p1 p2))
  (setq p2l (polar p2 (+ ang pi) R)
	p2r (polar p2 (- ang ang1) R))
  (setq p1l (polar p1 (+ ang (* 0.5 pi)) 35)
	p3l (polar p1 ang 35)
	p1r (polar p1 (- ang (* 0.5 pi)) B))
  
;  (command "LINE" p1l p2l "")
;  (command "LINE" p1r p2r "")
;  (command "arc" p2r "e" p2l "r" R)

  (push-os)
  (command "pline" p1r p2r "a" "ce" p2 p2l "l" p3l "a" "ce" p1 p1l "")
  (pop-os)
);defun