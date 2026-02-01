;**************************************
; Program : pole
;           pole
;           Suk-Jong Yi(http://user.chollian.net/~dolljong)
;           2000.10.08
;**************************************
;
;**************************************

(defun C:pole(/

)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

  (push-env)                                    ;환경변수값 대피

  (setq bpole 3
	txtele 10)

  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "1")

  (setq ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))

  (setq knownelvp (getpoint "\nKnown Eelevation point: "))
  (setq knownelv (getreal "\nElevation: "))
  (setq uppnt (getpoint "\nUpper point: "))
  (setq lowpnt (getpoint "\nLower point: "))
  (setq xpnt (getpoint "\nX-point: "))

  (setq dyup (- (cadr uppnt) (cadr knownelvp)))
  (setq dylow (- (cadr knownelvp) (cadr lowpnt)))

  (setq upelv (+ knownelv (/ dyup 1000))
	lowelv (- knownelv (/ dylow 1000)))

  (if (> (rem upelv 10) 0)
    (setq upelv1 (* 10 (1+ (fix (/ upelv 10)))))
  )  

  (setq lowelv1 (* 10 (fix (/ lowelv 10))))
  
  
  (princ upelv) (princ lowelv)
  (princ upelv1) (princ lowelv1)
  
  (setq upelvp (list (car xpnt) (+ (cadr uppnt) (* (- upelv1 upelv) 1000))))

  (draw_pole upelvp upelv1 (- upelv1 lowelv1) txtele 5 bpole)
  
  (pop-env)   ;환경변수값 복귀

  (setq *error* oer seterr nil)
  
  (princ)


); of defun

(defun draw_pole(ipnt ielv lpole delv div width)

  (setq ds (getvar "DIMSCALE"))
  (setq th (* ds (getvar "DIMTXT")))
  
  (setq ipntx (car ipnt)
	ipnty (cadr ipnt)
	lsolid (/ delv div)
	halfwidth (* 0.5 ds width))

  (setq p1 (list (- ipntx halfwidth) ipnty)
	p2 (list (+ ipntx halfwidth) ipnty)
	p3 (list (car p1) (- ipnty (* 1000 lpole)))
	p4 (list (car p2) (- ipnty (* 1000 lpole)))
   );setq

  (command "line" p1 p2 p4 p3 "c")
  
  (setq nsolid (fix (/ lpole lsolid)))

  (setq hsolid (* lsolid 1000))	

  (setq count 0)
  (repeat nsolid
    (setq p1y (- ipnty (*  count hsolid))
	  p2y (- ipnty (* (1+ count) hsolid)))
    (if (= (rem count 2) 0)
      (setq x (- ipntx halfwidth))
      (setq x (+ ipntx halfwidth))
    );if
    (setq p1 (list ipntx p1y)
	  p2 (list ipntx p2y)
	  p3 (list x p1y)
	  p4 (list x p2y)
    );setq
    (command "solid" p1 p2 p3 p4 "")
    (setq count (1+ count))
  );repeat

  (setq nelvtxt (fix (/ lpole delv))
	count 0
	hdelv (* delv 1000))
  
  (princ hdelv)
  
  (setq lentxtl (* 8 th))
  (repeat (1+ nelvtxt)
    (setq txt (rtos (- ielv (* count delv)) 2 3))
    (setq y (- ipnty (* hdelv count)))
    (setq p1 (list (- ipntx halfwidth) y) 
          p2 (list (- (car p1) lentxtl) y)
	  p3 (list (- (car p1) (* 0.5 th)) (+ y (* 0.5 th)))
    );setq
    (command "LINE" p1 p2 "")
    (command "TEXT" "J" "R" p3 th "0" txt "")
    (setq count (1+ count))
  );repeat  
)  


