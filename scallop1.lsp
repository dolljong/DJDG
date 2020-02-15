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
  
;  (command "LINE" p1l p2l "")
;  (command "LINE" p1r p2r "")
;  (command "arc" p2r "e" p2l "r" R)

  (push-os)
  (command "pline" p1r p2r "a" "r" r p2l "l" p1l "")
  (pop-os)
  	
);defun

(defun c:scallop2(
		  /
		  )
  (setq p1 (getpoint "\nPick start point: "))
  (setq p2 (getpoint "\nPick end point: "))
  (setq thick (getreal "\nThickness of Rib: "))
  
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