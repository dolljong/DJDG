;-------------------------
; Program : LAP
;           Draw LAP splice
;           Yi Suk Jong
;           04/03/15
;-----------------------------

(defun c:lap(
	     / lapgap lnent spnti spnt sidepoint laplength spointofline epointofline
	       angleofline angleofline90 p1 crosspnt distanceofcrosspnt
	       angleofsidepoint angleoflongsidepoint p3 p11 p22)
  (setq lapgap 30)
  (setq lnent (entget (car (entsel "\nSelect line: "))))
  (initget "Center")
  (setq spnti (getpoint "\Pick start point or [Center]: "))
  (if (= spnti "Center")
    (setq spnt (getpoint "\nPick Center point: "))
    (setq spnt spnti)
  );if
  (setq sidepoint (getpoint "\nPick Side point: "))
  (setq laplength (getreal "\nLap length(mm): "))

  (setq spointofline (cdr (assoc 10 lnent)))
  (setq epointofline (cdr (assoc 11 lnent)))
  (setq angleofline (angle spointofline epointofline))
  (setq angleofline90 (+ angleofline (* pi 0.5)))

  (setq p1 (inters spnt (polar spnt angleofline90 1000 ) spointofline epointofline nil))

  (setq crosspnt (inters sidepoint (polar sidepoint angleofline90 100) spointofline epointofline nil))
  (setq distanceofcrosspnt (distance crosspnt sidepoint))
  (setq angleofsidepoint (angle crosspnt sidepoint))
  (setq angleoflongsidepoint (angle (polar p1 angleofsidepoint distanceofcrosspnt) sidepoint))
  
;  (setq deltaang (dang (angle spointofline epointofline) (angle spointofline sidepoint)))
;  (if (< deltaang 0)
;    (setq sign -1)
;    (setq sign 1)
;  );if

  (if (= spnti "Center")
    (setq p3 (polar p1 angleofsidepoint lapgap)             ;case of center 
	  p11 (polar p3 angleofline (* 0.5 laplength))
	  p22 (polar p11 (+ angleofline pi) laplength))
    (setq p11 (polar p1 angleofsidepoint lapgap) ;case of left or right
	  p22 (polar p11 angleoflongsidepoint laplength))
  );if

  (push-os)
  (command "line" p11 p22 "")
  (pop-os)
  
);defun



;---------------------------------------
; program : MLAP
;           multi lap splice
;           Yi Suk Jong
;           04/08/13
;---------------------------------------
(defun c:mlap(
	      / gap bline engl entbl blp1 blp2 ssrebar nss sidepnt1 kw angsidepnt1 crspnt1 lensidepnt1 index
	        entrebar pnt10 pnt11 cp vang1 p11 sidepnt3 vang3 p12
	        
	     )
  (setq gap 30)
;	laplength 1200)
  
  (setq bline (entsel "\nSelect base line: "))
  (setq enbl (car bline))
  (setq entbl (entget enbl))
  (setq blp1 (cdr (assoc 10 entbl))
	blp2 (cdr (assoc 11 entbl)))
  (setq ssrebar (ssget "F" (list blp1 blp2)))
  (if (ssmemb enbl ssrebar) (setq ssrebar (ssdel enbl ssrebar)))  ;ssdel base line
  (setq nss (sslength ssrebar))
  
  (djdg_mtredraw ssrebar 3)

;  (alert "test")
  (setq sidepnt1 (getpoint "\nPick side point: "))
  (initget "All Half")
  (setq kw (getkword "\nAll or Half: "))
  (if (= kw nil) (setq kw "Half"))
  (setq laplength (getrealold laplength "\nLap lenght(mm)"))

  (setq angsidepnt1 (v_angle blp1 blp2 sidepnt1))
  (setq crspnt1 (inters blp1 blp2 sidepnt1 (polar sidepnt1 angsidepnt1 10) nil))
  (setq lensidepnt1 (distance sidepnt1 crspnt1))

  (setq index 0)
  
;  (setq distlist nil)
;  (repeat nss
;    (setq entrebar (entget (ssname ssrebar index)))
;    (setq pnt10 (cdr (assoc 10 entrebar))
;	  pnt11 (cdr (assoc 11 entrebar)))
;    (setq dst (distance blp1 (inters blp1 blp2 pnt10 pnt11)))
;    (setq distlist (append distlist (list dst)))
;    (setq index (1+ index))
;  );repeat						    

  (setq index 0)
  (repeat nss
    (if (or (= kw "All") (and (= kw "Half") (= (rem index 2) 0)))
      (progn
        (setq entrebar (entget (ssname ssrebar index)))
        (setq pnt10 (cdr (assoc 10 entrebar))
	      pnt11 (cdr (assoc 11 entrebar)))
        (setq cp (inters pnt10 pnt11 blp1 blp2))
        (setq vang1 (v_angle pnt10 pnt11 blp2))

        (setq p11 (polar cp vang1 gap))

        (setq sidepnt3 (polar p11 angsidepnt1 lensidepnt1))
        (setq vang3 (v_angle cp p11 sidepnt3))

        (setq p12 (polar p11 vang3 laplength))

        (push-os)(command "line" p11 p12 "")(pop-os)
      );progn
    );if  
	  
    (setq index (1+ index))
  );repeat
  
  (djdg_mtredraw ssrebar 4)
  
);defun  