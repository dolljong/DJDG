;---------------------------------
; program : AHOLE
;           Anchorage Hole
;           By Yi Suk Jong
;           2004/3/13
;---------------------------------

(defun c:ahole()
  (setq p1 (getpoint "\nPick first point of anchorage edge: "))
  (setq p2 (getpoint "\nPick second point  of anchorage edge: "))
  (setq ln (entsel "\nSelect line: "))
  (setq ang1 10.0)
  (setq ang1 (dtor ang1))
  (setq lnent (entget (car ln)))
  (setq lnpnt (cadr ln))
  (setq lnp1 (cdr (assoc 10 lnent)))
  (setq lnp2 (cdr (assoc 11 lnent)))
  (setq a1 (angle p1 p2))
  (setq a2 (angle p1 lnp2))
  (setq dangle (dang a1 a2))
  (if (> dangle 0)
    (progn
      (setq ang (+ a1 (* pi 0.5)))
      (setq b1 (- ang ang1))
      (setq b2 (+ ang ang1))
    );progn
    (progn
      (setq ang (- a1 (* pi 0.5)))
      (setq b1 (+ ang ang1))
      (setq b2 (- ang ang1))
    );progn
  );if

  (setq p3 (polar p1 b2 100))
  (setq p4 (polar p2 b1 100))
  
  (setq p3 (inters p1 p3 lnp1 lnp2 nil))
  (setq p4 (inters p2 p4 lnp1 lnp2 nil))

  (push-os)
  (command "line" p1 p3 "")
  (command "line" p2 p4 "")
  (pop-os)
  
  
  
  
);defun
