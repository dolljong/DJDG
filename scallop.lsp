; ------------------------------
; program : scallop
;           draw scallop
;            Yi suk jong
;             04/03/15
;------------------------------
(defun c:scallop(
		 / lnsel1 lnsel2 pside radius lnent1 lnent2 lnpick1 lnpick2 ln1p1 ln1p2
		   ip ang11 ang12 ang13 angln1 ang21 ang22 ang23 angln2 angside angbig angsmall
		   angstart angend startpoint endpoint)

  (setq lnsel1 (entsel "\nSelect line-1: "))  ;select entity
  (setq lnsel2 (entsel "\nSelect line-2: "))
  (setq pside  (getpoint "\nPick side: "))
  (setq radius (getreal "\nRadius: "))
  
  (setq lnent1 (entget (car lnsel1)))  ;entity information
  (setq lnent2 (entget (car lnsel2)))

  (setq lnpick1 (cadr lnsel1))        ; pick point
  (setq lnpick2 (cadr lnsel2))
  
  (setq ln1p1 (cdr (assoc 10 lnent1)))  ; end point line-1
  (setq ln1p2 (cdr (assoc 11 lnent1)))
  
  (setq ln2p1 (cdr (assoc 10 lnent2)))  ; end point line-2
  (setq ln2p2 (cdr (assoc 11 lnent2)))

  (setq ip (inters ln1p1 ln1p2 ln2p1 ln2p2 nil))  ;intersection point

  (setq ang11 (angle ip ln1p1))   ;line-1
  (setq ang12 (angle ip ln1p2))
  (setq ang13 (angle ip lnpick1))

  (if (< (abs (dang ang11 ang13)) (abs (dang ang12 ang13)))
    (setq angln1 ang11)
    (setq angln1 ang12)
  );if  
   

  (setq ang21 (angle ip ln2p1))   ;line-1
  (setq ang22 (angle ip ln2p2))
  (setq ang23 (angle ip lnpick2))

  (if (< (abs (dang ang21 ang23)) (abs (dang ang22 ang23)))
    (setq angln2 ang21)
    (setq angln2 ang22)
  );if  

  (setq angside (angle ip pside))

  (if (> angln1 angln2)
    (setq angbig angln1 angsmall angln2)
    (setq angbig angln2 angsmall angln1)
  );if

  (if (and (< angside angbig) (> angside angsmall))
    (setq angstart angsmall  angend angbig)
    (setq angstart angbig angend angsmall)
  );

  (setq startpoint (polar ip angstart radius))
  (setq endpoint (polar ip angend radius))

  (push-os)
  (command "arc" "c" ip startpoint endpoint)
  (pop-os)

  
);defun