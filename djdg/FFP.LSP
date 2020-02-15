;*************************************
;  Program : FFP
;            oFFset Polar
;            By Jong-Suk Yi
;            1995. 2. 27
;*************************************

(defun c:FFP(/
ffd spnt epnt ang dst ffdst num
ent ffpnt n entpnt entpp ffpnt
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
  (setvar "CMDECHO" 0)
  (setq ffd (getvar "OFFSETDIST"))
  (setq spnt (getpoint "\nPick start point: "))
  (setq epnt (getpoint spnt "\nPick end point: "))
  (setq ang (angle spnt epnt))
  (setq dst (distance spnt epnt))
  (princ "\nOffset distance<")
  (princ ffd)
  (princ ">: ")
  (setq ffdst (getdist))
  (if (= ffdst nil)
    (setq ffdst ffd)
  )
  (setq num (fix (/ dst ffdst)))
  (setq ent (entsel))
; First offset
  (setq ffpnt (polar spnt ang ffdst))
  (command "offset" ffdst ent ffpnt "")
; Repeat offset
  (setq n 2)
  (repeat (- num 1)
    (setq ent (ssget "L"))
    (setq enty (ssname ent 0))
    (setq entpnt (polar spnt ang (* ffdst (- n 1))))
    (setq entpp (list enty entpnt))
    (setq ffpnt (polar spnt ang (* ffdst n)))
    (command "offset" ffdst entpp ffpnt "")
    (setq n (1+ n))
  ) ; of repeat
  (setvar "CMDECHO" 1)
  (setq *error* oer seterr nil)
  (princ)
);; of defun FFP
