;---------------------------
; program : conarc
;           chord on arc
;           by Yi Suk-Jong
;           2000/6/22
;---------------------------
; get chord point on a arc
(defun c:conarc(
		/ arcent spnt chordlen
		)
  (setq arcent (entget (car (entsel))))

  (setq spnt (getpoint "\n Pick Start point: "))
  (setq chordlen 39)
  (setq epnt (ChordOnArc arcent spnt chordlen))
  
;  (command "line" spnt epnt "")
  (command "insert" "beam40" spnt "" "" epnt)
    
);defun

;---------------------------
; function : ChordOnArc
;            by Yi Suk-Jong
;            2000/6/22
;---------------------------
; 
(defun ChordOnArc(arcent spnt chordlen
		  / arcent spnt chordlen r cen ang alpha pnt)
  (setq r   (cdr (assoc 40 arcent))
	cen (cdr (assoc 10 arcent)))
  
  (setq ang (angle cen spnt))

  (setq alpha (* 2 (asin (/ chordlen (* 2.0 r)))))

  (setq pnt (polar cen (- ang alpha) r))
  
)

;------------------------------------------
; function : asin
;            arc sin
;            Yi Suk Jong
;            2000/4/25
;------------------------------------------
; asin(a) : arcsin°ª return(radian)
;------------------------------------------
(defun asin(a / a)
  (cond
    ((= a 1) (/ pi 2.0))
    ((= a -1) (/ pi -2.0))
    (T (atan (/ a (sqrt (- 1 (* a a))))))
  );cond  
);defun  
