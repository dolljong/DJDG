;*******************************************
; Fuction : ALCROSS
;           Arc & Line CROSS point
;           By Suk-Jong Yi
;           1995/6/26
;*******************************************

(defun C:CRSPOINT(/
)

(setq aent (entget (car (entsel "\nSelect Arc: "))))  ; ARC entity ¬åÈ‚
(setq a (car (cdr (assoc 10 aent))))
(setq b (cadr (cdr (assoc 10 aent))))
(setq r (cdr (assoc 40 aent)))
(setq sa (cdr (assoc 50 aent)))
(setq ea (cdr (assoc 51 aent)))
(setq lent (entget (car (entsel "\nSelect Line: "))))
(setq x1 (car (cdr (assoc 10 lent))))
(setq x2 (car (cdr (assoc 11 lent))))
(setq y1 (cadr (cdr (assoc 10 lent))))
(setq y2 (cadr (cdr (assoc 11 lent))))
(setq c (/ (- y1 y2) (- x1 x2)))
(setq d (- y2 (* c x2)))
(setq crspnt (cross a b r sa ea c d))
(command "CIRCLE" crspnt "1")

(prin1)
) ;of defun






(defun CROSS(a b r sa ea c d /
;a b r sa ea c d
)

(push-env)

(setq a1 (+ 1 (* c c)))
(setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))
(setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))
(setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
(setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
(setq y1 (+ (* c x1) d))
(setq y2 (+ (* c x2) d))

(setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))
(setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))

(if (inang sa ea ang1)
  (list x1 y1 0.0)
  (if (inang sa ea ang2)
    (list x2 y2 0.0)
    nil
  ) ;of if
) ;of if
)

(defun inang(a1 a2 a3 /
a1 a2 a3
)
(if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))
(if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)
                                nil)
)
