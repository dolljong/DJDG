;******************************************
; Function : WHICH4
;            WHICH
;            Suk-Jong Yi
;            1995. 3. 15
;******************************************
(defun CHICH4(ang)

(cond
  ((<= ang (dtor 90.0)) (setq w4 1))
  ((and (> ang (dtor 90.0)) (<= ang (dtor 180.0))) (setq w4 2))
  ((and (> ang (dtor 180.0)) (<= ang (dtor 270.0))) (setq w4 3))
  ((and (> ang (dtor 270.0)) (<= ang (dtor 360.0))) (setq w4 4))
) ; of cond

) ; of defun
