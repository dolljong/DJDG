;************************************
; Program : TRI
;           TRIangle marking
;           Suk-Jong Yi
;           1995. 3. 15
;************************************

(defun C:TRI(/
            ds      ent     sp      ep      ang     ipnt    sdang
            sdsgn   dp2     dp1     dx      dy      ans     vt
            ht      w4      tsgn    crspnt  txtsgn  ttxtsgn lr
            vtpnt   htpnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR


  (setq oer *error* *error* seterr)                     ;내장에러함수 가동

  (push-env)                                            ;환경변수 대피

  (setq ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))

  (setq ent (entget (car (entsel "\nPick a line: "))))
  (setq sp (cdr (assoc 10 ent)))
  (setq ep (cdr (assoc 11 ent)))
  (setq ang (angle sp ep))

  (setq ipnt (getpoint "\nPick insert point: "))
  (setq sdang (angle sp ipnt))
  (setq sdsgn (/ (dang ang sdang) (abs (dang ang sdang))))

  (setq dp2 (polar ipnt ang (* 5 ds)))
  (setq dp1 (polar ipnt (+ ang pi) (* 5 ds)))
  (setq dx (abs (- (car dp1) (car dp2))))
  (setq dy (abs (- (cadr dp1) (cadr dp2))))

  (initget 0 "Horizontal Vertical")
  (setq ans (getkword "Which side is 1 (Horizontal/<Vertical>): "))
  (if (= ans nil) (setq ans "Vertical"))
  (if (= ans "Vertical")
    (progn
      (setq vt "1")
      (setq ht (rtos (/ dx dy) 2 3))
    ) ; of progn
    (progn
      (setq ht "1")
      (setq vt (rtos (/ dy dx) 2 3))
    ) ; of progn
  ) ; of if


  (setq w4 (which4 ang))
  (cond
    ((or (= w4 1) (= w4 3)) (setq w4sgn 1))
    ((or (= w4 2) (= w4 4)) (setq w4sgn -1))
  )

  (setq tsgn (* sdsgn w4sgn))
  (cond
    ((< tsgn 0) (setq crspnt (list (car dp2) (cadr dp1))))
    ((> tsgn 0) (setq crspnt (list (car dp1) (cadr dp2))))
  )
  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "1")
  (command "PLINE" crspnt dp1 dp2 "C")
  (setvar "CECOLOR" oldclr)

  (cond
    ((or (= w4 1) (= w4 2)) (setq txtsgn -1))
    ((or (= w4 3) (= w4 4)) (setq txtsgn 1))
  ) ; of cond

  (setq ttxtsgn (* sdsgn txtsgn))
  (if (> ttxtsgn 0)
        (setq lr "ML")
        (setq lr "MR")
  ) ; of if

  (if (> txtsgn 0)
      (progn
         (setq vtpnt (list (+ (car crspnt) (* ds 2.0 ttxtsgn))
            (+ (cadr crspnt) (* dy 0.5 tsgn)) 0.0))
         (setq htpnt (list (+ (car crspnt) (* 0.5 dx ttxtsgn -1))
            (+ (cadr crspnt) (* ds th tsgn -1)) 0.0))
      ) ; of progn
      (progn
          (setq vtpnt (list (+ (car crspnt) (* ds 2.0 ttxtsgn))
            (+ (cadr crspnt) (* dy 0.5 -1 tsgn)) 0.0))
          (setq htpnt (list (+ (car crspnt) (* 0.5 dx ttxtsgn -1))
            (+ (cadr crspnt) (* ds th tsgn)) 0.0))
      ) ; of progn
  ) ; of if

  (command "TEXT" "J" "M" htpnt (* th ds) "0" ht)
  (command "TEXT" "J" lr vtpnt (* th ds) "0" vt)

  (pop-env)                                 ;환경변수 복귀

  (setq *error* oer seterr nil)

) ; of defun

;******************************************
; Function : WHICH4
;            WHICH
;            Suk-Jong Yi
;            1995. 3. 15
;******************************************
(defun WHICH4(ang)

(cond
  ((<= ang (dtor 90.0)) (setq w4 1))
  ((and (> ang (dtor 90.0)) (<= ang (dtor 180.0))) (setq w4 2))
  ((and (> ang (dtor 180.0)) (<= ang (dtor 270.0))) (setq w4 3))
  ((and (> ang (dtor 270.0)) (<= ang (dtor 360.0))) (setq w4 4))
) ; of cond

) ; of defun
