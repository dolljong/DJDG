;********************************
; Program : TAV
;           Text Arrange Vertical
;           By Suk-Jong Yi
;           1995/5/27
;********************************
; Text재정렬 (수직정렬)

(defun C:TAV(/
ents nent ans w72 xpnt count ent
old10 old11 old72 new10 new11 new72

)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq ents (ssget))
(setq nent (sslength ents))

(initget "Left Middle Right")                    ;
(setq ans (getkword "\n<Left>/Middle/Right: "))
(if (= ans nil) (setq ans "Left"))
(cond
  ((= ans "Left")   (setq w72 0))
  ((= ans "Middle") (setq w72 1))
  ((= ans "Right")  (setq w72 2))
) ;of cond

(setq xpnt (getpoint "\nPick Point: "))

(setq count 0)
(repeat nent
  (setq ent (entget (ssname ents count)))
  (if (= "TEXT" (cdr (assoc 0 ent)))
    (progn
      (setq old10 (assoc 10 ent))                 ;시작점
      (setq old11 (assoc 11 ent))                 ;삽입점
      (setq old72 (assoc 72 ent))                 ;정렬방식
      (setq new10 (cons 10 (list (car xpnt) (cadr (cdr old10)))))
      (setq new11 (cons 11 (list (car xpnt) (cadr (cdr old11)))))
      (setq new72 (cons 72 w72))
      (if (= (cdr old72) 0)
        (progn                                    ; left정렬인 경우
          (if (= w72 0)
              (setq ent (subst new10 old10 ent))
            (progn
               (setq new11 (cons 11 (list (car xpnt) (cadr (cdr old10)))))
               (setq ent (subst new11 old11 ent))
            );of progn
          ) ;of if
          (setq ent (subst new72 old72 ent))
        ) ;of progn
        (progn                                           ; left정렬이 아닌 경우
          (if (= w72 0)
             (setq ent (subst new10 old10 ent))
             (setq ent (subst new11 old11 ent))
          ) ;of if
          (setq ent (subst new72 old72 ent))
        ) ;of progn
      ) ;of if
      (entmod ent)
    ) ;of progn
  ) ;of if
  (setq count (+ count 1))
);of repeat
  (setq *error* oer seterr nil)
(princ)
) ;of defun

