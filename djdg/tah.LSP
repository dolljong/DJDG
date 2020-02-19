;********************************
; Program : TAH
;           Text Arrange Holizontal
;           By Suk-Jong Yi
;           1995/5/29
;********************************
; Text재정렬 (수평정렬)

(defun C:TAH(/
ents nent ypnt count ent
old10 old11 old72 new10 new11

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

(setq ypnt (getpoint "\nPick Point: "))

(setq count 0)
(repeat nent
  (setq ent (entget (ssname ents count)))
  (if (= "TEXT" (cdr (assoc 0 ent)))
    (progn
      (setq old10 (assoc 10 ent))                 ;시작점
      (setq old11 (assoc 11 ent))                 ;삽입점
      (setq old72 (assoc 72 ent))                 ;수평정렬방식
      (setq old73 (assoc 73 ent))                 ;수직정렬방식
      (setq new10 (cons 10 (list (car (cdr old10)) (cadr ypnt))))
      (setq new11 (cons 11 (list (car (cdr old11)) (cadr ypnt))))
      (if (and (= (cdr old72) 0) (= (cdr old73) 0))
        (setq ent (subst new10 old10 ent))
        (progn
          (setq ent (subst new11 old11 ent))
          (setq ent (subst (cons 73 0) old73 ent))
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

