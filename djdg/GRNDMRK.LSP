;*************************************
; Program : GRNDMRK
;           GRouND MaRK
;           By Suk-Jong Yi
;           98/10/14, 95/6/30
;*************************************

(defun C:GRNDMRK(/
p1 p2 ent p3 intp ang sabun
)
  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

  (setq p1 (getpoint "\nFirst point or press ENTER to select: "))       ;시작점
  (if (= p1 nil)
    (setq ent (entget (car (entsel "\nSelect line: ")))
          p1 (cdr (assoc 10 ent))
          p2 (cdr (assoc 11 ent)))
    (setq p2 (getpoint p1 "\nPick point second point: "))   ;끝점
  );if

  (setq p3 (getpoint "\nPick insert point: "))
  (setq intp (inters (list (car p1) (cadr p1))
                     (list (car p2) (cadr p2))
                     (list (car p3) (cadr p2))
                     (list (car p3) (- (cadr p3) 1.0)) nil))

  (setq ang (angle p1 p2))             ;두점의 각
  (setq sabun (which4 ang))  ;몇사분면인지 판단

  (cond
    ((= sabun 2)  ;2사분면일때
      (setq ang (- ang pi))
    );sub cond
    ((= sabun 3)  ;3사분면일때
      (setq ang (- ang pi))
    );sub cond
    ((= sabun 4)  ;4사분면일때
      (setq ang (- ang (* pi 2)))
    );sub cond
  );cond

  (command "INSERT" (strcat (prefix) "BLOCKS/GRNDMRK")         ;
                    intp (getvar "DIMSCALE") ""
                    (rtod ang))


  (setq *error* oer seterr nil)                             ;에러루틴 복귀

  (prin1)

) ;of defun
