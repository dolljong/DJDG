;****************************************
;*    CTW
;*              Change Text Width
;*              By Suk-Jong Yi
;*              1997/1/13
;****************************************
; 텍스트의 Width를 바꿔줌
;****************************************
(defun C:CTW()
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

   (initget "Height All Select")
   (setq ans (getkword "\nHeight/All/<Select>: "))
   (cond
       ((or (= ans nil) (= ans "Select"))
           (princ "\nSelect text: ")
           (setq ss1 (ssget))
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((0 . "TEXT")))) ; All text
       ) ;of ans=All
       ((= ans "Height")
           (progn                                   ; Height text
               (setq cr (getreal "Enter text height: "))
               (setq sslst (list (cons 40 cr) '(0 . "TEXT")))
               (setq ss1 (ssget "X" sslst))
           ) ;of progn
       ) ;of ans="Height"
   ); of cond
   (setq num (sslength ss1))
   (princ num)
   (princ " found")
   (initget "Pick")
   (setq ncr (getdist "\nPick text/<New Width>: "))
   (if (= ncr "Pick")
       (setq ncr (cdr (assoc 41 (entget (car (entsel "\nPick text"))))))
   ) ;of if ncr=Pick
   (setq index 0)
   (setq cnum 0)
   (repeat num
       (setq entl (entget (ssname ss1 index)))
       (setq index (1+ index))
       (setq ass (assoc 0 entl))
       (if (= "TEXT" (cdr ass))
           (progn
               (setq cnum (1+ cnum))
               (setq ass1 (assoc 40 entl))    ;높이구하기
               (setq ass1 (assoc 41 entl))    ;폭구하기
               (setq ass10 (assoc 10 entl))   ;시점
               (setq ass11 (assoc 11 entl))   ;정렬점
               (setq ass72 (assoc 72 entl))   ;정렬방식
                (if (= (cdr ass72) 5 )        ;정렬방식이 fit일때
                  (progn
                    (setq ali (cons (car ass72) 1))   ;새 정렬방식을 center로
                    (setq mp (mid-point (cdr ass10) (cdr ass11))) ;center point
                    (setq nmp (cons (car ass11) mp))        ;center점 정보만들기
                    (setq entl (subst ali ass72 entl))      ;정렬방식 정보고치기
                    (setq entl (subst nmp ass11 entl))      ;center점 정보고치기
                  ) ;of progn
                ) ;of if
               (setq co (cons (car ass1) ncr))       ;새폭정보만들기
               (setq entl (subst co ass1 entl))      ;새폭정보바꾸기
               (entmod entl)
               (princ)
           ) ;of progn
       ) ;of if
   ) ;of repeat
   (terpri)
   (princ cnum)
   (princ " Modified")
  (setq *error* oer seterr nil)
   (princ)
) ;of defun

