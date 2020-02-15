;****************************************
;*    CTH
;*              Change Text Height
;*              By Suk-Jong Yi
;               98/12/16: Scale option 추가
;               98/8/26 : MTEXT 인식가능하도록 수정
;*              95/2/5
;****************************************

(defun C:CTH()

  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

   (initget "Height All Select")
   (setq ans (getkword "\nHeight/All/<Select>: "))
   (cond
       ((or (= ans nil) (= ans "Select"))
           (princ "\nSelect text: ")
           (setq ss1 (ssget))
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((-4 . "<OR")
                                  (0 . "TEXT")
                                  (0 . "MTEXT")
                                  (-4 . "OR>")))) ; All text
       ) ;of ans=All
       ((= ans "Height")
           (progn                                   ; Height text
               (setq cr (getreal "Enter text height: "))
               (setq sslst (list
                             '(-4 . "<OR")
                               '(-4 . "<AND")
                                 (cons 40 cr)
                                 '(0 . "TEXT")
                               '(-4 . "AND>")
                               '(-4 . "<AND")
                                 (cons 40 cr)
                                 '(0 . "MTEXT")
                               '(-4 . "AND>")
                             '(-4 . "OR>")
                           );list
               );setq
               (setq ss1 (ssget "X" sslst))
           ) ;of progn
       ) ;of ans="Height"
   ); of cond
   (setq num (sslength ss1))
   (princ num)
   (princ " found")
   (initget "Pick Scale")
   (setq ncr (getdist "\nPick text/Scale/<New height>: "))
   (cond
     ((= ncr "Pick")            ;새 글자크기를 선택된 기존글자 크기로
       (setq ncr (cdr (assoc 40 (entget (car (entsel "\nPick text"))))))
     );of sub cond
     ((= ncr "Scale")           ;새 글자크기는 기존글자에 scale값을 곱하도록
       (setq Hscale (getreal "\Enter Scale: "))
     ) ;of sub cond
   ) ;of cond

   (setq index 0)
   (setq cnum 0)
   (repeat num
       (setq entl (entget (ssname ss1 index)))
       (setq index (1+ index))
       (setq ass (assoc 0 entl))
       (if (or (= "TEXT" (cdr ass)) (= "MTEXT" (cdr ass)))
         (progn
           (setq cnum (1+ cnum))
           (setq ass1 (assoc 40 entl))
           (if (= ncr "Scale")
             (setq newTH (* Hscale (cdr ass1)))
             (setq newTH ncr)
           );of if
           (setq co (cons (car ass1) newTH))
           (setq entl1 (subst co ass1 entl))
           (entmod entl1)                       ;새로운 크기로 업데이트
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

