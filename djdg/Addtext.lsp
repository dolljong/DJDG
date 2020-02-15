;****************************************
;*    ADDTEXT
;*              ADD to TEXT
;*              By Suk-Jong Yi
;*              99/6/8
;****************************************

(defun C:addtext(
 / ans ss1 num index cnum entl ass ass1 newtext co entl1
)

  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

   (initget "Old New")
   (setq ansdec (getkword "\nDecimail Precision New/<Old>: "))
   (cond
     ((or (= ansdec "Old") (= ansdec nil))
       (setq ansdec  "Old")
     );subcond 
     ((= ansdec "New")
       (setq dec (getint "\nEnter Decimal: "))
     );subcond 
   );if

   (initget "All Select")
   (setq ans (getkword "\nAll/<Select>: "))
   (cond
       ((or (= ans nil) (= ans "Select"))
           (princ "\nSelect text: ")
           (setq ss1 (ssget     '((-4 .   "<OR")
                                  ( 0 .  "TEXT")
                                  ( 0 . "MTEXT")
                                  (-4 .   "OR>")))) ; All text
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((-4 . "<OR")
                                  (0 . "TEXT")
                                  (0 . "MTEXT")
                                  (-4 . "OR>")))) ; All text
       ) ;of ans=All
   ); of cond

   (setq num (sslength ss1))                        ;선택된 text갯수 출력
   (princ num)
   (princ " found")

   (setq addval (getreal "\nEnter Add value: "))    ;더해질 값 지정

   (setq index 0)
   (setq cnum 0)
   (repeat num                                      ;text갯수만큼 반복
       (setq entl (entget (ssname ss1 index)))
       (setq index (1+ index))
       (setq ass (assoc 0 entl))
       (if (or (= "TEXT" (cdr ass)) (= "MTEXT" (cdr ass)))
         (progn
           (setq ass1 (assoc 1 entl))         ;text내용
           (if (is-num (cdr ass1))
             (progn
               (if (= ansdec "Old")
                  (setq dec (get_decnum (cdr ass1)))
               );if
               (setq newtext (rtos (+ addval (atof (cdr ass1))) 2 dec))
               (setq co (cons (car ass1) newtext))
               (setq entl1 (subst co ass1 entl))
               (entmod entl1)                       ;새로운 크기로 업데이트
               (setq cnum (1+ cnum))
             );progn
           );if
           (princ)
         ) ;of progn
       ) ;of if
   ) ;of repeat

   (terpri)
   (princ cnum)
   (princ " Modified")

;  (setq *error* oer seterr nil)

   (princ)

) ;of defun

;************************************
; Function : IS-NUM
;            IS NUMber ?
;            By Suk-Jong Yi
;            1996/2/23
;************************************
; 문자열이 숫자인가?를 판단해준다.
;************************************

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (= ch 44)
             (= ch 46)
             (and (= count 1) (= ch 43))
             (and (= count 1) (= ch 45))
         )
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun

;-------------------------------------
; functin : get_decnum
;           by Yi Suk Jong (www.bridgecad.co.kr)
;           01/06/21
;-------------------------------------
;주어진 string의 소수점 자리수를 돌려준다.
;-------------------------------------
(defun get_decnum(txt)
  (setq txtl (strlen txt))
  (setq position (str_position txt "."))
  (if (= position nil) (setq position 0))
  (setq ndec (- (strlen txt) position))
  (if (= ndec txtl)
    0
    ndec
  );if
);of defun

;----------------------------------
; function : str_position
;            Yi Suk Jong
;            00/7/15
;----------------------------------
; str1 : long string
; str2 : short string
;----------------------------------
(defun str_position(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
	len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1)) nil count)
);defun str_position

