;****************************************
;*    DELSHORT
;*              Delete Short lines
;*              By Suk-Jong Yi
;*              1999. 5. 19
;****************************************

(defun C:DELSHORT()
  (defun SETERR(s)                      ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

   (initget "All Select")
   (setq ans (getkword "\nAll/<Select>: "))

   (cond
       ((or (= ans nil) (= ans "Select"))           ;select선택시
           (princ "\nSelect Lines: ")
           (setq ss1 (ssget))
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((0 . "LINE"))))   ; 모든 lines선택
       ) ;of ans=All
   ); of cond

   (setq num (sslength ss1))                    ;선택된 line의 갯수

   (princ num)
   (princ " Line found")

   (setq len (getdist "\nEnter Length: "))

   (setq index 0)
   (setq dnum 0)                                ;지운갯수
   (repeat num                                  ;선택된 entity갯수만큼 반복
     (setq entl (entget (ssname ss1 index)))
     (setq ass (cdr (assoc 0 entl)))
     (if (= ass "LINE")
       (progn
         (setq ass10 (cdr (assoc 10 entl))            ;시작점좌표
               ass11 (cdr (assoc 11 entl))            ;끝점 좌표
               l (distance ass11 ass10))        ;line길이
         (if (< l len)
           (progn
             (entdel (ssname ss1 index))           ;line지우기
             (setq dnum (1+ dnum))                 ;지운 갯수 추가
           );progn
         );if
       );progn
     );if
     (setq index (1+ index))                        ;다음 line으로
   ) ;of repeat

   (princ dnum)
   (princ " Line Deleted")

  (setq *error* oer seterr nil)

   (princ)
) ;of defun

