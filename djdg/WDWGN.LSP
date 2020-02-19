;****************************************************************
; Program : WDWGN
;           Write DWG Name
;           Yi Suk Jong
;           97/1/13
;****************************************************************
;도면명을 도면에다 써줌
;****************************************************************

(defun C:WDWGN(/ th ds th fn p)

  (setq th 2.0)                             ; text높이

  (setq ds (getvar "DIMSCALE"))             ;스케일값
  (setq th (* th ds))
;  (setq fn (strcat (dwg_name) ".DWG"))     ;file이름
  (setq fn (dwg_name))                      ;file이름
  (setq fn (substr fn 1 (- (strlen fn) 4)))
  (setq p (getpoint "\nPick Insert point: ")) ;삽입점
  (command "TEXT" p th "0" fn)              ;file이름 기입

  (princ)

) ;of defun


;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open 명령으로 파일을 불러올 경우 DWGNAME이 full path명이 되므로
; full path명중 파일명 부분을 추출해냄
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;파일이름 인식
  (setq ls (strlen dn))                                 ;string 길이
  (setq count ls)                                  ;마지막 string부터
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (substr dn (1+ count) (- ls count))
    (substr dn count (- ls (1- count)))
  ) ;of if

) ;of defun


