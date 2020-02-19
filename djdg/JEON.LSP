;*********************************************
; Program : JEON
;           JEON myeon
;           Suk-Jong Yi
;           96/5/20
;*********************************************
; 배근도에서 box 안에 전면이라고 표시해준다.
;*********************************************

(defun C:JEON(/)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수 저장

  (setq B 12
        H  4)

  (setq ds 50)                                               ;스케일 값

  (princ "\nPick insert point: ")

  (command "INSERT" (strcat (prefix) "blocks/jeon") pause 1 1 "0")

  (setq  ipnt (cdr (assoc 10 (entget (entlast))))) ;삽입된 블럭의 insert point

  (setq p1 (list (- (car ipnt) (* ds B))
                 (+ (cadr ipnt) (* ds H)) 0.0))             ;왼쪽 상단
  (setq p2 (list (+ (car ipnt) (* ds B))
                 (- (cadr ipnt) (* ds H)) 0.0))             ;오른쪽 하단
  (setq p3 (list (- (car ipnt) (* ds B))
                 (- (cadr ipnt) (* ds H)) 0.0))             ;왼쪽 하단
  (setq p4 (list (+ (car ipnt) (* ds B))
                 (+ (cadr ipnt) (* ds H)) 0.0))             ;오른쪽 상단

  (setq p11 (list (- (car ipnt) (* ds B) -1)
                  (+ (cadr ipnt) (* ds H) -1) 0.0))         ;trim point 왼쪽상단
  (setq p22 (list (+ (car ipnt) (* ds B) -1)
                  (- (cadr ipnt) (* ds H) -1) 0.0))         ;trim point 오른쪽하단
  (setq p33 (list (- (car ipnt) (* ds B) -1)
                  (- (cadr ipnt) (* ds H) -1) 0.0))         ;trim point 왼쪽하단
  (setq p44 (list (+ (car ipnt) (* ds B) -1)
                  (+ (cadr ipnt) (* ds H) -1) 0.0))         ;trim point 오른쪽상단

  (setq oldc (getvar "CECOLOR"))                            ;초록색으로
  (setvar "CECOLOR" "GREEN")
  (command "PLINE" p1 "W" "0.0" "0.0" p4 p2 p3 "C")         ;사각형 그리기
  (setvar "CECOLOR" oldc)                                   ;원래색으로

  (setq lstent (entlast))                                   ;사각형 선택

  (command "TRIM" lstent "" "f" p11 p44 ""
                            "f" p44 P22 ""
                            "f" p22 P33 ""
                            "f" p33 p11 "" "")        ;잘라내기

  (command "ERASE" "WP" p11 p44 p22 p33 "" lstent "")              ;사각형 안 지우기
  (command "REDRAW")

  (pop-env)                                                 ;환경변수 복귀
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
