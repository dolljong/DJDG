;*************************************
;  Program : ARD
;            ARray Distance
;            By Jong-Suk Yi
;            1995. 2. 27
;*************************************
; 주어진 구간을 array한다.
;*************************************

(defun c:ARD(/
ffd spnt epnt ang dst ffdst num
ent ffpnt n entpnt entpp ffpnt
)
  (defun SETERR(s)                              ;내장 에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)             ;내장에러루틴 가동

  (setvar "CMDECHO" 0)

  (setq ent (ssget))                               ;array 엔티티 선택
  (setq spnt (getpoint "\nPick start point: "))     ;시작점 입력
  (setq epnt (getpoint spnt "\nPick end point: "))  ;끝점 입력

  (setq dx (- (car epnt) (car spnt)))               ;x차
  (setq dy (- (cadr epnt) (cadr spnt)))             ;y차

  (if (> (abs dx) (abs dy))                         ;큰쪽 거리 표시
    (progn (princ "\nDistance = ") (princ dx))
    (progn (princ "\nDistance = ") (princ dy))
  ) ;of if

  (setq arl (getdist "\nArray distance: "))          ;array간격

  (if (< dx 0) (setq dxsgn -1) (setq dxsgn 1))      ;x 부호
  (if (< dy 0) (setq dysgn -1) (setq dysgn 1))      ;y 부호

  (setq arxn (1+ (fix (abs (/ dx arl))))                 ;x갯수
        aryn (1+ (fix (abs (/ dy arl)))))                ;y갯수

  (if (> arxn aryn)                                 ;각격 지정
    (setq ardst (* arl dxsgn)
          aryn 1
          remain (rem dx arl))
    (setq ardst (* arl dysgn)
          arxn 1
          remain (rem dy arl))
  ) ;of if

  (command "ARRAY" ent "" "R" aryn arxn ardst)      ;array수행

  (princ "\nRemainder = ") (princ remain)           ;나머지 표시

  (setvar "CMDECHO" 1)
  (setq *error* oer seterr nil)
  (princ)
);; of defun ARD
