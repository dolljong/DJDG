;*************************************
;  Program : ARSLOP
;            ARray SLOP
;            By Jong-Suk Yi
;            1998. 1. 22
;*************************************
; 경사진 면을 따라 array한다.
;*************************************

(defun c:ARSLOP(/
ent spnt epnt spntx spnty dx dy slpnt1 slpnt2 sldx sldy slent arl
kword temp dxsgn dysgn arxn aryn totalx totaly ardstx ardsty repeatn
angbase angbase90 intspnt intepnt angdia ldia
count cpnt
)
  (defun SETERR(s)                                          ;내장 에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                        ;내장에러루틴 가동

  (setvar "CMDECHO" 0)

  (setq ent (ssget))                                        ;array 엔티티 선택

  (setq spnt (getpoint "\nPick start point: "))             ;시작점 입력
  (setq epnt (getpoint spnt "\nPick end point: "))          ;끝점 입력
  (setq spntx (car spnt)                                    ;시작점 x좌표
        spnty (cadr spnt))                                  ;시작점 y좌표
  (setq dx (- (car epnt) (car spnt)))                       ;x차
  (setq dy (- (cadr epnt) (cadr spnt)))                     ;y차

  (if (< dx 0) (setq dxsgn -1) (setq dxsgn 1))              ;x 부호
  (if (< dy 0) (setq dysgn -1) (setq dysgn 1))              ;y 부호

;--- array간격/방향 입력
  (setq arl (getdist "\nArray distance: "))                 ;array간격
  (initget "X Y D")                ;x,y,diagonal 방향지정
  (setq kword (getkword "\nX-direction/Y-direction/Diagonal: "))

;--- base라인 또는 2점으로 기울기입력

  (initget "2P")
  (setq slent (entsel "\nSelect base line/2P: "))
  (cond
    ((= slent "2P")                                             ;2점으로 각도지정
      (setq slpnt1 (getpoint "\nPick start point: "))           ;slop시작점
      (setq slpnt2 (getpoint slpnt1 "\nPick end point: "))      ;slop끝점
      (setq sldx (- (car slpnt2) (car slpnt1)))                 ;slop x차
      (setq sldy (- (cadr slpnt2) (cadr slpnt1)))               ;slop y차
    ) ;of sub cond
    ((/= slent "2P")
      (setq slent (entget (car slent)))                          ;line을 선택해 각도지정
      (setq slpnt1 (cdr (assoc 10 slent)))                       ;시점
      (setq slpnt2 (cdr (assoc 11 slent)))                       ;종점
      (setq sldx (- (car slpnt2) (car slpnt1))
            sldy (- (cadr slpnt2) (cadr slpnt1)))
    ) ;of sub cond
  ) ;of cond

  (cond
    ((= kword "X")                        ;x방향 array인 경우
      (if (< (* sldx dxsgn) 0)
        (setq temp   slpnt1
              slpnt1 slpnt2
              slpnt2 temp))
    ) ;of sun cond
    ((= kword "Y")                        ;y방향 array인 경우
      (if (< (* sldy dysgn) 0)
        (setq temp   slpnt1
              slpnt1 slpnt2
              slpnt2 temp))
    ) ;of sun cond
  ) ;of cond

  (setq sldx (- (car slpnt2) (car slpnt1))
        sldy (- (cadr slpnt2) (cadr slpnt1)))


;--- 갯수/길이 산정
  (setq arxn (fix (abs (/ dx arl)))                         ;x갯수
        aryn (fix (abs (/ dy arl))))                         ;y갯수

  (setq totalx (* arxn arl)                                  ;x방향 총길이
        totaly (* aryn arl))                                  ;y방향 총길이

;--- x방향/y방향 판단
  (cond
    ((= kword "X")                                          ;x방향 array일때
      (setq ardstx (* arl dxsgn))                           ;x방향 거리
      (setq ardsty (/ (* (/ sldy (abs sldx)) totalx) arxn)) ;y방향 거리
      (setq repeatn arxn)                                   ;copy 갯수
    ) ;of sub cond
    ((= kword "Y")                                          ;y방향 array일때
      (setq ardstx (/ (* (/ sldx (abs sldy)) totaly) aryn)) ;x방향 거리
      (setq ardsty (* arl dysgn))                           ;y방향 거리
      (setq repeatn aryn)                                   ;copy 갯수
    ) ;of sub cond
    ((= kword "D")                                          ;y방향 array일때
      (setq angbase (angle slpnt1 slpnt2))          ;base라인의 각도
      (setq angbase90 (+ angbase (/ pi 2.0)))         ;base라인의 직각
      (setq intspnt (inters slpnt1 slpnt2
                            spnt (polar spnt angbase90 1.0) nil))  ;base라인 위의 시작점
      (setq intepnt (inters slpnt1 slpnt2
                            epnt (polar epnt angbase90 1.0) nil))  ;base라인 위의 끝점
      (setq angdia (angle intspnt intepnt))         ;base라인의 각도
      (setq ldia (distance intspnt intepnt))        ;base라인길이
      (setq repeatn (fix (abs (/ ldia arl))))       ;y갯수
    ) ;of sub cond
  ) ;of cond

;--- copy명령 수행
  (command "copy" ent "" "M" spnt)                          ;copy명령시작

  (cond
    ((or (= kword "X") (= kword "Y"))                     ;x,y방향 array인 경우
      (setq count 1)
      (repeat repeatn                                           ;copy갯수만큼 반복
        (setq cpnt (list (+ spntx (* ardstx count))         ;copy point
                          (+ spnty (* ardsty count))
                          0.0))
        (command cpnt)                                  ;copy명령 수행
        (setq count (1+ count))
      ) ;of repeat
      (command "")                                      ;copy 명령 끝냄
    ) ;of sub cond
    ((= kword "D")                                      ;x,y방향 array인 경우
      (setq count 1)
      (repeat repeatn                                   ;copy갯수만큼 반복
        (setq cpnt (polar spnt angdia (* count arl)))  ;copy point
        (command cpnt)                                      ;copy명령 수행
        (setq count (1+ count))
      ) ;of repeat
      (command "")                                          ;copy 명령 끝냄
    ) ;of sub cond
  ) ;of cond

;--- 총길이 및 갯수,나머지 표시
  (cond
    ((= kword "X")                                          ;x방향 array일때
      (terpri) (princ repeatn)
      (princ "@")
      (princ ardstx)
      (princ "=")
      (princ totalx)
    ) ;of sub cond
    ((= kword "Y")                                          ;y방향 array일때
      (terpri) (princ repeatn)
      (princ "@")
      (princ ardsty)
      (princ "=")
      (princ totaly)
    ) ;of sub cond
    ((= kword "D")                                          ;Diagonal방향 array일때
      (terpri) (princ repeatn)
      (princ "@")
      (princ arl)
      (princ "=")
      (princ (* arl repeatn))
    ) ;of sub cond
  ) ;of cond

  (setvar "CMDECHO" 1)

  (setq *error* oer seterr nil)                             ;내장에러루틴 복귀

  (princ)

);; of defun ARD
