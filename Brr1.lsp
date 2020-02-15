;*******************************
; Program : BRR1
;           draw BaRRier
;           Yi Suk-Jong
;           00/7/28
;*******************************
; 방호벽을 그려준다 (도로공사) H=1.27m
;*******************************

(defun C:BRR1(/
              pk pp ent p1 p2 dst1 dst2 ip dx dy        ;지역변수 정의
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq pk (entsel "\nSelect line: "))                  ;선택정보

  (setq pp (cadr pk))                                   ;선택점
  (setq ent (entget (car pk)))                          ;선택 entity정보
  (setq p1 (cdr (assoc 10 ent))
        p2 (cdr (assoc 11 ent)))

  (setq dst1 (distance pp p1)                           ;시점과 선택점의 거리
        dst2 (distance pp p2))                          ;종점과 선택점의 거리

  (if (< dst1 dst2)                                     ;가까운 점을 insert point로
    (setq ip p1
          dx (- (car p2) (car p1))                      ;x편차
          dy (- (cadr p2) (cadr p1)))                   ;y편차
    (setq ip p2
          dx (- (car p1) (car p2))
          dy (- (cadr p1) (cadr p2)))
  ) ;of if

  (if (> dx 0)
    (barrier1 0 ip (/ dy dx -0.01))                      ;왼쪽 방호벽 호출
    (barrier1 1 ip (/ dy dx 0.01))                       ;오른쪽 방호벽 호출
  ) ;of if

  (setq *error* oer seterr nil)

) ;of defun

;********************************************
; Function : BARRIER1
;            DRAW BARRIER
;            Suk-Jong Yi
;            96/7/3
;********************************************
;방호책을 그려준다.
; 넘어오는 값
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun BARRIER1( LR ip SL / LR ip SL)
  (if (= LR 0)                                      ;왼쪽연석
    (progn
      (setq inpo (list (+ (car ip) 30)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;바깥쪽아래
      (setq inpi (list (+ (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;안쪽아래
      (command "PLINE" inpo "@0,1350" "@230,0" "@70,-970"
                           "@120,-175" inpi "")                             ;연석그리기
    ) ;of PROGN
    (progn
      (setq inpo (list (- (car ip) 30.0)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;바깥쪽아래
      (setq inpi (list (- (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;안쪽아래
      (command "PLINE" inpo "@0,1350" "@-230,0" "@-70,-970"
                           "@-120,-175" inpi "")                            ;연석그리기
    ) ;of progn
  ) ;of IF
) ;of defun


