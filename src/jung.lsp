;*******************************
; Program : JUNG
;           JUNG bun dae
;           Yi Suk-Jong
;           98/1/22
;*******************************
; 중앙분리대를 그려준다 (도로공사)
;*******************************

(defun C:JUNG(/
              pk pp ent p1 p2 dst1 dst2 ip dx dy        ;지역변수 정의
)
  (defun SETERR(s)                                      ;내장 에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                     ;내장 에러루틴 가동

  (setq pk (entsel "\nSelect line: "))                  ;슬라브 상단선 선택

  (setq pp (cadr pk))                                   ;선택점
  (setq ent (entget (car pk)))                          ;슬라브상단선의 시작점과 끝점
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
    (jungbundae 0 ip (/ dy dx -0.01))                      ;왼쪽 중분대 호출
    (jungbundae 1 ip (/ dy dx 0.01))                       ;오른쪽 중분대 호출
  ) ;of if

  (setq *error* oer seterr nil)                         ;에러루틴 복귀

) ;of defun

;********************************************
; Function : JUNGBUNDAE
;            draw JUNBUNDAE
;            Suk-Jong Yi
;            98/1/22
;********************************************
;중분대를 그려주는 펑션.
; 넘어오는 값
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun JUNGBUNDAE( LR ip SL / LR ip SL)
  (if (= LR 0)                                                  ;왼쪽연석
    (progn
;      (setq inpo (list (+ (car ip) 30)
;                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;바깥쪽아래
      (setq inpi (list (+ (car ip) 280.0)
                       (+ (cadr ip) (* sl -280.0 0.01)) 0.0))   ;안쪽아래
      (command "PLINE" ip "@0,890" "@95,0" "@60,-560"
                           "@125,-175" inpi "")                 ;중분대 그리기
    ) ;of PROGN
    (progn
;      (setq inpo (list (- (car ip) 30.0)
;                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;바깥쪽아래
      (setq inpi (list (- (car ip) 280.0)
                       (+ (cadr ip) (* sl -280.0 0.01)) 0.0))   ;안쪽아래
      (command "PLINE" ip "@0,890" "@-95,0" "@-60,-560"
                           "@-125,-175" inpi "")                ;중분대그리기
    ) ;of progn
  ) ;of IF
) ;of defun


