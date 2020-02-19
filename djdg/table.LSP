;*************************************************
; Program : TABLE
;           TABLE draw
;           Suk-Jong Yi
;           96/5/14
;*************************************************
; 주어진 칸수와 줄수를 가진 테이블을 만들어준다
; (줄간격 7mm로 고정)
;*************************************************

(defun C:TABLE(/
 ds ip  cn  rn  pc  ys  ye  xs  xe  count x spnt epnt y oldc
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                            ;환경변수 대피

  (setq ds (getvar "DIMSCALE"))                         ;스케일값

  (setq ip (getpoint "\nInsert point: "))                ;삽입점
  (setq cn (getint "\nColumn Number: "))                ;칸수
  (setq rn (getint "\nRow Number: "))                   ;줄수
  (setq pc (getint "\nPitch of Column(mm)<20>: "))      ;칸수
  (if (= pc nil) (setq pc 20))                           ;blank입력시 20mm

  (setq ys (cadr ip))                                    ;수직선 시작 y좌표
  (setq ye (- ys (* rn 7.0 ds)))                        ;수직선 끝 y좌표

  (setq xs (car ip))                                    ;수평선 시작 x좌표
  (setq xe (+ xs (* cn pc ds)))                         ;수평선 끝 x좌표

  (setq oldc (getvar "CECOLOR"))                        ;현재색 대피
  (setvar "CECOLOR" "RED")                              ;현재색을 붉은 색으로
  (setq count 0)                                        ;수직선 그리기
  (repeat (1+ cn)
    (setq x (+ xs (* count pc ds)))                     ;수직선 x좌표
    (setq spnt (list x ys 0.0))                         ;수직선 시작점
    (setq epnt (list x ye 0.0))                         ;수직선 끝점
    (if (or (= count 0) (= count cn))                   ;처음과 끝선은 흰색으로
      (progn
        (setvar "CECOLOR" "WHITE")
        (command "LINE" spnt epnt "")
        (setvar "CECOLOR" "RED")
      ) ;of progn
      (command "LINE" spnt epnt "")                       ;line그리기
    ) ;of IF
    (setq count (1+ count))                             ;다음 수직선으로
  ) ;of REPEAT

  (setq count 0)                                        ;수평선 그리기
  (repeat (1+ rn)
    (setq y (- ys (* count 7.0 ds)))                     ;수평선 x좌표
    (setq spnt (list xs y 0.0))                         ;수평선 시작점
    (setq epnt (list xe y 0.0))                         ;수평선 끝점
    (if (or (= count 0) (= count rn))                   ;처음과 끝선은 흰색으로
      (progn
        (setvar "CECOLOR" "WHITE")
        (command "LINE" spnt epnt "")
        (setvar "CECOLOR" "RED")
      ) ;of progn
      (command "LINE" spnt epnt "")                       ;line그리기
    ) ;of IF
    (setq count (1+ count))                             ;다음 수평선으로
  ) ;of REPEAT

  (setvar "CECOLOR" oldc)                               ;대피색 복귀

  (pop-env)                                             ;환경변수 복귀
  (setq *error* oer seterr nil)
  (princ)
) ;of defun
