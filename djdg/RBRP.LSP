;**************************************
; Program : RBRP
;           ReBar Round Plan
;           By Suk-Jong Yi
;           96/5/23
;**************************************
; 원형으로 철근을 배근한다. (평면)
; 콘크리트 선, 띠철근까지 그려준다.
;**************************************

(defun C:RBRP(/
    r       ctc     bn      loop    cenccen     oldc    dent
    cen     pdia    nlay    count   cover
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;환경변수 대피

  (setq cen (getpoint "\nPick Center point: "))                 ;철근 배근의 중심
  (setq pdia (* 1000 (getreal "\nPost DIA(m): ")))              ;post직경
  (command "CIRCLE" cen "D" pdia)                               ;post그리기

  (setq nlay (getint "\nNumber of layer: "))                    ;몇단 철근인가?

  (setq count 1)                                                ;첫번째 단부터
  (repeat nlay
    (princ "\n*** ") (princ count) (princ " LAYER")
    (setq cover (getreal " Cover(mm): "))                        ;덮개두께
    (setq r (- (/ pdia 2) cover))                               ;주철근 배근 반지름

    ;(setq r (getreal "\nRadius: "))                            ;철근 배근의 반지름

    (initget "Number")
    (setq ctc (getreal "\nNumber/<C.T.C(mm)>: "))               ; ctc 입력받음
    (if (= ctc "Number")
      (setq bn (getint "\nNumber of REBAR: "))                  ;철근 갯수 직접입력
      (setq bn (fix (/ (* 2.0 pi r) ctc)))                      ; 철근 갯수 계산
    ) ;of if
    (setq ctc (/ (* 2.0 pi r) bn))                              ; 갯수에 의한 ctc계산

    (princ "\nNumber of rebar: ")                               ; 철근 갯수 보여줌
    (princ bn)
    (princ "  New C.T.C.(mm): ")                                ; ctc보여줌
    (princ ctc)

    (initget "Num Ctc Ok")                                      ; 수정 여부 물어봄
    (setq loop (getkword "\nchange Number/Ctc/<Ok>: "))
    (if (= loop nil) (setq loop "Ok"))

    (while (/= loop "Ok")                   ; 철근간격 및 갯수 반복  수정입력
      (cond
        ((= loop "Num")                                         ; 갯수 수정
          (setq bn (getreal "\nNumber of Rebar: "))             ;갯수 입력
          (setq ctc (/ (* 2.0 pi r) bn))                        ;ctc계산
        ) ;of loop=Num
        ((= loop "Ctc")                                         ; ctc수정
          (setq ctc (getreal "\nC.T.C.(mm): "))                 ;ctc입력
          (setq bn (fix (/ (* 2.0 pi r) ctc)))                  ;갯수 계산
          (setq ctc (/ (* 2.0 pi r) bn))                        ;갯수에 의한 ctc계산
        ) ;of loop=Ctc
      ) ;of cond
      (princ "\nNumber of rebar: ")                             ; 갯수 출력
      (princ bn)
      (princ "  New C.T.C.(mm): ")                              ; ctc출력
      (princ ctc)
      (initget "Num Ctc Ok")                                    ; 다시 수정할 것인가?
      (setq loop (getkword "\nchange Number/Ctc/Ok: "))
      (if (= loop nil) (setq loop "Ok"))                        ; return입력시 OK로 봄
    ) ;of while


    (setq ccen (polar cen (* 1.5 pi) r))                        ;첫 철근 위치(하단)

    (setq oldc (getvar "CECOLOR"))                              ;현재색 대피
    (setvar "CECOLOR" "2")                                 ;색을 노란색으로
    (command "CIRCLE" cen "D" (- pdia (* 2 (- cover 15))))      ;띠철근
    (command "DONUT" "0" "30" ccen "")    ;도나스 그리기
    (setvar "CECOLOR" oldc)                                     ;현재색 되돌리기

    (setq dent (ssget "L"))                                     ;도나스 엔티티 선택
    (command "ARRAY" (list (ssname dent 0) ccen) "" "P" cen (fix bn) "360" "Y")  ;array

    (setq count (1+ count))                                     ;다음 단으로
  ) ;of repeat

  (pop-env)                                                     ;환경변수값 되돌리기
  (setq *error* oer seterr nil)
  (prin1)

) ;of defun
