;************************************
; Program : 2PD
;           2 Point Distance
;           By Suk-Jong Yi
;           1995/5/23
;************************************
; 두점 사이의 거리를 도면에 표시해줌
;************************************

(defun C:2PD(/
              th    ta    pref    pnt1   pnt2    txt    tpnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setvar "CMDECHO" 0)
;  (setq th (getvar "TEXTSIZE"))
  (setq th (getvar "DIMTXT"))                 ;text의 크기는 치수의 크기로
  (setq pref (getstring "\nPrefix: "))        ;머리글 입력

  (while (setq pnt1 (getpoint "\nPick first point: "))  ;첫점 입력
    (setq pnt2 (getpoint pnt1 "\nPick second point: "))      ;들째점 입력

    (setq ang (angle pnt1 pnt2))                        ;두점이 이루는 각

    (setq wh4 (which4 ang))                             ;몇사분면에 있는가?

    (cond                                               ;1~4사분면에 있을 때
       ((= wh4 1)
         (setq ta ang)
       )
       ((= wh4 2)
         (setq ta (- ang pi))
       )
       ((= wh4 3)
         (setq ta (- ang pi))
       )
       ((= wh4 4)
         (setq ta (- ang (* 2 pi)))
       )
    );of cond

    (setq dst (distance pnt1 pnt2))                    ;두점의 거리 구하기
    (if (< dst 1000.0)
      (setq txt (rtos dst 2 (getvar 0)))                          ;1000미만일 때
      (setq txt (rtos (* dst 0.001) 2 (getvar "LUPREC")))                ;1000이상일 때
    ) ;of if(dst < 1000)

    (if (/= pref nil)
      (setq txt1 (strcat pref txt))                     ;글머리 붙이기
      (setq txt1 txt)                                   ;글머리 없을 때
    ) ;of if

    (princ "\nTEXT: ") (princ txt1)                      ;현재 텍스트 보여주기

    (initget "Prefix")
    (setq tpnt (getpoint "\nChange [P]refix/<Pick point>: "))       ;삽입점 입력 받음
    (if (= tpnt "Prefix")                               ;글머리 바꾸고 싶을 때
      (progn
        (setq pref (getstring "\nNew Prefix: "))            ;머리글 입력
        (setq txt1 (strcat pref txt))                       ;글머리 더하기
        (princ "\nTEXT: ") (princ txt1)                     ;현재 텍스트 보여주기
        (setq tpnt (getpoint "\nPick point: "))             ;삽입점 입력 받기
      ) ;of progn
    ) ;of if

    (command "TEXT" "C" tpnt th (rtod ta) txt1)                 ;텍스트 씀
  ) ;of while
  (setq *error* oer seterr nil)
) ;of defun
