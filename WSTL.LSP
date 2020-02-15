;************************************
; Program : WSTL
;           Write STeel Length
;           By Suk-Jong Yi
;           1998/11/19
;************************************
; 선택한 강재의 길이를 써준다
; wlen.lsp은 각각 선택하게 되어있지만
; wstl은 선택된 부재에 대해서 길이를 적어준다.
;************************************

(defun C:WSTL(/
              th    ta    pnt1   pnt2    txt    tpnt
)

  (defun SETERR(s)                          ;내장에러루틴
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

  (setvar "CMDECHO" 0)
  (setq th (getvar "TEXTSIZE"))

  (princ "\nPrefix<")(princ pref)(princ ">: ")
  (setq npref (getstring))                                  ;머리글 입력

  (if (/= npref "") (setq pref npref))                      ;Enter입력시 예전값으로

  (setq pref (strcase pref))                                ;대문자로 변환

;  (while (setq pnt1 (getpoint "\nPick first point: "))  ;첫점 입력
;    (setq pnt2 (getpoint pnt1 "\nPick second point: "))      ;들째점 입력

  (while (setq lent (entget (car (entsel "\nSelect Line Entity: "))))  ;line선택

    (setq pnt1 (cdr (assoc 10 lent))
          pnt2 (cdr (assoc 11 lent)))

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

    (setq dst (distance pnt1 pnt2))                    	;두점의 거리 구하기
    (if (< dst 1000.0)
      (setq txt (rtos dst 2 0))                         ;1000미만일 때
      (progn						;100mm단위로 자르기
        (if (< (rem dst 100) 50)
          (setq dst (- dst (rem dst 100)))
          (setq dst (- dst (rem dst 100) -100))
        );if

        (setq txt (rtos (* dst 0.001) 2 3))                ;1000이상일 때

      );progn
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
        (princ "\n")(princ "Old Prefix : ")(princ pref)
        (setq pref (getstring "\nNew Prefix: "))            ;머리글 입력
        (setq txt1 (strcat pref txt))                       ;글머리 더하기
        (princ "\nTEXT: ") (princ txt1)                     ;현재 텍스트 보여주기
        (setq tpnt (getpoint "\nPick point: "))             ;삽입점 입력 받기
      ) ;of progn
    ) ;of if

    (command "TEXT" "C" tpnt th (rtod ta) txt1)                 ;텍스트 씀
  ) ;of while

  (setq *error* oer seterr nil)                             ;내장에러루틴 복귀

) ;of defun
