;***********************************
; Program : QTABLE
;           Quattro TABLE insert
;           By Suk-Jong Yi
;           1995/5/31
;***********************************
; Qpro 의 파일을 캐드로 불러온다.
; 쿼트로에서 작업후 출력을 파일로 한다.
; 이 프로그램은 그 파일을 불러온다.
; box안의 내용이 문자이면 왼쪽 정렬,
; 숫자이면 오른 정렬을 한다.

(defun C:QTABLE(/
dsc     ipnt    ipx     ipy     th      th2     rgap    rgap2   fn      in-f
row     clm     strl    pflag   floc    vflag   vloc    ch      p1      p2
txt     rslt    txt1    fspn    bspn    num     txtpnt  tr-il
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(push-env)                                          ;환경변수 대피

(setq dsc (getvar "DIMSCALE")                       ; dimscale값
      th (getvar "DIMTXT"))                         ; text크기

(setq ipnt (getpoint "\nPick insert point: "))      ;table의 insert point 입력
(setq ipx (car ipnt)                                ;insert point x,y값
      ipy (cadr ipnt))

(setq th (* th dsc)                                ;text 높이를 dimtxt
      th2 (/ th 2.0))                               ;text높이의 반
(setq rgap (* 7.0 dsc)                              ;line간격을 7.0mm로
      rgap2 (/ rgap -2.0))                          ;line간격의 반

(setq fn (getfiled "Input" "" "prn" 0))         ;파일이름 입력
(setq in-f (open fn "r"))                       ;file open
(setq row 0)                                    ; 줄번호

(while (setq il (read-line in-f))               ;input line
  (setq clm 1)                                  ; 열번호
  (setq strl (strlen il))                       ;문자열의 문자 갯수
  (setq pflag 0                                 ; + flag
        ploc  0)                                ; + location
  (setq vflag 0                                 ; | flag
        vloc  0)                                ; | location
  (repeat strl                                  ;문자 갯수만큼 반복
    (setq ch (substr il clm 1))                 ;문자한개 뽑아냄
    (cond
      ((= ch "+")                               ; + 기호를 만났을 경우
;        (if (= (substr il (- clm 1) 1) "-")    ; -+ 처음이면 라인을 안그림
        (if (>= clm 2)                            ;ADD
        (if (and (= (substr il (- clm 1) 1) "-")  ;ADD
                 (>= clm) 2)                      ;ADD
          (progn
            (setq p1 (list (+ ipx (* ploc th))
                           (+ ipy (* row rgap2))))   ;라인 첫점
            (setq p2 (list (+ ipx (* clm th))
                           (+ ipy (* row rgap2))))       ;라인 끝점
            (setvar "CECOLOR" "RED")
            (command "LINE" p1 p2 "")                    ;라인 그림
            (setvar "CECOLOR" "BYLAYER")
            (setq pflag 1)
            (setq ploc clm)
          ) ;of progn
        ) ;of if
        ) ;of if                                    ;ADD
        (if (= (substr il (+ clm 1) 1) "-")     ; +-
          (setq pflag 1
                ploc clm)
        ) ;of if
      ) ;of cond ch="+"
      ((= ch "|")                               ; | 기호를 만났을 경우
        (setq p1 (list (+ ipx (* clm th))
                       (+ ipy (* (- row 1) rgap2)))
              p2 (list (+ ipx (* clm th))
                       (+ ipy (* (+ row 1) rgap2))))
        (setvar "CECOLOR" "RED")
        (command "LINE" p1 p2 "")
        (setvar "CECOLOR" "BYLAYER")
        (if (= vflag 0)                 ; 처음이면 수직선 사이의
          (setq vflag 1                 ; 글자 뽑아내지 않음
                vloc clm)
          (progn                        ;두번째 이면 글자 뽑아냄
            (setq txt (substr il (+ vloc 1) (- clm vloc 1)))  ;글자
            (setq rslt (sp-trunc txt))
            (setq txt1 (car rslt))
            (if (/= txt1 nil)
              (progn
                (setq fspn (- (cadr rslt) 1)
                      bspn (- (strlen txt) (caddr rslt))
                       num (cadddr rslt))
                (if num
                  (progn
                    (setq txtpnt (list (+ ipx (- (* (- clm bspn) th) th2))   ;글자위치
                                       (+ ipy (- (* row rgap2) th2))))
                    (setvar "CECOLOR" "WHITE")
                    (command "TEXT" "R" txtpnt th "0" txt1)             ;글자 씀
                    (setvar "CECOLOR" "BYLAYER")
                  ) ;of progn THEN
                  (progn
                    (setq txtpnt (list (+ ipx (+ (* (+ vloc fspn) th) th2))   ;글자위치
                                       (+ ipy (- (* row rgap2) th2))))
                    (setvar "CECOLOR" "WHITE")
                    (command "TEXT" txtpnt th "0" txt1)             ;글자 씀
                    (setvar "CECOLOR" "BYLAYER")
                  ) ;of progn ELSE
                ) ;of if num?
              ) ;of if txt /= nil
            ) ;of if txt1
          ) ; of progn
        ) ;of if vflag=0
        (setq vflag 1)
        (setq vloc clm)
      ) ;of cond ch="|"
    ) ;of cond
    (setq clm (+ clm 1))                            ; 다음 칼럼으로
  ) ;of repeat charactor
  (if (and (= vflag 0) (= pflag 0) (/= strl 0))
    (progn
      (setq tr-il (sp-trunc il))
      (setq txt (car tr-il)
            fspn (- (cadr tr-il) 1))
      (setq txtpnt (list (+ ipx (* fspn th) th2)
                         (+ ipy (- (* row rgap2) th2))))
      (command "TEXT" txtpnt th "0" txt)
    ) ;of progn
  ) ;of if
  (setq row (+ row 1))                              ; 다음 줄로
) ;of while read line

(close in-f)                                        ; 파일 닫음

(pop-env)                                           ; 환경변수값 복귀

(princ)
  (setq *error* oer seterr nil)
) ;of defun


;************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;************************************
; 입력문자열의 앞,뒤에 있는 빈칸을 짤라낸다.
; 리턴값은
; (짤라낸 문자열, 첫 문자 나오는 위치, 마지막 문자 나오는 위치, 숫자인가?)

(defun SP-TRUNC(txt /
txtl frntn backn txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if

);of defun


;************************************
; Function : IS-NUM
;            IS NUMber ?
;            By Suk-Jong Yi
;            1995/6/1
;************************************
; 문자열이 숫자인가?를 판단해준다.

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (and (>= ch 43) (<= ch 46)))
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun
