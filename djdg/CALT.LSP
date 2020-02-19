;*******************************************
; Program : CALT
;           CALculate and text to drawing
;           By Suk-Jong Yi
;           1995/6/22
;*******************************************
;사칙연산을 하고 그 결과를 도면에 표시
;*******************************************

(defun C:CALT(/
                ssent   ds      ents    nent    count   sum     scount
                txt     ans     lp      nlp     tpnt    numtxt  th
                nssent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                ;환경변수 대피
  (setq ds (getvar "DIMSCALE"))             ;scale factor구함
  (setq th (getvar "DIMTXT"))

  (setq total 0)

  (initget 1 "Sum")
  (setq oper (entsel "\nSummation/<Select text>: ")) ;숫자 text입력

  (while (= oper nil)
    (princ "\Select entity or Sum")
    (initget 1 "Sum")
    (setq oper (entsel "\nSummation/<Select text>: ")) ;숫자 text입력
  ) ;of while

  (if (/= oper "Sum")
    (progn
      (setq total (atof (cdr (assoc 1 (entget (car oper))))))
      (princ "\nTotal = ") (princ total)
      (initget 1 "Sum + - * / Exit Insert")
      (setq oper (getkword "\nSum / + / - / * / / /Insert/Exit:"))
    ) ;of THEN
  ) ;of IF

  (while (or (= oper "Sum") (= oper "+") (= oper "-") (= oper "*") (= oper "/"))
    (cond
      ((= oper "Sum")
        (princ "\nSelect text: ")
        (setq ents (ssget))                       ;entity 입력
        (setq nent (sslength ents))               ;entity 갯수

        (setq count 0)                            ;반복명령 변수 초기화
        (setq sum 0)
        (setq scount 0)

        (repeat nent                              ;entity갯수 만큼 반복
          (setq ent (entget (ssname ents count))) ;entity 정보
          (if (= (cdr (assoc 0 ent)) "TEXT")      ;entity가 text인 경우
            (progn
              (setq txt (cdr (assoc 1 ent)))      ;text의 내용
              (if (is-num txt)                    ;text의 내용이 숫자라면
                (progn                            ;entity정보 selection set에 추가
                  (if (= scount 0) (setq ssent (ssadd (ssname ents count)))
                                   (setq ssent (ssadd (ssname ents count) ssent))
                  ) ;of if
                  (redraw (ssname ents count) 3)  ;text강조
                  (setq sum (+ sum (atof txt)))   ;text의 합 구함
                  (setq scount (1+ scount))       ;숫자인 text의 갯수
                ) ;of progn
              ) ;of if txt is number
            ) ;of progn
          ) ;of if entity=text
          (setq count (1+ count))                 ;다음 entity로
        ) ;of repeat

        (setq nssent (sslength ssent))
        (setq count 0)
        (repeat nssent
          (redraw (ssname ssent count) 4)                 ;강조된 entity를 정상으로
          (setq count (1+ count))
        ) ;of repeat

        (princ "\n") (princ scount) (princ " Founded")    ;몇개의 text가 선택?는가를 출력
        (princ "\nSumation: ") (princ sum)                ;총계를 출력
        (setq total (+ total sum))
      ) ;of cond(= oper "Sum")

      ((or (= oper "+") (= oper "-") (= oper "*") (= oper "/"))
        (initget "Num")
        (setq txt_ent (entsel "\nNumber/<Select number text>: "))
        (if (= txt_ent "Num")
          (cond
            ((= oper "+") (setq total (+ total (getreal "\nNumber: "))))
            ((= oper "-") (setq total (- total (getreal "\nNumber: "))))
            ((= oper "*") (setq total (* total (getreal "\nNumber: "))))
            ((= oper "/") (setq total (/ total (getreal "\nNumber: "))))
          ) ;of THEN
          (progn
            (setq txt_ent (entget (car txt_ent)))
            (if (= txt_ent nil)
              (setq txt_str nil)
              (setq txt_str (cdr (assoc 1 txt_ent)))
            ) ;of IF
            (while (or (= (is-num txt_str) nil)  (= txt_str nil))
              (princ "\nEntity is Not NUMBER")
              (while (= (setq txt_ent (entsel "\nSelect number text: ")) nil))
              (if (= txt_ent nil)
                (setq txt_str nil)
                (setq txt_str (cdr (assoc 1 txt_ent)))
              ) ;of IF
            ) ;of WHILE
            (cond
              ((= oper "+") (setq total (+ total (atof txt_str))))
              ((= oper "-") (setq total (- total (atof txt_str))))
              ((= oper "*") (setq total (* total (atof txt_str))))
              ((= oper "/") (setq total (/ total (atof txt_str))))
            ) ;of COND
          ) ;of ELSE
        ) ;of IF(= txt_ent "Num")
      ) ;of cond(or (= oper "+") (= oper "-") (= oper "*") (= oper "/"))

    ) ;of COND

    (princ "\nTotal = ") (princ total)
    (initget 1 "Sum + - * / Exit Insert")
    (setq oper (getkword "\nSum / + / - / * / / /Insert/Exit:"))

  ) ;of WHILE

  (if (= oper "Insert")
    (progn
      (setq lp (getvar "LUPREC"))
      (princ "\nNumber of digits to right of decimal point (0 to 8): <")
      (princ lp)                                    ;소숫점 이하 자릿수 물음
      (princ ">: ")
      (setq nlp (getint))
      (if (= nlp nil) (setq nlp lp))                ;return입력시 옛 자릿수로
      (setq tpnt (getpoint "\nPick insert point: "))  ;삽입점 입력
      (setq numtxt (rtos total 2 nlp))                  ;숫자를 string으로
      (command "TEXT" "R" tpnt (* th ds) "0" numtxt)         ;text 명령수행
    ) ;of THEN
  ) ;of if

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)
) ;of defun

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
             (= ch 44) (= ch 46))
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun
