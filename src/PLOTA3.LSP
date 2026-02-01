;****************************************************************************
; Program : PLOTA1
;           PLOTA1
;           By Suk-Jong Yi
;           1995/11/3
;****************************************************************************
; 도면내에 있는 모든 Border를 출력 (A1용)
; Device  : 명령내리기 전에 미리 선택
; Scale   : Fit / Factor 두가지 옵션
;           (Factor를 선택했을 때 DIMSCALE 변수자동인식)
; plt이름 : Border가 한개인 경우 - DWG name과 같게
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;****************************************************************************

(defun C:PLOTA1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

  (setq bdr_B 800                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER*"                             ;블럭 이름
        xg    -5                                    ;x gap
        yg    -5)                                   ;y gap

;*** 파일 리스트 읽기
;  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name입력
  (setq opf (open "PLT.LST" "r"))                          ;file open
  (setq dwglst nil)
  (if opf
    (progn
      (setq count 1)
      (while (setq dwgn (read-line opf))
        (setq dwglst (append dwglst (list dwgn)))
        (setq count (1+ count))
      ) ;of while
      (close opf)
    ) ;of progn
  ) ;of IF

  (princ "PLT LIST: ") (princ dwglst)

  (setq dwgnum (length dwglst))
  (setq opf (open "PLT.LST" "w"))
  (setq count 1)
  (repeat (1- dwgnum)
    (write-line (nth count dwglst) opf)
    (setq count (1+ count))
  ) ;of repeat
  (close opf)

  (princ dwglst)

  (setq ds (getvar "DIMSCALE"))                     ;scale 값구함

  (setq dwgn (getvar "DWGNAME"))
  (setq dwgnext (car dwglst))
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))  ;filter list
;  (princ f_list)
  (setq ss_lst (ssget "X" f_list))                          ;entity 선택

  (if (/= ss_lst nil)                                       ;border가 있을 때만
    (progn
      (setq ss_num (sslength ss_lst))                   ;선택된 entity갯수

      (setq index 0)
      (repeat ss_num                        ;선택된 border 갯수만큼 반복
        (princ "\nDWG NUMBER: ") (princ index)
        (if (= index 0)
          (setq pltn dwgn)
          (progn
            (if (<= (strlen dwgn) 6)
              (setq pltn1 dwgn)
              (setq pltn1 (substr dwgn 1 6))
            ) ;of IF

            (if (<= index 9)
              (setq pltn (strcat pltn1 "0" (itoa index)))
              (setq pltn (strcar pltn1 (itoa index)))
            ) ;of IF
          ) ;of progn
        ) ;of IF
        (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity정보
        (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
        (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
        (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                             (+ (cadr ipnt) (* yg i_scale))))
        (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                             (+ (cadr ipnt) (* bdr_H i_scale))))
    ;    (setvar "CMDDIA" 0)                                     ;command echo OFF
        (if (= index 0)
          (progn
            (princ "\nPLT NUM: ") (princ index) (princ "\n")
            (setq pnts (strcat (rtos (car low_left) 2 3)
                           "," (rtos (cadr low_left) 2 3)
                           " " (rtos (car up_right) 2 3)
                           "," (rtos (cadr up_right) 2 3)))
            (setq scl (strcat "1=" (rtos ds 2 3)))
            (setq scrf (strcat dwgn ".scr"))
            (setq scr (open scrf "w"))
            (write-line "PLOT W" scr)
            (write-line pnts scr)
            (write-line "Y" scr)
            (write-line "N" scr)
            (write-line " " scr)
            (write-line " " scr)
            (write-line " " scr)
            (write-line " " scr)
            (write-line scl scr)
            (write-line pltn scr)
             (princ "\nINDEX = 0")
          ) ;of THEN
          (progn
            (setq pnts (strcat (rtos (car up_left) 2 3)
                           "," (rtos (cadr up_left) 2 3)
                           " " (rtos (car low_right) 2 3)
                           "," (rtos (cadr low_right) 2 3)))
            (setq pltxt (strcat "PLOT W " pnts " N " pltn))       ;change anything
            (write-line pltxt scr)
          ) ;of ELSE
        ) ;of IF
        (setvar "CMDDIA" 1)                                     ;command echo ON
    ;    (princ pltn) (princ " is Plotted") (terpri)
        (setq index (1+ index))                                 ;다음 border로
      ) ;of repeat

      (if (> dwgnum 0)
        (write-line (strcat "OPEN " dwgnext " PLOTA1") scr)
      ) ;of IF
      (close scr)
      (command "SCRIPT" scrf)                                   ;script file선택
    ) ;of THEN
    (progn                                                      ;border가 없을 때
      (princ "\nBORDER NOT FOUND")
      (setq scrf (strcat dwgn ".scr"))
      (setq scr (open scrf "w"))
      (write-line (strcat "OPEN " dwgnext " PLOTA1") scr)
      (close scr)
      (command "SCRIPT" scrf)
    ) ;of ELSE
  ) ;of if
;  (POP-ENV)                                                 ;환경변수 복귀
  (princ)
) ;of defun



