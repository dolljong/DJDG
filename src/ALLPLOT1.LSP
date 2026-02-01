;****************************************************************************
; Program : ALLPLOT
;           ALL PLOT
;           By Suk-Jong Yi
;           96/6/27
;****************************************************************************
; 도면내에 있는 모든 Border를 출력 (A1용)
; Device  : 명령내리기 전에 미리 선택
; Scale   : Fit / Factor 두가지 옵션
;           (Factor를 선택했을 때 DIMSCALE 변수자동인식)
; plt이름 : Border가 한개인 경우 - DWG name과 같게
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;****************************************************************************

(defun C:ALLPLOT1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

  (setq bdr_B 800                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER*"                             ;블럭 이름
        xg    -5                                    ;x gap
        yg    -5)                                   ;y gap

  (setq ds (getvar "DIMSCALE"))                     ;scale 값구함

  (setq dwgn (dwg_name))                            ;file이름 인식
  (setq dwgnl (strlen dwgn))
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))  ;filter list
  (setq ss_lst (ssget "X" f_list))                          ;entity 선택
  (setq ss_num (sslength ss_lst))                   ;선택된 entity갯수

  (if (> ss_num 9)                                  ;border가 10개가 넘을 경우
    (if (> dwgnl 6)                                 ;파일이름이 6자가 넘을 경우
      (setq pltn1 (substr dwgn 1 6))
      (setq pltn1 dwgn)
    ) ;of if
    (if (> dwgnl 7)                                 ;파일이름이 7자가 넘을 경우
      (setq pltn1 (substr dwgn 1 7))
      (setq pltn1 dwgn)
    ) ;of if
  ) ;of IF

  (setq index 0)
  (repeat ss_num                                    ;선택된 border 갯수만큼 반복
    (princ "\nDWG NUMBER: ") (princ index)
    (if (= index 0)
      (setq pltn dwgn)                              ;첫번째 border인 경우
      (if (and (<= index 9) (> ss_num 9))
        (setq pltn (strcat pltn1 "0" (itoa index))) ;열개이상의 보더중 1~9일때 0을 삽입
        (setq pltn (strcat pltn1 (itoa index)))     ;
      ) ;of IF
    ) ;of IF
    (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity정보
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                         (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))
    (if (findfile pltn0)                                    ; 같은 이름의 plt파일이 존재하면
      (command "DEL" pltn0)                                 ;기존 plt파일 지우기
    ) ;of if
    (if (= index 0)
      (progn
        (princ "\nPLT NUM: ") (princ index) (princ "\n")
        (setq pnts (strcat (rtos (car low_left) 2 3)
                       "," (rtos (cadr low_left) 2 3)
                       " " (rtos (car up_right) 2 3)
                       "," (rtos (cadr up_right) 2 3)))
        (setq scl (strcat "1=" (rtos ds 2 3)))
        (setq scr (open "a1.scr" "w"))
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
        (setq pnts (strcat (rtos (car low_left) 2 3)
                       "," (rtos (cadr low_left) 2 3)
                       " " (rtos (car up_right) 2 3)
                       "," (rtos (cadr up_right) 2 3)))
        (setq pltxt (strcat "PLOT W " pnts " N " pltn))       ;change anything
        (write-line pltxt scr)
      ) ;of ELSE
    ) ;of IF
    (setvar "CMDDIA" 1)                                     ;command echo ON
;    (princ pltn) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;다음 border로
  ) ;of repeat

  (close scr)

  (command "SCRIPT" "A1")
;  (POP-ENV)                                                 ;환경변수 복귀
  (princ)
) ;of defun


;*********************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;*********************************

(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;파일이름 인식
  (setq ls (strlen dn))                                 ;string 길이
  (setq count ls)                                  ;마지막 string부터
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (substr dn (1+ count) (- ls count))
    (substr dn count (- ls (1- count)))
;    (princ (substr dn (1+ count) (- ls count)))
;    (princ (substr dn count (- ls (1- count))))
  ) ;of if

) ;of defun


