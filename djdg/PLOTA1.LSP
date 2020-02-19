;****************************************************************************
; Program : PLOTA1
;           PLOTA1
;           By Suk-Jong Yi
;           1995/11/3
;****************************************************************************
; 도면내에 있는 모든 Border를 출력 (HP GL/2용)
; Device  : 명령내리기 전에 미리 선택되어있어야 함
; Scale   : DIMSCALE변수값을 자동으로 인식한다.
; plt이름 : Border가 한개인 경우 - DWG name과 같게 출력
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;                                       바꿔서 출력
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
  (setq opf (open "PLOTA1.LST" "r"))                          ;file open
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
  (setq opf (open "PLOTA1.LST" "w"))
  (setq count 1)
  (repeat (1- dwgnum)
    (write-line (nth count dwglst) opf)
    (setq count (1+ count))
  ) ;of repeat
  (close opf)

  (princ dwglst)

  (setq ds (getvar "DIMSCALE"))                     ;scale 값구함

  (setq dwgn (getvar "DWGNAME"))
  (setq dwgnl (strlen dwgn))                        ;파일 이름의 길이
  (setq dwgnext (car dwglst))                       ;다음파일
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))  ;filter list
  (setq ss_lst (ssget "X" f_list))                          ;entity 선택



  (if (/= ss_lst nil)                                       ;border가 있을 때만
    (progn
      ;---------- border를 x,y방향으로 sort
      (setq ssn_lst (sort_xy ss_lst))
      (setq ss_num (sslength ss_lst))                   ;선택된 entity갯수
      (if (> ss_num 9)                                  ;border가 10개가 넘을 경우
        (if (> dwgnl 6)                                 ;파일이름이 6자가 넘을 경우
          (setq pltn1 (substr dwgn 1 6))
          (setq pltn1 dwgn)
        ) ;of if
        (if (> dwgnl 7)                                   ;파일이름이 7자가 넘을 경우
          (setq pltn1 (substr dwgn 1 7))
          (setq pltn1 dwgn)
        ) ;of if
      ) ;of IF

      (setq index 0)
      (repeat ss_num                        ;선택된 border 갯수만큼 반복
;        (princ "\nDWG NUMBER: ") (princ index)
        (if (= index 0)
          (setq pltn dwgn)
          (if (and (<= index 9) (> ss_num 9))
            (setq pltn (strcat pltn1 "0" (itoa index)))
            (setq pltn (strcat pltn1 (itoa index)))
          ) ;of IF
        ) ;of IF
        (setq bdr_ent (entget (nth index ssn_lst)))     ;border entity정보
;        (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity정보
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

;----------------------------------------------------------------
; function SORT_XY
;          Yi Suk Jong
;          97/7/24
;----------------------------------------------------------------
; 주어진 entity list를 x,y좌표를 이용하여 sort한다.
; sort방법은
;     1. y값이 비슷한 것끼리 행을 만든다
;     2. 각행들은 x값으로 sort한다.
; 넘어오는 값
;      entity list
; 넘어가는 값
;      sort된 entity list
;----------------------------------------------------------------
(defun SORT_XY(ss_lst
/ ss_lst  ss_num  ssn_lst  row_col  row  cy  cn  y  ygap  ytol
  count1  rown    coln
)
  (setq ss_num (sslength ss_lst))              ;list갯수

  ;------- border엔티티명 list만들기
  (setq ssn_lst nil)
  (setq count 0)
  (repeat ss_num
    (setq ssn_lst (append ssn_lst (list (ssname ss_lst count))))
    (setq count (1+ count))
  ) ;of repeat

   ;------- insert y값으로 정렬
  (setq ssn_lst (reverse (sort_ent ssn_lst 10 2)))  ;오름차순-->내림차순으로 변경

  ;------- 행과 열로 나누기
  (setq row_col nil)                                            ;행렬list 비우기
  (setq row nil)                                                ;행 list비우기
  (setq count 0)
  (setq cy (nth 2 (assoc 10 (entget (nth count ssn_lst)))))     ;현재 y값
  (setq cn 0)                                                   ;현재 번호
  (setq count 1)                                                ;첫번째 요소부터
  (repeat (1- ss_num)
    (setq y (nth 2 (assoc 10 (entget (nth count ssn_lst)))))    ;현재 y값
    (setq ygap (abs (- cy y)))                                  ;y값차
    (if (> ygap ytol)                        ;y값차가 border높이를 넘을때
      (progn
        (setq count1 cn)
        (repeat (- count cn)                 ;row형성
          (setq row (append row (list (nth count1 ssn_lst))))
          (setq count1 (1+ count1))
        ) ;of repeat
        (setq row_col (append row_col (list row)))          ;row를 행렬에 추가
        (setq cn count)
        (setq cy y)
        (setq row nil)
      ) ;of progn
    ) ;of if
    (setq count (1+ count))                                     ;다음 요소로
  ) ;of repeat
  (setq count1 cn)                                              ;마지막 row처리
  (repeat (- ss_num cn)
    (setq row (append row (list (nth count1 ssn_lst))))
    (setq count1 (1+ count1))
  ) ;of repeat
  (setq row_col (append row_col (list row)))

  ; ------------- row별로 나누어져 있는 list를 한개의 list로 통합
  (setq ssn_lst nil)
  (setq rown (length row_col))                          ;row수
  (setq count 0)                                        ;첫번 row부터
  (repeat rown
    (setq row (sort_ent (nth count row_col) 10 1))      ;x좌표로 sort
    (setq coln (length row))                            ;현재 row의 column수
    (setq count1 0)                                     ;첫번째 column부터
    (repeat coln
      (setq ssn_lst (append ssn_lst (list (nth count1 row))))  ;entity이름 list에 추가
      (setq count1 (1+ count1))
    ) ;of repeat
    (setq count (1+ count))
  ) ;of repeat
  (setq ssn_lst ssn_lst)
) ;of defun

;******************************************************
; Function : SORT_ENT
;           SORT ENT
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; SSGET list를 sort해준다.
; 넘어오는 값
;     ALIST : SORT되어야할 SSGET LIST
;       ASS : 기준이 되는 sub list (예:insert point --> 10)
         TH : sub list의 몇번째 atom을 기준으로 정렬할 것인가를 알려준다.
; 넘겨지는 값
;             SORT된 LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq min (nth th (assoc ass (entget (nth 0 rlist)))))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;현재 list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;현재 값
      (if (< c_val min)                            ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq min c_val)                          ;최소값을 현재 값으로
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;다음 list로
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;최소값을 sort된 list에 추가
    (setq rlist (del_atom rlist min_th))            ;남은list에서 최소 list 제거
    (setq count (1- count))                         ;한개 줄여서
  ) ;of repeat
;-------------- test용 source ---------------------------------------------
;  (setq count 0)
;  (repeat nl
;    (princ (nth th (assoc ass (entget (nth count slist))))) (princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat
;--------------------------------------------------------------------------
  (setq slist slist)
) ;of defun


;************************************************
; Function : DEL_ATOM
;           DELete ATOM
;           Yi Suk-Jong
;           1996/2/23
;************************************************
; list에서 특정 atom을 지운다
; 넘어오는 값
;             b_list : 축출전 list
;               anth : 축출되야할 atom의 위치
; 넘겨가는 값
;                    : 축출후 list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;지역변수
)

  (setq nlist (length b_list))                      ;list의 갯수

  (setq a_list nil)                                 ;빈 list생성
  (setq count 0)                                    ;첫번째 list부터

  (repeat nlist                                     ;list갯수만큼 반복
    (if (/= count anth)                             ;지정된 atom이 아닌경우만
      (setq a_list (append a_list (list (nth count b_list))))   ;list에다 추가
    ) ;of if
    (setq count (1+ count))
  ) ;of repeat

  (setq a_list a_list)

) ;of defun


