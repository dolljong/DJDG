;**************************************************
; Program : splitdwg
;           split drawing
;           Yi Suk-Jong
;           99/4/2
;**************************************************
; 한개의 파일에 여러개의 도각이 있을 경우 도면 번호
; 또는 도면명등을 이용하여 각각의 도각을 wblock으로
; 여러개의 파일로 만들어준다. 도면명 및 도면 번호는
; 블럭내의 $dwgname, $dwgnum을 이용합니다.
;**************************************************

(defun C:splitdwg( /
;                    ds    dwgn    f_list   ss_lst   ss_num    index
;                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
;                                                              low_right
)

;  (push-env)                                        ;환경변수 대피

  (setq bdr_B 815                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER*"                             ;블럭 이름
        xg    -15                                   ;x gap
        yg    -5                                    ;y gap
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row허용높이

;  (initget "File Plotter")
;  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
;  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))
  
  (setq fname_ll (list 744.7743 2.0925 0)                    ;file name text low left좌표
        fname_ur (list 774.7743 12.0925 0))                   ;file name text up right좌표

;  (initget "Fit Scale")
;  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale: "))


  (setq dwgn (dwg_name))                                ;파일 이름
  (setq dwgnl (strlen dwgn))                            ;파일 이름의 길이
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))    ;filter list
  (setq ss_lst (ssget "X" f_list))                      ;entity 선택
  (setq ss_num (sslength ss_lst))                       ;선택된 entity갯수

  ;------ 선택된 border를 x,y방향으로 sort
;  (setq ssn_lst (sort_xy ss_lst))

  (setq ppoint (ipnt_nblk bdrname "$PP"))         ;print window영역 point
  (setq namepoint (ipnt_nblk bdrname "$dwgname"))         ;name window영역 point
  (setq numppoint (ipnt_nblk bdrname "$dwgnum"))         ;num window
  
;------ border삽입점 x,y값 출력 (test용 source)
;  (setq count 0)
;  (repeat ss_num                                                ;border수만큼 반복
;    (setq ip (cdr (assoc 10 (entget (nth count ssn_lst)))))     ;y좌표잡아내기
;    (princ ip) ;(princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat


;  (setq pltn1 dwgn)                                   ;출력파일은 cad방에 생김
;  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))      ;   "       dwg방에 생김

  ;--------- 첫번째 border부터 출력하기
  (setq index 0)                        ;첫번째 border부터
  (repeat ss_num                        ;선택된 border 갯수만큼 반복
    (setq bdr_ent (entget (ssname ss_lst index)))     ;border entity정보
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor

    (setq txtpnt_ll (mapcar '(lambda (x) (* i_scale x)) fname_ll)) ;text위치를 scale만큼 펑튀기
    (setq txtpnt_ur (mapcar '(lambda (x) (* i_scale x)) fname_ur))

    (setq txtpnt_ll (mapcar '+ ipnt txtpnt_ll))     ;border insert만큼 더하기
    (setq txtpnt_ur (mapcar '+ ipnt txtpnt_ur))

    (setq txt (ssget "W" txtpnt_ll txtpnt_ur))      ;filename text잡아내기

    (setq dwgname (strcat (getvar "DWGPREFIX")      ;full path filename만들기
                          (cdr (assoc 1 (entget (ssname txt 0))))))

;    (if (= fitscl "Scale") (setq fitscl (strcat "1=" (rtos i_scale))))

    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                         (+ (cadr ipnt) (* bdr_H i_scale))))

;    (setvar "CMDDIA" 0)                                     ;command echo OFF
;    (setq pltn0 (strcat pltn ".plt"))

    (princ "\nBorder ")
    (princ index)   (princ ":")
    (princ dwgname)

    (while (findfile (strcat dwgname ".dwg"))
      (progn
        (princ "\nDWG ") (princ dwgname) (princ " --> ")
        (setq dwgname (strcat dwgname "$1"))
        (princ dwgname)
      );progn
    );while

    (command "WBLOCK" dwgname "" ipnt "c" low_left up_right "")

;    (setvar "CMDDIA" 1)                                     ;command echo ON

;    (princ pltn) (princ " is Plotted") (terpri)

    (setq index (1+ index))                                 ;다음 border로
  ) ;of repeat

;  (pop-env)                                                 ;환경변수 복귀

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

;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open 명령으로 파일을 불러올 경우 DWGNAME이 full path명이 되므로
; full path명중 파일명 부분을 추출해냄
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;파일이름 인식
  (setq ls (strlen dn))                                 ;string 길이
  (setq count ls)                                       ;마지막 string부터
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg에서 '.dwg'제거

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
