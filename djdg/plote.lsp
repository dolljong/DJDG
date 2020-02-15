;****************************************************************************
; Program : ALLPLOT
;           ALL PLOT
;           By Suk-Jong Yi
;           1997/12
;****************************************************************************
; 도면내에 있는 모든 Border를 출력
; Device  : 명령내리기 전에 미리 선택
; plt이름 : Border가 한개인 경우 - DWG name과 같게
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;****************************************************************************

(defun C:plote( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                           low_right
)

  ;(push-env)                                        ;환경변수 대피

  (setq offset "-5.0,-2.0"
        bdr_B 815                                   ;border의 폭
        bdr_H 570                                   ;border의 높이(동대구-경주)
;        bdr_H 600                                   ;border의 높이(수인리)
        bn    "BORDER*"                             ;블럭 이름
        xg    -15                                   ;x gap 여백으로서 $pp가 없을때만 작동(동대구-경주)
        yg     -5                                    ;y gap (동대구-경주)
;        xg    -25                                   ;x gap(수인리)
;        yg    0                                     ;y gap(수인리)
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row허용높이

  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (initget "Fit Scale A3")
  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale/<A3>: "))


  (setq pltn (dwg_name))                                ;파일 이름
  (setq pltn (strcat (getvar "DWGPREFIX") pltn))  

    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))

; r14용 plot routine
;    (command "PLOT" "W" low_left up_right
;             "5"  ;Enter choice, 0-5 <0>:
;             "N"  ;Do you want to change plotters?
;             "N"  ;Do you want to change any of the above parameters?
;            fplot ;Write the plot to a file?
;             "M"  ;Size units (Inches or Millimeters)
;             ""   ;plot origin in Millimeters
;             ""   ;Enter the Size or Width,Height (in millimeters)
;             ""   ;Rotate plot clockwise 0/90/180/270 degrees
;             ""   ;Remove hidden lines?
;           fitscl ;Plotter Millimeters=Drawing units or Fit or ?
;            "0"   );Enter choice, 0-5

; 2000용 plot routine
;    (setq ctr "center")
    ;command
(command "PLOT"
"y"                 ;Detailed plot configuration? [Yes/No] <No>: y
""                  ;Enter a layout name or [?] <Model>:
"PostScript Level 2.pc3"
;"HP LaserJet 4V"   ;Enter an output device name or [?] <HP LaserJet 4V>:
;"HP LaserJet 5000LE PCL 6"
;"A3 297 x 420 mm"   ;Enter paper size or [?] <A3 297 x 420 mm>:
;"A3 (297 x 420 mm)"
;"A4 210 x 297 mm"
;"ISO A4 (210.00 x 297.00 MM)"  
"ISO A4 (297.00 x 210.00 MM)"
"m"                 ;Enter paper units [Inches/Millimeters] <Inches>: m
"Portrait"
;"Landscape"
;	     "Landscape"         ;Enter drawing orientation [Portrait/Landscape] <Landscape>:
"n"                 ;Plot upside down? [Yes/No] <No>:
;"w"                 ;Enter plot area [Display/Extents/Limits/View/Window] <Display>: w
"E"
;low_left            ;Enter lower left corner of window <0.000000,0.000000>:
;up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
;fitscl              ;Enter plot scale (Plotted Millimeters=Drawing Units) or [Fit] <Fit>:
"F"
;offset	     
"CENTER"
;"-5.0,-2.0"            ;Enter plot offset (x,y) or [Center] <0.00,0.00>: center
"Y"                ;Plot with plot styles? [Yes/No] <Yes>:
"acad.ctb"          ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:
"Y"               ;Plot with lineweights? [Yes/No] <Yes>:
"N"                ;Remove hidden lines? [Yes/No] <No>:
fplot               ;Write the plot to a file [Yes/No] <N>:
);command	      
(if (= fplot "Y") (command pltn))  ;(Enter file name <C:\Program Files\ACAD2000\djdg\sample>:

(command  "N")                 ;Save changes to model tab [Yes/No]? <N>
(command "y")                 ;Proceed with plot [Yes/No] <Y>: y

;(command "line"
;	 low_left            ;Enter lower left corner of window <0.000000,0.000000>:
;up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
;"")
;    (if (/= (getvar "PLOTID") "Default System Printer")     ;디폴트 씨스템 프린터일때 통과
;        (command "N"))                                      ;다른 프린터일 때 autospool "N"
;    (if (= fplot "Y")                                       ;file로 출력할 때
;        (command "N"))                                      ;Autospool "N"
;
;    (if (= fplot "Y")                                       ;file로 출력할때
;        (command pltn))
;
;    (command)                                               ;plot명령 끝냄

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)
;    (setq index (1+ index))                                 ;다음 border로


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
  / ss_lst ss_num ssn_lst row_col row  cy cn y ygap count1 rown coln
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
;  (princ "\nin subroutine : ")
;  (princ ytol)
;   (princ ssn_lst)
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
;         TH : sub list의 몇번째 atom을 기준으로 정렬할 것인가를 알려준다.
; 넘겨지는 값
;             SORT된 LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      minv
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq minv (nth th (assoc ass (entget (nth 0 rlist)))))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;현재 list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;현재 값
      (if (< c_val minv)                            ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq minv c_val)                          ;최소값을 현재 값으로
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

;********************************************
; function : ipnt_nblk
;            insert point of nested block
;            Yi Suk-Jong
;            1999/7/15
;********************************************
; 기능 : 부모block의 table data를 뒤져 아들블록들의
;        삽입점들을 돌려준다. 도각의 프린트영역, 도면명영역,
;        도면변호영역 등을 찾기위한 함수이다.
; 넘어오는 값
;   pblkname : parent block name ;부모블록이름
;   sblkname : son block name    ;아들블록이름
; 넘어가는 값
;   ipnt_list : insert point list;아들블록의 삽입점(insert point기준 좌표)
; 예)
; (ipnt_nblk "BORDERKK" "$PUL")
;     -> borderkk라는 이름의 block안의 $PUL이란 블럭이
;        삽입된 점들을 리턴해준다.
;     -> 사용자는 위에서 얻어진 insert point값에 블록의 scale
;        값을 곱한 후 insert점좌표값에다 더하면 $PUL들의 절대좌표를
;        구할 수 있다.
;******************************

(defun ipnt_nblk(pblkname sblkname
/ tlbh bname t-1 t-list ipnt_list
)


  
  (setq tblh (tblsearch "BLOCK" pblkname))   ;table data head
  (setq base_point (cdr (assoc 10 tblh)))  ;block의 base point

  (setq t-1 (cdr (assoc -2 tblh)))           ;block내 첫 entity명

  (if (= (cdr (assoc 70 tblh)) 0)
    (setq bname sblkname)
    (setq bname (strcat pblkname "|" sblkname))
  );if  
  
  (setq ipnt_list nil)                      ;insert point list를 만듦

  (setq t-list (entget t-1))                ;첫번째 entity

  (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;           (= (strcase (strcat pblkname "|" sblkname))
           (= (strcase bname)	   
              (cdr (assoc 2 t-list))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
  );if

  (while (setq t-1 (entnext t-1))           ;다음 entity
    (setq t-list (entget t-1))
    (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;             (= (strcase (strcat pblkname "|" sblkname))
             (= (strcase bname)
	     (strcase (cdr (assoc 2 t-list)))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
    );if

  );while

  (setq ipnt_list ipnt_list)                    ;Return insert point list

);defun
