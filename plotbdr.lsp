;****************************************************************************
; Program : PLOTBDR
;           PLOT BorDeR
;           By Suk-Jong Yi
;           1997/1/9,98/7/1
;****************************************************************************
; 2004/11/18
;  2002에서 사용가능하도록 수정
;-----------------------------------
; 도면내에 있는 모든 (BLOCK이름이 BDR$로 시작하는)Border를 출력해준다.
; Border  : block으로 되어있어야 하며
;           plot시 window로 찍어줄 두 점이 point로 되어있어야 하며
;           block의 이름은 BDR$로 시작하여야 한다.
;           (MKBDR.LSP이용 가능)
; Device  : 명령내리기 전에 미리 선택
; Scale   : 명령내리기 전에 미리 선택1
; plt이름 : DWG파일 이름 + 0, 1, 2, 3....
;****************************************************************************

(defun C:PLOTBDR( /
                    dwgn    dwgnl   f_list   ss_lst   ss_num   index
                    pltn    pltn1   bdr_ent  bdr_nm   ipnt     i_scale
                    dxy     dx      dy       pnt2     plotn0

)                                          ;지역변수정의

  
 (if (not printer_name)
   (progn
     (setq fn (strcat (prefix) "djdg/allplot.set"))
     (setq opf (open fn "r"))                           ;open file
     (setq printer_name (read-line opf))
     (setq printer_papersize (read-line opf))
     (setq style_name (read-line opf))
     (setq offset (read-line opf))
     (close opf) 
   );progn
 );if  


  (setq dwgn (dwg_name))                                    ;파일 이름
  (setq dwgnl (strlen dwgn))                                ;파일 이름의 길이

  (initget "All Select")
  (setq ans (getkword "\nAll/<Select>: "))
  (cond
    ((or (= ans nil) (= ans "Select"))
      (setq ss_lst (ssget))
    ) ;of ans-nil
    ((= ans "All")
      (setq f_list (list (cons 0 "INSERT") (cons 2 "BDR*")))    ;filter list
      (princ f_list)
      (setq ss_lst (ssget "X" f_list))                          ;entity 선택
    ) ;of ans=All
  ) ;of cond

  (setq ss_num (sslength ss_lst))               ;선택된 entity갯수

  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))

  (setq index 0)                        ;첫번째 border부터
  (repeat ss_num                        ;선택된 border 갯수만큼 반복
    (setq pltn (strcat pltn1 (itoa index)))         ;dwg file명에 숫자붙이기
    (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity정보
    (setq bdr_nm (cdr (assoc 2 bdr_ent)))           ;border이름
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
    (setq dxy (bdr_size bdr_nm)                     ;border size 알아냄
          dx (car dxy)                              ;x크기
          dy (cadr dxy))                            ;y크기
    (setq pnt2 (list (+ (car ipnt) dx)              ;border의 우측 아랫점
                     (+ (cadr ipnt) dy)))
    (setvar "CMDDIA" 0)                             ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))

;    (command "PLOT" "W" ipnt pnt2 "N") ;Enter choice, 0-5 <0>:


(setq fitscl "Fit"
      fplot  "No")
    
; 2000용 plot routine
;    (setq ctr "center")
    ;command

(command "PLOT"
"y"                 ;Detailed plot configuration? [Yes/No] <No>: y
""                  ;Enter a layout name or [?] <Model>:
printer_name        ;Enter an output device name or [?] <HP LaserJet 4V>:	 
;"HP LaserJet 4V"   ;Enter an output device name or [?] <HP LaserJet 4V>:
;"HP LaserJet 5000LE PCL 6"
;"HP LaserJet 5100 PCL 6"
printer_papersize   ;Enter paper size or [?] <A3 297 x 420 mm>:	 
;"A3 297 x 420 mm"  ;Enter paper size or [?] <A3 297 x 420 mm>:
;"A3 (297 x 420 mm)"
;"A3"
"m"                 ;Enter paper units [Inches/Millimeters] <Inches>: m
"Landscape"
;	     "Landscape"         ;Enter drawing orientation [Portrait/Landscape] <Landscape>:
"n"                 ;Plot upside down? [Yes/No] <No>:
"w"                 ;Enter plot area [Display/Extents/Limits/View/Window] <Display>: w
ipnt            ;Enter lower left corner of window <0.000000,0.000000>:
pnt2            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
fitscl              ;Enter plot scale (Plotted Millimeters=Drawing Units) or [Fit] <Fit>:
offset	     
;"-5.0,-2.0"            ;Enter plot offset (x,y) or [Center] <0.00,0.00>: center
"Y"                ;Plot with plot styles? [Yes/No] <Yes>:
style_name         ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:	 
;"acad.ctb"          ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:
"Y"               ;Plot with lineweights? [Yes/No] <Yes>:
"N"                ;Remove hidden lines? [Yes/No] <No>:
fplot               ;Write the plot to a file [Yes/No] <N>:
);command	      
(if (= fplot "Y") (command pltn))  ;(Enter file name <C:\Program Files\ACAD2000\djdg\sample>:

(command  "N")                 ;Save changes to model tab [Yes/No]? <N>
(command "y")                 ;Proceed with plot [Yes/No] <Y>: y




    

;    (if (/= (getvar "PLOTID") "Default System Printer")     ;디폴트 씨스템 프린터일때 통과
;        (command "N"))                                      ;다른 프린터일 때 autospool "N"

;    (command pltn0)                                          ;plt name입력

    (setvar "CMDDIA" 1)                         ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri) ;출력 메시지
    (setq index (1+ index))                     ;다음 border로

  ) ;of repeat

  (princ)
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
  (setq count ls)                                  ;마지막 string부터
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (substr dn (1+ count) (- ls count))
    (substr dn count (- ls (1- count)))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg에서 '.dwg'제거

) ;of defun

;****************************************************************
; function BDR_SIZE
;          BorDeR SIZE
;          Yi Suk-Jong
;          97/1/9
;****************************************************************
; BLOCK table을 검색하여 border BLOCK의 크기를 찾아준다.
; 넘어오는 값
;   BDR_NM : BorDeR NaMe (크기를 알고 싶은 border이름)
; 넘어가는 값
;            X, Y 크기 (삽입점으로부터 떨어진 거리)
;****************************************************************

(defun BDR_SIZE( BDR_NM / head ent1 pnt1 ent2 pnt2)
  (setq head (tblsearch "BLOCK" bdr_nm))        ;block table 찾음
  (setq ent1 (cdr (assoc -2 head))
        pnt1 (cdr (assoc 10 (entget ent1))))    ;첫번째 point 좌표
  (setq ent2 (entnext ent1)
        pnt2 (cdr (assoc 10 (entget ent2))))    ;두번째 point 좌표
  (list (- (car pnt2) (car pnt1))               ;x차이
        (- (cadr pnt2) (cadr pnt1)))            ;y차이
) ;of defun

