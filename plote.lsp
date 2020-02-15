;****************************************************************************
; Program : ALLPLOT
;           ALL PLOT
;           By Suk-Jong Yi
;           1997/12
;****************************************************************************
; ���鳻�� �ִ� ��� Border�� ���
; Device  : ��ɳ����� ���� �̸� ����
; plt�̸� : Border�� �Ѱ��� ��� - DWG name�� ����
;           Border�� �ΰ� �̻��� ��� - DWG���� �̸��� ������ ���ڸ� ��ȣ��
;****************************************************************************

(defun C:plote( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                           low_right
)

  ;(push-env)                                        ;ȯ�溯�� ����

  (setq offset "-5.0,-2.0"
        bdr_B 815                                   ;border�� ��
        bdr_H 570                                   ;border�� ����(���뱸-����)
;        bdr_H 600                                   ;border�� ����(���θ�)
        bn    "BORDER*"                             ;�� �̸�
        xg    -15                                   ;x gap �������μ� $pp�� �������� �۵�(���뱸-����)
        yg     -5                                    ;y gap (���뱸-����)
;        xg    -25                                   ;x gap(���θ�)
;        yg    0                                     ;y gap(���θ�)
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row������

  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (initget "Fit Scale A3")
  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale/<A3>: "))


  (setq pltn (dwg_name))                                ;���� �̸�
  (setq pltn (strcat (getvar "DWGPREFIX") pltn))  

    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))

; r14�� plot routine
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

; 2000�� plot routine
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
;    (if (/= (getvar "PLOTID") "Default System Printer")     ;����Ʈ ������ �������϶� ���
;        (command "N"))                                      ;�ٸ� �������� �� autospool "N"
;    (if (= fplot "Y")                                       ;file�� ����� ��
;        (command "N"))                                      ;Autospool "N"
;
;    (if (= fplot "Y")                                       ;file�� ����Ҷ�
;        (command pltn))
;
;    (command)                                               ;plot��� ����

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)
;    (setq index (1+ index))                                 ;���� border��


;  (pop-env)                                                 ;ȯ�溯�� ����

  (princ)
) ;of defun
;----------------------------------------------------------------
; function SORT_XY
;          Yi Suk Jong
;          97/7/24
;----------------------------------------------------------------
; �־��� entity list�� x,y��ǥ�� �̿��Ͽ� sort�Ѵ�.
; sort�����
;     1. y���� ����� �ͳ��� ���� �����
;     2. ������� x������ sort�Ѵ�.
; �Ѿ���� ��
;      entity list
; �Ѿ�� ��
;      sort�� entity list
;----------------------------------------------------------------
(defun SORT_XY(ss_lst
  / ss_lst ss_num ssn_lst row_col row  cy cn y ygap count1 rown coln
)
  (setq ss_num (sslength ss_lst))              ;list����

  ;------- border��ƼƼ�� list�����
  (setq ssn_lst nil)
  (setq count 0)
  (repeat ss_num
    (setq ssn_lst (append ssn_lst (list (ssname ss_lst count))))
    (setq count (1+ count))
  ) ;of repeat

   ;------- insert y������ ����
  (setq ssn_lst (reverse (sort_ent ssn_lst 10 2)))  ;��������-->������������ ����

  ;------- ��� ���� ������
  (setq row_col nil)                                            ;���list ����
  (setq row nil)                                                ;�� list����
  (setq count 0)
  (setq cy (nth 2 (assoc 10 (entget (nth count ssn_lst)))))     ;���� y��
  (setq cn 0)                                                   ;���� ��ȣ
  (setq count 1)                                                ;ù��° ��Һ���
  (repeat (1- ss_num)
    (setq y (nth 2 (assoc 10 (entget (nth count ssn_lst)))))    ;���� y��
    (setq ygap (abs (- cy y)))                                  ;y����
    (if (> ygap ytol)                        ;y������ border���̸� ������
      (progn
        (setq count1 cn)
        (repeat (- count cn)                 ;row����
          (setq row (append row (list (nth count1 ssn_lst))))
          (setq count1 (1+ count1))
        ) ;of repeat
        (setq row_col (append row_col (list row)))          ;row�� ��Ŀ� �߰�
        (setq cn count)
        (setq cy y)
        (setq row nil)
      ) ;of progn
    ) ;of if
    (setq count (1+ count))                                     ;���� ��ҷ�
  ) ;of repeat
  (setq count1 cn)                                              ;������ rowó��
  (repeat (- ss_num cn)
    (setq row (append row (list (nth count1 ssn_lst))))
    (setq count1 (1+ count1))
  ) ;of repeat
  (setq row_col (append row_col (list row)))

  ; ------------- row���� �������� �ִ� list�� �Ѱ��� list�� ����
  (setq ssn_lst nil)
  (setq rown (length row_col))                          ;row��
  (setq count 0)                                        ;ù�� row����
  (repeat rown
    (setq row (sort_ent (nth count row_col) 10 1))      ;x��ǥ�� sort
    (setq coln (length row))                            ;���� row�� column��
    (setq count1 0)                                     ;ù��° column����
    (repeat coln
      (setq ssn_lst (append ssn_lst (list (nth count1 row))))  ;entity�̸� list�� �߰�
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
; open ������� ������ �ҷ��� ��� DWGNAME�� full path���� �ǹǷ�
; full path���� ���ϸ� �κ��� �����س�
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;�����̸� �ν�
  (setq ls (strlen dn))                                 ;string ����
  (setq count ls)                                       ;������ string����
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg���� '.dwg'����

) ;of defun


;******************************************************
; Function : SORT_ENT
;           SORT ENT
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; SSGET list�� sort���ش�.
; �Ѿ���� ��
;     ALIST : SORT�Ǿ���� SSGET LIST
;       ASS : ������ �Ǵ� sub list (��:insert point --> 10)
;         TH : sub list�� ���° atom�� �������� ������ ���ΰ��� �˷��ش�.
; �Ѱ����� ��
;             SORT�� LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      minv
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq minv (nth th (assoc ass (entget (nth 0 rlist)))))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;���� list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;���� ��
      (if (< c_val minv)                            ;���� ���� min���� ������
        (progn
          (setq min_th count1)                      ;�ּҰ���ġ�� ���� ��ġ��
          (setq minv c_val)                          ;�ּҰ��� ���� ������
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;���� list��
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;�ּҰ��� sort�� list�� �߰�
    (setq rlist (del_atom rlist min_th))            ;����list���� �ּ� list ����
    (setq count (1- count))                         ;�Ѱ� �ٿ���
  ) ;of repeat
;-------------- test�� source ---------------------------------------------
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
; list���� Ư�� atom�� �����
; �Ѿ���� ��
;             b_list : ������ list
;               anth : ����Ǿ��� atom�� ��ġ
; �Ѱܰ��� ��
;                    : ������ list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;��������
)

  (setq nlist (length b_list))                      ;list�� ����

  (setq a_list nil)                                 ;�� list����
  (setq count 0)                                    ;ù��° list����

  (repeat nlist                                     ;list������ŭ �ݺ�
    (if (/= count anth)                             ;������ atom�� �ƴѰ�츸
      (setq a_list (append a_list (list (nth count b_list))))   ;list���� �߰�
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
; ��� : �θ�block�� table data�� ���� �Ƶ��ϵ���
;        ���������� �����ش�. ������ ����Ʈ����, �������,
;        ���麯ȣ���� ���� ã������ �Լ��̴�.
; �Ѿ���� ��
;   pblkname : parent block name ;�θ����̸�
;   sblkname : son block name    ;�Ƶ����̸�
; �Ѿ�� ��
;   ipnt_list : insert point list;�Ƶ����� ������(insert point���� ��ǥ)
; ��)
; (ipnt_nblk "BORDERKK" "$PUL")
;     -> borderkk��� �̸��� block���� $PUL�̶� ����
;        ���Ե� ������ �������ش�.
;     -> ����ڴ� ������ ����� insert point���� ����� scale
;        ���� ���� �� insert����ǥ������ ���ϸ� $PUL���� ������ǥ��
;        ���� �� �ִ�.
;******************************

(defun ipnt_nblk(pblkname sblkname
/ tlbh bname t-1 t-list ipnt_list
)


  
  (setq tblh (tblsearch "BLOCK" pblkname))   ;table data head
  (setq base_point (cdr (assoc 10 tblh)))  ;block�� base point

  (setq t-1 (cdr (assoc -2 tblh)))           ;block�� ù entity��

  (if (= (cdr (assoc 70 tblh)) 0)
    (setq bname sblkname)
    (setq bname (strcat pblkname "|" sblkname))
  );if  
  
  (setq ipnt_list nil)                      ;insert point list�� ����

  (setq t-list (entget t-1))                ;ù��° entity

  (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;           (= (strcase (strcat pblkname "|" sblkname))
           (= (strcase bname)	   
              (cdr (assoc 2 t-list))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
  );if

  (while (setq t-1 (entnext t-1))           ;���� entity
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
