;****************************************************************************
; Program : ALLPLOT
;           ALL PLOT
;           By Suk-Jong Yi
;           1997/12
;****************************************************************************
; ���e���A ���e ���e Border�i �b
; Device  : �w�w������ ��A ���� ��Ȃ
; plt���q : Border�a �e���� �w�� - DWG name�� �{�A
;           Border�a ���� ���w�� �w�� - DWG�a�� ���q�� �a���b ���a�i ��ѡ��
;****************************************************************************

(defun C:PLOTA3( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

;  (push-env)                                        ;�ŉw�e�� ��ϡ

  (setq bdr_B 815                                   ;border�� ͢
        bdr_H 570                                   ;border�� ����
        bn    "BORDER*"                             ;�i�� ���q
        xg    -15                                   ;x gap
        yg    -5                                    ;y gap
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row��w����

  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter"))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (PRINC "\NFPLOT: ") (PRINC FPLOT)

  (initget "Fit Scale")
  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale: "))


  (setq dwgn (dwg_name))                                ;�a�� ���q
  (setq dwgnl (strlen dwgn))                            ;�a�� ���q�� ����
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))    ;filter list
  (setq ss_lst (ssget "X" f_list))                      ;entity ��Ȃ
  (setq ss_num (sslength ss_lst))                       ;��Ȃ�E entity����

  ;------ ��Ȃ�E border�i x,y�wз�a�� sort
  (setq ssn_lst (sort_xy ss_lst))


;------ border�s���� x,y�t �b (test�w source)
;  (setq count 0)
;  (repeat ss_num                                                ;border���e�q �e��
;    (setq ip (cdr (assoc 10 (entget (nth count ssn_lst)))))     ;y���a�s�a����
;    (princ ip) ;(princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat

  ;-------- plt�a�� ���q ���a��(R12�w)
;  (if (> ss_num 9)                                  ;border�a 10���a ��i �w��
;    (if (> dwgnl 6)                                   ;�a�����q�� 6�a�a ��i �w��
;      (setq pltn1 (substr dwgn 1 6))
;      (setq pltn1 dwgn)
;    ) ;of if
;    (if (> dwgnl 7)                                   ;�a�����q�� 7�a�a ��i �w��
;      (setq pltn1 (substr dwgn 1 7))
;      (setq pltn1 dwgn)
;    ) ;of if
;  ) ;of IF

;  (setq pltn1 dwgn)                                   ;�b�a���e cad�w�A ����
;  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))      ;   "       dwg�w�A ����
  (setq pltn1 dwgn)                                   ;�b�a���e cad�w�A ����

  ;--------- ���弁 border���� �b�a��
  (setq index 0)                        ;���弁 border����
  (repeat ss_num                        ;��Ȃ�E border �����e�q �e��
    (if (= index 0)
;      (setq pltn dwgn)
      (setq pltn pltn1)
      (if (and (<= index 9) (> ss_num 9))
        (setq pltn (strcat pltn1 "0" (itoa index)))
        (setq pltn (strcat pltn1 (itoa index)))
      ) ;of IF
    ) ;of IF
    (setq bdr_ent (entget (nth index ssn_lst)))     ;border entity����
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border�� scale factor
    (if (= fitscl "Scale") (setq fitscl (strcat "1=" (rtos i_scale))))
    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border�� ���b �a��
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border�� ���b ��
                         (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
;    (setq pltn0 (strcat pltn ".plt"))
;    (if (findfile pltn0)                                    ; �{�e ���q�� plt�a���� �����a�e
;      (command "DEL" pltn0)                                 ;���� plt�a�� ������
;    ) ;of if
;    (command "PLOT" "W" low_left up_right "N" pltn)  ;plot�w�w  r12�� �w��

;    (if (= (getvar "PLOTID") "Default System Printer")      ;plot�w�w r14�� �w��
;      (command "PLOT" "W" low_left up_right "0"  pltn)      ;default system printer�� �w�� spool�a�� ���ᥡ�� �g�q
;      (command "PLOT" "W" low_left up_right "0" "N" pltn)   ;���e �a���ᷥ �w��
;    ) ;of if

    (PRINC "\NPLT NAME") (PRINC PLTN)
    (command "PLOT" "W" low_left up_right
             "5"  ;Enter choice, 0-5 <0>:
             "N"  ;Do you want to change plotters?
             "N"  ;ADD ^Z
             "N"  ;ADD ^D
             "N"  ;Do you want to change any of the above parameters?
            fplot ;Write the plot to a file?
             "M"  ;Size units (Inches or Millimeters)
             ""   ;plot origin in Millimeters
             ""   ;Enter the Size or Width,Height (in millimeters)
             ""   ;Rotate plot clockwise 0/90/180/270 degrees
             ""   ;Adjust area boundaris for pen width
             ""   ;Remove hidden lines?
             fitscl ;Plotter Millimeters=Drawing units or Fit or ?
             "0"   ;Enter choice, 0-5

    (if (/= (getvar "PLOTID") "Default System Printer")
        "N")

    (if (= fplot "Y")
        pltn))

;    (command)

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;�a�q border��
  ) ;of repeat

;  (pop-env)                                                 ;�ŉw�e�� ����

  (princ)
) ;of defun

;----------------------------------------------------------------
; function SORT_XY
;          Yi Suk Jong
;          97/7/24
;----------------------------------------------------------------
; ���ụ entity list�i x,y���a�i ���w�a�a sort�e�a.
; sort�w��e
;     1. y�t�� ���u�e ������ З�i �e�e�a
;     2. �bЗ�i�e x�t�a�� sort�e�a.
; ��ᵡ�e �t
;      entity list
; ���a�e �t
;      sort�E entity list
;----------------------------------------------------------------
(defun SORT_XY(ss_lst
/ ss_lst  ss_num  ssn_lst  row_col  row  cy  cn  y  ygap  ytol
  count1  rown    coln
)
  (setq ss_num (sslength ss_lst))              ;list����

  ;------- border�Eˡˡ�w list�e�i��
  (setq ssn_lst nil)
  (setq count 0)
  (repeat ss_num
    (setq ssn_lst (append ssn_lst (list (ssname ss_lst count))))
    (setq count (1+ count))
  ) ;of repeat

   ;------- insert y�t�a�� ���i
  (setq ssn_lst (reverse (sort_ent ssn_lst 10 2)))  ;���q�a��-->�����a���a�� �e�w

  ;------- З�� �i�� �a����
  (setq row_col nil)                                            ;З�ilist ������
  (setq row nil)                                                ;З list������
  (setq count 0)
  (setq cy (nth 2 (assoc 10 (entget (nth count ssn_lst)))))     ;�e�� y�t
  (setq cn 0)                                                   ;�e�� ��ѡ
  (setq count 1)                                                ;���弁 �a������
  (repeat (1- ss_num)
    (setq y (nth 2 (assoc 10 (entget (nth count ssn_lst)))))    ;�e�� y�t
    (setq ygap (abs (- cy y)))                                  ;y�t�a
    (if (> ygap ytol)                        ;y�t�a�a border�����i ��i��
      (progn
        (setq count1 cn)
        (repeat (- count cn)                 ;row�w��
          (setq row (append row (list (nth count1 ssn_lst))))
          (setq count1 (1+ count1))
        ) ;of repeat
        (setq row_col (append row_col (list row)))          ;row�i З�i�A �a
        (setq cn count)
        (setq cy y)
        (setq row nil)
      ) ;of progn
    ) ;of if
    (setq count (1+ count))                                     ;�a�q �a����
  ) ;of repeat
  (setq count1 cn)                                              ;�a���b row�១
  (repeat (- ss_num cn)
    (setq row (append row (list (nth count1 ssn_lst))))
    (setq count1 (1+ count1))
  ) ;of repeat
  (setq row_col (append row_col (list row)))

  ; ------------- row�i�� �a����a ���e list�i �e���� list�� ɷ�s
  (setq ssn_lst nil)
  (setq rown (length row_col))                          ;row��
  (setq count 0)                                        ;���� row����
  (repeat rown
    (setq row (sort_ent (nth count row_col) 10 1))      ;x���a�� sort
    (setq coln (length row))                            ;�e�� row�� column��
    (setq count1 0)                                     ;���弁 column����
    (repeat coln
      (setq ssn_lst (append ssn_lst (list (nth count1 row))))  ;entity���q list�A �a
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
; open �w�w�a�� �a���i ���ᵩ �w�� DWGNAME�� full path�w�� �A�a��
; full path�w�� �a���w �����i Ё��
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;�a�����q ����
  (setq ls (strlen dn))                                 ;string ����
  (setq count ls)                                       ;�a���b string����
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg�A�� '.dwg'�A��

) ;of defun


;******************************************************
; Function : SORT_ENT
;           SORT ENT
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; SSGET list�i sortЁ���a.
; ��ᵡ�e �t
;     ALIST : SORT�A�ᴡ�i SSGET LIST
;       ASS : ������ �A�e sub list (��:insert point --> 10)
         TH : sub list�� �y�弁 atom�i �����a�� ���i�i �����a�i �i�a���a.
; ��a���e �t
;             SORT�E LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�E list�e�q
  (setq rlist alist)                        ;�A���t�i �e �a�ỡ list

  (setq count nl)                           ;list �������� �e���� ���e�� �e��

  (repeat nl                                        ;list�����e�q
    (setq min (nth th (assoc ass (entget (nth 0 rlist)))))             ;���弁 list�i �b�e �t�a��
    (setq min_th 0)                                 ;�A���t�� ��á�i ��q�a��
    (setq count1 1)                                 ;���弁 list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;�e�� list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;�e�� �t
      (if (< c_val min)                            ;�e�� �t�� min���a �b�i��
        (progn
          (setq min_th count1)                      ;�A���t��á�i �e�� ��á��
          (setq min c_val)                          ;�A���t�i �e�� �t�a��
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;�a�q list��
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;�A���t�i sort�E list�A �a
    (setq rlist (del_atom rlist min_th))            ;�q�elist�A�� �A�� list �A��
    (setq count (1- count))                         ;�e�� ���a��
  ) ;of repeat
;-------------- test�w source ---------------------------------------------
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
; list�A�� �b�� atom�i �����a
; ��ᵡ�e �t
;             b_list : �� list
;               anth : �A���i atom�� ��á
; ��a�a�e �t
;                    : ҁ list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;���b�e��
)

  (setq nlist (length b_list))                      ;list�� ����

  (setq a_list nil)                                 ;�� list����
  (setq count 0)                                    ;���弁 list����

  (repeat nlist                                     ;list�����e�q �e��
    (if (/= count anth)                             ;�����E atom�� �a���w���e
      (setq a_list (append a_list (list (nth count b_list))))   ;list�A�a �a
    ) ;of if
    (setq count (1+ count))
  ) ;of repeat

  (setq a_list a_list)

) ;of defun