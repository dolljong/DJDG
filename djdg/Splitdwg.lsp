;**************************************************
; Program : splitdwg
;           split drawing
;           Yi Suk-Jong
;           99/4/2
;**************************************************
; �Ѱ��� ���Ͽ� �������� ������ ���� ��� ���� ��ȣ
; �Ǵ� ��������� �̿��Ͽ� ������ ������ wblock����
; �������� ���Ϸ� ������ش�. ����� �� ���� ��ȣ��
; �������� $dwgname, $dwgnum�� �̿��մϴ�.
;**************************************************

(defun C:splitdwg( /
;                    ds    dwgn    f_list   ss_lst   ss_num    index
;                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
;                                                              low_right
)

;  (push-env)                                        ;ȯ�溯�� ����

  (setq bdr_B 815                                   ;border�� ��
        bdr_H 570                                   ;border�� ����
        bn    "BORDER*"                             ;���� �̸�
        xg    -15                                   ;x gap
        yg    -5                                    ;y gap
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row������

;  (initget "File Plotter")
;  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
;  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))
  
  (setq fname_ll (list 744.7743 2.0925 0)                    ;file name text low left��ǥ
        fname_ur (list 774.7743 12.0925 0))                   ;file name text up right��ǥ

;  (initget "Fit Scale")
;  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale: "))


  (setq dwgn (dwg_name))                                ;���� �̸�
  (setq dwgnl (strlen dwgn))                            ;���� �̸��� ����
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))    ;filter list
  (setq ss_lst (ssget "X" f_list))                      ;entity ����
  (setq ss_num (sslength ss_lst))                       ;���õ� entity����

  ;------ ���õ� border�� x,y�������� sort
;  (setq ssn_lst (sort_xy ss_lst))

  (setq ppoint (ipnt_nblk bdrname "$PP"))         ;print window���� point
  (setq namepoint (ipnt_nblk bdrname "$dwgname"))         ;name window���� point
  (setq numppoint (ipnt_nblk bdrname "$dwgnum"))         ;num window
  
;------ border������ x,y�� ��� (test�� source)
;  (setq count 0)
;  (repeat ss_num                                                ;border����ŭ �ݺ�
;    (setq ip (cdr (assoc 10 (entget (nth count ssn_lst)))))     ;y��ǥ��Ƴ���
;    (princ ip) ;(princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat


;  (setq pltn1 dwgn)                                   ;��������� cad�濡 ����
;  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))      ;   "       dwg�濡 ����

  ;--------- ù��° border���� ����ϱ�
  (setq index 0)                        ;ù��° border����
  (repeat ss_num                        ;���õ� border ������ŭ �ݺ�
    (setq bdr_ent (entget (ssname ss_lst index)))     ;border entity����
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border�� scale factor

    (setq txtpnt_ll (mapcar '(lambda (x) (* i_scale x)) fname_ll)) ;text��ġ�� scale��ŭ ��Ƣ��
    (setq txtpnt_ur (mapcar '(lambda (x) (* i_scale x)) fname_ur))

    (setq txtpnt_ll (mapcar '+ ipnt txtpnt_ll))     ;border insert��ŭ ���ϱ�
    (setq txtpnt_ur (mapcar '+ ipnt txtpnt_ur))

    (setq txt (ssget "W" txtpnt_ll txtpnt_ur))      ;filename text��Ƴ���

    (setq dwgname (strcat (getvar "DWGPREFIX")      ;full path filename�����
                          (cdr (assoc 1 (entget (ssname txt 0))))))

;    (if (= fitscl "Scale") (setq fitscl (strcat "1=" (rtos i_scale))))

    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border�� ���� �Ʒ�
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border�� ���� ��
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

    (setq index (1+ index))                                 ;���� border��
  ) ;of repeat

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
/ ss_lst  ss_num  ssn_lst  row_col  row  cy  cn  y  ygap  ytol
  count1  rown    coln
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
  (setq ssn_lst ssn_lst)
) ;of defun

;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open �������� ������ �ҷ��� ��� DWGNAME�� full path���� �ǹǷ�
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
         TH : sub list�� ���° atom�� �������� ������ ���ΰ��� �˷��ش�.
; �Ѱ����� ��
;             SORT�� LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq min (nth th (assoc ass (entget (nth 0 rlist)))))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;���� list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;���� ��
      (if (< c_val min)                            ;���� ���� min���� ������
        (progn
          (setq min_th count1)                      ;�ּҰ���ġ�� ���� ��ġ��
          (setq min c_val)                          ;�ּҰ��� ���� ������
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