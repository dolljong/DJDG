(defun c:testsort()
  (setq ytol 517)
  (setq kk (sort_xy (ssget)))
  (setq c 0
	str "")
  (repeat (length kk)
    (setq ename (nth c kk))
    (setq str (strcat str (cdr (assoc 1 (entget ename)))))
    (setq c (1+ c))			     
  );repeat			   
  (princ str)
);testsort  

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
; / ss_lst  ss_num  ssn_lst  row_col  row  cy  cn  y  ygap  ytol
;  count1  rown    coln
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