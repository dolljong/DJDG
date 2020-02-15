;******************************************************************
; Program : BCROSS
;           BREAK CROSS
;           Yi Suk-Jong
;           1996/3/22
;*****************************************************************
; ���õ� ��ƼƼ���� �������� �̷���� ���ڸ� �������ش�.
; ���ѻ��� : - ������ ���� �Ǵ� ������ ȣ�� �̷���� ������ �ν�
;            - poly line�νĺҰ�
;*****************************************************************

(defun C:BCROSS(/
cen     count   count1  crs_dst crs_num crs_pnt crs_pnt2    dx      dy
ent     ent1    entype  entype1 ep      ep1     ep2         ipnt1   ipnt2
pnt1    pnt_lst r       snum    snum    sp      sp1         sp2     ssent
ssnum                                                   ;�������� ����
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  
;  (setq oer *error* *error* seterr)

  (push-env)                                            ;ȯ�溯�� ����
  (setq ssent (ssget))                                  ;entity����
  (setq snum (sslength ssent))                          ;entity�� ����

  (setq ipnt1 (getpoint "\nPick base point: "))         ;������ �Է�
  (setq ipnt2 (getpoint ipnt1 "\nPick destination point: "))    ;��ǥ�� �Է�
  (setq dx (- (car ipnt2) (car ipnt1)))                         ;x����
  (setq dy (- (cadr ipnt2) (cadr ipnt1)))                       ;y����

  (setq count 0)                                        ;ù��° entity����
  (repeat snum                                          ;entity������ŭ �ݺ�
    (setq ent (entget (ssname ssent count)))            ;���ؼ�����
    (setq entype (cdr (assoc 0 ent)))                   ;entity type
    (cond
      ((= entype "LINE")                                ;���ؼ��� line�� ���
        (setq sp (cdr (assoc 10 ent))                   ;���ؼ��� ������
              ep (cdr (assoc 11 ent)))                  ;���ؼ��� ����
      )
      ((= entype "ARC")                                 ;ȣ�� ���
        (progn
          (setq cen (cdr (assoc 10 ent)))               ;ȣ�� �߽�
          (setq r   (cdr (assoc 40 ent)))               ;ȣ�� �ݰ�
          (setq sp (polar cen (cdr (assoc 50 ent)) r))  ;ȣ�� ������
          (setq ep (polar cen (cdr (assoc 51 ent)) r))  ;ȣ�� ����
        ) ;of progn
      ) ;of entype=arc
    ) ;of cond

    (setq sp2 (list (+ (car sp) dx) (+ (cadr sp) dy) 0.0))  ;��ǥ��ȯ

    (setq pnt_lst (list (append sp2 (list 0.0))))           ;���� list

    (setq count1 0)                                         ;������ �˻�
    (repeat snum                                            ;entity������ŭ �ݺ�
      (if (/= count count1)                     ;���ؼ��� ���� entity�� �� skip
        (progn
          (setq ent1 (entget (ssname ssent count1)))    ;entity����
          (setq entype1 (cdr (assoc 0 ent1)))           ;entity type

          (cond
            ((and (= entype "LINE") (= entype1 "LINE")) ;������ ����
              (setq sp1 (cdr (assoc 10 ent1))           ;������
                    ep1 (cdr (assoc 11 ent1)))          ;����
              (setq crs_pnt (inters sp ep sp1 ep1))     ;�μ��� ����
            ) ;of entype=LINE entype1=LINE

            ((and (= entype "ARC") (= entype1 "LINE"))  ;ȣ�� ����
              (setq sp1 (cdr (assoc 10 ent1))           ;������
                    ep1 (cdr (assoc 11 ent1)))          ;����
              (setq crs_pnt (cross ent sp1 ep1))        ;entity�� ARC�϶�
            ) ;of entype=ARC entype1=LINE

            ((and (= entype "LINE") (= entype1 "ARC"))
              (setq crs_pnt (cross ent1 sp ep))
            ) ;of entype=LINE entype1=ARC

          ) ;of cond

          (if (/= crs_pnt nil)                          ;������ �����Ҷ���
            (progn
              (setq crs_dst (distance sp crs_pnt))      ;���������� �������� �Ÿ�
              (setq crs_pnt2 (list (+ (car crs_pnt) dx) ;��ǥ��ȯ
                                   (+ (cadr crs_pnt) dy) 0.0))
              (setq pnt_lst (append pnt_lst             ;����list,�Ÿ� list �����
                                    (list (append crs_pnt2 (list crs_dst)))))
            ) ;of PROGN
          ) ;of IF
        ) ;of PROGN
      ) ;of IF
      (setq count1 (1+ count1))                             ;���� entity��
    ) ;of REPEAT
    (setq ep2 (list (+ (car ep) dx) (+ (cadr ep) dy) 0.0))  ;��ǥ��ȯ
    (setq pnt_lst (append pnt_lst                           ;�������� �߰�
                          (list (append ep2 (list (distance sp ep))))))
    (setq pnt_lst (sort_list pnt_lst 3))                ;������� sort
    (setq crs_num (length pnt_lst))                     ;������ ����
    (setq pnt1 (del_atom (nth 0 pnt_lst) 3))            ;�Ÿ� atom �����
    (command "LINE" pnt1)                               ;line�׸��� ����(ù��)
    (setq count1 1)
    (repeat (1- crs_num)                                ;�ι�°���� ����������
      (setq pnt1 (del_atom (nth count1 pnt_lst) 3))     ;point���ϱ�
      (command pnt1)                                    ;line�׸���
      (setq count1 (1+ count1))                         ;���� ������
    ) ;of repeat
    (command "")
    (setq count (1+ count))                             ;���� entity��
  ) ;of REPEAT                                          ;������ entity����

  (pop-env)                                             ;ȯ�溯�� ����
  (setq *error* oer seterr nil)
  (princ)

) ;of defun CRSL


;******************************************************
; Function : SORT_LIST
;           SORT LIST
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; list�� sort���ش�.
; �Ѿ���� ��
;     ALIST : SORT�Ǿ���� LIST
;      AIDX : ������ �Ǵ� sub list (ù sub list = 0)
; �Ѱ����� ��
;             SORT�� LIST
;******************************************************

(defun SORT_LIST(alist aidx
/       alist       nl       rlist       slist        count      mini
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq mini (nth aidx (nth 0 rlist)))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;���� list
      (setq c_val (nth aidx (nth count1 rlist)))    ;���� ��
      (if (< c_val mini)                             ;���� ���� min���� ������
        (progn
          (setq min_th count1)                      ;�ּҰ���ġ�� ���� ��ġ��
          (setq mini c_val)                          ;�ּҰ��� ���� ������
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;���� list��
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;�ּҰ��� sort�� list�� �߰�
    (setq rlist (del_atom rlist min_th))            ;����list���� �ּ� list ����
    (setq count (1- count))                         ;�Ѱ� �ٿ���
  ) ;of repeat
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


