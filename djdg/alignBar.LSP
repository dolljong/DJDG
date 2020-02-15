;**************************************************
; Program : ALIGNBAR
;           Align BAR detail
;           Yi Suk-Jong(dolljong@dreamwiz.com)
;           01/01/26
;**************************************************
; bar detail ����
; 
;**************************************************

(defun C:alignBAR( 
  / ssc ipnt ygap ix iy nc clist cent cc cx cy rc p1 p2 ss  llist lent lpnt1 lpnt2 ssbox ipnt2 )

  (defun SETERR(s)					;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)			;���忡����ƾ �⵿

  (push-env)                                ;ȯ�溯�� ����


  (setq ssc (ssget '((0 . "CIRCLE"))))      ;circle���� �Է�
  (setq ipnt (getpoint  "\nSpecify insert point: "))  ;pick insert point
  (setq ygap (getdist ipnt "\nEnter Y-gap: "))
  (setq ix (car ipnt)       ;x
	iy (cadr ipnt))     ;y
	
  (setq nc (sslength ssc))                  ;circle�� ����
  (setq clist (reverse (sort_byassoc ssc 10 2)))      ;make circle list that is sorted by y-coord
;  (setq llist nil)                          ;�� line-list ����

  (setq count 0)
  (repeat nc                                            ;circle������ŭ �ݺ�
    (setq cent (entget (nth count clist)))             ;circle����
    (setq cc (cdr (assoc 10 cent))
	  cx (car cc)
	  cy (cadr cc))                     ;center ��ǥ
    (setq rc (cdr (assoc 40 cent)))                     ;radius (��)
    (setq p1 (list (- cx rc) (+ cy rc) 0.0)
	  p2 (list (+ cx rc) (- cy (* 2 rc)) 0.0)
          ss (ssget "C" p1 p2 '((0 . "LINE"))))                ; MARKING/LINE ��Ƴ���

    (setq lent (entget (ssname ss 0))
          lpnt1 (cdr (assoc 10 lent))
	  lpnt2 (cdr (assoc 11 lent)))
    (if (< (car lpnt1) (car lpnt2))
      (setq p2 (list (car lpnt2) (- cy (* 2 rc)) 0.0))
      (setq p2 (list (car lpnt1) (- cy (* 2 rc)) 0.0))
    );if  
    (setq ssbox (ssget "c" p1 p2))
    (setq ipnt2 (list ix (- iy (* count ygap)) 0.0))
    (command "copy" ssbox "" cc ipnt2)
    (setq count (1+ count))                             ;���� circle��
  ) ;of repeat


  (pop-env)                                             ;ȯ�溯�� ����

  (princ "\nNormal terminated")                         ;���������� ���� ǥ��
  (setq *error* oer seterr nil)
  (princ)

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


;******************************************************
; Function : SORT_byassoc
;           SORT by assoc
;           Yi Suk-Jong(dolljong@dreamwiz.com
;           2001/01/26
;******************************************************
; list�� sort���ش�.
; �Ѿ���� ��
;     ALIST : SORT�Ǿ���� LIST, (ssget)���·� �Ѿ��
;      ass  : ������ �� assoc index
;      AIDX : ������ �Ǵ� sub list (ex: (10 1.0 2.0 3.0) 2.0-->2 
; �Ѱ����� ��
;             SORT�� entity LIST
;******************************************************
; ex: (sort_byassoc (ssget) 10 2) sort by y-coord
(defun SORT_BYASSOC(ss ass aidx
  /       ssn  count alist nl slist rlist minv min_th c_list c_val count1 )
  
  (setq ssn (sslength ss))
  (setq count 0
	alist nil)
  (repeat ssn
    (setq alist (append alist (list (ssname ss count)))) 
    (setq count (1+ count))
  );repeat

  
  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq minv (nth aidx (assoc ass (entget (nth 0 rlist)))))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;���� list
      (setq c_val (nth aidx (assoc ass (entget c_list))))    ;���� ��
      (if (< c_val minv)                             ;���� ���� min���� ������
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
  (setq slist slist)
) ;of defun