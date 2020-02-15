;*******************************
; Program : MTXT
;           Modify TeXT
;           Yi Suk-Jong
;           01/12/7
;*******************************
; ���������� text�� �������ش�.(����,�������� ���ð���)
;*******************************

(defun C:MTXT(/
                txtformat          startnum       stepnum      ans       num         
                numtxt        msg        esel       irepeat    txtent
                startpoint           
                count      ent       index       ipnt      ntxt  
                numss   sslst      tlst             txt       txtim
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;ȯ�溯������
 
  (setq txtformat (getstringold oldtxtformat "\nEnter text format"))  ;text format�Է�
  (setq oldtxtformat txtformat)
 
  (setq startnum (getintold oldstartnum "\nEnter Start Number")) ;���۹�ȣ �Է�
  (setq oldstartnum startnum)

  (setq stepnum (getintold oldstepnum "\nEnter Step Number")) ;�����Է�
  (setq oldstepnum stepnum)
  
  (initget "Individual Selected")                           ;�Ѱ��� Ȥ�� �Ѳ����� ����
  (setq ans (getkword "\nIndividual/Selected <I>: "))
  (if (= ans nil) (setq ans "Individual"))
  (cond
    ((= ans "Individual")				;�Ѱ��� ������ ��
      (setq num startnum)
      (setq numtxt (str_subst (itoa num) "#" txtformat))
      (setq msg (strcat "\n[" numtxt "]Pick Text or [Format(" txtformat
			")/Start(" (itoa startnum)
			")/sTep(" (itoa stepnum)
			")]: "))
      (initget "Format Start sTep")
      (setq esel (entsel msg))
      (setq irepeat 1)
      (while (/= esel nil)
        (cond
	  ((= esel "Format")
            (setq txtformat (getstringold oldtxtformat "\nEnter text format"))
            (setq oldtxtformat txtformat)
	  );subcond
	  ((= esel "Start")
            (setq startnum (getintold oldstartnum "\nEnter Start Number"))
            (setq oldstartnum startnum)
	    (setq irepeat 0)
	  );subcond
	  ((= esel "sTep")
            (setq stepnum (getintold oldstepnum "\nEnter Step Number"))
            (setq oldstepnum stepnum)
	    (setq startnum num
	          irepeat 0)
  	  );subcond
	  (T
	    (setq txtent (entget (car esel)))
            (entmod (subst (cons 1 numtxt) (assoc 1 txtent) txtent))
	  );subcond
        );cond
        (setq num (+ startnum (* irepeat stepnum)))
	(setq irepeat (1+ irepeat))
        (setq numtxt (str_subst (itoa num) "#" txtformat))
        (setq msg (strcat "\n[" numtxt "]Pick Text or [Format(" txtformat
  			  ")/Start(" (itoa startnum)
			  ")/sTep(" (itoa stepnum)
			  ")]: "))
        (initget "Format Start sTep")     
        (setq esel (entsel msg))
      );endwhile
    );subcond
    ((= ans "Selected")						;�Ѳ����������ϴ°��
      (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity�� ����
      (setq startpoint (getpoint "\nPick Start point: "))
      (setq numss (sslength sslst))                         ;ss-list�� ����
      (princ numss)
      (princ " text found\n")                               ;���õ� text����

      (setq count 0                                         ;ù text����
            tlst nil)

      (repeat numss                                             ;text������ŭ
        (setq ipnt (assoc 10 (entget (ssname sslst count))))    ;������
        (setq txtim (cons (ssname sslst count) (list (distance startpoint (cdr ipnt)))))           ;��ƼƼ�̸��� ������������ �Ÿ��� list����
        (setq tlst (append tlst (list txtim)))                  ;text list�� �߰�
        (setq count (1+ count))                                 ;���� text��
      ) ;of repeat

      (setq tlst (sort_list tlst 1)) ;����� ������ ����
     
      (setq irepeat 0)
      (repeat numss						;Text����
        (setq num (+ startnum (* irepeat stepnum)))
;	(setq irepeat (1+ irepeat))
        (setq numtxt (str_subst (itoa num) "#" txtformat))
	
        (setq txtent (entget (car (nth irepeat  tlst))))
	
        (entmod (subst (cons 1 numtxt) (assoc 1 txtent) txtent))
	(setq irepeat (1+ irepeat))
      );repeat
    );subcond 
  );cond
  (pop-env)                                             ;ȯ�溯�� ����

  (setq *error* oer seterr nil)
  (princ)  
);defun



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
/       alist       nl       rlist       slist        count      minv
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq minv (nth aidx (nth 0 rlist)))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;���� list
      (setq c_val (nth aidx (nth count1 rlist)))    ;���� ��
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


  