;************************************************
; Program : DDMCHT
;           Dialog Multi CHange Text
;           Yi Suk-Jong
;           96/5/8
;************************************************
; ���������� text�� �������ش�. (Dialog Box�̿�)
;************************************************

(defun C:DDMCHT(/                                  ;�������� ����
                ans      count     index      ipnt       numss
                sslst    tlst      txt        txtim
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;ȯ�溯�� ����

  (initget "Horizontal Vertical")                           ;�������⼳��
  (setq ans (getkword "\nHorizontal/Vertical <V>: "))
  (if (= ans nil) (setq ans "Vertical"))                    ;default=��������
  (if (= ans "Vertical")
    (setq index 3)
    (setq index 2)
  ) ;of IF

  (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity�� ����
  (setq numss (sslength sslst))                         ;ss-list�� ����
  (princ numss)
  (princ " text found\n")                               ;���õ� text����

  (setq count 0                                         ;ù text����
        tlst nil)

  (repeat numss                                             ;text������ŭ
    (setq ipnt (assoc 10 (entget (ssname sslst count))))    ;������
    (setq txtim (cons (ssname sslst count) ipnt))           ;��ƼƼ�̸��� ���������� list����
    (setq tlst (append tlst (list txtim)))                  ;text list�� �߰�
    (setq count (1+ count))                                 ;���� text��
  ) ;of repeat

  (if (= ans "Vertical")                                    ;���������� ��� y���� ��������
    (setq tlst (reverse (sort_list tlst index)))            ;��������� ��� x���� ��������
    (setq tlst (sort_list tlst index))
  ) ;of IF

  (setq count 0)
  (repeat numss
    (setq ent (entget (nth 0 (nth count tlst))))        ;text entity����
    (setq txt (cdr (assoc 1 ent)))                      ;text����
    (redraw (nth 0 (nth count tlst)) 3)                 ;text����
    (NT_DIALOG txt)                                     ; Dialog Box���Ͽ� text�Է�
    (if (/= new_txt "")                                    ;��text ������ ��������
      (entmod (subst (cons 1 new_txt) (assoc 1 ent) ent))) ;text����
    (redraw (nth 0 (nth count tlst)) 4)                 ;������ entity����
    (setq count (1+ count))                             ;���� text��
  ) ;of repeat

  (pop-env)                                             ;ȯ�溯�� ����

  (setq *error* oer seterr nil)
  (princ)
) ;of defun


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

;;;
;;; new text�Է� ��ƾ
;;;
(defun NT_DIALOG ( old_txt / dcl_id old_txt)

  (setq dcl_id (load_dialog "DJDG"))
  (if (not (new_dialog "txtedit" dcl_id)) (exit))

  (set_tile "text_edit" old_txt)

  (action_tile "text_edit" "(set_txt)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(set_txt)")
  (mode_tile "text_edit" 2)
  (start_dialog)
  (unload_dialog dcl_id)

) ;of defun NT_DIALOG

(defun SET_TXT( / in )
  (setq in (get_tile "text_edit"))
  (setq new_txt in)
) ; of defun SET_TXT

