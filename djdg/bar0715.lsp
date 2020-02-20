;**************************************************
; Program : BAR
;           BAR
;           Yi Suk-Jong
;           96/9/4
;**************************************************
; ö�ٻ󼼵����� ö�����ǥ�� data�� ������ش�.
; �Ǽ� ���� �ν��ϵ��� ��ħ 98/7/11
; Dialog box�� ���� �ɼ� �����ϵ��� ���� 01/12/06
;**************************************************

(defun C:BAR( /
             sblist bblist 
	     scl ans ssc nc llist count cent cc rc fn ipnt ans34)

  (defun SETERR(s)                                      ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                     ;���忡����ƾ �⵿

  (push-env)                                ;ȯ�溯�� ����

  (bardlg)

;  (alert (rtos #roundup))

;  (exit)
  
  (setq scl (getvar "DIMSCALE"))            ;scale�� ��Ƴ���
;  (if (= #tr34 "1")
;    (setq ans34 "Yes")
;    (setq ans34 "No")
;  );if
  (setq ans "Table")
;;;  (initget "File Table All")
;;;  (setq ans (getkword "\nFile/Table/All<All>: "))
;;;
;;;  (initget "Yes No")
;;;  (setq ans34 (getkword "\n3��4���� �����Ͻðڽ��ϱ�?<Yes>: "))
;;;  (if (= ans34 nil) (setq ans34 "Yes"))
;;;
;;; 
;;;  (if (= ans34 "Yes")
;;;    (princ "\n3��4���� �����Ͽ����ϴ�.")
;;;    (princ "\n3��4���� �������� �ʾҽ��ϴ�.")
;;;  );if
;;;
;;;  (initget "Yes No")
;;;  (setq d22add (getkword "\nAdd 6% for D22? <No>: "))
;;;  (if (= d22add nil) (setq d22add "No"))

  (setq ssc (ssget '((0 . "CIRCLE"))))      ;circle���� �Է�
  (setq nc (sslength ssc))                  ;circle�� ����

  (setq llist nil)                          ;�� line-list ����

  (setq count 0)
  (repeat nc                                            ;circle������ŭ �ݺ�
    (setq cent (entget (ssname ssc count)))             ;circle����
    (setq cc (cdr (assoc 10 cent)))                     ;center ��ǥ
    (setq rc (cdr (assoc 40 cent)))                     ;radius (������)
    (setq llist (append llist (list (b_mark cc rc #roundup))))   ;Marking ã��
    (setq count (1+ count))                             ;���� circle��
  ) ;of repeat

  (cond
    ((= ans "File")
      (progn
        (setq fn (getfiled "Open data file" "" "dat" 1))      ;file�̸� �Է¹���
        (write_data llist fn)                                 ;���Ϸ� �����
      ) ;of progn
    ) ;of sub_cond
    ((= ans "Table")
      (if (= #addg "1")
	(progn
          (setq spblist (split_blist llist 22))
          (setq bblist (car spblist))
          (setq sblist (cadr spblist))
          (if bblist
            (progn
              (setq ipnt (getpoint "\nInsert point(D22~D32): "))             ;������ �Է¹���
              (bar_list bblist #addb ipnt scl)                             ;bar list�׸���
            );progn
          );if
          (if sblist
            (progn
              (setq ipnt (getpoint "\nInsert point(D10~D19): "))             ;������ �Է¹���
              (bar_list sblist #adds ipnt scl)                             ;bar list�׸���
            );progn
          );if
        );progn
        (progn
          (setq ipnt (getpoint "\nInsert point: "))             ;������ �Է¹���
          (bar_list llist #addratio ipnt scl)                             ;bar list�׸���
        ) ;of progn
      );if
    ) ;of sub_cond
    ((= ans "All")
      (progn
        (setq fn (getfiled "Open data file" "" "dat" 1))      ;file�̸� �Է¹���
        (write_data llist fn)                                 ;file�� �����
        (setq ipnt (getpoint "\nInsert point: "))             ;������ �Է¹���
        (bar_list llist ipnt scl)                             ;bar list�׸���
      ) ;of progn
    ) ;of sub_cond
  ) ;of cond

  (pop-env)                                             ;ȯ�溯�� ����

  (princ "\nNormal terminated")                         ;���������� ���� ǥ��
  (setq *error* oer seterr nil)
  (princ)

) ;of defun



;**************************************************
; Function : B_MARK
;            Bar MARKing
;            Yi Suk-Jong
;            96/9/5
;**************************************************
; ö�� �󼼵����� macking(ö�ٹ�ȣ)�� ö�ٵ���Ÿ�� ã�� ����
; ������ �ǵ����ش�. (������ ���� ������ ��ܰ� ������ ����)
; �Ѿ���� ��:
;      IP : Insert point (���� �߽�)
;       R : Radius       (���� ������)
;     A34 : 3��4�� ����(Yes/No)
; �Ѿ�� ��:
;     MARK : ö�� ��ŷ
;      DIA : ö���� ����
;        L : ö���� ����(L,L',A.V.L�� ���ÿ� ������ ��� ���� ū ������)
;        N : ö���� ����
;**************************************************

(defun B_MARK(IP R A34 /
  A34 ix iy p1 p2 p3 ss ssn count tlst sent etype l1 l2 end_p nt
  txt1 txt2 MARK LD LD1 AVL L txt txtl N maxl
  )

  (setq ix (car IP)                         ;������ x
        iy (cadr IP)                        ;       y
        p1 (list (- ix R) (+ iy R))         ;
        p2 (list (+ ix R) (- iy R))         ;
        p3 (list (+ ix R) (+ iy R))         ;
        ss (ssget "C" p1 p2)                ; MARKING/LINE ��Ƴ���
        ssn (sslength ss)
        count 0
        tlst nil)

  (cond
    ((= A34 4) (setq roundv 0.0035))
    ((= A34 5) (setq roundv 0.0045))
  );cond  
    
  (repeat ssn                                 ;��ƼƼ ������ŭ �ݺ�
    (setq sent (entget (ssname ss count)))
    (setq etype (cdr (assoc 0 sent)))
    (cond
      ((= etype "TEXT")                       ;��ƼƼ�� �ؽ�Ʈ�� ���
        (progn
           (setq tlst (append tlst (list (cdr (assoc 1 sent))))) ;�ؽ�Ʈ ����Ʈ�� �߰�
        ) ;of progn
      ) ;of etype=TEXT
      ((= etype "LINE")                       ;��ƼƼ�� ������ ���
        (progn
          (setq l1 (cdr (assoc 10 sent))          ;ù��
                l2 (cdr (assoc 11 sent)))         ;����
          (if (> (car l1) (car l2))             ;����ã��
            (setq end_p l1)
            (setq end_p l2)
          ) ;of if
        ) ;of progn
      ) ;of etype=LINE
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat                                  ;���� ��ƼƼ��

  (setq nt (length tlst))                       ;text����
  (if (= nt 2)                                  ;text������ �ΰ��� (��: A1-1)
    (progn
      (setq txt1 (car (sp-trunc (nth 0 tlst)))
            txt2 (car (sp-trunc (nth 1 tlst))))
      (if (= (substr txt1 1 1) "-")               ;-�� ���� ���� �ڷ��ؼ� ���ϱ�
        (setq MARK (strcat txt2 txt1))
        (setq MARK (strcat txt1 txt2))
      ) ;of if
    ) ;of progn
    (setq MARK (nth 0 tlst))                     ;text������ �Ѱ��� (��: A2)
  ) ;of if

  ;---------------------------
  ; DIA , ����, ���� ��Ƴ���
  ;---------------------------

  (setq ss (ssget "C" p3 end_p '(( 0 . "TEXT"))))
  (setq ssn (sslength ss)
        count          0
        DIA            ""
        LD             0.0
        LD1            0.0
        AVL            0.0
        L              0.0
        N                0)

  (repeat ssn
    (setq txt (car (sp-trunc (cdr (assoc 1 (entget (ssname ss count))))))  ;�ؽ�Ʈ ����
          txtl                                        (strlen txt))        ;�ؽ�Ʈ ����

    (cond
      ((or (= (setq txt3 (substr txt 1 3)) "D13")
           (= txt3 "D10") (= txt3 "D16")  (= txt3 "D19") (= txt3 "D22")
           (= txt3 "D25") (= txt3 "D29") (= txt3 "D32")
           (= txt3 "H10") (= txt3 "H13")  (= txt3 "H16") (= txt3 "H19")
           (= txt3 "H22") (= txt3 "H25") (= txt3 "H29")
           (= txt3 "H32"))                                      ; DIA
        (setq DIA TXT)
      ) ;of sub_cond
      ((= (substr txt 1 2) "L=")                                ; L=
        (setq L (atof1 (substr txt 3 (- txtl 2))))
      ) ;of sub_cond
      ((= (substr txt 1 3) "L'=")                               ; L'=
        (setq LD (atof1 (substr txt 4 (- txtl 3))))
      ) ;of sub_cond
      ((= (substr txt 1 3) "L`=")                               ; L`=
        (setq LD1 (atof1 (substr txt 4 (- txtl 3))))
      ) ;of sub_cond
      ((= (substr txt 1 6) "A.V.L=")                            ;A.V.L=
        (setq AVL (atof1 (substr txt 7 (- txtl 6))))
      ) ;of sub_cond
      ((= (substr txt 1 2) "N=")                                ;N=
;        (setq N (atoi (substr txt 3 (- txtl 2))))                     ;�����ιٲ�
        (setq N (atof (substr txt 3 (- txtl 2))))                     ;�����ιٲ�
      ) ;of sub_cond
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat

  (setq maxl L)
  (if (> LD maxl) (setq maxl LD))
  (if (> LD1 maxl) (setq maxl LD1))             ; L, L'�� ���� ū �� ���
  (if (> AVL 0.0) (setq maxl AVL))              ;A.V.L�� ������ A.V.L �� ���̷�

  (if (= maxl 0.0) (alert (strcat "<" MARK ">" " LENGTH NOT FOUND")))
  (if (= DIA "") (alert (strcat "<" MARK ">" " DIA NOT FOUND")))
  (if (= N 0) (alert (strcat "<" MARK ">" " NUMBER NOT FOUND")))

  (if (> A34 0)                                 ;3��4���� ���뿩��
    (progn
      (setq rm (rem maxl 0.01))                     ; 3�� 4��
      (if (>= rm roundv)                            ;�������� 3.5MM�̻��� ��
        (setq L (+ maxl (- 0.01 rm)))                  ;�ø�
        (setq L (- maxl rm))                        ;�������� 3.5MM������ ��
      ) ;of if                                         ;����
    );progn
    (setq L maxl)
  );if

  (list MARK DIA L N)

) ;of defun


;**************************************
; Function : ATOF1
;            ATOF-1 (STRING --> REAL)
;            Yi Suk-Jong
;            96/9/5
;**************************************

(defun atof1(STR / n count dot)
  (setq     n (strlen str)
        count            1
        dot              0)
  (repeat n
    (if (= (substr str count 1) ".") (setq DOT count))
    (setq count (1+ count))
  ) ;of repeat
  (if (= dot 0) (* 0.001 (atof str))
                (atof str))
) ;of defun


;**************************************
; Function : ATOI1
;            ATOI-1 (STRING --> INTEGER)
;            Yi Suk-Jong
;            96/9/5
;**************************************
;
(defun ATOI1( STR / n count str chr)
  (setq     n (strlen str)
        count            1)
  (while (/= (setq chr (substr str count 1)) "")
    (if (= chr ",")
      (setq str (strcat (substr str 1 (1- count))
                        (substr str (1+ count) (- n count)))
              n (1- n))
    ) ;of if
    (setq count (1+ count))
  ) ;of while
  (atoi str)
) ;of defun


;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; �� �Լ��� ,�� �Ҹ��� data�� ������ �Ѱ��� list�� �����ش�.
; �̶� ����ȯ ���� ��� data�� ���ڿ��� return�ȴ�.
;******************************************************************

(defun DATA-IN(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;�Ѿ�� ���ڿ�
   (setq strl (strlen arg1))                    ;�Ѿ�� ���ڿ��� ����
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;������� ��ġ
   (setq nchr 1)                                ;���⹮�� ����
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;���� �Ѱ�
      (if (or (= subs ",") (= subs ""))         ;���� ���ڰ� ,�̰ų� ���϶�
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;������ġ����
            (if (= rslt nil)
               (setq rslt (list lst))                  ;�������� �������
               (setq rslt (append rslt (list lst)))    ;���������� �߰�
            ) ;of if
            (setq nchr 0)                       ;���ⰹ�� �ٽ� 0����
            (setq strt (1+ count))              ;���� ��������� �������ڷ�
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;���� ���ڷ�
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;���� ���� �Ѱ� ����
   ) ;of repeat
   (setq arg1 rslt)                             ;������ ����
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; �Է¹��ڿ��� ��,�ڿ� �ִ� ��ĭ�� ©�󳽴�.
; ���ϰ���
; (©�� ���ڿ�,
;  ù ���� ������ ��ġ,
;  ������ ���� ������ ��ġ,
;  �����ΰ�?)
;***************************************************************************

(defun SP-TRUNC(txt
/               txtl        frntn       backn       txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if

);of defun


;************************************
; Function : IS-NUM
;            IS NUMber ?
;            By Suk-Jong Yi
;            1996/2/23
;************************************
; ���ڿ��� �����ΰ�?�� �Ǵ����ش�.
;************************************

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (= ch 44)
             (= ch 46)
             (and (= count 1) (= ch 43))
             (and (= count 1) (= ch 45))
         )
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

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
/       alist       nl       rlist       slist        count      min1
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq min1 (nth aidx (nth 0 rlist)))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;���� list
      (setq c_val (nth aidx (nth count1 rlist)))    ;���� ��
      (if (lt c_val min1)                             ;���� ���� min���� ������
        (progn
          (setq min_th count1)                      ;�ּҰ���ġ�� ���� ��ġ��
          (setq min1 c_val)                          ;�ּҰ��� ���� ������
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
; Function : LT
;           Less Then
;           Yi Suk-Jong
;           1996/2/27
;************************************************
; �μ�-1�� �μ�-2���� �������� �Ǵ����ش�.
; �Ѿ���� ��:
;       ARG1 : �μ�-1
;       ARG2 : �μ�-2
; �Ѿ�� ��:
;       T    : �μ�-1�� �μ�-2���� ���� ��
;       nil  : �μ�-1�� �μ�-2���� ���� ������
;************************************************

(defun LT1(arg1 arg2)
  (if (and (is-num arg1) (is-num arg2))
    (< (atof arg1) (atof arg2))
    (< arg1 arg2)
  ) ;of if
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


;*******************************************
; Function : BAR_LIST
;            draw BAR LIST
;            Yi Suk-Jong
;            1996/2/26
;*******************************************
; �־��� bar data�� bar list�� �׸�
; �Ѿ���� ��:
;      BLIST : bar data
;      IPNT  : insert point
;      S     : Scale factor
;*******************************************

(defun BAR_LIST(blist add ipnt s
/
       add     ipnt    s
       bd      bdx     bl      blist    blt     blx     bn      bnx     cdia
       cl      count   dcount  fdia     gtw     gtwa    hy      ix      iy
       lh      ln      ly      nb       nbx     ndia    oldc    sbcount sbn
       slist   stl     th      tl       tlist   tlx     tw      twa     twat
       twax    twt     twx     uwx      vy1     vy1     vy2     y       add_factor
	hh
)


  (setq th (getvar "DIMTXT"))                                 ;���� ũ��
  (setq lh 8)                                   ;�� line�� ����
  (setq hh 10)                                  ; header line����
  
;;;  (setq add (getreal "\n������<3%>: "))
;;;  (if (= add nil)                               ;�����Է½� �������� 3%
;;;    (setq add_factor 1.03)
    (setq add_factor (+ 1 (/ add 100)))
;;;  );if

  (setq th (* th s))                            ;����ũ�� scale���
  (setq lh (* lh s))                            ;�ٳ��� scale���
  (setq hh (* hh s))                            ; header line���� scale���

  (setq ln (length blist))                      ;ö�� data ����

  (setq blist (reverse (sort_list blist 1)))    ;ö�� ������ �������� sort

  (setq fdia (nth 1 (nth 0 blist)))             ;ù ö�� ����

  (setq ix   (car ipnt)
        iy   (cadr ipnt)
        bnx  (+ ix (* 10.0 s))
        bdx  (+ ix (* 30.0 s))
        blx  (+ ix (* 68 s))
        nbx  (+ ix (* 88 s))
        tlx  (+ ix (* 118 s))
        uwx  (+ ix (* 138 s))
        twx  (+ ix (* 168 s))
        twax (+ ix (* 198 s)))

  (setq slist nil                               ;sub list
        tlist nil)                              ;total list

  (setq tl   0.0                                ;���� ö�� ���� ��
        tw   0.0)                               ;�� �߷�

  (setq count 0)                                ;�ι�° ö�ٺ���
  (repeat ln                                    ;������ �ݺ�
    (setq cdia (nth 1 (nth count blist)))       ;������ ö������
    (if (= fdia cdia)                           ;���������� ��������� ���� ��
      (setq slist (append slist (list (nth count blist))))
      (progn
        (setq tlist (append tlist (list slist)))
        (setq slist (list (nth count blist)))
        (setq fdia cdia)
      ) ;of progn ELSE
    ) ;of if
    (setq count (1+ count))                         ;���� ö������
  ) ;of repeat

  (setq tlist (append tlist (list slist)))          ;������ ö�ٹ��� �߰�

;  (setq hy (- iy (/ lh 2.0)))                 ;Header�� y��ġ
  (setq hy (- iy (/ hh 2.0)))                 ;Header�� y��ġ
  (command "TEXT" "M" (list (+ ix (* 100.0 s)) (+ iy (* th 5)))
                  (* 6.0 s) "0.0" "ö �� �� �� ǥ")
  (command "TEXT" "M" (list (+ ix (*  10.0 S)) hy) th "0.0" "�� ȣ")
  (command "TEXT" "M" (list (+ ix (*  30.0 S)) hy) th "0.0" "�� ��")
  (command "TEXT" "M" (list (+ ix (*  55.0 S)) hy) th "0.0" "��    ��")
  (command "TEXT" "M" (list (+ ix (*  80.0 S)) hy) th "0.0" "�� ��")
  (command "TEXT" "M" (list (+ ix (* 105.0 S)) hy) th "0.0" "��  ��  ��")
  (command "TEXT" "M" (list (+ ix (* 130.0 S)) hy) th "0.0" "�����߷�")
  (command "TEXT" "M" (list (+ ix (* 155.0 S)) hy) th "0.0" "��  ��  ��")
  (command "TEXT" "M" (list (+ ix (* 185.0 S)) hy) th "0.0" "�� ��")


;  (command "TEXT" "M" (list (+ ix (* 185.0 s)) (- hy lh)) th "0.0" "ADD 3%")

  (command "TEXT" "M" (list (+ ix (* 185.0 s)) (- hy lh)) th "0.0"
           (strcat "ADD " (rtos (* (- add_factor 1.0) 100) 2 0) "%"))

;   (command "INSERT" (strcat (prefix) "blocks/bhead") ipnt scl scl "0")

  (setq ly (- hy  (/ hh 2.0)))                      ;line y��ǥ

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "7")
  (command "LINE" (list ix iy) (list (+ ix (* 200 s)) iy) "")   ;��line�׸���
  (setvar "CECOLOR" "1")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")   ;�Ʒ�line�׸���
  (setvar "CECOLOR" "7")
  (command "LINE" (list (+ ix (*   0 s)) iy) (list (+ ix (*   0 s)) ly) "")
  (setvar "CECOLOR" "1")
  (command "LINE" (list (+ ix (*  20 s)) iy) (list (+ ix (*  20 S)) ly) "")
  (command "LINE" (list (+ ix (*  40 s)) iy) (list (+ ix (*  40 S)) ly) "")
  (command "LINE" (list (+ ix (*  70 s)) iy) (list (+ ix (*  70 S)) ly) "")
  (command "LINE" (list (+ ix (*  90 s)) iy) (list (+ ix (*  90 S)) ly) "")
  (command "LINE" (list (+ ix (* 120 s)) iy) (list (+ ix (* 120 S)) ly) "")
  (command "LINE" (list (+ ix (* 140 s)) iy) (list (+ ix (* 140 S)) ly) "")
  (command "LINE" (list (+ ix (* 170 s)) iy) (list (+ ix (* 170 S)) ly) "")
  (setvar "CECOLOR" "7")
  (command "LINE" (list (+ ix (* 200 s)) iy) (list (+ ix (* 200 S)) ly) "")
  (setvar "CECOLOR" oldc)


;  (setq iy (- iy (* 1.5 lh)))                       ;���ο� insert point
  (setq iy (- iy (+ hh (* 0.5 lh))))                       ;���ο� insert point
  (setq ndia (length tlist))                        ;���� ����

  (setq cl 0                                        ;���� line��ȣ
        gtw 0.0                                     ;Grand total weight
        gtwa 0.0)

  (setq dcount 0)                                   ;DIA count

  (repeat ndia                                      ;DIA������ŭ �ݺ�
    (setq dlist (sort_list (nth dcount tlist) 0))   ;DIA�� ����
    (setq cdia (nth 1 (nth 0 dlist)))               ;���� ����
    (setq sbn (length dlist))                       ;������ ����
    (setq stl 0.0)                                  ;Sub total length
    (setq vy1 cl)

    (setq sbcount 0)                                ;Sub bar count
    (repeat sbn                                     ;ö�� ������ŭ�ݺ�
      (setq bn (nth 0 (nth sbcount dlist))          ;ö�ٹ�ȣ
            bd (nth 1 (nth sbcount dlist))          ;ö������
            bl (nth 2 (nth sbcount dlist))          ;ö�ٱ���
            nb (nth 3 (nth sbcount dlist)))         ;ö�ٰ���
      (if (/= sbcount 0) (setq bd (chr 34)))        ;ó������ �ƴϸ� ���� ǥ��
      (if (< bl 1.0)
        (setq blt (rtos (* bl 1000.0) 2 0))
        (setq blt (rtos bl 2 3))
      ) ;of if
      (setq tl  (* bl nb)                           ;����*����
            stl (+ stl tl))                         ;�Ұ����

      (setq y    (- iy (* cl lh)))                  ;���� line y��ġ

      (command "TEXT" "M" (list bnx y) th "0.0" bn)              ;��ȣ
      (command "TEXT" "M" (list bdx y) th "0.0" bd)              ;����
      (command "TEXT" "MR" (list blx y) th "0.0" blt)            ;����
;      (command "TEXT" "MR" (list nbx y) th "0.0" (itoa nb))        ;��������
;      (command "TEXT" "MR" (list nbx y) th "0.0" (rtos nb 2 3))    ;�Ǽ�����

      (command "TEXT" "MR" (list nbx y) th "0.0"        ;��������,�Ǽ������Ǵ�
        (if (> (- nb (fix nb)) 0)                       ;�Ҽ��� ���ϰ�����
          (rtos nb 2 3)                                 ;�Ǽ�����
          (rtos nb 2 0)
        );if
      );command

      (command "TEXT" "MR" (list tlx y) th "0.0" (rtos tl 2 3))  ;�ѱ���

      (setq ly (- y (/ lh 2.0)))                                    ;line y��ǥ
      (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "1")
      (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line�׸���
      (setvar "CECOLOR" oldc)

      (setq sbcount (1+ sbcount))                                   ;����ö��
      (setq cl (+ cl 1))                                            ;���� line
    ) ;of repeat

    (cond
      ((= cdia "D10") (setq uw 0.560))
      ((= cdia "D13") (setq uw 0.995))
      ((= cdia "D16") (setq uw 1.560))
      ((= cdia "D19") (setq uw 2.250))
      ((= cdia "D22") (setq uw 3.040))
      ((= cdia "D25") (setq uw 3.980))
      ((= cdia "D29") (setq uw 5.040))
      ((= cdia "D32") (setq uw 6.230))
      ((= cdia "H10") (setq uw 0.560))
      ((= cdia "H13") (setq uw 0.995))
      ((= cdia "H16") (setq uw 1.560))
      ((= cdia "H19") (setq uw 2.250))
      ((= cdia "H22") (setq uw 3.040))
      ((= cdia "H25") (setq uw 3.980))
      ((= cdia "H29") (setq uw 5.040))
      ((= cdia "H32") (setq uw 6.230))
    ) ;of cond

    (setq tw (* stl uw 0.001)                                   ;���߷�
          twt (rtos tw 2 3)
          tw (atof twt)
;          twa (* tw 1.03)                                       ;���߷�(����)
          twa (* tw add_factor)                                       ;���߷�(����)
          twat (rtos twa 2 3)
          twa (atof twat)
          gtw (+ gtw tw)
          gtwa (+ gtwa twa))                                    ;��ü�߷�

    (setq    y  (- iy (* lh cl)))                               ;���� line y��ǥ

    (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "��      ��")
;    (command "INSERT" (strcat (prefix) "BLOCKS/BSUBT")         ;�Ұ�
;                      (list (/ (+ ix nbx) 2.0) y)
;                      scl scl "0")
    (command "TEXT" "MR" (list tlx y) th "0.0" (rtos stl 2 3))  ;�ѱ��� �Ұ�
    (command "TEXT" "MR" (list uwx y) th "0.0" (rtos uw 2 3))   ;�����߷�
    (command "TEXT" "MR" (list twx y) th "0.0" twt)   ;���߷��Ұ�
    (command "TEXT" "MR" (list twax y) th "0.0" twat) ;���߷��Ұ�(����)

    (setq ly (- y (/ lh 2.0)))                                    ;line y��ǥ

    (setq vy1 (- iy (* vy1 lh) (/ lh -2.0))
          vy2 (- iy (* cl lh) (/ lh -2.0)))

    (setq oldc (getvar "CECOLOR"))  (setvar "CECOLOR" "1")
    (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line�׸���
    (command "LINE" (list (+ ix (*  20 s)) vy1) (list (+ ix (*  20 S)) vy2) "")
    (command "LINE" (list (+ ix (*  40 s)) vy1) (list (+ ix (*  40 S)) vy2) "")
    (command "LINE" (list (+ ix (*  70 s)) vy1) (list (+ ix (*  70 S)) vy2) "")
    (setvar "CECOLOR" oldc)

    (setq cl (1+ cl))                                           ;���� line��ȣ
    (setq dcount (1+ dcount))                                   ;���� ��������
  ) ;of repeat

  (setq y (- iy (* lh cl)))                                 ;y��ǥ

  (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "��      ��")
;  (command "INSERT" (strcat (prefix) "BLOCKS/BGRDT")            ;�Ѱ�
;                    (list (/ (+ ix nbx) 2.0) y)
;                    scl scl "0")
  (command "TEXT" "MR" (list twx y) th "0.0" (rtos gtw 2 3))    ;õü�߷�
  (command "TEXT" "MR" (list twax y) th "0.0" (rtos gtwa 2 3))  ;õü�߷�(����)
  (command "TEXT" "M" (list (- twx (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twax (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")

  (setq ly (- y (/ lh 2.0)))                                    ;line y��ǥ

  (setq vy1 (+ iy (/ lh 2.0))
        vy2 (- iy (* (1+ cl) lh) (/ lh -2.0)))

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "7")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")
  (command "LINE" (list ix vy1) (list ix vy2) "")
  (setvar "CECOLOR" "1")
  (command "LINE" (list (+ ix (* 90 s)) vy1) (list (+ ix (* 90 s)) vy2) "")
  (command "LINE" (list (+ ix (* 120 s)) vy1) (list (+ ix (* 120 s)) vy2) "")
  (command "LINE" (list (+ ix (* 140 s)) vy1) (list (+ ix (* 140 s)) vy2) "")
  (command "LINE" (list (+ ix (* 170 s)) vy1) (list (+ ix (* 170 s)) vy2) "")
  (setvar "CECOLOR" "7")
  (command "LINE" (list (+ ix (* 200 s)) vy1) (list (+ ix (* 200 s)) vy2) "")
  (setvar "CECOLOR" oldc)

) ;of defun



;*************************************************
; Function : WRITE_DATA
;            WRITE DATA file
;            Yi Suk-Jong
;            96/9/6
;*************************************************
; list�� ������ data list�� �޾Ƽ� ���Ϸ� ������ش�.
; �Ѿ���� ��
;     LLIST : ����ϰ���� list
;        FN : File Name
;*************************************************

(defun WRITE_DATA( LLIST FN / nl count opf nl line nb)

  (setq nl (length LLIST)                               ;list�� ����
        count           0)                              ;ù list����

  (setq opf (open fn "w"))                              ;file����

  (repeat nl                                            ;list������ŭ �ݺ�
    (setq nb (nth 3 (nth count llist)))
    (setq line (strcat (nth 0 (nth count llist)) ","
                       (nth 1 (nth count llist)) ","
                       (rtos (nth 2 (nth count llist)) 2 3) ","
;                       (itoa (nth 3 (nth count llist)))
                       (if (> (- nb (fix nb)) 0)        ;�Ҽ��� ���ϰ�����
                         (rtos nb 2 3)                  ;�Ǽ�����
                         (rtos nb 2 0)                  ;��������
                       );if
               );strcat
    );setq
    (write-line line opf)                               ;���Ͽ� ����
    (setq count (1+ count))                             ;���� list��
  ) ;of repeat

  (close opf)                                           ;���� �ݱ�
  (princ "\nDATA FILE ")
  (princ FN)
  (princ " is made")

) ;of defun

;************************************************
; Function : LT
;           Less Then
;           Yi Suk-Jong
;           1996/2/27
;************************************************
; �μ�-1�� �μ�-2���� �������� �Ǵ����ش�.
; �Ѿ���� ��:
;       ARG1 : �μ�-1
;       ARG2 : �μ�-2
; �Ѿ�� ��:
;       T    : �μ�-1�� �μ�-2���� ���� ��
;       nil  : �μ�-1�� �μ�-2���� ���� ������
;************************************************

(defun LT(arg1 arg2)
  (setq hpos1 (str_position arg1 "-")
        hpos2 (str_position arg2 "-"))
  (if (and (= hpos1 nil) (= hpos2 nil))
    (lt1 arg1 arg2)
    (progn
      (if hpos1
        (setq arg1f (substr arg1 1 (1- hpos1)))
        (setq arg1f arg1)
      );if
      (if hpos2
        (setq arg2f (substr arg2 1 (1- hpos2)))
        (setq arg2f arg2)
      );if      
      (if (= arg1f arg2f)
        (lt1 arg1 arg2)
        (lt1 arg1f arg2f)
      ) ;of if
    );progn
  );if
;  (if (and (is-num arg1) (is-num arg2))
;    (< (atof arg1) (atof arg2))
;    (< arg1 arg2)
;  ) ;of if
) ;of defun

;----------------------------------
; function : str_position
;            Yi Suk Jong
;            00/7/15
;----------------------------------
; str1 : long string
; str2 : short string
;----------------------------------
(defun str_position(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
        len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1)) nil count)
);defun str_position

;-----------------------------------
; function: split_blist
;           Yi Suk Jong
;           01/11/30
;-----------------------------------
(defun split_blist( blist idia /
                    )
;  (setq blist (reverse (sort_list blist 1)))    ;ö�� ������ �������� sort
  (setq bbarlist nil
        sbarlist nil)
  (setq nbar (length blist))
  (setq ibar 0)
  (repeat nbar
    (setq diaofbar (atoi (substr (nth 1 (nth ibar blist)) 2 2)))
    (if (>= diaofbar idia)
      (setq bbarlist (append bbarlist (list (nth ibar blist))))
      (setq sbarlist (append sbarlist (list (nth ibar blist))))
    )
    (setq ibar (1+ ibar))
  );repeat
  (list bbarlist sbarlist)
);defun


(defun bardlg(/  
                  dcl_id )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "bar" dcl_id)) (exit))         ;ddscl.dcl ���� scl����  

;  (if (= #tr34 "1") (set_tile "tr34" "1"))
  (cond
    ((= #roundup 4) (set_tile "round4" "1"))
    ((= #roundup 5) (set_tile "round5" "1"))
    ((= #roundup 0) (set_tile "roundno" "1"))
    ((= #roundup nil)
      (set_tile "roundno" "1")
      (setq #roundup 0))
  );cond  
  (if (/= #addratio nil) (set_tile "addratio" (rtos #addratio 2 0)) (set_tile "addratio" "3"))
  (if (/= #adds nil) (set_tile "adds" (rtos #adds 2 0)) (set_tile "adds" "3"))
  (if (/= #addb nil) (set_tile "addb" (rtos #addb 2 0)) (set_tile "addb" "6"))
  (if (= #addg "1") (set_tile "addg" "1")(set_tile "addg" "0"))
  (set_addg)
  ;  (alert #addg)
;;;  (set_tile "div" (rtos 10 2 0))
;;;  (set_tile "prop" (strcat "Y = aX^2 = " (rtos #a 1 6) " X^2"))
;;;  (set_tile "cap" #cap)  
;;;  (set_tile "both" #both)
;;;  (set_tile "length"  (strcat "L= " (rtos h 2 3)))
;;;  (set_tile "height"  (strcat "H= " (rtos l2 2 3)))


;  (action_tile "tr34" "(set_tr34)")     ;user ���� box
  (action_tile "round4" "(setq #roundup 4)")
  (action_tile "round5" "(setq #roundup 5)")
  (action_tile "roundno" "(setq #roundup 0)")  
  (action_tile "addg" "(set_addg)")               ;user ���� box
  (action_tile "addratio" "(set_val $key)")               ;user ���� box
  (action_tile "adds" "(set_val $key)")               ;user ���� box
  (action_tile "addb" "(set_val $key)")               ;user ���� box    
  (action_tile "cancel" "(do_cancel)")                ;CALCEL button
  (action_tile "accept" "(do_accept)")                ;CALCEL button
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun para-DIALOG

(defun set_tr34 ()
  (setq #tr34 (get_tile "tr34"))
);defun

    
(defun set_addg ()
  (setq #addg (get_tile "addg"))
  (if (= #addg "0")
    (progn
      (mode_tile "adds" 1)
      (mode_tile "addb" 1)
      (mode_tile "addratio" 0)
    );progn
    (progn
      (mode_tile "adds" 0)
      (mode_tile "addb" 0)
      (mode_tile "addratio" 1)      
    );progn  
  );if
);defun

(defun set_val(key )
  (setq val (atof (get_tile key)))
  (if (<= val 0.0)
    (progn
      (set_tile "error" "Invalid Input")
      (mode_tile key 2)
      (mode_tile key 3)
    );progn
    (progn
      (cond
        ((= key "addratio") (setq #addratio val))
        ((= key "adds") (setq #adds val))
        ((= key "addb") (setq #addb val))
      );cond
      (set_tile "error" "")
    );progn
  );if
);defun

(defun do_cancel()
  (done_dialog)
  (exit)
);defun

(defun do_accept()           ;dialog box�� ������ ���� ��� �Է� ����Ÿ Ȯ��
  (if (and (set_val "addratio")
           (set_val "adds") 
           (set_val "addb"))
    (done_dialog)
  ) ;of IF
) ;of defun


