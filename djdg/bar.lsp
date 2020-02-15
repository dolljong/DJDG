;Program : BAR 	: ö�ٻ󼼵��� �̿��Ͽ� ö�����ǥ �����
;Program : BARV : ö�ٵ����������� �̿��Ͽ� ����ó���� ö�����ǥ �����
;----------------------------------------------------------
; Function : B_MARK ; ö�� �󼼵����� marking(ö�ٹ�ȣ)�� ö�ٵ���Ÿ�� ã�� ����
; 		      ������ �ǵ����ش�. (������ ���� ������ ��ܰ� ������ ����)
; 		      �Ѿ���� ��:  IP : Insert point (���� �߽�)
;       			    R : Radius       (���� ������)
;     				    A34 : 3��4�� ����(Yes/No)
; 			�Ѿ�� ��:
;     				    MARK : ö�� ��ŷ
;      				    DIA : ö���� ����
;        			    L : ö���� ����(L,L',A.V.L�� ���ÿ� ������ ��� ���� ū ������)
;        			    N : ö���� ����
;**************************************
; Function : ATOF1  ;            ATOF-1 (STRING --> REAL)  
; Function : ATOI1  ;            ATOI-1 (STRING --> INTEGER)
; Function : DATA-IN;                DATA file IN
; Function : SP-TRUNC;            SPace TRUNCation
; Function : IS-NUM;            IS NUMber ?
; Function : SORT_LIST;           SORT LIST
; Function : LT;           Less Then
; Function : DEL_ATOM;           DELete ATOM
; Function : BAR_LIST;            draw BAR LIST
; Function : WRITE_DATA;            WRITE DATA file
; Function : LT;           Less Then
; function : str_position
; function: split_blist
; function : bardlg (bar dialog)
; function : djdg_lengthtxt;		Length of text ; text���� ���ϱ�


;**************************************************
; Program : BAR
;           BAR
;           Yi Suk-Jong
;           96/9/4
;**************************************************
; ö�ٻ󼼵����� ö�����ǥ�� data�� ������ش�.
; �Ǽ� ���� �ν��ϵ��� ��ħ 98/7/11
; Dialog box�� ���� �ɼ� �����ϵ��� ���� 01/12/06
; Attribute�� �̿��� bar detail�� �ν��ϵ��� ����.
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

  (setq th (getvar "DIMTXT"))                                 ;���� ũ��
  (setq scl (getvar "DIMSCALE"))            ;scale�� ��Ƴ���
  
  (setq #TH (* th scl))                     ;text����

  (setq secondtbl nil)
  
  (push-env)                                ;ȯ�溯�� ����

  (bardlg)					;���̾�α׷� �Է¹���.


  (setq scl (getvar "DIMSCALE"))            ;scale�� ��Ƴ���

   (setq ssc (ssget '((-4 . "<OR")           ;circle��� block�̸��� djdg_ratt�� insert ����
                         (0 . "CIRCLE")
                         (-4 . "<AND")
                           (0 . "INSERT")
                           (2 . "djdg_ratt")
                         (-4 . "AND>")
                       (-4 . "OR>"))))
  
  (setq nc (sslength ssc))                  ;circle �� att�� ����

  (setq llist nil)                          ;�� line-list ����

  (setq count 0)
  (repeat nc                                            ;circle������ŭ �ݺ�
    (setq cent (entget (ssname ssc count)))             ;circle����
    (setq cc (cdr (assoc 10 cent)))                     ;center ��ǥ
    (setq rc (cdr (assoc 40 cent)))                     ;radius (������)
    (setq etype (cdr (assoc 0 cent)))			;entity�� type(Circle or insert(att))
    (if (= etype "CIRCLE")				;���� ���� attribute insert�� ��� �и��ؼ� ȣ��.
      (setq llist (append llist (list (b_mark cc rc #roundup))))   ;Marking ã�Ƽ� (("A1" "D29" 12.5 50) ... )�� ���� ö������ �����.
      (setq llist (append llist (list (b_marka (cdr (assoc -1 cent )) #roundup)))) ;attribute�������� ö������ ã�� list�� �߰�.
    );if  
    (setq count (1+ count))                             ;���� circle��
  ) ;of repeat

  (cond
    ((= #BAR_outtype "FILE")
      (progn
        (setq fn (getfiled "Open data file" "" "dat" 1))      ;file�̸� �Է¹���
        (write_data llist fn)                                 ;���Ϸ� �����
      ) ;of progn
    ) ;of sub_cond
    ((or (= #BAR_outype "DRAW") (= #BAR_outype nil))
      (if (= #addg "1")
	(progn
          (setq spblist (split_blist llist 22))				;22�������� �ڸ���
          (setq bblist (car spblist))   				;�տ��� 22~32
          (setq sblist (cadr spblist))					;�ڿ��� 13~19
          (if bblist
            (progn
              (setq ipnt (getpoint "\nInsert point(D22~D32): "))             ;������ �Է¹���
              (setq bound (bar_list bblist #addb ipnt scl))                  ;bar list�׸���
	      (command "line" (car bound) (cadr bound))				;text�� bound check
            );progn
          );if
          (if sblist
            (progn
	      (if secondtbl 
	        (setq ipnt (list (car ipnt) (- #YBL (* #TH 15))))                           ;���������ϱ� �����̺��� ������ y��-
                (setq ipnt (getpoint "\nInsert point(D10~D19): "))             ;������ �Է¹���
              );if
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
    ((= #BAR_outtype "ALL")
      (progn
        (setq fn (getfiled "Open data file" "" "dat" 1))      ;file�̸� �Է¹���
        (write_data llist fn)                                 ;file�� �����
        (setq ipnt (getpoint "\nInsert point: "))             ;������ �Է¹���
        (bar_list llist #addratio ipnt scl)                             ;bar list�׸���
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
; ö�� �󼼵����� marking(ö�ٹ�ȣ)�� ö�ٵ���Ÿ�� ã�� ö���������� �Ѱ��ش�.
; (������ ���� ������ ��ܰ� ������ ����)
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


;**************************************************
; Function : B_MARKA
;            get Bar MARKing from Attribute
;            Yi Suk-Jong
;            05/12/22
;**************************************************
; Attribute�� ������� ö�� �󼼵����� marking(ö�ٹ�ȣ)�� ö�ٵ���Ÿ�� ã��
; ö���������� �Ѱ��ش�.
; (������ ���� ������ ��ܰ� ������ ����)
; (b_marka ename a34)
;       ename : entity name
;       A34 : 3��4�� ����(Yes/No)
; �����(MARK DIA L N)
;     MARK : ö�� ��ŷ(string)
;      DIA : ö���� ����(string)
;        L : ö���� ����(L,L',A.V.L�� ���ÿ� ������ ��� ���� ū ������)(�Ǽ�)
;        N : ö���� ����(�Ǽ�)
;**************************************************

(defun B_MARKA(ename A34 /
  		attv roundv txt1 txt2 nt diastr lenstr lentstr
	       numstr txt3 splen splent spnum maxl rm )

  (setq attv (djdg_getattv ename))	;��ü attribute ��Ƴ���.
  
  (cond					
    ((= A34 4) (setq roundv 0.0035))
    ((= A34 5) (setq roundv 0.0045))
  );cond  
  
  (setq txt1 (cdr (assoc "MARK1" attv))		;marking text-1
	txt2 (cdr (assoc "MARK2" attv)))	;marking text-1

  (setq tlst nil)
  (if (/= txt1 "") (setq tlst (append tlst (list txt1))))	;text list�� �߰�
  (if (/= txt2 "") (setq tlst (append tlst (list txt2))))
  
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
  
  (setq DIA            ""
        LD             0.0
        L              0.0
        N                0)

  (setq diastr (cdr (assoc "DIA" attv)))  	;ö�� dia
  (setq lenstr (cdr (assoc "LEN" attv)))	;ö�� net����
  (setq lentstr (cdr (assoc "LENT" attv)))	;ö�� total����.
  (setq numstr (cdr (assoc "NUM" attv)))	;����.

  ;----- Diaüũ 
      (if (or (= (setq txt3 (substr diastr 1 3)) "D13")
           (= txt3 "D10") (= txt3 "D16")  (= txt3 "D19") (= txt3 "D22")
           (= txt3 "D25") (= txt3 "D29") (= txt3 "D32")
           (= txt3 "H10") (= txt3 "H13")  (= txt3 "H16") (= txt3 "H19")
           (= txt3 "H22") (= txt3 "H25") (= txt3 "H29")
           (= txt3 "H32"))                                      ; DIA
        (setq DIA txt3)
      ) ;if
  
	(setq splen (djdg_splitstr lenstr "="))
  	(if (= (length splen) 2)
	  (setq L (atof1 (cadr splen)))
	);if
  
  	(setq splent (djdg_splitstr lentstr "="))
  	(if (= (length splent) 2)
	  (setq LD (atof1 (cadr splent)))
	);if  
  
  	(setq spnum (djdg_splitstr numstr "="))
  	(if (= (length spnum) 2)
	  (setq N (atof (cadr spnum)))
	);if  

  
  (setq maxl L)
  (if (> LD maxl) (setq maxl LD))

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
)


  (setq th (getvar "DIMTXT"))                           ;���� ũ��
  (setq lh 8)                                   	;�� line�� ����
  (setq secondtbl T)
;;;  (setq add (getreal "\n������<3%>: "))
;;;  (if (= add nil)                               ;�����Է½� �������� 3%
;;;    (setq add_factor 1.03)
    (setq add_factor (+ 1.0 (/ add 100.0)))
;;;  );if

  (setq th (* th s))                            ;����ũ�� scale���
  (setq #TH th)
  (setq lh (* lh s))                            ;�ٳ��� scale���

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

  (setq hy (- iy (/ lh 2.0)))                 ;Header�� y��ġ

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

  (setq ly (- hy  (/ lh 2.0)))                      ;line y��ǥ

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "WHITE")
  (command "LINE" (list ix iy) (list (+ ix (* 200 s)) iy) "")   ;��line�׸���
  (setvar "CECOLOR" "RED")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")   ;�Ʒ�line�׸���
  (setvar "CECOLOR" "WHITE")
  (command "LINE" (list (+ ix (*   0 s)) iy) (list (+ ix (*   0 s)) (- iy lh)) "")
  (setvar "CECOLOR" "RED")
  (command "LINE" (list (+ ix (*  20 s)) iy) (list (+ ix (*  20 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (*  40 s)) iy) (list (+ ix (*  40 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (*  70 s)) iy) (list (+ ix (*  70 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (*  90 s)) iy) (list (+ ix (*  90 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (* 120 s)) iy) (list (+ ix (* 120 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (* 140 s)) iy) (list (+ ix (* 140 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (* 170 s)) iy) (list (+ ix (* 170 S)) (- iy lh)) "")
  (setvar "CECOLOR" "WHITE")
  (command "LINE" (list (+ ix (* 200 s)) iy) (list (+ ix (* 200 S)) (- iy lh)) "")
  (setvar "CECOLOR" oldc)


  (setq iy (- iy (* 1.5 lh)))                       ;���ο� insert point
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
      (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "RED")
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

    (setq oldc (getvar "CECOLOR"))  (setvar "CECOLOR" "RED")
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
  (setq txttotal (rtos gtw 2 3))    ;��ü�߷� text
  (setq txttotala (rtos gtwa 2 3))    ;��ü�߷�(����) text
  
  (command "TEXT" "MR" (list twx y) th "0.0" txttotal)    ;õü�߷�
  (command "TEXT" "MR" (list twax y) th "0.0" txttotala)  ;õü�߷�(����)

  (setq ldec1 (- (djdg_lengthtxt (substr txttotal (- (strlen txttotal) 3) 4))
	         (* 0.5 (djdg_lengthtxt "."))))                                                 ;�Ҽ������� ����
  (setq ldec2 (- (djdg_lengthtxt (substr txttotala (- (strlen txttotala) 3) 4))
                 (* 0.5 (djdg_lengthtxt "."))))
;  (command "TEXT" "M" (list (- twx (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
;  (command "TEXT" "M" (list (- twax (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twx ldec1) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twax ldec2) (+ y (/ th 2.0))) th "0.0" "T")

  (setq ly (- y (/ lh 2.0)))                                    ;line y��ǥ

  (setq vy1 (+ iy (/ lh 2.0))
        vy2 (- iy (* (1+ cl) lh) (/ lh -2.0)))

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "WHITE")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")
  (command "LINE" (list ix vy1) (list ix vy2) "")
  (setvar "CECOLOR" "RED")
  (command "LINE" (list (+ ix (* 90 s)) vy1) (list (+ ix (* 90 s)) vy2) "")
  (command "LINE" (list (+ ix (* 120 s)) vy1) (list (+ ix (* 120 s)) vy2) "")
  (command "LINE" (list (+ ix (* 140 s)) vy1) (list (+ ix (* 140 s)) vy2) "")
  (command "LINE" (list (+ ix (* 170 s)) vy1) (list (+ ix (* 170 s)) vy2) "")
  (setvar "CECOLOR" "WHITE")
  (command "LINE" (list (+ ix (* 200 s)) vy1) (list (+ ix (* 200 s)) vy2) "")
  (setvar "CECOLOR" oldc)
  
  ;(command "circle" (list (+ ix (* 200 s)) vy2) 100)
  (setq #YBL vy2)  ;y of bottom line
  (list (list (car ipnt) vy2) (list (+ ix (* 200 s)) (cadr ipnt)))	;�����ϴ�-���������ǥ
) ;of defun



;*************************************************
; Function : WRITE_DATA
;            WRITE DATA file
;            Yi Suk-Jong
;            96/9/6
;*************************************************
; list�� ������ data list�� �޾Ƽ� ���Ϸ� ������ش�.
; �Ѿ���� ��
;     LLIST : ����ϰ����� list
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

;------------------------------
; function : bardlg (bar dialog)
;	Yi Suk Jong
;------------------------------
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
  (cond							;output type�ʱ�ȭ
    ((= #BAR_outtype "DRAW") (set_tile "drawtbl" "1"))
    ((= #BAR_outtype "FILE") (set_tile "fileout" "1"))
    ((= #BAR_outtype nil) (set_tile "drawtbl" "1")(setq #BAR_outtype "DRAW"))
  );cond  
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
  (action_tile "drawtbl" "(setq #BAR_outtype \"DRAW\")")
  (action_tile "fileout" "(setq #BAR_outtype \"FILE\")")
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

;----------------------------
; function : djdg_lengthtxt
;		Length of text
;	Yi Suk Jong
;----------------------------
(defun djdg_lengthtxt(txt)
  (setq tb (textbox (list (cons 1 txt)))) 	;textbox
  (- (car (nth 1 tb)) (car (nth 0 tb)))         ;length of text
);defun


(defun c:test()
   (setq ssc (ssget '((-4 . "<OR")           ;solid�� solid hatch�� ����
                         (0 . "CIRCLE")
                         (-4 . "<AND")
                           (0 . "INSERT")
                           (2 . "djdg_ratt")
                         (-4 . "AND>")
                       (-4 . "OR>"))))
  (djdg_mtredraw ssc 3)
);


;**************************************************
; Program : BARV : ������ ����� ö�����ǥ �����
;           Bar material list with Variable
;           Yi Suk-Jong
;           1996/5/7
;**************************************************
; data file�� �о� ö�� ���ǥ�� ������ش�
; �Ǽ� ������ �ν��ϵ��� ��ħ 98/7/11
; �������� ����ڰ� �Է��ϵ��� ��ħ 99/1/27
; Pier��ȣ�� ������, �������� �÷����� �ϵ��� ���� 06/08/10
;**************************************************

(defun C:BARV(
/       scl     ilist    llist   fn      opf     ilnum
        num_marking      num_var l_count count   num_table
        tname   var_list ch      inline  ibar_list
        tcount  total_list       str0    str1    str2
        str3    lst      bcount
        llist   ipnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)                     ;���忡����ƾ ����

  (push-env)                                            ;ȯ�溯�� ����

  (setq scl (getvar "DIMSCALE"))                        ;�����ϰ� ����


  (setq ilist nil                                       ;�� input line list����
        llist nil)                                      ;�� data line-list ����


  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name�Է�
  (setq opf (open fn "r"))                          ;file open

  (while (setq ch (read-line opf))                  ;������ �д´�
    (if (/= (substr ch 1 1) ";")                    ; ";"�Է½� ����
      (setq ilist (append ilist (list ch)))         ;ilist�� �߰�
    );if
  );while

  (setq ilnum (length ilist))                       ;input line��
  (setq num_marking 0                               ;marking ��
        num_var     0)                              ;variable ��

  (setq count 0)
  (repeat ilnum
    (if (= (strcase (substr (nth count ilist) 1 3)) "VAR")  ;var�� �߰ߖV�� ��
      (setq num_marking count                               ;marking����
;            num_var (- ilnum count 1))                      ;variable����-->table����
	    num_table (- ilnum count 1))			;table����(p1,p2,p3...)
    );if
    (setq count (1+ count))
  );repeat


  (if (= num_table 0)
    (setq num_marking   ilnum                           ;variable�� ���� ���
          num_table     1    )                          ;��ŷ����=line����, ���̺�����=1
    (progn                                            ;bariable�� �ִ� ���
;      (setq tnames (data-in (nth num_marking ilist))) ;table�̸�
      (setq vnames (data-in (nth num_marking ilist))) ;variable�̸�      
;      (setq num_table (1- (length tnames)))           ;table����
      (setq num_var (1- (length vnames)))           ;Variable����      

      (setq count (1+ num_marking)             
            tbl_list nil)

      (repeat num_table                          ;table ����Ʈ �����(("P1" 1.2 3.4) ("P2" 1.2 4.5))
        (setq tbl_list (append tbl_list (list (data-in (nth count ilist)))))
        (setq count (1+ count))
      );repeat
    );progn
  );if

  (setq count 0                             ;(( "1" "d29" "1.4" "45") ...)
        ibar_list nil)
  (repeat num_marking                       ;marking�Է��ٵ� ����
    (setq  ibar_list (append ibar_list (list (data-in (nth count ilist)))))
    (setq count (1+ count))
  );repeat

  (if (= num_var 0) (setq num_var 1))
  (setq tcount 0)                                       ;table������ŭ �ݺ�
  (setq total_list nil)                                 ;�� table����Ʈ
  (repeat num_table
    (setq bcount 0)
    (setq llist nil)
    (repeat num_marking                                 ;marking������ŭ �ݺ�
      (setq inline (nth bcount ibar_list))
      (setq str0 (strcase (car (sp-trunc (nth 0 inline)))))       ;õ�ٹ�ȣ
      (setq str1 (strcase (car (sp-trunc (nth 1 inline)))))       ;ö������
      (setq str2 (car (sp-trunc (nth 2 inline))))       ;ö�ٱ���
      (setq str3 (car (sp-trunc (nth 3 inline))))       ;ö�ٰ���(�Ǽ�)
      (if (= (substr str1 1 1) "$")                     ;ö�������� ������ ��
        (setq str1 (findvar tbl_list vnames str1 tcount))
      );if
      (if (= (substr str2 1 1) "$")                     ;ö�ٱ��̰� �����϶�
        (setq str2 (atof (findvar tbl_list vnames str2 tcount)))
        (setq str2 (atof str2))
      );if
      (if (= (substr str3 1 1) "$")                     ;ö�ٰ����� ������ ��
        (setq str3 (atof (findvar tbl_list vnames str3 tcount)))
        (setq str3 (atof str3))
      );if
      (setq lst (list str0 str1 str2 str3))                 ;ö��1���� ���� data
      (setq llist (append llist (list lst)))                ;llist�� �߰�
      (setq bcount (1+ bcount))
    );repeat

    (setq total_list (append total_list (list llist)))      ;�� ö�ٵ���Ÿ


    (setq tcount (1+ tcount))
  );repeat


  (princ "\n") (princ num_marking) (princ " Marking found")
  (princ "\n") (princ num_table)   (princ " Table found")
  (princ "\n") (princ num_var) (princ " Variable found")

  (close opf)                                       ;file close


  (setq dsth (* (getvar "dimscale") (getvar "dimtxt")))		;dimscale�� ����� text����
  (setq gap (* dsth 5))						; ���̺� ���� 5���ڸ�ŭ  
  (setq br (getpoint "\nInsert point: "))               	;������ �Է¹���
  (setq count 0)  (repeat num_table                                         	;TABLE������ŭ �ݺ�
    (setq tname (strcat "[" (car (nth count tbl_list)) "]"))			;table name
    (setq ip (list (+ (car br) (* (if (> count 0) 1 0) gap)) (cadr br)))	;������
    (command "text" (list (car ip) (+ (cadr ip) (* dsth 0.5)))
			  dsth  "0" tname) 			;table���� ����    
    (setq bound (bar_list (nth count total_list) 3 ip scl))              ;bar list�׸��� bound return����
    (setq br (nth 1 bound))
    (setq count (1+ count))
  );repeat

  (pop-env)                                             ;ȯ�溯�� ����

  (princ "\nNormal terminated")                         ;���������� ���� ǥ��

;  (setq *error* oer seterr nil)

  (princ)

) ;of defun


;-----------------------------------------------------------------------
;function : findvar
;           �ش纯���� ���� ���Ƴ���.
;           99/4/5
;           Yi Suk Jong
;-----------------------------------------------------------------------
; (findvar var_list var tindex)
;>>> �Ѿ���� ��
; tbl_list : table list    ��: (("P1" "3.5" "2.4" "7.0") ("P2" "5" "7" "5"))
; varlist : ���� list	   ��: ("var" "$A1L" "$A1N")
; varname : �������ϴ� ������  ��: $AL
;  tindex : table�� index ��: var, P1, P2, P3 �϶�  P1�� ���Ͽ��� 0
;>>> ��뿹
; (findvar var_list $AL 0) : P1�� ���Ͽ� $AL�� �ش��ϴ� ��(string)
;-----------------------------------------------------------------------
(defun findvar(tbl_list vlist varname tindex
/ vpos found
;var_list var tindex
)
  (setq found nil)
  (setq vpos (vl-position varname vlist))                 ;ã�����ϴ� �������� idnex
  (setq found (nth vpos (nth tindex tbl_list)))		
  (if (/= found nil)
    (setq found found)
    (setq found nil)
  );if
);defun