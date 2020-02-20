;**************************************************
; Program : HBLIST1
;           Hangul Bar LIST-1
;           Yi Suk-Jong
;           1996/5/7
;**************************************************
; data file�� �о� ö�� ���ǥ�� ������ش�
; �Ǽ� ������ �ν��ϵ��� ��ħ 98/7/11
; �������� ����ڰ� �Է��ϵ��� ��ħ 99/1/27
;**************************************************

(defun C:HBLIST(
/       fn      opf     l_count     ch      inline      lst
        llist   ipnt    scl
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)

(push-env)                                              ;ȯ�溯�� ����


  (setq llist nil)                                      ;�� line-list ����

  (initget "File Entity")
  (setq answ (getkword "\nFile/Entity <File>: "))

  (if (or (= answ nil) (= answ "File"))                 ;return�Է��̳� F�Է½�
    (progn
      (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name�Է�
      (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file�� ���� ���
        (progn
           (setq count 1)
           (while (setq ch (read-line opf))             ;������ �д´�
              (princ (chr 13))                          ;�Է��� �޼��� ���
              (princ count)
              (princ " Line Processing...")
              (setq inline (data-in ch))
              (setq lst (list                           ;���� data�� ���� data��
                           (strcase (car (sp-trunc (nth 0 inline))))   ;ö�ٹ�ȣ
                           (strcase (car (sp-trunc (nth 1 inline))))   ;ö������
                           (atof (nth 2 inline))                       ;ö�ٱ���
;                           (atoi (nth 3 inline))))                     ;ö�ٰ���(����)
                           (atof (nth 3 inline))))                     ;ö�ٰ���(�Ǽ�)
              (setq llist (append llist (list lst)))                   ;llist�� �߰�
              (setq count (1+ count))                   ;line��ȣ ����
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file�� ���� ���
      ) ;of if
      (close opf)                                           ;file close
    ) ;of progn THEN
    (progn                                                  ;Entity�� �Է��� ���
      (setq tent (ssget '((0 . "TEXT"))))                   ;text�� select
      (setq tn    (sslength tent)                           ;text����
            count               0)                          ;ù text����
      (repeat tn                                            ;text������ŭ �ݺ�
        (setq ch (cdr (assoc 1 (entget (ssname tent count)))))  ;text����
        (princ (chr 13))                                        ;�۾��߸޼���
        (princ (1+ count))
        (princ " Line Processing...")
        (setq inline (data-in ch))
        (setq lst (list                                     ;���� data�� ���� data��
                     (strcase (car (sp-trunc (nth 0 inline))))   ;ö�ٹ�ȣ
                     (strcase (car (sp-trunc (nth 1 inline))))   ;ö������
                     (atof (nth 2 inline))                       ;ö�ٱ���
;                     (atoi (nth 3 inline))))                     ;ö�ٰ���(����)
                     (atof (nth 3 inline))))                     ;ö�ٰ���(�Ǽ�)
        (setq llist (append llist (list lst)))                   ;llist�� �߰�
        (setq count (1+ count))                                  ;���� text��
      ) ;of repeat
    ) ;of progn ELSE
  ) ;of IF

  (setq ipnt (getpoint "\nInsert point: "))             ;������ �Է¹���
  (setq scl  (getdist "\nScale factor: "))

  (bar_list llist ipnt scl)                             ;bar list�׸���

  (pop-env)                                             ;ȯ�溯�� ����

  (princ "\nNormal terminated")                         ;���������� ���� ǥ��
  (setq *error* oer seterr nil)
  (princ)

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
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list�� ����

  (setq slist nil)                          ;�� sort�� list����
  (setq rlist alist)                        ;�ִ밪�� ������ ������ list

  (setq count nl)                           ;list �������� �Ѱ��� ���鼭 �ݺ�

  (repeat nl                                        ;list������ŭ
    (setq min (nth aidx (nth 0 rlist)))             ;ù��° list�� ���� ������
    (setq min_th 0)                                 ;�ּҰ��� ��ġ�� ó������
    (setq count1 1)                                 ;�ι�° list����
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;���� list
      (setq c_val (nth aidx (nth count1 rlist)))    ;���� ��
      (if (lt c_val min)                             ;���� ���� min���� ������
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

(defun LT(arg1 arg2)
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

(defun BAR_LIST(blist ipnt s
/
       bd      bdx     bl      blist    blt     blx     bn      bnx     cdia
       cl      count   dcount  fdia     gtw     gtwa    hy      ix      iy
       lh      ln      ly      nb       nbx     ndia    oldc    sbcount sbn
       slist   stl     th      tl       tlist   tlx     tw      twa     twat
       twax    twt     twx     uwx      vy1     vy1     vy2     y       add_factor
)


  (setq th (getvar "DIMTXT"))        ;���� ũ��
  (setq lh 7)
                                     ;�� line�� ����
  (setq add (getreal "\n������<3%>: "))
  (if (= add nil)                               ;�����Է½� �������� 3%
    (setq add_factor 1.03)
    (setq add_factor (+ 1 (/ add 100)))
  );if

;  (setq add_factor 1.03)                        ;������

  (setq th (* th s))                            ;����ũ�� scale���
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
;  (command "TEXT" "M" (list (+ ix (* 100.0 s)) (+ iy (* th 5)))
;                  (* 6.0 s) "0.0" "B A R  L I S T")
;  (command "TEXT" "M" (list (+ ix (*  10.0 S)) hy) th "0.0" "MARK")
;  (command "TEXT" "M" (list (+ ix (*  30.0 S)) hy) th "0.0" "DIA")
;  (command "TEXT" "M" (list (+ ix (*  55.0 S)) hy) th "0.0" "LENGTH")
;  (command "TEXT" "M" (list (+ ix (*  80.0 S)) hy) th "0.0" "NUM")
;  (command "TEXT" "M" (list (+ ix (* 105.0 S)) hy) th "0.0" "TOTAL L.")
;  (command "TEXT" "M" (list (+ ix (* 130.0 S)) hy) th "0.0" "UNIT W.")
;  (command "TEXT" "M" (list (+ ix (* 155.0 S)) hy) th "0.0" "TOTAL W.")
;  (command "TEXT" "M" (list (+ ix (* 185.0 S)) hy) th "0.0" "REMARKS")
  (command "TEXT" "M" (list (+ ix (* 185.0 s)) (- hy lh)) th "0.0"
           (strcat "ADD " (rtos (* (- add_factor 1.0) 100) 2 0) "%"))

  (command "INSERT" (strcat (prefix) "blocks/bhead") ipnt scl scl "0")

  (setq ly (- hy  (/ lh 2.0)))                                  ;line y��ǥ
;  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "WHITE")
;  (command "LINE" (list ix iy) (list (+ ix (* 200 s)) iy) "")   ;��line�׸���
;  (setvar "CECOLOR" "RED")
;  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")   ;�Ʒ�line�׸���
;  (setvar "CECOLOR" "WHITE")
;  (command "LINE" (list (+ ix (*   0 s)) iy) (list (+ ix (*   0 s)) (- iy lh)) "")
;  (setvar "CECOLOR" "RED")
;  (command "LINE" (list (+ ix (*  20 s)) iy) (list (+ ix (*  20 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (*  40 s)) iy) (list (+ ix (*  40 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (*  70 s)) iy) (list (+ ix (*  70 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (*  90 s)) iy) (list (+ ix (*  90 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (* 120 s)) iy) (list (+ ix (* 120 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (* 140 s)) iy) (list (+ ix (* 140 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (* 170 s)) iy) (list (+ ix (* 170 S)) (- iy lh)) "")
;  (setvar "CECOLOR" "WHITE")
;  (command "LINE" (list (+ ix (* 200 s)) iy) (list (+ ix (* 200 S)) (- iy lh)) "")
;  (setvar "CECOLOR" oldc)

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
;      (command "TEXT" "MR" (list nbx y) th "0.0" (itoa nb))  ;��������

      (command "TEXT" "MR" (list nbx y) th "0.0"
        (if (> (- nb (fix nb)) 0)                         ;�Ҽ��� ���ϰ�����
          (rtos nb 2 3)                                    ;�Ǽ�����
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
          twa (* tw add_factor)                                       ;���߷�(����)
          twat (rtos twa 2 3)
          twa (atof twat)
          gtw (+ gtw tw)
          gtwa (+ gtwa twa))                                    ;��ü�߷�

    (setq    y  (- iy (* lh cl)))                               ;���� line y��ǥ

;    (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "S U B  T O T A L")
    (command "INSERT" (strcat (prefix) "BLOCKS/BSUBT")         ;�Ұ�
                      (list (/ (+ ix nbx) 2.0) y)
                      scl scl "0")
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

;  (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "G R A N D  T O T A L")
  (command "INSERT" (strcat (prefix) "BLOCKS/BGRDT")            ;�Ѱ�
                    (list (/ (+ ix nbx) 2.0) y)
                    scl scl "0")
  (command "TEXT" "MR" (list twx y) th "0.0" (rtos gtw 2 3))    ;õü�߷�
  (command "TEXT" "MR" (list twax y) th "0.0" (rtos gtwa 2 3))  ;õü�߷�(����)
  (command "TEXT" "M" (list (- twx (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twax (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")

  (setq ly (- y (/ lh 2.0)))                                    ;line y��ǥ

  (setq vy1 (+ iy (/ lh 2.0))
        vy2 (- iy (* (1+ cl) lh) (/ lh -2.0)))

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "7")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line�׸���
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
