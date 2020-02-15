

;------------------------------------------------------------
; frunction : circle2mark
;             By Yi Seok Jong
;             2000/9/1
;------------------------------------------------------------
; argument: entityname
; return : marking
;ex) (circle2mark entname)  --> A1-1
;------------------------------------------------------------
(defun circle2mark(centname /
		   centname cent cc rc ix iy p1 p2 ss ssn count tlst sent etype l1 l2 end_p nt txt1 txt2 mark)
  (setq cent (entget centname))             ;circle����
  (setq cc (cdr (assoc 10 cent)))                     ;center ��ǥ
  (setq rc (cdr (assoc 40 cent)))                     ;radius (������)
  (setq ix (car cc)                         ;������ x
        iy (cadr cc)                        ;       y
        p1 (list (- ix rc) (+ iy rc))         ;
        p2 (list (+ ix rc) (- iy rc))         ;
        ss (ssget "C" p1 p2)                ; MARKING/LINE ��Ƴ���
        ssn (sslength ss)
        count 0
        tlst nil)

  (repeat ssn                                 ;��ƼƼ ������ŭ �ݺ�
    (setq sent (entget (ssname ss count)))
    (setq etype (cdr (assoc 0 sent)))
    (cond
      ((= etype "TEXT")                       ;��ƼƼ�� �ؽ�Ʈ�� ���
        (if (< (distance (cdr (assoc 10 sent)) cc) rc)
          (setq tlst (append tlst (list (cdr (assoc 1 sent))))) ;�ؽ�Ʈ ����Ʈ�� �߰�
        );if
      ) ;of etype=TEXT
      ((= etype "LINE")                       ;��ƼƼ�� ������ ���
        (progn
          (setq l1 (cdr (assoc 10 sent))          ;ù��
                l2 (cdr (assoc 11 sent)))         ;����
          (if (> (car l1) (car l2))             ;����ã��
            (setq end_p l1)
            (setq end_p l2)
          ) ;of if
;          (print tlst)(print sent)	  
        ) ;of progn
      ) ;of etype=LINE
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat                             
  
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
  mark
);defun

;------------------------------------------------------------
; frunction : CB
;             Change BarMarking type
;             By Yi Seok Jong
;             2000/9/1
;------------------------------------------------------------
(defun c:CB(
	    /  ssc nc tlst count ent etype l1 l2 cc rc ntext mklst textcount enttxt
	       mklst diatxt nt txt1 txt2 mark ipnt sidepnt)
  (setq ssc (ssget))      ;circle���� �Է�
  (setq nc (sslength ssc))                  ;entity�� ����

  (setq tlst nil)                          ;�� line-list ����

  (setq count 0)
  (repeat nc                                            ;entity������ŭ �ݺ�
    (setq ent (entget (ssname ssc count)))             ;entity����
    (setq etype (cdr (assoc 0 ent)))                   ;entity type
    (cond
      ((= etype "TEXT")                       ;��ƼƼ�� �ؽ�Ʈ�� ���
        (setq tlst (append tlst (list (ssname ssc count)))) ;�ؽ�Ʈ ����Ʈ�� �߰�
      ) ;of etype=TEXT
      ((= etype "LINE")                       ;��ƼƼ�� ������ ���
        (setq l1 (cdr (assoc 10 ent))          ;ù��
              l2 (cdr (assoc 11 ent)))         ;����
      ) ;of etype=LINE
      ((= etype "CIRCLE")
        (setq cc (cdr (assoc 10 ent)))                     ;center ��ǥ
        (setq rc (cdr (assoc 40 ent)))                     ;radius (������)
      );of etype=circle 
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat

  (setq ntext (length tlst)
	mklst nil
	textcount 0)    ;number of text entity
  (repeat ntext
    (setq enttxt (entget (nth textcount tlst)))
    (if (< (distance (cdr (assoc 10 enttxt)) cc) rc)
      (setq mklst (append mklst (list (cdr (assoc 1 enttxt)))))
      (setq diatxt (cdr (assoc 1 enttxt)))
    );if
    (setq textcount (1+ textcount))
  );repeat

  (setq nt (length mklst))                       ;text����
  (if (= nt 2)                                  ;text������ �ΰ��� (��: A1-1)
    (progn
      (setq txt1 (car (sp-trunc (nth 0 mklst)))
            txt2 (car (sp-trunc (nth 1 mklst))))
      (if (= (substr txt1 1 1) "-")               ;-�� ���� ���� �ڷ��ؼ� ���ϱ�
        (setq MARK (strcat txt2 txt1))
        (setq MARK (strcat txt1 txt2))
      ) ;of if
    ) ;of progn
    (setq MARK (nth 0 mklst))                     ;text������ �Ѱ��� (��: A2)
  ) ;of if

  (setq ipnt (getpoint "\nPick basepoint: ")
	sidepnt (getpoint ipnt "\nPick sidepoint: "))
  
  (barmark ipnt sidepnt mark diatxt)
  (command "ERASE" ssc "")
;  (print mark )  (print diatxt)
  
  (princ)
);defun


;------------------------------------------------------
; function : barmark
;            By Yi Seok Jong
;            2000/9/1
;------------------------------------------------------
(defun barmark(ep p3 mark diatxt /
	       ep p3 mark diatxt th  ds rc dx xsgn dy ysgn blen p4 ip cp diaxy txtrot oldclr)
;  (setq p3 (getpoint ep "\nPick base line: "))              ;base line point
  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))           ;text height
  
  (setq rc (* ds 3.5))                  ;������

  (setq dx (- (car p3) (car ep)))
  (if (< dx 0)                                          ;base line�� x�����ν�
    (setq xsgn -1)
    (setq xsgn 1)
  ) ;of if

  (setq dy (- (cadr p3) (cadr ep)))
  (if (<  dy 0)                                         ;base line�� y�����ν�
    (setq ysgn -1)
    (setq ysgn  1)
  ) ;of if

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line�� ����

  (if (> (abs dx) (abs dy))                                             ;�����ֳ� ���ֳ�?
    (progn
      (setq p4 (list (+ (car ep) (* blen xsgn)) (cadr ep)))
      (cecolor "red")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dx 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (+ (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 0)                                   ;textȸ����
    ) ;of progn
    (progn
      (setq p4 (list (car ep) (+ (cadr ep) (* blen ysgn))))
      (cecolor "red")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dy 0)
        (setq ip p4)	
        (setq ip ep)
      ) ;of if
      (setq cp (list (- (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* 4 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 90)
    ) ;of progn
  ) ;of if


  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "red")
  (command "CIRCLE" cp rc)
  (setvar "CECOLOR" oldclr)
  
  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "WHITE")
  (txtinc mark cp txtrot)
  (command "TEXT" diaxy (* th ds) txtrot (strcase diatxt))
  (setvar "CECOLOR" oldclr)
);

;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; �� �ȿ� ö�ٹ�ȣ�� �������ش�.
; �Ѿ���� ��
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ipnt  txtl count)

  (setq th (getvar "DIMTXT"))               ;textũ��=ġ��ũ��

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
        (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
        (progn
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
  ) ;of IF
) ;of DEFUN

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
);defun

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

