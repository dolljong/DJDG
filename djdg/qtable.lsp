;***********************************
; Program : QTABLE
;           Quattro TABLE insert
;           By Suk-Jong Yi
;           1995/5/31
;***********************************
; Qpro �� ������ ĳ��� �ҷ��´�.
; ��Ʈ�ο��� �۾��� ����� ���Ϸ� �Ѵ�.
; �� ���α׷��� �� ������ �ҷ��´�.
; box���� ������ �����̸� ���� ����,
; �����̸� ���� ������ �Ѵ�.

(defun C:QTABLE(/
dsc     ipnt    ipx     ipy     th      th2     rgap    rgap2   fn      in-f
row     clm     strl    pflag   floc    vflag   vloc    ch      p1      p2
txt     rslt    txt1    fspn    bspn    num     txtpnt  tr-il
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(push-env)                                          ;ȯ�溯�� ����

(setq dsc (getvar "DIMSCALE")                       ; dimscale��
      th (getvar "DIMTXT"))                         ; textũ��

(setq ipnt (getpoint "\nPick insert point: "))      ;table�� insert point �Է�
(setq ipx (car ipnt)                                ;insert point x,y��
      ipy (cadr ipnt))

(setq th (* th dsc)                                ;text ���̸� dimtxt
      th2 (/ th 2.0))                               ;text������ ��
(setq rgap (* 7.0 dsc)                              ;line������ 7.0mm��
      rgap2 (/ rgap -2.0))                          ;line������ ��

(setq fn (getfiled "Input" "" "prn" 0))         ;�����̸� �Է�
(setq in-f (open fn "r"))                       ;file open
(setq row 0)                                    ; �ٹ�ȣ

(while (setq il (read-line in-f))               ;input line
  (setq clm 1)                                  ; ����ȣ
  (setq strl (strlen il))                       ;���ڿ��� ���� ����
  (setq pflag 0                                 ; + flag
        ploc  0)                                ; + location
  (setq vflag 0                                 ; | flag
        vloc  0)                                ; | location
  (repeat strl                                  ;���� ������ŭ �ݺ�
    (setq ch (substr il clm 1))                 ;�����Ѱ� �̾Ƴ�
    (cond
      ((= ch "+")                               ; + ��ȣ�� ������ ���
;        (if (= (substr il (- clm 1) 1) "-")    ; -+ ó���̸� ������ �ȱ׸�
        (if (>= clm 2)                            ;ADD
        (if (and (= (substr il (- clm 1) 1) "-")  ;ADD
                 (>= clm) 2)                      ;ADD
          (progn
            (setq p1 (list (+ ipx (* ploc th))
                           (+ ipy (* row rgap2))))   ;���� ù��
            (setq p2 (list (+ ipx (* clm th))
                           (+ ipy (* row rgap2))))       ;���� ����
            (setvar "CECOLOR" "RED")
            (command "LINE" p1 p2 "")                    ;���� �׸�
            (setvar "CECOLOR" "BYLAYER")
            (setq pflag 1)
            (setq ploc clm)
          ) ;of progn
        ) ;of if
        ) ;of if                                    ;ADD
        (if (= (substr il (+ clm 1) 1) "-")     ; +-
          (setq pflag 1
                ploc clm)
        ) ;of if
      ) ;of cond ch="+"
      ((= ch "|")                               ; | ��ȣ�� ������ ���
        (setq p1 (list (+ ipx (* clm th))
                       (+ ipy (* (- row 1) rgap2)))
              p2 (list (+ ipx (* clm th))
                       (+ ipy (* (+ row 1) rgap2))))
        (setvar "CECOLOR" "RED")
        (command "LINE" p1 p2 "")
        (setvar "CECOLOR" "BYLAYER")
        (if (= vflag 0)                 ; ó���̸� ������ ������
          (setq vflag 1                 ; ���� �̾Ƴ��� ����
                vloc clm)
          (progn                        ;�ι�° �̸� ���� �̾Ƴ�
            (setq txt (substr il (+ vloc 1) (- clm vloc 1)))  ;����
            (setq rslt (sp-trunc txt))
            (setq txt1 (car rslt))
            (if (/= txt1 nil)
              (progn
                (setq fspn (- (cadr rslt) 1)
                      bspn (- (strlen txt) (caddr rslt))
                       num (cadddr rslt))
                (if num
                  (progn
                    (setq txtpnt (list (+ ipx (- (* (- clm bspn) th) th2))   ;������ġ
                                       (+ ipy (- (* row rgap2) th2))))
                    (setvar "CECOLOR" "WHITE")
                    (command "TEXT" "R" txtpnt th "0" txt1)             ;���� ��
                    (setvar "CECOLOR" "BYLAYER")
                  ) ;of progn THEN
                  (progn
                    (setq txtpnt (list (+ ipx (+ (* (+ vloc fspn) th) th2))   ;������ġ
                                       (+ ipy (- (* row rgap2) th2))))
                    (setvar "CECOLOR" "WHITE")
                    (command "TEXT" txtpnt th "0" txt1)             ;���� ��
                    (setvar "CECOLOR" "BYLAYER")
                  ) ;of progn ELSE
                ) ;of if num?
              ) ;of if txt /= nil
            ) ;of if txt1
          ) ; of progn
        ) ;of if vflag=0
        (setq vflag 1)
        (setq vloc clm)
      ) ;of cond ch="|"
    ) ;of cond
    (setq clm (+ clm 1))                            ; ���� Į������
  ) ;of repeat charactor
  (if (and (= vflag 0) (= pflag 0) (/= strl 0))
    (progn
      (setq tr-il (sp-trunc il))
      (setq txt (car tr-il)
            fspn (- (cadr tr-il) 1))
      (setq txtpnt (list (+ ipx (* fspn th) th2)
                         (+ ipy (- (* row rgap2) th2))))
      (command "TEXT" txtpnt th "0" txt)
    ) ;of progn
  ) ;of if
  (setq row (+ row 1))                              ; ���� �ٷ�
) ;of while read line

(close in-f)                                        ; ���� ����

(pop-env)                                           ; ȯ�溯���� ����

(princ)
  (setq *error* oer seterr nil)
) ;of defun


;************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;************************************
; �Է¹��ڿ��� ��,�ڿ� �ִ� ��ĭ�� ©�󳽴�.
; ���ϰ���
; (©�� ���ڿ�, ù ���� ������ ��ġ, ������ ���� ������ ��ġ, �����ΰ�?)

(defun SP-TRUNC(txt /
txtl frntn backn txt1
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
;            1995/6/1
;************************************
; ���ڿ��� �����ΰ�?�� �Ǵ����ش�.

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (and (>= ch 43) (<= ch 46)))
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun
