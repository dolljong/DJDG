;**************************************
; Program : B3
;           Bar Mark 3
;           Suk-Jong Yi
;           1995. 3. 16~17, 5/30
;**************************************
;ö�ٸ�ŷ�� �Ʒ��� ����� ������ش�.
;            ��
;           /|\
;         /  |  \
;**************************************

(defun C:B3(/
plst oldclr ds entlst mpnt nent           ;�������� ����
index ent entype sp ep mp npnt
vtx1 nxt1 vtx2 vtx1p vtx2p cenp
cp ang minang maxang dtang mang
ccen mk dia diaxy
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

  (push-env)                                    ;ȯ�溯���� ����
;  (setq oldclr (getvar "CECOLOR"))
;  (setvar "CECOLOR" "1")

  (setq ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))                     ;textũ��
  (setq rc (* 5.0 ds)                            ;��ŷ���� ������
        diaxg 6)                                  ;���� �߽ɿ��� ������������� x�Ÿ�
        
  (setq entlst (ssget))                           ;��ŷ��� entity����
  (setq mpnt (getpoint "Pick marking point: "))   ;��ŷ ���� �׸� ��ġ����
  (setq nent (sslength entlst))                   ;��ŷ��� ��ƼƼ����

  (setq plst nil)                 ;plst: ��ŷ��� entity����  ����Ʈ����Ʈ
  (setq npnt 0)       ;����Ʈ ���� (��ŷ��� ��ƼƼ���� /= ��ŷ�������Ʈ����)
  (setq index 0)
  (repeat nent                                    ;��� ��ƼƼ������ŭ �ݺ�
    (setq ent (entget (ssname entlst index)))
    (setq entype (cdr (assoc 0 ent)))             ;��ƼƼŸ�� ����
    (cond
      ((= entype "LINE")                          ;��ƼƼ�� �����ΰ��
        (setq sp (cdr (assoc 10 ent)))            ;������ ������
        (setq ep (cdr (assoc 11 ent)))            ;������ ?{��
        (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;�����߰��� X
                       (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y
        (setq plst (append plst (list mp)))       ;�߰����� ��ŷ����Ʈ�� �߰�
        (setq npnt (1+ npnt))                     ;��ŷ����Ʈ�� ���� ����
      ) ;of entype="LINE"
      ((= entype "POLYLINE")                            ;���������� ���(ö���� �������� �׸� ���)
        (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;ù������
        (if (/= (abs (cdr (assoc 42 vtx1))) 0)         ;���������� ��ũ���ΰ�?
          (progn
            (setq vtx2 (entget (setq nxt2 (entnext nxt1))))   ;�ι�°�� ����
            (setq vtx1p (cdr (assoc 10 vtx1)))                ;ù�� ����Ʈ
            (setq vtx2p (cdr (assoc 10 vtx2)))                ;��°�� ����Ʈ
            (setq cenp (list (/ (+ (car vtx1p) (car vtx2p)) 2.0)     ;��Ÿ����Ʈ X
                             (/ (+ (cadr vtx1p) (cadr vtx2p)) 2.0))) ;           Y
            (setq plst (append plst (list cenp)))             ;����Ʈ����Ʈ�� �߰�
            (setq npnt (1+ npnt))                             ;����Ʈ ���� ����
          ) ;of progn
        ) ;of if
      ) ;of entype="PLINE"
      ((= entype "LWPOLYLINE")      ;LW���������� ���(ö���� �������� �׸� ���)
;        (princ "LWPOLYLINE")
        (setq pnt1 (getLwVert ent 0))
        (setq pnt2 (getLwVert ent 1))
        (setq cp (mid-point pnt1 pnt2))
        (setq plst (append plst (list cp)))
        (setq npnt (1+ npnt))
      ) ;of entype="LWPLOLYLINE"
      ((= entype "CIRCLE")                                ;��ƼƼ�� ��Ŭ�� ���
        (setq cp (cdr (assoc 10 ent)))                    ;��Ŭ�� ��Ÿ����Ʈ
        (setq plst (append plst (list cp)))               ;����Ʈ����Ʈ�� �߰�
        (setq npnt (1+ npnt))                             ;����Ʈ ���� ����
      ) ;of entype="CIRCLE"
    ) ;of cond
  (setq index (1+ index))                                 ;index=��ƼƼ��ȣ
  ) ; of repeat


  (setq ang (angle mpnt (nth 0 plst)))        ;ù���� ���� ���Ѵ�
  (setq minang ang)                           ;�ִ밢�� �ּҰ��� ù���� �������Ѵ�
  (setq maxang ang)
  (setq index 0)
  (repeat npnt                                ;����Ʈ ������ŭ �ݺ�
    (setq ang (angle mpnt (nth index plst)))  ;��ŷ���� ����Ʈ�� ���� ���� ���Ѵ�
    (if (<= ang minang) (setq minang ang))    ;�ִ밢�� �ּҰ� ã��
    (if (>= ang maxang) (setq maxang ang))
    (cecolor "1")
    (command "LINE" mpnt (nth index plst) "") ;����Ʈ���� ��ŷ�������� �����׸���
    (popcolor)
    (setq index (1+ index))                   ;����Ʈ������ŭ �ݺ� ��������Ʈ��.
  ) ;of repeat

  ;-------------------------------
  ; base line�� making/dia ����
  ;-------------------------------

  (setq p3 (getpoint mpnt "\nPick base line: "))              ;base line point

  (setq dx (- (car p3) (car mpnt)))
  (if (< dx 0)                                          ;base line�� x�����ν�
    (setq xsgn -1)
    (setq xsgn 1)
  ) ;of if

  (setq dy (- (cadr p3) (cadr mpnt)))
  (if (<  dy 0)                                         ;base line�� y�����ν�
    (setq ysgn -1)
    (setq ysgn  1)
  ) ;of if

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line�� ����

  (if (> (abs dx) (abs dy))                                             ;�����ֳ� ���ֳ�?
    (progn
      (setq p4 (list (+ (car mpnt) (* blen xsgn)) (cadr mpnt)))
      (cecolor "1")
      (command "LINE" mpnt p4 "")
      (popcolor)
      (if (< dx 0)
        (setq ip p4)
        (setq ip mpnt)
      ) ;of if
      (setq cp (list (+ (car ip) rc)
                     (+ (cadr ip) rc)))
      (setq diaxy (list (+ (car cp) (* diaxg ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 0)                                   ;textȸ����
    ) ;of progn
    (progn
      (setq p4 (list (car mpnt) (+ (cadr mpnt) (* blen ysgn))))
      (cecolor "1")
      (command "LINE" mpnt p4 "")
      (popcolor)
      (if (< dy 0)
        (setq ip p4)
        (setq ip mpnt)
      ) ;of if
      (setq cp (list (- (car ip) rc)
                     (+ (cadr ip) rc)))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* diaxg ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 90)
    ) ;of progn
  ) ;of if


  (cecolor "1")
  (command "CIRCLE" cp rc)
  (popcolor)


  (setq mk (getstring "\nEnter Marking: "))
  (txtinc mk cp txtrot)
  (setq dia (getstring "\nEnter Rebar Dia: "))
; (command "TEXT" diaxy (* 2.5 ds) txtrot (strcase dia))
  (cecolor "7")
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (popcolor)
;  (setvar "CECOLOR" oldclr)

  (pop-env)   ;ȯ�溯���� ����

;  (setq *error* oer seterr nil)
  (princ)


); of defun


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

(defun TXTINC(TXT IPNT TXTROT / th ds)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))               ;textũ��=ġ��ũ��

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
	(progn
	  (cecolor "7")
          (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
	  (popcolor)
        );progn  
        (progn
	  (cecolor "7")
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
	  (popcolor)
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (progn
      (cecolor "7")
      (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
      (popcolor)
    );progn
  ) ;of IF
) ;of DEFUN

; -------------------------------------
; function : getLwVert
; LwPolyline�� Vertex�� ô��
; �μ�: vlist  : vertext list
;       tmpctr : ������ vertext ��ȣ 0,1,2
; -------------------------------------

  (defun getLwVert (vlist tmpctr / count tmp)
;    (setq vlist (entget (car (entsel))))       		;��

    (setq count 0)					;ù vertex ã�ư�
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )
    ;; If the counter reaches the number of vertices,
    ;; reset ctr and tmpctr to zero again.
    (if (= tmpctr (cdr (assoc 90 vlist)))
        (progn
        (setq ctr 0)
        (setq tmpctr 0)
        )
    )
    (setq tmp (nth (+ count (* tmpctr 4)) vlist))
    (setq tmp (append tmp (list(cdr (assoc 38 vlist)))))
    (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))
;    (setq tmp (cons 10 pt1))
    (setq pt1 pt1)
  ) ;of defun
