;**************************************            
; Program : B4            
;           Rebar marking-4            
;           Suk-Jong Yi            
;           96/7/29            
;**************************************            
; ö�ٸ�ŷ�� �Ʒ��� ����� ������ش�.            
;    -----------��            
;  /  /  /  /            
;**************************************            
            
(defun C:B4(/            
 th plst crsp-lst            
)            
            
  (defun SETERR(s)                              ;���忡����ƾ ����
    (if (/= s "Function cancelled")            
        (princ (strcat "\nError: " s))            
    ); of If            
    (setq *error* oer seterr nil)            
    (princ)            
  ); of SETERR            
  (setq oer *error* *error* seterr)            
            
  (push-env)                                    ;ȯ�溯�� ����            
            
  (setq ds (getvar "DIMSCALE")                    ;scale ��            
        th (getvar "DIMTXT")                      ;textũ��� ġ��ũ��� ����            
        rc (* ds 5.0)                             ;��ŷ���� ������            
        diaxg 6)                                  ;������������ ������������� x�Ÿ�  
          
;  (setq oldclr (getvar "CECOLOR"))            
;  (setvar "CECOLOR" "RED")            
            
  (setq entlst (ssget))                           ;��ŷ��� entity����            
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
        (setq ep (cdr (assoc 11 ent)))            ;������ ����            
        (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;�����߰��� X            
                       (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y            
        (setq plst (append plst (list mp)))       ;�߰����� ��ŷ����Ʈ�� �߰�            
        (setq npnt (1+ npnt))                     ;��ŷ����Ʈ�� ���� ����            
      ) ;of entype="LINE"            
      ((= entype "POLYLINE")      ;���������� ���(ö���� �������� �׸� ���)            
        (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;ù������            
        (if (= (abs (cdr (assoc 42 vtx1))) 1.0)         ;���������� ��ũ���ΰ�?            
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
            
  ;* base line ���۰� ������Ʈ �Է�            
  (setq blpnt1 (getpoint "\nPick base line first point: "))            
  (setq blpnt2 (getpoint blpnt1 "\nPick base line second point: "))            
  (setq blang (angle blpnt2 blpnt1))          ;���̽������� ����(��Ű����������)            
            
  ;*ù��° ����Ʈ�� ���̽����ΰ� ������ ��            
  (setq fpnt (nth 0 plst))            
  (setq fang (angle blpnt2 fpnt))                                 ;ù°����Ʈ�� ��            
  (setq dtasgn (/ (dang blang fang) (abs (dang blang fang))))     ;ù���� ��ȣ            
  (setq ll-ang (+ blang (/ pi 4.0 dtasgn) pi))              ;���������� ��            
  (setq tmppnt (polar fpnt ll-ang 1.0))            
  (setq fcrs-pnt (inters blpnt1 blpnt2 fpnt tmppnt nil)) ;ù��° ũ�ν�����Ʈ            
            
            
  ;* ��ŷ������ ���� �ָ� ������ ����Ʈ ã��            
  (setq fdst (distance blpnt2 fcrs-pnt))          ;ù���� �Ÿ��� ���Ѵ�            
  (setq maxdst fdst)                              ;�ִ�Ÿ��� ù���� �Ÿ��� �Ѵ�            
  (setq index 0)            
  (repeat npnt                                    ;����Ʈ ������ŭ �ݺ�            
    (setq pnt (nth index plst))                   ;���� ��            
    (setq tmppnt (polar pnt ll-ang 1.0))          ;�ӽ���            
    (setq crs-pnt (inters blpnt1 blpnt2 pnt tmppnt nil))   ;ũ�ν�����Ʈ            
    (setq crsp-lst (append crsp-lst (list crs-pnt)))  ;ũ�ν� ����Ʈ ����Ʈ�� �߰�            
    (cecolor "red")
    (command "LINE" pnt crs-pnt "")               ;������ �׸���            
    (popcolor)
    (setq dst (distance blpnt2 crs-pnt))          ;��ŷ���� ����Ʈ�� �������� �Ÿ�            
    (if (>= dst maxdst)                           ;�ִ� �Ÿ� ����Ʈ�ΰ�?            
      (progn            
        (setq maxpnt crs-pnt)            
        (setq maxdst dst)            
      ) ;of progn            
    ) ;of if            
    (setq index (1+ index))                       ;����Ʈ������ŭ �ݺ�            
  ) ;of repeat            
            
  ;�ּ� �Ÿ� ����Ʈ ã��            
  (setq maxdst 0)            
  (setq index 0)            
  (repeat npnt            
    (setq pnt (nth index crsp-lst))            
    (setq dst (distance maxpnt pnt))            
    (if (>= dst maxdst)            
      (progn            
        (setq minpnt pnt)            
        (setq maxdst dst)            
      ) ;of progn            
    ) ;of if            
    (setq index (1+ index))            
  ) ;of repeat            
            
            
  ;* ���̽����� üũ            
  (setq mx-bl2dst (distance maxpnt blpnt2))            
  (setq mx-mindst (distance maxpnt minpnt))            
  (if (<= mx-bl2dst mx-mindst)            
    (setq blpnt2 (polar minpnt (+ blang pi) (* ds 7.0))))            
            
  ;* ��ŷ���� �߽���            
  (setq ccen (polar blpnt2 (+ blang pi) rc))            
            
  ;* ���̽� ���� ��ŷ�� �� ö�� ���� ǥ��            
  
  (cecolor "red")
  (command "LINE" maxpnt blpnt2 "")                    ;���̽����� �ױ�  
  (popcolor)            
            
  (setq mpnt blpnt2)            
            
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
      (cecolor "red")
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
      (cecolor "red")
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
            
            
  (cecolor "red")
  (command "CIRCLE" cp rc)
  (popcolor)

            
;  (setvar "CECOLOR" "WHITE")            
  (setq mk (getstring "\nEnter Marking: "))            
  (txtinc mk cp txtrot)            
  (setq dia (getstring "\nEnter Rebar Dia: "))
  (cecolor "bylayer")
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))            
  (popcolor)
            
  (pop-env)   ;ȯ�溯���� ����            
            
  (setq *error* oer seterr nil)            
  (princ)            
            
) ;of defun BM4            
            
;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; �� ���� ö�ٹ�ȣ�� �������ش�.
; �Ѿ���� ��
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ds)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))               ;text�ݱ�=ļ���ݱ�

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
	(progn
	  (cecolor "bylayer")
          (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
	  (popcolor)
	);progn
        (progn
	  (cecolor "bylayer")
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
	  (popcolor)
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (progn
      (cecolor "bylayer")
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
;    (setq vlist (entget (car (entsel))))                       ;��            
            
    (setq count 0)                                      ;ù vertex ã�ư�            
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