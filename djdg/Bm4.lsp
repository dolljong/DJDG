;**************************************    
; Program : BM4    
;           Rebar Marking-4    
;           Suk-Jong Yi    
;           1995. 3. 17    
;**************************************    
; ö�ٸ�ŷ�� �Ʒ��� ����� ������ش�.    
;    -----------��    
;  /  /  /  /    
;**************************************    
    
(defun C:BM4(/    
plst crsp-lst    
)    
    
  (defun SETERR(s)                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
(push-env)    
    
(setq ds (getvar "DIMSCALE")    
      th (getvar "DIMTXT"))    
    
;(setq cr (* ds 3.5))    
;(setq th (* ds 2.5))    
(setq cr (* ds 4.5))            ;��ŷ���� ũ��    
(setq th (* ds th))             ;text�� ũ��    
    
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
;      (princ "LWPOLYLINE")    
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
  (cecolor "1")
  (command "LINE" pnt crs-pnt "")               ;������ �ױ�  
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
(setq ccen (polar blpnt2 (+ blang pi) cr))    
    
;* ���̽� ���� ��ŷ�� �� ö�� ���� ǥ��    
(cecolor "1")
(command "LINE" maxpnt blpnt2 "")                       ;���̽����� �׸���    
(command "CIRCLE" ccen cr)                              ;��ŷ�� �׸���    
(popcolor)
(setq mk (getstring "\nEnter Marking: "))               ;��ŷ��Ī �Է�    
(txtinc mk ccen 0.0)                                    ;��ŷ��Ī ����    
(setq dia (getstring "\nEnter Rebar Dia: "))            ;ö�� ���̾� �Է�    
(setq diaxy (list (+ (car ccen) (* 4 ds)) (- (cadr ccen) (* 4 ds)) 0.0))
(cecolor "bylayer")
(command "TEXT" diaxy th "0" (strcase dia))          ;ö�ٴ��̾� ��  
(popcolor)
    
(pop-env)    
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
; LwPolyline�� Vertex�� ô�Ƴ�    
; �μ�: vlist  : vertext list    
;       tmpctr : ������ vertext ��ȣ 0,1,2    
; -------------------------------------    
    
  (defun getLwVert (vlist tmpctr / count tmp)    
;    (setq vlist (entget (car (entsel))))                       ;�����    
    
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
    
