;----------------------------------------------
; program : ASLAB
;           draw Approach slab
;           By Yi Suk-Jong
;           2000/6/24
;----------------------------------------------
(defun c:aslab(
               / ipnt gsel gent gpnt ent_type v_list salblen
                 slabt hunchl bracketl parapeth paveh tempnt1
                 tempnt2 tempnt3 tempnt4 ang270 slabepnt1
                 slabepnt2 hunchepnt1 hunchepnt2 ipnt1 ipnt2
                 bracketepnt
	       )
  (setq ipnt (getpoint "\nInsert point: "))
  
  (setq gsel (entsel "\nSelect design level: "))       
  (setq gent (entget (car gsel)))                
  (setq gpnt (cdr gsel))                        

;  (noripf-dialog)

;  (if (= #meter 1)                       
;     (setq steph 6.0 sodanb 1.0)        
;     (setq steph 6000 sodanb 1000)
;  );of if
  
  (if (> (caar gpnt) (car ipnt)) (setq sgn 1) (setq sgn -1))  

  (setq ent_type (cdr (assoc 0 gent)))
  (cond
    ((= ent_type "LWPOLYLINE")
      (setq v_list (car (mk_vertlist (car gsel))))               ;vertex list�����
    );subcond
    ((= ent_type "POLYLINE")
      (setq v_list (car (mk_vertlistp (car gsel))))              ;vertex list�����
    );subcond
    ((= ent_type "LINE")                                         ;line�� ���
      (setq v_list (list (cdr (assoc 10 gent)) (cdr (assoc 11 gent))))
    );subcond 
  );cond

  (setq slablen (getreal "\nLength of Approach slab: "))

  (if (> slablen 1000)
    (setq slabt 400
          hunchl 2000
	  bracketl 300
	  parapeth 880
	  paveh 80)
    (setq slabt 0.4
          hunchl 2
	  bracketl 0.3
	  parapeth 0.88
	  paveh 0.08)
  );if
  
  (setq tempnt1 (list (+ (car ipnt) (* sgn slablen)) (+ (cadr ipnt) slablen) 0)
	tempnt2 (list (car tempnt1) (- (cadr ipnt) slablen) 0)
	tempnt3 (list (+ (car ipnt) (* sgn (+ hunchl bracketl))) (+ (cadr ipnt) slablen) 0)
	tempnt4 (list (car tempnt3) (- (cadr ipnt) slablen) 0)
  );setq

  (setq slabepnt (car (cp_line_pline tempnt1 tempnt2 v_list))
        hunchepnt (car (cp_line_pline tempnt3 tempnt4 v_list))
  );	

  (setq ang270 (* pi 1.5))
  
  (setq slabepnt1 (polar slabepnt ang270 paveh)
	slabepnt2 (polar slabepnt1 ang270 slabt)
	hunchepnt1 (polar hunchepnt ang270 paveh)
	hunchepnt2 (polar hunchepnt1 ang270 slabt))

  (setq ipnt1 (polar ipnt ang270 paveh)
	ipnt2 (polar ipnt ang270 parapeth))

  (setq bracketepnt (list (+ (car ipnt2) (* sgn bracketl)) (cadr ipnt2) 0))

  (command "LINE" ipnt1 ipnt2 bracketepnt hunchepnt2 slabepnt2 slabepnt1 ipnt1 "")	
			  
  
);defun


; -------------------------------------
; function : mk_vertlist
; LwPolyline�� vertex list�� ������ش�.
; �μ�: vname  : vertext entity name
;                 (car (entsel)) ���·� �Ѿ�;��Ѵ�.
; -------------------------------------

  (defun mk_vertlist (vname
  /  vlist count nvert tmp vert_list pt1
                     );of local variable

    (setq vlist (entget vname))                          ;��ƼƼ ����

    (setq count 0)                                      ;ù vertex ã�ư�
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )                                                   ;ù° vertex ��ġ


    (setq nvert (cdr (assoc 90 vlist)))                 ;vertext��

    (setq vert_list nil)                                 ;�� list�����
    (setq index 0)                                      ;ùvertex����

    (repeat nvert
      (setq tmp (nth (+ count (* index 4)) vlist))     ;(10 x y)
      (setq tmp (append tmp (list (cdr (assoc 38 vlist)))))  ;z��ǥ�߰�
      (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))  ;ucs��ǥ�� ġȯ
      (setq vert_list (append vert_list (list pt1)))         ;vertexlist���߰�
      (setq index (1+ index))                                      ;���� vertext��
    ); repeat

     (setq vert_list (list vert_list nvert))
  ) ;of defun


; -------------------------------------
; function : mk_vertlistp
; Polyline�� vertex list�� ������ش�.
; �μ�: vname  : vertext entity name
;                (car (entsel)) ���·� �Ѿ�;��Ѵ�.
; -------------------------------------
(defun mk_vertlistp(vname
/
)

  (setq vert_list nil                             ;����Ʈ �ʱ�ȭ
        count 0                                   ;���� ����
        nxt vname)                                ;ù ��ƼƼ �̸�

  (while (setq nxt (entnext nxt))
    (progn
      (setq ent (entget nxt))                        ;��ƼƼ��������
      (if (= (cdr (assoc 0 ent)) "VERTEX")           ;�����϶���
        (setq vert_list (append vert_list (list (cdr (assoc 10 ent))))
              count (1+ count))                      ;�����߰�
      );if
    );progn
  );while

  (setq vert_list (list vert_list count))            ;�������

);defun


;------------------------------------
; function : cp_line_pline
;            cross point of line and polyline
;            Yi suk jong
;            99/10/8
;------------------------------------
; ��� : line�� pline�� ������ ã���ش�
;
; �Ѿ���� ��
;     pnt1 : ������ ù°��
;     pnt2 : ������ ��°��
;     plist: polyline�� point list
;
; �Ѿ�� �� : �������� list�� ��� �ѱ�
;-----------------------------------------------

(defun cp_line_pline(pnt1 pnt2 plist
/ pnt1 pnt2 plist n z pnt12d pnt22d cplist index pp1
  )
  (setq n (length plist))               ;polyline�� point����
  (setq z (nth 2 (nth 0 plist)))         ;polyline�� z��ǥ

  (setq pnt12d (list (car pnt1) (cadr pnt1))   ;xy��ǥ����
        pnt22d (list (car pnt2) (cadr pnt2)))  ;xy��ǥ����

  (setq cplist nil) ;����list����
  (setq index 1)

  (repeat (1- n)
    (setq pp1 (reverse (cdr (reverse (nth (1- index) plist))))  ;xy��ǥ����
          pp2 (reverse (cdr (reverse (nth     index  plist))))) ;xy��ǥ����
    (setq cp (inters pnt12d pnt22d pp1 pp2))        ;������ ã��
    (if cp
      (progn
        (setq cp (append cp (list z)))                ;z��ǥ�߰�
        (setq cplist (append cplist (list cp))) ;�������� ������ �� ���ϱ�
      );progn
    );if
    (setq index (1+ index))                     ;������������
  );repeat
    (if cplist cplist)                          ;�������� �����ϸ� ����

);defun  