;----------------------------------------------
; program : ASLAB1
;           draw Approach slab for Ascon
;           By Yi Suk-Jong
;           2000/6/24
;----------------------------------------------
(defun c:aslab1(
               / ipnt gsel gent gpnt ent_type v_list salblen
                 slabt hunchl bracketl parapeth paveh tempnt1
                 tempnt2 tempnt3 tempnt4 ang270 slabepnt1
                 slabepnt2 hunchepnt1 hunchepnt2 ipnt1 ipnt2
                 bracketepnt
	       )

  (setq bbracket 300
	tslab    400
	tbase    100
	bbase    100
	slop     4)
  
  (setq ipnt (getpoint "\nInsert point: "))
  (setq sidepnt (getpoint "\nPick side point: "))
  (setq len (getreal "\nLength of A-SLAB(m): ")
	len (* 1000 len))

  
  (setq ipntx (car ipnt)
	ipnty (cadr ipnt))

  (if (> ipntx  (car sidepnt))
    (setq bbracket (* bbracket -1)
	  bbase (* bbase -1)
	  len (* len -1))
  )  

  (setq p1 (list             ipntx  (+ ipnty tslab))
	p2 (list (+ ipntx bbracket) (cadr p1))
	p3 (list (+ ipntx len) (- (cadr p2) (* (abs (- len bbracket)) (* slop 0.01))))
	p4 (list (car p3) (- (cadr p3) tslab))
	p5 (list (+ ipntx bbracket) ipnty)
	p6 (list (+ (car p4) bbase) (- (cadr p4) (abs (* bbase (* slop 0.01)))))
	p7 (list (car p5) (- (cadr p5) tbase))
	p8 (list (car p6) (- (cadr p6) tbase))
  );setq 

  (command "line" ipnt p1 p2 p3 p4 "")    ;draw aslab
  (command "line" ipnt p5 p6 p8 p7 "")    ;draw base con'c
	
  
  
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