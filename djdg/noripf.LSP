;-------------------------------
; program : noripf
;           Nori profile
;           by Yi Suk-Jong
;           2000/6/23
;-------------------------------
;���: �븮�� ���鵵�� �׷���
;����: �븮�� �������� ����ְ� ���ݼ��� ��������
(defun c:noripf(
  / steph ipnt gsel gent gpnt ent_type v_list count miny
    cy slop nextp sodanb sodanp cpnt
		)

  (setq #meter 1)
  
  (setq ipnt (getpoint "\nPick insert point: "))  ;�븮������
  (setq gsel (entsel "\nSelect Ground: "))        ;���ݼ�����
  (setq gent (entget (car gsel)))                 ;���ݼ�����
  (setq gpnt (cdr gsel))                          ;���ݼ�������

  (noripf-dialog)

  (if (= #meter 1)                        ;���� ���� �и����� ������ ����
     (setq steph 6.0 sodanb 1.0)          ;�Ҵ� ���� �� �Ҵ� �� ����
     (setq steph 6000 sodanb 1000)
  );of if
  
  (if (> (caar gpnt) (car ipnt)) (setq sgn 1) (setq sgn -1))   ;�븮��������ȣ

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

  (setq count 1                                 ;�ּ� y���� ó�� node������
	miny (nth 1 (nth 0 v_list)))
  
  (repeat (1- (length v_list))                  ;�ּ� y�� ���ϱ�
    (setq cy (nth 1 (nth count v_list)))
    (if (<= cy miny) (setq miny cy))
    (setq count (1+ count))
  );repeat
  
  (setq slop 1.5)                               ;���ʼҴ��� 1:1.5��

  
  (setq nextp (list (+ (car ipnt) (* slop sgn steph)) ;ù����� �� ��
		    (- (cadr ipnt) steph)))

  (setq count 0)

  (while (and (> (cadr ipnt) miny)          ;���� ���� �ּ�y������ �۰ų� ���ݼ��� ��ĥ ������ �ݺ�
	      (= (setq cpnt (car (cp_line_pline ipnt nextp v_list))) nil))
    (setq sodanp (list (+ (car nextp) (* sodanb sgn)) (cadr nextp)))
    (command "line" ipnt nextp sodanp "" )
    (2ptext ipnt nextp (strcat "1:" (rtos slop 2 1)))
    (setq count (1+ count))
    (if (> count 0) (setq slop 1.8))
    (setq nextp (list (+ (car sodanp) (* slop sgn steph))
               	      (- (cadr sodanp) steph)))
    (setq ipnt sodanp)
  ); while
  (command "LINE" ipnt cpnt "")
  (2ptext ipnt cpnt (strcat "1:" (rtos slop 2 1)))
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

;---------------------------
; function : noripf-dialog
;            2000/6/24
;---------------------------
; ���̾�α� �ڽ��� ���ؼ� noripf�� �ɼ��� �Է¹޴´�.
;---------------------------
(defun noripf-dialog(/  
                  dcl_id )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "noripf" dcl_id)) (exit))         ;ddscl.dcl ���� scl����  
  
  (set_tile "meter" "1")
  (set_tile "milimeter" "0")
  (set_tile "wslop" "1")  

  
  (action_tile "meter" "(setq #meter 1)")               ;user ���� box  
  (action_tile "milimeter" "(setq #meter 0)")                     ;grid ON/OFF toggle  
  (action_tile "wslop" "(set-wslop)")                     ;grid ON/OFF toggle  
  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun nori-DIALOG

(defun set-wslop()
  (setq #wslop (get_tile "wslop"))
)

;-----------------------------
; function : 2ptext
;            2 point text
;            By Yi Suk-Jong
;            2000/6/23
;-----------------------------
; ������ �ؼ����� �ϴ� text�� ������ش�.
;  �̶� �ؽ�Ʈ�� ũ��� dimscale x dimtxt �̸�
;  �ؽ�Ʈ�� ��ġ �� ȸ������ ġ���� ��Ģ�� ����
;  ���������.
;-----------------------------
(defun 2ptext(p1 p2 text
	      / p1 p2 text th ang wh4 ang2 tang txtpnt oldosmode)

  (setq th (* (getvar "DIMSCALE") (getvar "DIMTXT")))
  
  (setq ang (angle p1 p2))
  
  (setq wh4 (which4 ang))                              ;���и鿡 �ִ°�?

  (cond                                                ;1~4��и鿡 ���� ��
     ((= wh4 1)
       (setq ang2 ang)
       (setq tang (+ ang (* pi 0.5)))
     )
     ((= wh4 2)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))      
     )
     ((= wh4 3)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))      
     )
     ((= wh4 4)
       (setq ang2 (- ang (* 2 pi)))
       (setq tang (+ ang (* pi 0.5)))      
     )
  );of cond

  (setq txtpnt (polar (mid-point p1 p2) tang (* th 0.5)))  ;text point

  (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)
  (command "TEXT" "J" "C" txtpnt th (rtod ang2) text "")
  (setvar "OSMODE" oldosmode)

);defun of 2ptext