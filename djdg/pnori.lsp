; Program : PNORI 	:   Plan NORI
; function : mk_vertlistp:   Polyline�� vertex list�� ������ش�.
; function : near_vert   : nearest vertix
; function : PointOnPline : �־��� polyline�� �������� ���� ��ġ�� �ִ� ���� ��ǥ�� ����


;************************************
; Program : PNORI
;           Plan NORI
;           By Suk-Jong Yi
;           98/8/28
;************************************
; ������ �븮�� �׷��ش�. (�븮�������� ��±��� 4mm)

(defun C:PNORI(
/ sp ep dst ang delta_dst blk_name
  div_n ans rot_ang count
              ); of variable

  (defun SETERR(s)                                  ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  ;(setq oer *error* *error* seterr)                   ;���忡����ƾ ����

  (push-env)                                          ;ȯ�溯�� ����

;  (setq ent_pline (entget (car (entsel "\nSelect Polyline: ")))) ;polyline����
  (setq ent_pline (car (entsel "\nSelect Polyline: "))) ;polyline����

  (setq sp (getpoint "\nPick start point: "))
  (setq ep (getpoint sp "\nPick end point: "))

  (setq ent_type (cdr (assoc 0 (entget ent_pline))))
  (cond
    ((= ent_type "LWPOLYLINE")
      (setq v_list (mk_vertlist ent_pline))               ;vertex list�����
    );subcond
    ((= ent_type "POLYLINE")
      (setq v_list (mk_vertlistp ent_pline))               ;vertex list�����
    );subcond
  );cond

  ;poly line�� ���� ���� ���
  (setq nvlist (cadr v_list)                          ;vertext����
        v_list (car v_list))                          ;vertext list

;�������� ������ ���尡��� ��� ã��
  (setq spnode (near_vert v_list sp))               ;�������� ���尡��� ���
  (setq epnode (near_vert v_list ep))               ;������ ���尡��� ���

  (if (< epnode spnode)                             ;node������ �������� ������
    (setq v_list (reverse v_list)                   ;�ݴ��� ��� �����ٲپ���
          spnode (near_vert v_list sp)
          epnode (near_vert v_list ep))
  );if

  (princ "\nspnode: ") (princ spnode)
  (princ "\nepnode: ") (princ epnode)

;--- �������� ������� ã��

  (cond
    ((= spnode 0)
      (setq spnextnode 1))                          ;�ִ밡������� 0�̸�
    (T
      (if (< (distance (nth (1- spnode) v_list) sp) ;�ִ밡��� ����� �ճ�忡�� ���� ���������� �Ÿ�
             (distance (nth (1- spnode) v_list) (nth spnode v_list))) ;�ִ밡��� ����� �ճ�忡�� �ִ밡��� ������ �Ÿ�
        (setq spnextnode spnode)                    ;�������������� ���尡�����
        (setq spnextnode (1+ spnode))               ;��������������      "        ����
      ) ;if
    );subcond
  );of cond

;--- ���� �������
  (cond
    ((= epnode (1- nvlist))
      (setq epnextnode (1- nvlist)))                ;�ִ밡������� ���������̸�
    (T
      (if (< (distance (nth (1- epnode) v_list) ep) ;�ִ밡��� ����� �ճ�忡�� ���� ���������� �Ÿ�
             (distance (nth (1- epnode) v_list) (nth epnode v_list))) ;�ִ밡��� ����� �ճ�忡�� �ִ밡��� ������ �Ÿ�
        (setq epnextnode epnode)
        (setq epnextnode (1+ epnode))
      ) ;if
    );subcond
  );of cond

  (princ "\nspnextnode: ") (princ spnextnode)
  (princ "\nepnextnode: ") (princ epnextnode)

;--- ����ڰ� ������ ���������� �̷���� vertix����Ʈ�����
  (setq nvlist (list sp ))      ;����ڰ� ������ �������� �̷���� vertix list
  (setq nllist (list 0 ))       ;                "                 vertext ���� list

  (setq clen 0)                                 ;���� ���� = 0
  (setq lastpnt sp)                             ;�������� ����������

  (setq index spnextnode)

  (while (< index epnextnode)
    (setq crntpnt (nth index v_list))                           ;��������Ʈ
    (setq clen (+ clen (distance lastpnt crntpnt)))             ;�������
    (setq nvlist (append nvlist (list crntpnt)))                ;node�߰�
    (setq nllist (append nllist (list clen)))                   ;���������߰�
    (setq index (1+ index))                    ;��������
    (setq lastpnt crntpnt)                     ;�������� ��������
  );while

  (setq nvlist (append nvlist (list ep))                  ;����ڰ� ������ ������ ����߰�
        nllist (append nllist (list (+ clen (distance (nth (1- index) v_list) ep)))))

  (setq vlnum (length nllist)                   ;vlist�� ����
        vllen (nth (1- vlnum) nllist))          ;pline�� �ѱ���

  (princ "vlnum:") (princ vlnum)
  (princ "vllen:") (princ vllen)

  ;--- �븮 ���� �Է��ϱ�

  (setq noridir (getpoint "\nPick Nori Direction: "))
  (if (<= (dang (angle sp noridir) (angle sp ep)) 0) ;�������� �븮�� �׸����
    (setq d90 (* pi 0.5))                           ;-90��
    (setq d90 (* pi -0.5))                            ;+90��
  );if

  (setq norilength (getdist "\nLength of Nori: ")) ;�븮���� �Է�

;  (setq norilength 1)

  (setq gap_default (* 4 (getvar "DIMSCALE")))      ;�븮������(��±��� 4mm)
  (princ "\n�븮�� ���� <") (princ gap_default)
  (setq gap_nori (getdist ">: "))                   ;�븮�� �����Է�
  (if (= gap_nori nil) (setq gap_nori gap_default))

;  (setq gap_nori 0.1)

  ;--- �븮 �׸���
  (setq clen 0)                                     ;�������
  (setq ccnt 0)                                     ;���� ����

  (while (<= clen vllen)                            ;����븮��ġ�� �Ÿ���
    (setq tmp (PointonPline nvlist nllist clen))    ;������� ���� (��ǥ,node)
    (setq vsp (nth (1- (cadr tmp)) nvlist))          ;�������� �ִ� ������ ������
    (setq vep (nth (cadr tmp) nvlist))               ;          "          ����
    (setq seang (angle vsp vep))                      ;������-���� ��
    (setq p1 (car tmp))                                     ;�븮���� ������
    (if (= (rem ccnt 5) 0)                          ;�ټ������� �Ѱ��� ���
      (setq tmp norilength)                     ;�������� ª��(��)
      (setq tmp (* 0.5 norilength))             ;�������� ª��(��)
    );if
    (setq p2 (polar p1 (+ seang d90) tmp))      ;�븮���� ����
    (command "LINE" p1 p2 "")                   ;line�׸���
    (setq ccnt (1+ ccnt))                       ;ī��Ʈ �ϳ� ���ϱ�
    (setq clen (+ clen gap_nori))               ;����nori�����Ÿ�
  );while

;  (princ "vllist") (princ vllist)

  (pop-env)                         ;ȯ�溯�� ����

  (setq *error* oer seterr nil)     ;������ƾ ����

  (princ)
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

;--------------------------------------
; function : near_vert
;            nearest vertix
;            by Yi Suk-Jong
;            98/8/30
;--------------------------------------
; ���尡���  nodeã��
;--------------------------------------
(defun near_vert(vlst pnt
/ nvert middist near_node index dist
)
  (setq nvert (length vlst))            ;vertix�� ���ϱ�

  (setq mindist  (distance (nth 0 vlst) pnt))       ;�ִܰŸ� = ù������ �Ÿ�
  (setq near_node 0)                                ;������� 0

  (setq index 1)                                    ;ù������
  (repeat (1- nvert)
    (setq dist (distance (nth index vlst) pnt))     ;�־������� �������� �Ÿ�
    (if (<= dist mindist)                            ;�Ÿ��� ���� ������
      (setq mindist dist                            ;�ִܰŸ� ����
            near_node index)                        ;�ִ�node ����
    );if
    (setq index (1+ index))                         ;���� ����
  ); repeat
  (setq near_node near_node)                        ;���ϰ������ ����
) ;defun

;------------------------------------
; function : PointOnPline
;            By Yi Suk-Jong
;            98/9/3
;------------------------------------
; �־��� polyline�� ��������
; ���� ��ġ�� �ִ� ���� ��ǥ�� ����
;   Plist     : Polyline�� point list
;   Vllist    : Vertex �����Ÿ� list
;   Distfrom0 : Distance from start point
;   retuen : (list pnt node)  <-- pnt: x,y   node:��������ȣ
;-------------
(defun PointOnPline(Plist Vllist Distfrom0
 / plist Vllist distfrom0 nvertex pnt index node
                   )
  ;------ vertext���� ���ϱ�

  (setq nvertex (length Plist))

  (setq pnt nil)

  (setq index 1)
  (while (< index nvertex)
    (cond
      ((< Distfrom0 (nth index vllist))     ;�־��� �Ÿ��� ���̿� ���� ��
        (setq pnt (polar (nth (1- index) Plist)                 ;�ٷ� ����
               (angle (nth (1- index) Plist) (nth index Plist)) ;����
               (- Distfrom0 (nth (1- index) vllist))))          ;�Ÿ���
        (setq node index)                                       ;�������
        (setq index (1+ nvertex))                               ;while�� ������ ���� ū�� �Է�
      );sub cond
      ((= Distfrom0 (nth index vllist))     ;�־��� �Ÿ��� index��° ����϶�
        (setq pnt (nth index Plist))                            ;x,y��ǥ
        (setq node index)                                       ;����ȣ�� index
        (setq index (1+ nvertex))                               ;while�� ������ ���� ū�� �Է�
      );sub cond
      ( T (setq index (1+ index)))          ;�־��� �Ÿ��� ���̿� ���� ��
    );cond
  );while

  (if (= pnt nil)
    nil                                     ;pnt�� �ƹ� �Է� ������ nil return
    (list pnt node)                         ;pnt, node return
  );if

);defun

