;************************************
; Program : CSEC
;           Cross SECtion
;           By Suk-Jong Yi
;           99/10/8
;************************************
; ���� Ⱦ�ܵ��� �׷��ش�

(defun C:CSEC(
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

  (setq oer *error* *error* seterr)                   ;���忡����ƾ ����

  (push-env)                                          ;ȯ�溯�� ����
  (setq v_list nil)

  (setq polylines (ssget  '((-4 . "<OR")           ; lwpolyline�̳� polyline�� ����
                              (0 . "LWPOLYLINE")
                              (0 . "POLYLINE")
                           (-4 . "OR>"))))

  (setq sp (getpoint "\nPick start point: "))       ;������
  (setq ep (getpoint sp "\nPick end point: "))      ;����

  (setq n_polylines (sslength polylines))             ;���õ� polyline�� ����

  (setq count 0)
  (setq csec_list nil)                                      ;Ⱦ�� data����

  (repeat n_polylines                                       ;entity������ŭ �ݺ�

    (setq ent_pline (ssname polylines count))               ;entity�̸�

    (setq ent_type (cdr (assoc 0 (entget ent_pline))))
    (cond
      ((= ent_type "LWPOLYLINE")
        (setq v_list (mk_vertlist ent_pline))               ;vertex list�����
      );subcond
      ((= ent_type "POLYLINE")
        (setq v_list (mk_vertlistp ent_pline))              ;vertex list�����
      );subcond
    );cond

    ;poly line�� ���� ���� ���
    (setq nvlist (cadr v_list)                          ;vertext����
          v_list (car v_list))                          ;vertext list

    (setq cp_list (cp_line_pline sp ep v_list))         ;������ ã��

    (if (/= cp_list nil)                                 ;������ �����ϸ�
      (setq csec_list (append csec_list cp_list))        ;Ⱦ�� ���� �߰�
    );if

    (setq count (1+ count))                             ;���� polyline����
  );repeat

  (setq csec_count 0)
  (repeat (length csec_list)
    (command "point" (nth csec_count csec_list))
    (setq csec_count (1+ csec_count))
  );repeat

  ;�������� �������������� �Ÿ� �߰��ϱ�
  (setq csec_d_list nil)                        ;�Ÿ��� �߰��� point list����
  (setq csec_count 0)
  (repeat (length csec_list)
    (setq nth_point (nth csec_count csec_list))
    (setq csec_d_list
      (append csec_d_list
        (list  (append nth_point
                  (list (distance (list (car sp) (cadr sp) 0)
                          (list (car nth_point) (cadr nth_point) 0)
                        )
                  )
               )
        )
      )
    )
    (setq csec_count (1+ csec_count))           ;���� point��
  );repeat

;  (princ csec_d_list)
  (setq csec_d_list (sort_plist csec_d_list 3))
;  (princ csec_d_list)

  (draw_csec csec_d_list)

);defun CSEC

; ������ elevation������ �̿��� Ⱦ�ܸ鵵 �׸���
(defun draw_csec(csec_data
 / ip first_dist first_z csec_count nth_csec current_dist current_z pnt)
  (setq ip (getpoint "\nPick Insert point: "))
  (setq first_dist (nth 3 (nth 0 csec_d_list))) ;ù��° ������ sp�κ��� �Ÿ�
  (setq first_z    (nth 2 (nth 0 csec_d_list))) ;ù��° ������ z��
  (command "pline" ip)

  (setq csec_count 1)
  (repeat (1- (length csec_list))
    (setq nth_csec (nth csec_count csec_d_list))
    (setq current_z    (nth 2 nth_csec)
          current_dist (nth 3 nth_csec))
    (setq pnt (list (+ (car ip) (- current_dist first_dist))
                    (+ (cadr ip) (- current_z first_z)) 0))
    (command pnt)
    (setq csec_count (1+ csec_count))
  );repeat
    (command "")
);defun draw_csec



(defun test()

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
;/ pnt1 pnt2 plist n z pnt12d pnt22d cplist index pp1
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

;******************************************************
; Function : SORT_PLIST
;           SORT Polyline LIST
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

(defun SORT_plist(alist aidx
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
      (if (<= c_val min)                             ;���� ���� min���� ������
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
