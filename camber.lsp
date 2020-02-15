;*****************************************
;       CAMBER
;           draw CAMBER
;           Jong-Suk Yi
;           1996. 2. 12
;*****************************************
; �� Ǯ�׸��� ó����(CAMBER)�� �׷��ش�.
; DATA �Է»����� (������ȣ, X-��ǥ, Y-��ǥ) �̴�.
;  update list
; --- 03/07/14(��) 
;   - ķ������ factor�� �� �� �ֵ��� ����
;   - ķ������ precision�� ������ �� �ֵ��� ����
;   - Dimzin���� 0���� setting�� ����

(defun C:CAMBER(
 /               gap     bdrx     bdry    yfac   rh       fcw      trg     th
                 fn      opf      nlist   comb   drw      ch       nnum    ncamb
                 ncomb   ndraw    xmax    xmin   xscale   yscale   cw      node
                 X       YC       ccount  count  cnt_comb draw_cnt
                 ncmb    maxy_lst n_node  maxy   n_case   cn       n_y
                 camby   firstx   lastx   nd     xx       bpnt     ap      b1
                 b2      cp       txtpnt  yy     oldc     cmb_cnt  c_count
                 n_count pnt      table   ntbl   tblh     maxn     tcount  node1
                 node2   tbly     tblpx   tblpy
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)

  ;----- ���� �ʱⰪ ���� (Camber��)
  (setq gap (getint "\nLEFT MARGIN (mm)<60>: "))    ;������ camber������ ����(����)
  (if (= gap nil) (setq gap 60))                    ;return�Է½� gap=60mm
  (setq bdrx 780)                                   ;border�� xũ��
  (setq bdry 557)                                   ;border�� yũ��
;  (setq yfac 100)                                   ;y-factor
;  (setq yfac 1000)                                   ;y-factor

  ;----- ���� �ʱⰪ ���� (Camber table)
  (setq rh 7)                                     ;row height (table �ٳ���)
  (setq fcw 20)                                   ;first column width
  (setq trg 2)                                    ;text right gap

  (setvar "CMDECHO" 0)                            ;command echo off
  (setvar "BLIPMODE" 0)                           ;blip mode off
  (setvar "DIMzin" 0)
  (setq th (getvar "DIMTXT"))                     ;text size ����

  (command "ZOOM" "W" "0,0" (list bdrx bdry))       ;������ �� ���� ���̱�

  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))    ;file name�Է�
  (setq opf (open fn "r"))                        ;file open

  (setq nlist nil)                                ;�� node-list ����
  (setq comb nil)                                 ;�� ���� ����
  (setq drw nil)                                 ;�� draw-list����

  ;----- data file�κ��� DATA�Է�

  (if opf                                         ;file�� ���� ���
    (progn
       (while (and (/= (setq ch (read-line opf)) nil)         ;������ ��
                   (/= (strcase ch) "COMB")                   ;comb��� �ܾ�
                   (/= (strcase ch) "DRAW")                   ;comb��� �ܾ�
                   (/= ch ""))                                ;��ĭ
            (setq nlist (append nlist (list (strloc ch))))    ;nlist�� �߰�
       ) ;of while
       (if (or (= ch "COMB") (= ch "comb"))                 ;comb�϶�
         (while (and (/= (setq ch (read-line opf)) nil)     ;comb����Ÿ �б�
                     (/= ch "")
                     (/= (strcase ch) "DRAW"))
            (setq comb (append comb (list ch)))
         ) ;of while
       ) ;of if
       (if (or (= ch "DRAW") (= ch "draw"))
         (while (and (/= (setq ch (read-line opf)) nil)
                     (/= ch ""))
           (setq drw (append drw (list (strloc ch))))
         ) ;of while
       ) ;of if
    ) ;of progn
    (princ "\nFile not found")                    ;file�� ���� ���
  ) ;of if
  (close opf)                                     ;file close

  ;----- �Է� line�� �� ����
  (setq nnum (length nlist))                         ;�Է� line�� ����
  (setq ncamb (- (length (nth 1 nlist)) 2))          ;camber�� ��
  (setq ncomb (length comb))                          ;�Է� Combination�� ����
  (setq ndraw (length drw))                          ;draw��

  (if (= drw nil)
    (progn (princ "\nDRAW not found") (exit))) ;draw���� �� ����

  ;----- �Է� line���� ǥ������
  (princ nnum) (princ "-NODE / ")
  (princ ncamb) (princ "-CAMBER(S) / ")
  (princ ncomb) (princ "-COMBINATION(S) / ")
  (princ ndraw) (princ "-DRAWING(S) FOUND")

  (setq xmax (atof (nth 1 (nth (1- nnum) nlist))))  ;�� ������ x��
  (setq xmin (atof (nth 1 (nth 0 nlist))))           ;ù�� X��

  (setq xscale (/ (- bdrx (* 2 gap)) xmax))          ;�������� x scale(���)
;  (setq yscale (getint "\nY-Scale: "))               ;y���� scale(�Է°�)

  (setq node nil)                                     ;node list �ʱ�ȭ
  (setq    x nil)                                     ;x�� list�ʱ�ȭ
  (setq   YC nil)                                     ;Y�� list�� list

  ;----- node list, x�� list �����
  (setq count 0)
  (repeat nnum                                        ;node������ŭ
    (setq node (append node (list (nth 0 (nth count nlist)))))
    (setq X    (append x (list (atof (nth 1 (nth count nlist))))))
    (setq count (1+ count))                           ;���� node��
  ) ;of repeat

  ;----- �ּ� column�� �� ���ϱ�/�Է¹ޱ�
  (setq min_xlen (* (+ (strlen (rtos (nth 0 (reverse x)) 2 3)) 2) th))
  (princ "\nColumn width (mm)<")
  (princ min_xlen)
  (setq cw (getint ">: "))                      ;camber table ĭ ���� �Է¹���
  (if (< cw min_xlen) (setq cw min_xlen))       ;�ּ������� ������ �ּ�������
  (setq cw (fix min_xlen))

  ;----- ķ���� factor�Է¹ޱ�
  
  (setq yfac (getreal "\nEnter Factor of Value <1.0>: "))
  (if (= yfac nil) (setq yfac 1.0))
  (setq vprec (getint "\nEnter Precision <3>: "))
  (if (= vprec nil) (setq vprec 3))


  ;----- Y�� list �����
  (setq ccount 2)
  (repeat ncamb                                           ;camber����ŭ
    (setq count 0)                                        ;ù������..
    (setq y nil)                                          ;y list �ʱ�ȭ
    (repeat nnum                                          ;������ ������..
      (setq cy (atof (nth ccount (nth count nlist))))     ;y�� ����
      (setq Y (append Y (list cy)))
      (setq count (1+ count))
    ) ;of repeat
    (setq YC (append YC (list Y)))                        ;y list ���� �����
    (setq ccount (1+ ccount))                             ;����camber��
  ) ;of repeat

  ;----- combination��� Y�� list�� �߰��ϱ�
  (setq cnt_comb 0)
  (repeat ncomb
    (setq yc (append yc (list (op (nth cnt_comb comb) yc)))) ;comb�� ����Ͽ� yc�� �߰�
    (setq cnt_comb (1+ cnt_comb))                             ;���� comb��
  ) ;of repeat

  (setq n_camber (length yc))                           ;camber�� �Ѽ�

  ;----- �ִ� y�� �� yscale���ϱ�
  (setq maxcy (abs (nth 0 (nth 0 yc))))                 ;�ִ� y���� ó�� ��
  (setq count_c 0)
  (repeat n_camber                                      ;camber�� ��ŭ
    (setq count_n 0)
    (repeat nnum                                        ;node�� ��ŭ
      (setq cy (nth count_n (nth count_c yc)))          ;y�� ����
      (if (> (abs cy) maxcy) (setq maxcy (abs cy)))     ;�ִ�y��ã��
      (setq count_n (1+ count_n))                       ;���� node��
    ) ;of repeat
    (setq count_c (1+ count_c))                         ;���� camber��
  ) ;of repeat
  (setq yscale (/ xmax 10 maxcy))                       ;y-scale = x������ 1/10

  ;----- camber�� �׸���
  (setq draw_cnt 0)
  (repeat ndraw                                  ;draw������ŭ �ݺ�
    (setq ncmb (length (nth draw_cnt drw)))      ;camber���� �� case��

    ;---- �������� y��ǥ ���ϱ�(���밪 y max)
    (setq maxy_lst nil)                             ; max-Y list �ʱ�ȭ
    (setq n_node 0)                                 ;ù node����
    (repeat nnum                                    ;case����ŭ �ݺ�
      (setq n (1- (atoi (nth 0 (nth draw_cnt drw)))))
      (setq maxy (nth n_node (nth n yc)))           ;ù��° case�� �ִ밪
      (setq n_case 0)                               ;��° case����
      (repeat ncmb
        (setq cn (1- (atoi (nth n_case (nth draw_cnt drw))))) ;case��ȣ
        (setq n_y (nth n_node (nth cn yc)))         ;n���� node�� y��
        (if (> (abs n_y) (abs maxy))                ;���� max���� Ŭ��
          (setq maxy n_y)
        ) ;of if
        (setq n_case (1+ n_case))                  ;���� case��
      ) ;of repeat                                 ;maxy list�� �߰�
      (setq maxy_lst (append maxy_lst (list maxy)))
      (setq n_node (1+ n_node))
    ) ;of repeat

    ;---- ���� �׸���
    (setq camby (+ 0.3 (* (/ 0.7 ndraw) (- ndraw (1+ draw_cnt)))))      ;camber�� y��ġ

    (setq firstx (list (+ (* xscale xmin) gap) (* bdry camby)))  ;ù x��
    (setq lastx (list (+ (* xscale xmax) gap) (* bdry camby)))   ;������ x��
    (command "LINE" firstx lastx "")                 ;base line�׸���

    ;---- node��ȣ/pier/�������׸���
    (setq count 0)                                  ;ù������..
    (repeat nnum                                    ;������ ������..
      (setq nd (nth count node))                    ;node����
      (setq xx (nth count x))

      (setq bpnt (list (+ (* xx xscale) gap) (* bdry camby)))  ;base point

      (if (= (substr nd 1 1) "+")               ;node�� fix��ǥ�� ������
        (progn
          (command "LINE" bpnt "@3<-60" "@3<180" "C")   ;fix�� ǥ��(�ﰢ��)
          (setq nd (substr nd 2))                   ;fix�� ǥ��(+)����
          (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier��ȣ ����
        )
        (if (= (substr nd 1 1) "-")            ;node�� move��ǥ�� ������
          (progn
            (command "LINE" bpnt "@3<-60" "@3<180" "C") ;move�� ǥ��(�ﰢ��)
            (setq b1 (list (- (+ (* xx xscale) gap) 1.5) ;���� ������
                           (- (* bdry camby) 3)))
            (setq b2 (list (+ (+ (* xx xscale) gap) 1.5) ;���� ����
                           (- (* bdry camby) 3)))
            (command "LINE" b1 b2 "")                   ;���� �׸�
            (setq nd (substr nd 2))                 ;move�� ǥ��(-)����
            (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier��ȣ ����
          ) ;of progn
        );of if
      ) ;of if

      (if (/= ap nil)                                     ;Abut/Pier�� ���
        (progn
          (setq cp (list (car bpnt) (- (cadr bpnt) 12)))  ;���� �߽�
          (command "CIRCLE" cp "4.0")                     ;���׸���
          (command "TEXT" "M" cp 3.0 0.0 ap)              ;text����
          (setq ap nil)
        ) ;of progn
      ) ;of if

      (setq nd (atoi nd))                     ;node�� ������
      (setq nd (itoa nd))                     ;node�� ���ڿ���(��������)
      (setq txtpnt (list (+ (* xx xscale) gap) (- (* bdry camby) 5))) ;text pnt
      (command "TEXT" "M" txtpnt th "0" nd)     ;node ��ȣ����

      (setq count (1+ count))
    ) ;of repeat
                                     ;���� node��
    ;----- �������׸���
    (setvar "CECOLOR" "1")
    (setq count 0)                                  ;ù������..
    (repeat nnum                                    ;������ ������..
      (setq xx (nth count x))
      (setq bpnt (list (+ (* xx xscale) gap) (* bdry camby)))  ;base point
      (setq yy (+ (* (nth count maxy_lst) yscale -1 xscale) (* bdry camby)))
      (command "LINE" bpnt
                      (list (car bpnt) yy) "")

      (setq count (1+ count))
    ) ;of repeat                                      ;���� node��
    (setvar "CECOLOR" "BYLAYER")

    ;---- camber�׸���
    (setq oldc (getvar "CECOLOR"))                    ;����� ����
    (setq cmb_cnt 0)
    (repeat ncmb
      (setq c_count (1- (atoi (nth cmb_cnt (nth draw_cnt drw)))))
      (setvar "CECOLOR" (itoa (1+ c_count)))               ;�� ����
      (setq n_count 0)
      (command "PLINE")
      (repeat nnum
        (setq xx (nth n_count x))
        (setq yy (nth n_count (nth c_count yc)))
        (setq pnt (list (+ (* xx xscale) gap)                         ;camber ��
                        (+ (* yy yscale -1 xscale) (* bdry camby))))

        (command pnt)                                             ;camber�� �׸���
        (setq n_count (1+ n_count))
      ) ;of repeat
      (command "")
      (setq cmb_cnt (1+ cmb_cnt))
    ) ;of repeat                                            ;���� camber��
    (setvar "CECOLOR" oldc)                                 ;���� ������
    (setq draw_cnt (1+ draw_cnt))
  ) ;of repeat                                              ;���� draw��

  ;------ table�׸���
  (setq table (append (list x) yc))
  (setq  ncamb (1+ n_camber))
  (setq ntbl (+ (/ (* (1+ nnum) cw) (- bdrx (* gap 2))) 1))  ;table����
  (setq tblh (* rh (+ 2 ncamb)))                             ;table ����

  (setq maxn (/ (- bdrx (* gap 2)) cw))         ;���� �ִ� node��

  (setq tcount 0)                               ;table count
  (repeat ntbl
    (setq node1 (1+ (* tcount maxn)))           ;���� node
    (setq node2 (* (1+ tcount) maxn))           ;�� node
    (if (> node2 nnum)                          ;�� node�� ������ node���� ũ��
      (setq node2 nnum))

    (setq tbly (/ camby 2.0))                   ;camber table y��ġ
    (setq tblpx (- gap fcw)
          tblpy (- (* bdry tbly) (* tcount (+ tblh cw))))   ;table�� ������ġ

    (camb_table tblpx tblpy node1 node2 table)              ;table�׸���

    (setq tcount (1+ tcount))
  ) ;of repeat

;  (setq *error* oer seterr nil)
  (princ)
) ;;of defun



;*******************************************************************
;     Function : STRLOC
;                get STRing LOCation
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; �� �Լ��� ,�� �Ҹ��� data�� ������ �Ѱ��� list�� �����ش�.
; �̶� ����ȯ ���� ��� data�� ���ڿ��� return�ȴ�.
;******************************************************************
(defun STRLOC(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;�Ѿ�� ���ڿ�
   (setq strl (strlen arg1))                    ;�Ѿ�� ���ڿ��� ����
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;������� ��ġ
   (setq nchr 1)                                ;���⹮�� ����
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;���� �Ѱ�
      (if (or (= subs ",") (= subs ""))         ;���� ���ڰ� ,�̰ų� ���϶�
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;������ġ����
            (if (= rslt nil)
               (setq rslt (list lst))                  ;�������� �������
               (setq rslt (append rslt (list lst)))    ;���������� �߰�
            ) ;of if
            (setq nchr 0)                       ;���ⰹ�� �ٽ� 0����
            (setq strt (1+ count))              ;���� ��������� �������ڷ�
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;���� ���ڷ�
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;���� ���� �Ѱ� ����
   ) ;of repeat
   (setq arg1 rslt)                             ;������ ����
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;****************************************************
; Function : CAMB_TABLE
;            CAMBer TABLE
;            Yi Suk-Jong
;            1996/2/14
;****************************************************
; �� �Լ��� �־��� data�� ������ Table������ش�
;  �޴� ����
;      ax : table�� ��������� x��ġ
;      ay : table�� ��������� y��ġ
;  anode1 : ���� node��ȣ
;  anode2 : �� node��ȣ
;  anlist : �Էµ� data list
;****************************************************

(defun CAMB_TABLE(ax ay anode1 anode2 anlist
/                 ipnt     noden     xl      oldc     hl       y        st_pnt
                  end_pnt  ey        f_sp    f_ep     vl       nx       sp
                  ep       nn        cc      nx       ny       nxy      nd
                  ncx      nc        ncy     ncoord   nctxt    ncxy
)

  (setq noden (+ 1 (- anode2 anode1)))              ;node ����
  (setq xl (+ fcw (* noden cw)))                    ;x����

  (setq oldc (getvar "CECOLOR"))                    ;���� �� ����
  (setvar "CECOLOR" "RED")                          ;table�� ���� ����������


  ;----- ���� �׸���
  (setq hl 0)                                       ;���� count

  (repeat (+ ncamb 2)                               ;���򼱱׸���
    (setq y (- ay (* hl rh)))                       ;���� y��
    (setq st_pnt (list ax y))                       ;���� ������
    (setq end_pnt (list (+ ax xl) y))               ;���� ����
    (command "LINE" st_pnt end_pnt "")              ;���� �׸���
    (setq hl (1+ hl))                               ;���� ����
  ) ;of repeat

  ;----- ������ �׸���
  (setq ey (- ay (* (+ ncamb 1) 7)))                ;�������� ���� y��
  (setq f_sp (list ax ay))                          ;ù��° start point
  (setq f_ep (list ax ey))                          ;ù��° end point
  (command "LINE" f_sp f_ep "")                     ;ù��° ������ �׸���

  (setq vl 0)                                       ;������ count

  (repeat (1+ noden)                                ;node���� ��ŭ �ݺ�
    (setq nx (+ ax fcw (* vl cw)))                  ;n��°�� X��ǥ
    (setq sp (list nx ay)                           ;������
          ep (list nx ey))                          ;����
    (command "LINE" sp ep "")                       ;������ �׸���
    (setq vl (1+ vl))                               ;���� ����������
  ) ;of repeat

  (setvar "CECOLOR" oldc)                           ;���� �ǵ�����

  ;----- node/x/camber����
  ;----- node��ȣ ����
  (setq nn anode1)                                  ;node count
  (setq cc 1)                                       ;column count

  (repeat noden                                     ;node���� ��ŭ �ݺ�
    (setq nx (+ ax fcw (/ cw 2.0) (* (1- cc) cw)))  ;n��° node�� x��ǥ
    (setq ny (- ay (/ rh 2.0)))                     ;node�� y��ǥ
    (setq nxy (list nx ny))                         ;node�� xy��ǥ
    (setq nd (nth (1- nn) node))                ;node��ȣ �Է°�
    (if (or (= (substr nd 1 1) "-") (= (substr nd 1 1) "+"))  ;Abut/Pier�ΰ��
      (setq nd (itoa (atoi (substr nd 2))))         ;ù����(+/-)�� ���� ����
      (setq nd (itoa (atoi nd)))                    ;���� ����
    ) ;of if
    (command "TEXT" "M" nxy th 0.0 nd)             ;node��ȣ ����

    ;----- X,camber ����
    (setq ncx (+ ax fcw cw (- 0.0 trg) (* (1- cc) cw)))   ;n��° ��ǥtext�� y��ǥ

    (setq nc 0)                                           ;ù��° camber����

    (repeat (1+ n_camber)                                 ;camber����ŭ �ݺ�
      (setq ncy (- ny (* rh (1+ nc))))                    ;camber text�� y��ǥ
      (setq ncoord (nth (1- nn) (nth nc anlist)))   ;camber��(�Է°�,�Ǽ�)
      (if (= nc 0)
        (setq nctxt (rtos ncoord 2 3))
;        (setq nctxt (rtos (* yfac ncoord -1) 2 3))          ;camber�� text
        (setq nctxt (rtos (* yfac ncoord -1) 2 vprec))          ;camber�� text
      ) ;of if
      (setq ncxy (list ncx ncy))                          ;text�� insert point
      (command "TEXT" "MR" ncxy th 0.0 nctxt)            ;text����
      (setq nc (1+ nc))                                   ;���� camber��
    ) ;of repeat

    (setq nn (1+ nn))                                     ;���� node��
    (setq cc (1+ cc))                                     ;���� column����
  ) ;of repeat
) ;of defun


;---------------------------------------------
; Function : OP
;            OPeration
;            Yi Suk Jong
;            97/6/23
;---------------------------------------------
; ����ڰ� �Է��� ����θ� ������ �����Ѵ�.
; �Է�: eq  : ������̴�. (���� +,-�� ����)
;       lst : ����� ����, list�� �Ǿ��ִ�.
; ex) (op "1+2" '((1 2) (2 3)))
;    --> (3 5)
;---------------------------------------------

(defun op(eq lst /
lst eq neq nlop lop oval count c val n cnt )
  (setq neq (strlen eq))                      ;���� ����
  (setq nlst (length (nth 0 lst)))            ;list����
  (setq nlop 0)                               ;���������� ��ġ
  (setq lop "+")                              ;��������
  (setq oval nil)                             ;����갪

  (setq count 1)                                        ;ù�� ���ں���
  (repeat (1+ neq)                                      ;������ ���ڱ���
    (setq c (substr eq count 1))                        ;�ѱ��� ����
    (if (or (= c "+") (= c "-") (= count (1+ neq)))     ; +,-�̰ų� ���϶�
      (if (= nlop 0)                                    ; ó���̸�
        (setq n (atoi (substr eq (1+ nlop) (- count nlop 1)))
              oval (nth (1- n) lst)                          ;ù������ ����������
              nlop count                                ;����������ġ
              lop c)                                    ;��������
        (progn                                          ;�ι�°�����̸�
          (setq val nil)                                ;�������� �ʱ�ȭ
          (setq n (atoi (substr eq (1+ nlop) (- count nlop 1))))   ;�������ڿ��� �������ڱ���=����
          (cond
            ((= lop "+")                                ;+�϶� +����
              (setq cnt 0)                              ;ù�� ������
              (repeat nlst
                (setq val (append val (list (+ (nth cnt oval)
                                         (nth cnt (nth (1- n) lst))))))
                (setq cnt (1+ cnt))
              ) ;of repeat
              (setq oval val)
            ) ;of sub-cond
            ((= lop "-")                                ;-�϶� -����
              (setq cnt 0)
              (repeat nlst
                (setq val (append val (list (- (nth cnt oval)
                                         (nth cnt (nth (1- n) lst))))))
                (setq cnt (1+ cnt))
              ) ;of repeat
              (setq oval val)                           ;���� ���� ����������
            ) ;of sub-cond
          ) ;of cond
          (setq nlop count                              ;���� ��ġ�� ����������ġ��
                lop c)                                  ;���� �����ڸ� �� �����ڷ�
        ) ;of progn
      ) ;of if
    ) ;of if
    (setq count (1+ count))                             ;���� ���ڷ�
  ); of repeat
  val
) ;of defun
