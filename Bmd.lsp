;*****************************************
;       BMD
;           draw Bending Moment Diagram
;           Jong-Suk Yi
;           1998. 2. 24
;*****************************************
; ���E�a���i �a�a���a.
; DATA ���b�a�w�e ����ѡ, X-���a, ���E�a�t���a.

(defun C:BMD(
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

  ;----- �b�� �����t ���� (Camber��)
  (setq gap (getint "\nLEFT MARGIN (mm)<60>: "))    ;���b�� camber������ �e�b(�a��)
  (if (= gap nil) (setq gap 60))                    ;return���b�� gap=60mm
  (setq bdrx 780)                                   ;border�� x�a��
  (setq bdry 557)                                   ;border�� y�a��
  (setq yfac 1)                                     ;y-factor

  ;----- �b�� �����t ���� (Camber table)
  (setq rh 7)                                     ;row height (table ������)
  (setq fcw 20)                                   ;first column width
  (setq trg 2)                                    ;text right gap

  (setvar "CMDECHO" 0)                            ;command echo off
  (setvar "BLIPMODE" 0)                           ;blip mode off
  (setq th (getvar "DIMTXT"))                      ;text size ����

  (command "ZOOM" "W" "0,0" (list bdrx bdry))       ;���b�� �� �a�A ������

   (princ "alsdkjfl")
  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))    ;file name���b
  (setq opf (open fn "r"))                        ;file open

  (setq nlist nil)                                ;�� node-list �e�q
  (setq comb nil)                                 ;�� ���s �e�q
  (setq drw nil)                                 ;�� draw-list�e�q

  ;----- data file������ DATA���b

  (if opf                                         ;file�� ���e �w��
    (progn
       (while (and (/= (setq ch (read-line opf)) nil)         ;�a���� �{
                   (/= (strcase ch) "COMB")                   ;comb�a�e �e��
                   (/= (strcase ch) "DRAW")                   ;comb�a�e �e��
                   (/= ch ""))                                ;���e
            (setq nlist (append nlist (list (strloc ch))))    ;nlist�A �a
       ) ;of while
       (if (or (= ch "COMB") (= ch "comb"))                 ;comb����
         (while (and (/= (setq ch (read-line opf)) nil)     ;comb�A���a ����
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
    (princ "\nFile not found")                    ;file�� ���e �w��
  ) ;of if
  (close opf)                                     ;file close

  ;----- ���b line�� �w ���q
  (setq nnum (length nlist))                         ;���b line�� ����
  (setq ncamb (- (length (nth 1 nlist)) 2))          ;camber�� ��
  (setq ncomb (length comb))                          ;���b Combination�� ����
  (setq ndraw (length drw))                          ;draw��

  (if (= drw nil)
    (progn (princ "\nDRAW not found") (exit))) ;draw���i �� ���a

  ;----- ���b line���w �a��Ё��
  (princ nnum) (princ "-NODE / ")
  (princ ncamb) (princ "-CAMBER(S) / ")
  (princ ncomb) (princ "-COMBINATION(S) / ")
  (princ ndraw) (princ "-DRAWING(S) FOUND")

  (setq xmax (atof (nth 1 (nth (1- nnum) nlist))))  ;�� �{�� x�t
  (setq xmin (atof (nth 1 (nth 0 nlist))))           ;���� X�t

  (setq xscale (/ (- bdrx (* 2 gap)) xmax))          ;���b���� x scale(���e)
;  (setq yscale (getint "\nY-Scale: "))               ;y�wз scale(���b�t)

  (setq node nil)                                     ;node list ������
  (setq    x nil)                                     ;x�t list������
  (setq   YC nil)                                     ;Y�t list�� list

  ;----- node list, x�t list �e�i��
  (setq count 0)
  (repeat nnum                                        ;node�����e�q
    (setq node (append node (list (nth 0 (nth count nlist)))))
    (setq X    (append x (list (atof (nth 1 (nth count nlist))))))
    (setq count (1+ count))                           ;�a�q node��
  ) ;of repeat

  ;----- �A�� column�� ͢ ���a��/���b�h��
  (setq min_xlen (* (+ (strlen (rtos (nth 0 (reverse x)) 2 3)) 2) th))
  (princ "\nColumn width (mm)<")
  (princ min_xlen)
  (setq cw (getint ">: "))                      ;camber table �e �췡 ���b�h�q
  (if (< cw min_xlen) (setq cw min_xlen))       ;�A��͢���a ��a�e �A��͢�a��
  (setq cw (fix min_xlen))

  ;----- Y�t list �e�i��
  (setq ccount 2)
  (repeat ncamb                                           ;camber���e�q
    (setq count 0)                                        ;������..
    (setq y nil)                                          ;y list ������
    (repeat nnum                                          ;�a���b ��a��..
      (setq cy (atof (nth ccount (nth count nlist))))     ;y�t 
      (setq Y (append Y (list cy)))
      (setq count (1+ count))
    ) ;of repeat
    (setq YC (append YC (list Y)))                        ;y list ���q �e�i��
    (setq ccount (1+ ccount))                             ;�a�qcamber��
  ) ;of repeat

  ;----- combination�i�� Y�t list�A �a�a��
  (setq cnt_comb 0)
  (repeat ncomb
    (setq yc (append yc (list (op (nth cnt_comb comb) yc)))) ;comb�i ���e�a�a yc�A �a
    (setq cnt_comb (1+ cnt_comb))                             ;�a�q comb��
  ) ;of repeat

  (setq n_camber (length yc))                           ;camber�� ����

  ;----- �A�� y�t �� yscale���a��
  (setq maxcy (abs (nth 0 (nth 0 yc))))                 ;�A�� y�t�e ��q �t
  (setq count_c 0)
  (repeat n_camber                                      ;camber�� �e�q
    (setq count_n 0)
    (repeat nnum                                        ;node�� �e�q
      (setq cy (nth count_n (nth count_c yc)))          ;y�t 
      (if (> (abs cy) maxcy) (setq maxcy (abs cy)))     ;�A��y�t�x��
      (setq count_n (1+ count_n))                       ;�a�q node��
    ) ;of repeat
    (setq count_c (1+ count_c))                         ;�a�q camber��
  ) ;of repeat
  (setq yscale (/ xmax 10 maxcy))                       ;y-scale = x������ 1/10

  ;----- camber�� �a����
  (setq draw_cnt 0)
  (repeat ndraw                                  ;draw�����e�q �e��
    (setq ncmb (length (nth draw_cnt drw)))      ;camber���A �i��i case��

    ;---- �����巁 y���a ���a��(�锁�t y max)
    (setq maxy_lst nil)                             ; max-Y list ������
    (setq n_node 0)                                 ;�� node����
    (repeat nnum                                    ;case���e�q �e��
      (setq n (1- (atoi (nth 0 (nth draw_cnt drw)))))
      (setq maxy (nth n_node (nth n yc)))           ;���弁 case�a �A���t
      (setq n_case 0)                               ;���� case����
      (repeat ncmb
        (setq cn (1- (atoi (nth n_case (nth draw_cnt drw))))) ;case��ѡ
        (setq n_y (nth n_node (nth cn yc)))         ;n�币 node�� y�t
        (if (> (abs n_y) (abs maxy))                ;�e�� max���a �i��
          (setq maxy n_y)
        ) ;of if
        (setq n_case (1+ n_case))                  ;�a�q case��
      ) ;of repeat                                 ;maxy list�A �a
      (setq maxy_lst (append maxy_lst (list maxy)))
      (setq n_node (1+ n_node))
    ) ;of repeat

    ;---- ���� �a����
    (setq camby (+ 0.3 (* (/ 0.7 ndraw) (- ndraw (1+ draw_cnt)))))      ;camber�� y��á

    (setq firstx (list (+ (* xscale xmin) gap) (* bdry camby)))  ;�� x��
    (setq lastx (list (+ (* xscale xmax) gap) (* bdry camby)))   ;�a���b x��
    (command "LINE" firstx lastx "")                 ;base line�a����

    ;---- node��ѡ/pier/������a����
    (setq count 0)                                  ;������..
    (repeat nnum                                    ;�a���b ��a��..
      (setq nd (nth count node))                    ;node
      (setq xx (nth count x))

      (setq bpnt (list (+ (* xx xscale) gap) (* bdry camby)))  ;base point

      (if (= (substr nd 1 1) "+")               ;node�A fix�e�a�� ���i��
        (progn
          (command "LINE" bpnt "@3<-60" "@3<180" "C")   ;fix�e �a��(�q�b�w)
          (setq nd (substr nd 2))                   ;fix�e �a��(+)�A��
          (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier��ѡ ���q
        )
        (if (= (substr nd 1 1) "-")            ;node�A move�e�a�� ���i��
          (progn
            (command "LINE" bpnt "@3<-60" "@3<180" "C") ;move�e �a��(�q�b�w)
            (setq b1 (list (- (+ (* xx xscale) gap) 1.5) ;���� ���b��
                           (- (* bdry camby) 3)))
            (setq b2 (list (+ (+ (* xx xscale) gap) 1.5) ;���� �{��
                           (- (* bdry camby) 3)))
            (command "LINE" b1 b2 "")                   ;���� �a��
            (setq nd (substr nd 2))                 ;move�e �a��(-)�A��
            (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier��ѡ ���q
          ) ;of progn
        );of if
      ) ;of if

      (if (/= ap nil)                                     ;Abut/Pier�� �w��
        (progn
          (setq cp (list (car bpnt) (- (cadr bpnt) 12)))  ;���� ����
          (command "CIRCLE" cp "4.0")                     ;���a����
          (command "TEXT" "M" cp 3.0 0.0 ap)              ;text�a��
          (setq ap nil)
        ) ;of progn
      ) ;of if

      (setq nd (atoi nd))                     ;node�i ������
      (setq nd (itoa nd))                     ;node�i ���a�i��(�����A��)
      (setq txtpnt (list (+ (* xx xscale) gap) (- (* bdry camby) 5))) ;text pnt
      (command "TEXT" "M" txtpnt th "0" nd)     ;node ��ѡ�⋡

      (setq count (1+ count))
    ) ;of repeat
                                     ;�a�q node��
    ;----- ������a����
    (setvar "CECOLOR" "1")
    (setq count 0)                                  ;������..
    (repeat nnum                                    ;�a���b ��a��..
      (setq xx (nth count x))
      (setq bpnt (list (+ (* xx xscale) gap) (* bdry camby)))  ;base point
      (setq yy (+ (* (nth count maxy_lst) yscale -1 xscale) (* bdry camby)))
      (command "LINE" bpnt
                      (list (car bpnt) yy) "")
      (setq maxyy (nth count maxy_lst))             ;�A���t
      (if (>= maxyy 0)
        (setq txtalign "MR")                        ;�A���t�� �������e
        (setq txtalign "ML"))                       ;���b���i
      (command "TEXT" "J" txtalign (list (car bpnt) yy) th "90"
                      (strcat " " (rtos maxyy 2 1) " "))

      (setq count (1+ count))
    ) ;of repeat                                      ;�a�q node��
    (setvar "CECOLOR" "BYLAYER")

    ;---- camber�a����
    (setq oldc (getvar "CECOLOR"))                    ;�e���� ��ϡ
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

        (command pnt)                                             ;camber�� �a����
        (setq n_count (1+ n_count))
      ) ;of repeat
      (command "")
      (setq cmb_cnt (1+ cmb_cnt))
    ) ;of repeat                                            ;�a�q camber��
    (setvar "CECOLOR" oldc)                                 ;���� ������
    (setq draw_cnt (1+ draw_cnt))
  ) ;of repeat                                              ;�a�q draw��

  ;------ table�a����
  (setq table (append (list x) yc))
  (setq  ncamb (1+ n_camber))
  (setq ntbl (+ (/ (* (1+ nnum) cw) (- bdrx (* gap 2))) 1))  ;table����
  (setq tblh (* rh (+ 2 ncamb)))                             ;table ����

  (setq maxn (/ (- bdrx (* gap 2)) cw))         ;�e�� �A�� node��

  (setq tcount 0)                               ;table count
  (repeat ntbl
    (setq node1 (1+ (* tcount maxn)))           ;���b node
    (setq node2 (* (1+ tcount) maxn))           ;�{ node
    (if (> node2 nnum)                          ;�{ node�a �a���b node���a �a�e
      (setq node2 nnum))

    (setq tbly (/ camby 2.0))                   ;camber table y��á
    (setq tblpx (- gap fcw)
          tblpy (- (* bdry tbly) (* tcount (+ tblh cw))))   ;table�� �s����á

    (camb_table tblpx tblpy node1 node2 table)              ;table�a����

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
; �� �q���e ,�� �����E data�i �a���� �e���� list�A ���ẅ�a.
; ���� �w�e�� ���� ���e data�e ���a�i�� return�E�a.
;******************************************************************
(defun STRLOC(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;��ᵥ ���a�i
   (setq strl (strlen arg1))                    ;��ᵥ ���a�i�� ����
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;���b ��á
   (setq nchr 1)                                ;���a ����
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;���a �e��
      (if (or (= subs ",") (= subs ""))         ;�e�� ���a�a ,����a �{����
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;���b��á����
            (if (= rslt nil)
               (setq rslt (list lst))                  ;�����t�� �����i��
               (setq rslt (append rslt (list lst)))    ;�����t�A�a �a
            ) ;of if
            (setq nchr 0)                       ;���� �a�� 0�a��
            (setq strt (1+ count))              ;�a�q ���b�i �a�q���a��
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;�a�q ���a��
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;���a ���� �e�� �w�a
   ) ;of repeat
   (setq arg1 rslt)                             ;�����t ����
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;****************************************************
; Function : CAMB_TABLE
;            CAMBer TABLE
;            Yi Suk-Jong
;            1996/2/14
;****************************************************
; �� �q���e ���ụ data�� ���q�� Table�e�i�ẅ�a
;  �h�e �t�e
;      ax : table�� ���b�w�e�� x��á
;      ay : table�� ���b�w�e�� y��á
;  anode1 : ���b node��ѡ
;  anode2 : �{ node��ѡ
;  anlist : ���b�E data list
;****************************************************

(defun CAMB_TABLE(ax ay anode1 anode2 anlist
/                 ipnt     noden     xl      oldc     hl       y        st_pnt
                  end_pnt  ey        f_sp    f_ep     vl       nx       sp
                  ep       nn        cc      nx       ny       nxy      nd
                  ncx      nc        ncy     ncoord   nctxt    ncxy
)

  (setq noden (+ 1 (- anode2 anode1)))              ;node ����
  (setq xl (+ fcw (* noden cw)))                    ;x����

  (setq oldc (getvar "CECOLOR"))                    ;�e�� �� ��ϡ
  (setvar "CECOLOR" "RED")                          ;table�� ���i �i�e���a��


  ;----- ���w�� �a����
  (setq hl 0)                                       ;���w�� count

  (repeat (+ ncamb 2)                               ;���w��a����
    (setq y (- ay (* hl rh)))                       ;���w�� y�t
    (setq st_pnt (list ax y))                       ;���w�� ���b��
    (setq end_pnt (list (+ ax xl) y))               ;���w�� �{��
    (command "LINE" st_pnt end_pnt "")              ;���w�� �a����
    (setq hl (1+ hl))                               ;�a�q ���w��
  ) ;of repeat

  ;----- ������ �a����
  (setq ey (- ay (* (+ ncamb 1) 7)))                ;�����巁 �{�� y�t
  (setq f_sp (list ax ay))                          ;���弁 start point
  (setq f_ep (list ax ey))                          ;���弁 end point
  (command "LINE" f_sp f_ep "")                     ;���弁 ������ �a����

  (setq vl 0)                                       ;������ count

  (repeat (1+ noden)                                ;node���� �e�q �e��
    (setq nx (+ ax fcw (* vl cw)))                  ;n�弁�� X���a
    (setq sp (list nx ay)                           ;���b��
          ep (list nx ey))                          ;�{��
    (command "LINE" sp ep "")                       ;������ �a����
    (setq vl (1+ vl))                               ;�a�q ������a��
  ) ;of repeat

  (setvar "CECOLOR" oldc)                           ;���� �A������

  ;----- node/x/camber�⋡
  ;----- node��ѡ �⋡
  (setq nn anode1)                                  ;node count
  (setq cc 1)                                       ;column count

  (repeat noden                                     ;node���� �e�q �e��
    (setq nx (+ ax fcw (/ cw 2.0) (* (1- cc) cw)))  ;n�弁 node�� x���a
    (setq ny (- ay (/ rh 2.0)))                     ;node�� y���a
    (setq nxy (list nx ny))                         ;node�� xy���a
    (setq nd (nth (1- nn) node))                ;node��ѡ ���b�t
    (if (or (= (substr nd 1 1) "-") (= (substr nd 1 1) "+"))  ;Abut/Pier���w��
      (setq nd (itoa (atoi (substr nd 2))))         ;�����a(+/-)�� ���� �A��
      (setq nd (itoa (atoi nd)))                    ;���� �A��
    ) ;of if
    (command "TEXT" "M" nxy th 0.0 nd)             ;node��ѡ �⋡

    ;----- X,camber �⋡
    (setq ncx (+ ax fcw cw (- 0.0 trg) (* (1- cc) cw)))   ;n�弁 ���atext�� y���a

    (setq nc 0)                                           ;���弁 camber����

    (repeat (1+ n_camber)                                 ;camber���e�q �e��
      (setq ncy (- ny (* rh (1+ nc))))                    ;camber text�� y���a
      (setq ncoord (nth (1- nn) (nth nc anlist)))   ;camber��(���b�t,����)
      (if (= nc 0)
        (setq nctxt (rtos ncoord 2 3))
;        (setq nctxt (rtos (* yfac ncoord -1) 2 3))      ;camber�� text(�ự�t�A -1�i ���q)
        (setq nctxt (rtos (* yfac ncoord) 2 3))          ;camber�� text(�ự�t�A -1�i ���q)
      ) ;of if
      (setq ncxy (list ncx ncy))                          ;text�� insert point
      (command "TEXT" "MR" ncxy th 0.0 nctxt)            ;text�a��
      (setq nc (1+ nc))                                   ;�a�q camber��
    ) ;of repeat

    (setq nn (1+ nn))                                     ;�a�q node��
    (setq cc (1+ cc))                                     ;�a�q column�a��
  ) ;of repeat
) ;of defun


;---------------------------------------------
; Function : OP
;            OPeration
;            Yi Suk Jong
;            97/6/23
;---------------------------------------------
; �a�w�a�a ���b�e �e�e���i �e�e�i ��З�e�a.
; ���b: eq  : �e�e�����a. (�e�� +,-�e �a�w)
;       lst : �e�e�I �t�i, list�� �A�᷶�a.
; ex) (op "1+2" '((1 2) (2 3)))
;    --> (3 5)
;---------------------------------------------

(defun op(eq lst /
lst eq neq nlop lop oval count c val n cnt )
  (setq neq (strlen eq))                      ;���� ����
  (setq nlst (length (nth 0 lst)))            ;list����
  (setq nlop 0)                               ;��e�e�a�� ��á
  (setq lop "+")                              ;��e�e�a
  (setq oval nil)                             ;�剁�e�t

  (setq count 1)                                        ;���� �i�a����
  (repeat (1+ neq)                                      ;�a���b �i�a�a��
    (setq c (substr eq count 1))                        ;�e�i�a ���ᐁ��
    (if (or (= c "+") (= c "-") (= count (1+ neq)))     ; +,-����a �{����
      (if (= nlop 0)                                    ; ��q���e
        (setq n (atoi (substr eq (1+ nlop) (- count nlop 1)))
              oval (nth (1- n) lst)                          ;���t�i�i �A���t�a��
              nlop count                                ;��e�e�a��á
              lop c)                                    ;��e�e�a
        (progn                                          ;���弁���ᷡ�e
          (setq val nil)                                ;�A���t�i ������
          (setq n (atoi (substr eq (1+ nlop) (- count nlop 1))))   ;��e�e�a�A�� �e�e�e�a�a��=���a
          (cond
            ((= lop "+")                                ;+���� +��З
              (setq cnt 0)                              ;���� �t����
              (repeat nlst
                (setq val (append val (list (+ (nth cnt oval)
                                         (nth cnt (nth (1- n) lst))))))
                (setq cnt (1+ cnt))
              ) ;of repeat
              (setq oval val)
            ) ;of sub-cond
            ((= lop "-")                                ;-���� -��З
              (setq cnt 0)
              (repeat nlst
                (setq val (append val (list (- (nth cnt oval)
                                         (nth cnt (nth (1- n) lst))))))
                (setq cnt (1+ cnt))
              ) ;of repeat
              (setq oval val)                           ;�e�� �t�i �A���t�a��
            ) ;of sub-cond
          ) ;of cond
          (setq nlop count                              ;�e�� ��á�i ��e�e�a��á��
                lop c)                                  ;�e�� �e�e�a�i �� �e�e�a��
        ) ;of progn
      ) ;of if
    ) ;of if
    (setq count (1+ count))                             ;�a�q �i�a��
  ); of repeat
  val
) ;of defun
