;-----------------------------------
; DJDG Function library
;------ �Ϲ� -----------------------
; djdg_issamepnt : is there same point
;------ ���� -----------------------
; v_angle(p1 p2 p3) : p1�� p2�� �̷�� ������ �����̸� p3�� ������ ����
; asin(a) 	: arcsin�� return(radian)
; cross		: CROSS point of arc & line
; crossent	: �� entity�� ������ �����ش�.
; cp_line_pline : cross point of line and polyline
; INANG		: a angle is IN the range of ANGle-1 and angle-2 ?(����� �ΰ� ���̿� �ִ°�?)
; djdg_angdist 	: distance angle : � ���� �־��� ������������ �Ÿ�
; textboxm	: textbox middle : �־��� text�� �߾��� ã��.
; function 	: rotatepnt	 : ����� ȸ���� ��ǥ�� ���Ѵ�.
; Function	: farest 	: seek farest 2 points
;------ entity���� -----------------
; mk_vertlist 		: LwPolyline�� vertex list�� ������ش�.
; djdg_getpoints 	: get points of entity
; rib_dim(str)
; getLwVert (vlist tmpctr )
;------ Draw ���� -----------------------
; djdg_insertarw1(p1 p2) 		: insert half arrow
; djdg_insertblk(bname p1 p2 dimscl) 	: insert block
; djdg_insertblkas(bname p1 p2) 	: insert block with angle & scale
; djdg_wtxtonline(p1 p2 text th gap) 	: write text on the line
; djdg_wtxtonline1(p1 p2 p3 text th gap): ���ؼ��� �������� �̿��Ͽ� �ؽ�Ʈ ����
; djdg_rect(p1 ang l t opt) : draw rectanlular
; djdg_sldimage : draw sldimage
; djdg_mtredraw(ss mode) :  multi redraw
;-------- ���ڿ� ���� ----------------------------
; data-in :
; str_position(str1 str2)
; SP-TRUNC : ���ڿ��� �յ��� ������ �������ش�.
; IS-NUM : ��������Ȯ�����ش�.
; djdg_splitprenum 	: S1�� ���� ����+��ȣ �� ����(prefix)�� ���ڷ� �и��Ѵ�.
; djdg_splitstr	; string�� �־��� ���ڿ��� split�Ѵ�.
; divide_str(arg1 arg2) : string arg1�� arg2�������� ����
;--------  dimension ���� ------------------------
; djdg_splitdimtxt	; dimtext�� split�Ѵ�.; ex) 10@1.250=12.500  --> ("10" "1.250" "12.500")
; djdg_splitdimtxtnum; dimtxt�� �߶� ���ڷ� �Ѱ��ش�.
; 				 ex)10@1.250=12.500  --> (10 1250 12500) "1.250" --> 1250�� ���� ����ȭ(dimlfac����)
; djdg_divlen	; ����(len)�� pitch�� ������ ��� �������� ������ ;            ex) dst=1250 pitch=150 --> mok = 8, re = 50 --> (8 50)
; rto_dimtxt(l)   	; real to dimtxt;  ex)  (rto_dimtxt 1000)  --> "1.000" (rto_dimtxt 999)  --> "999"
; djdg_dimtxtoreal	; dimtext�� ���� ������ �ٲ��ش�. ex) "1.250" --> 1250
; djdg_getdimlen  	; get dim length; dimension string�� ��ü���̸� ������.
;				  ex) "150+2@250+150" --> 800
; F_DH		; Function Dimension Horizontal
; F_DV		; Fuction Dimension Vertical
; F_DA		; Function Dimension Aligned
; djdg_angofdimline  ; �־��� ġ���� ġ����(dimension line) ����.
; djdg_makegoltotal  ; �־��� ������ ������ �̿��� ��ü�渦 ���Ѵ�.
;				  ex) (djdg_makegoltotal 2 1250) --> "2@1.250=2.500")
;--- sort function
; point-srot : sort points using base point or x, y

;------ Attribute����
; djdg_attwrite	; attribute�� ���Ե� insert�� att�� �������ش�.
; djdg_gettag	; get tag ; ��ƼƼ ���������� �ָ� ������ ���� tag�� ���� �����ش�.
; djdg_getattv	; attribute�� ��ü ������ �о� list�� ���� ��ȯ�Ѵ�.

;------ file ����
; djdg_readdatafile ; read data file

;----------------------------------
; function : str_position
;            Yi Suk Jong
;            00/7/15
;----------------------------------
; str_position(str1 str2)
; str1 : long string
; str2 : short string
;----------------------------------
; RIB.LSP
;----------------------------------
(defun str_position(str1 str2
		    /    str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
	len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1)) nil count)
);defun str_position


;-------------------------------------
; Functiom : v_angle
;            get Vertical angle
;            Yi Suk Jong
;            04/04/02
;-------------------------------------
; v_angle(p1 p2 p3)
;      p1,p2 : two point
;         p3 : side point
;-------------------------------------
; RIB.LSP
;-------------------------------------
(defun v_angle(p1 p2 p3
	       / ang ang90 ipnt)
  (setq ang (angle p1 p2))
  (setq ang90 (+ ang (* 0.5 pi)))
  (setq ipnt (inters p1 p2 p3 (polar p3 ang90 100) nil))
  (angle ipnt p3)
) ;of defun


;-------------------------------------
; Functiom : rib_dim
;            get rib dimension
;            Yi Suk Jong
;            04/04/02
;-------------------------------------
; rib_dim(str)
;      str: "200x20"
;      return : (200 20)
;-------------------------------------
; RIB.LSP
;-------------------------------------
(defun rib_dim(str
	      / star b thick)
  (setq star (str_position str "*"))
  (setq b (substr str 1 (1- star))
	thick (substr str (1+ star) (- (strlen str) star)))
  (list (atof b) (atof thick))
);defun

;------------------------------------------
; function : asin
;            arc sin
;            Yi Suk Jong
;            2000/4/25
;------------------------------------------
; asin(a) : arcsin�� return(radian)
;------------------------------------------
(defun asin(a / a)
  (cond
    ((= a 1) (/ pi 2.0))
    ((= a -1) (/ pi -2.0))
    (T (atan (/ a (sqrt (- 1 (* a a))))))
  );cond  
);defun  


;*******************************************************************
;     Function : divide_str
;                divide string
;                Jong-Suk Yi
;                2004. 5. 10
;******************************************************************
; �� �Լ��� �־��� string�� ������ �Ǵ� ���ڷ� ������ �Ѱ��� list�� �����ش�.
; �̶� ����ȯ ���� ��� data�� ���ڿ��� return�ȴ�.
;******************************************************************
(defun divide_str(arg1 arg2
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt    arg1     arg2 
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
      (if (or (= subs arg2) (= subs ""))         ;���� ���ڰ� ,�̰ų� ���϶�
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





;-------------------------------------
; function : djdg_insertarw1
;            isnert half arraw
;            Yi Suk Jong
;            04/06/03(THU)
;-------------------------------------
(defun djdg_insertarw1(p1 p2
		      / p1 p2 ang w4 ys ds ys)
  (setq ds (getvar "DIMSCALE"))                             ;������ ��  
  (setq ang (angle p1 p2)                                   ;������ �̷�� ��
        w4  (which4 ang))                                   ;���и��ΰ�?
  (if (or (= w4 1) (= w4 4))                                ;1,4�и��ϰ��
    (setq ys (* 1 ds))                                             ;y-scale =  1
    (setq ys (* -1 ds))                                            ;y-scale = -1
  ) ;of if
  (command "INSERT" (strcat (prefix) "blocks/arw1") p1 ds ys (rtod ang))
);defun


;-------------------------------------
; function : djdg_insertblk
;            isnert block
;            Yi Suk Jong
;            04/06/03(THU)
;-------------------------------------
; arguments
;   bname : block name
;   p1    : point for insert
;   p2    : point for angle(if nil : Angle=0)
;   dimscl: T   > use dimscale  (boolean)
;           nil > don't use dimscale
;-------------------------------------
(defun djdg_insertblk(bname p1 p2 dimscl
		      / p1 p2 ang w4 ys ds ys)
  (setq ds (getvar "DIMSCALE"))                             ;������ ��  
  (if (= dimscl nil) (setq ds 1.0))			;
  (if (= p2 nil)
    (setq ang 0)
    (setq ang (angle p1 p2))                                   ;������ �̷�� ��
  );if
  (command "INSERT" (strcat (prefix) "blocks/" bname) p1 ds ds (rtod ang))
);defun

;-------------------------------------
; function : djdg_insertblkas
;            isnert block with angle & scale
;            Yi Suk Jong
;            04/06/03(THU)
;-------------------------------------
; arguments
;   bname : block name
;   p1    : point for insert
;   p2    : point for angle(if nil : Angle=0)
;           Distance (P1~P2) = scale
;-------------------------------------
(defun djdg_insertblkas(bname p1 p2
		      / p1 p2 ang w4 ys ds ys)
  (setq scl (distance p1 p2))                             ;������ ��  
  (if (= p2 nil)
    (setq ang 0)
    (setq ang (angle p1 p2))                                   ;������ �̷�� ��
  );if
  (command "INSERT" (strcat (prefix) "blocks/" bname) p1 scl scl (rtod ang))
);defun


;---------------------------------------
; Function : djdg_wtxtonline(p1 p2 text th gap)
;            write text on the line
;            Yi Suk Jong
;            04/06/09
;---------------------------------------
; arguments
;   p1 : point1
;   p2 : point2
;   text : text
;   th   : text height
;   gap  : line ~ middle of text
;---------------------------------------
(defun djdg_wtxtonline(p1 p2 text th gap
		       / ta mp tp)
  (setq ta (ang4text p1 p2))    ;text angle
  (setq mp (mid-point p1 p2))    ;mid point of line
  (setq tp (polar mp (+ ta (* 0.5 pi)) gap))
  (push-os)
  (command "TEXT" "J" "M" tp th (rtod ta) text)
  (pop-os)
;  (command "line" p1 p2 "")
);defun

;---------------------------------------
; Function : djdg_wtxtonline1(p1 p2 p3 text th gap)
;            ���ؼ��� �������� �̿��Ͽ� �ؽ�Ʈ ����
;            wtxtonline�� ��� ���ؼ��� ���Ʒ��� gap���� �����ϱ� ������ ��Ʈ���� �����
;            write text on the line
;            Yi Suk Jong
;            06/08/05
;---------------------------------------
; arguments
;   p1 : point1(������)
;   p2 : point2(����)
;   p3 : side point
;   text : text(string)
;   th   : text height
;   gap  : line ~ middle of text (�׻�+����)
;---------------------------------------
(defun djdg_wtxtonline1(p1 p2 p3 text th gap
		       / ta mp tp)
  (setq ta (ang4text p1 p2))    ;text angle
  (setq va (v_angle p1 p2 p3))  ;vertical angle(��м����κ��� text��ġ���� ����)
  (setq mp (mid-point p1 p2))   ;mid point of line
  
  (setq tp (polar mp va gap))  ;text������ ���ϱ�
  (push-os)
  (command "TEXT" "J" "M" tp th (rtod ta) text)
  (pop-os)
;  (command "line" p1 p2 "")
);defun



;-------------------------------
; function : djdg_rect
;          : draw rectanlular
;            Yi Suk Jong
;            04/06/14
;-------------------------------
; arguments
;  p1 : insertion point
;   l : length
; thick : thickness
;  option : 0 > insert point is on the edge
;           1 > insert point is on the center
(defun djdg_rect( p1 ang l thick opt
		 /
		)
  (cond
    ((= opt 0)
     (setq pnt1 (polar (polar p1 ang l) (+ ang (* pi 0.5)) (* 0.5 thick)))
;     (setq pnt2 (polar pnt1 (+ ang pi) l))
;     (setq pnt3 (polar pnt2 (- ang (* 0.5 pi)) thick))
;     (setq pnt4 (polar pnt3 ang l))
    );sub cond 
    ((= opt 1)
     (setq pnt1 (polar (polar p1 ang (* 0.5 l)) (+ ang (* pi 0.5)) (* 0.5 thick)))
;     (setq pnt2 (polar pnt1 (+ ang pi) l))
;     (setq pnt3 (polar pnt2 (- ang (* 0.5 pi)) thick))
;     (setq pnt4 (polar pnt3 ang l))
    );sub cond 
  );cond
     (setq pnt2 (polar pnt1 (+ ang pi) l))
     (setq pnt3 (polar pnt2 (- ang (* 0.5 pi)) thick))
     (setq pnt4 (polar pnt3 ang l))
  (command "pline" pnt1 pnt2 pnt3 pnt4 "c")  
);defun


;-----------------------------------------------
; function : djdg_sldimage
;            draw sldimage
;            Yi Suk Jong
;            04/06/21
;-----------------------------------------------
(defun djdg_sldimage( tilename sldname erase / )
    ;arguments
    ; tilename : name of image tile
    ; sldname : name of slide
    ; erase : T  --> erase
    ;        nil --> don't erase
    (start_image tilename)                                  ;image ���̱�
    (if erase
      (fill_image 0 0 (dimx_tile tilename) (dimy_tile tilename) 5) ;erase image tile
    );if 
    (slide_image  0 0
                  (dimx_tile tilename) (dimy_tile tilename)   
                  sldname)
    (end_image)
    
);defun djdg_sldimage

(defun djdg_sclblk( / )
  3
);defun

;------------------------------------
; funtion : djdg_mtredraw
;           multi redraw
;           Yi Suk Jong
;           04/08/13
;------------------------------------
; ss : result of ssget 
; mode : 1 - redraw
;        2 - don't redraw
;        3 - highlight
;        4 - don't highlight
;------------------------------------
(defun djdg_mtredraw( ss mode / num index )
  (setq num (sslength ss))
  (setq index 0)
  (repeat num
    (redraw (ssname ss index) mode)
    (setq index (1+ index))
  );repeat  
);defun djdg_mtredraw


;------------------------------------
; funtion : djdg_issamepnt
;           is there same point
;           Yi Suk Jong
;           04/08/28
;------------------------------------
; p1 : point to be checked
; plist : point list
;------------------------------------

(defun djdg_issamepnt(p1 plist / num index issame)
  (setq num (length plist))  ;number of points
  (setq index 0)
  (setq issame nil)
  (repeat num
    (if (<= (distance p1 (nth index plist)) 0.0001)
      (setq issame T)
    )
    (setq index (1+ index))  ;next point
  );repeat  
  issame
) ;defun  djdg_issamepnt


;------------------------------------
; function : djdg_getpoints
;            get points of entity
;            Yi Suk Jong
;            04/04/03
;------------------------------------
; argument : ss-set
;	lnend : T/nil (line�� �߽����� �Ǵ� ������ �Ұ��ΰ�?)
; return : list of point(block:insert point, line: midpoint or end, circle/donut : center point)
; ex) djdg_getpoints(ss)
;     --->  ((0 0 0) (1 0 0) (2 0 0))
;------------------------------------
(defun djdg_getpoints(entlst lnend)

;  (setq mpnt (getpoint "Pick marking point: "))   ;��ŷ ���� �׸� ��ġ����
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
        (if lnend
	  (progn
	    (setq plst (append plst (list sp)))
	    (setq plst (append plst (list ep)))	  
	  );progn
	  (progn				;�߽����߰�
            (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;�����߰��� X
                       (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y
            (setq plst (append plst (list mp)))       ;�߰����� ��ŷ����Ʈ�� �߰�
	  );progn
	);if  
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
      ((= entype "INSERT")
        (setq cp (cdr (assoc 10 ent)))
        (setq plst (append plst (list cp)))
        (setq npnt (1+ npnt))
      ); entype="INSERT"
    ) ;of cond
    (setq index (1+ index))                                 ;index=��ƼƼ��ȣ
  ) ; of repeat
  plst
)  ;defun


;---------------------------
; function : djdg_loadltype
;            load line type
;            Yi Suk Jong
;            04/09/07
;---------------------------
(defun djdg_loadltype( lt / )
  (if (= (tblsearch "LTYPE" lt) nil)
    (command "linetype" "L" lt ""))
);defun


;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; �� �Լ��� ,�� �Ҹ��� data�� ������ �Ѱ��� list�� �����ش�.
; �̶� ����ȯ ���� ��� data�� ���ڿ��� return�ȴ�.
;******************************************************************

(defun DATA-IN(arg1
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


;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; �Է¹��ڿ��� ��,�ڿ� �ִ� ��ĭ�� ©�󳽴�.
; ���ϰ���
; (©�� ���ڿ�,
;  ù ���� ������ ��ġ,
;  ������ ���� ������ ��ġ,
;  �����ΰ�?)
;***************************************************************************

(defun SP-TRUNC(txt
/               txtl        frntn       backn       txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if

);of defun


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


;---------------------------------
;function : djdg_splitdimtxt
;           dimtext�� split�Ѵ�.
;           ex) 10@1.250=12.500  --> ("10" "1.250" "12.500")
;           Yi Suk Jong
;           05/02/21
;---------------------------------
(defun djdg_splitdimtxt(as1 / mk pos po1 totl divl divl1)
  (setq mk nil divl1 nil totl nil)
  (if (setq pos (vl-string-search "@" as1))
    (progn
      (setq mk (substr as1 1 pos)
	    divl (substr as1 (+ pos 2) (- (strlen as1)  pos)))
      (if (setq pos1 (vl-string-search "=" divl))
	(setq totl (substr divl (+ pos1 2) (- (strlen divl) pos1))
	      divl1 (substr divl 1 pos1))
	(setq divl1 divl)
      );if
      (list mk divl1 totl)
    );progn
    (list nil nil as1)  
  );
);defun

;-----------------------------------
; function : djdg_splitdimtxtnum
;            dimtxt�� �߶� ���ڷ� �Ѱ��ش�.
;            ex)  10@1.250=12.500  --> (10 1250 12500) "1.250" --> 1250�� ���� ����ȭ(dimlfac����)
;            Yi Suk Jong
;            05/02/21
;-----------------------------------
(defun djdg_splitdimtxtnum(as1 / mkn divln totln)
  (setq sp (djdg_splitdimtxt as1))
  (setq mk (nth 0 sp)
	divl (nth 1 sp)
	totl (nth 2 sp))
  (if (/= mk nil) (setq mkn (atoi mk)))
  (if (/= divl nil)
    (setq divln (djdg_dimtxtoreal divl));progn
  );if  
  (if (/= totl nil)
    (setq totln (djdg_dimtxtoreal totl))
  );if
  (list mkn divln totln)
);defun  

;---------------------------------
; function : djdg_dimtxtoreal
;            dimtext�� ���� ������ �ٲ��ش�.
;            ex) "1.250" --> 1250
;            dimlfac�� ����
;---------------------------------
(defun djdg_dimtxtoreal(dimtxt)
  (setq dt (vl-string-subst "." "," dimtxt))  ;","�� �ִ� ��� "."�� �ٲ㼭 �����Ѵ� ","�� ��� atof�Լ����� �ν����� ���ϹǷ�...
  (if (vl-string-search "." dt)               ;"."�� �ִ� ��� atof�� ����� 1000�� ���Ѵ�.
    (progn
      (setq realen (* (atof dt) 1000 ))
    );progn
    (setq realen (* (atof dt) ))		;"."�� ���� ��쿣 atof�� �����Ѵ�.
  );if
  realen
);defun

    
;---------------------------------
; function : djdg_divlen
;            ����(len)�� pitch�� ������ ��� �������� ������
;            ex) dst=1250 pitch=150 --> mok = 8, re = 50 --> (8 50)
;            Yi Suk Jong
;            05/02/21
;---------------------------------
(defun djdg_divlen(dst pitch)
  (setq re (rem dst pitch))    ;remain(������)
  (setq mok (fix (/ dst pitch)))  ;mok(��)
  (setq retrun (list mok re))
);defun


;----------------------------------------
; Funcion : rto_dimtxt(l)
;           real to dimtxt
;           Yi Suk Jong
;           04/04/03
;----------------------------------------
; argument : l (length)
; retun    : dim text
;  ex)  (rto_dimtxt 1000)  --> "1.000"
;       (rto_dimtxt 999)  --> "999"
(defun rto_dimtxt(l / l txt)
  (setvar "DIMZIN" 0)
  (if (< l 1000)
    (setq txt (rtos l 2 0))
    (progn
      (setq txt (rtos (* l 0.001) 2 3))
    );progn
  );if
  
  (setq txtlen (strlen txt))
  (setq count 1)      
      
  (while (and (/= (substr txt count 1) ".") (<= count (1+ txtlen)))    
    (setq count (1+ count))    
  );while    
  (if (> count txtlen)    
    txt    
    (strcat (substr txt 1 (1- count)) (getvar "DIMDSEP") (substr txt (1+ count) (- txtlen count)))    
  )    
  
);  defun

;------------------------------
; function : djdg_getdimlen
;            get dim length
;            dimension string�� ��ü���̸� ������.
;            ex) "150+2@250+150" --> 800
;            Yi Suk Jong
;            05/02/26
;------------------------------
(defun djdg_getdimlen(dmstr / dmstr)
  (setq sp (divide_str dmstr "+"))   ;+�������� �и���.("150" "2@250" "150")
  
  (setq totl 0)   ;�ѱ��� = 0
  
  (foreach atm sp  ;�� ��ҿ� ���ؼ� ���� ���ؼ� ���ϱ�.
    (setq spgol  (divide_str atm "@")) ;����̱������� ������.
    (if (= (length spgol) 2)    ;����̱������� ���� ����Ʈ�� 2�̻��϶� �� ����̰� ���� ��.
      (setq atl (* (atof (nth 0 spgol))
		   (djdg_dimtxtoreal (nth 1 spgol))))  ;atom length = ����� ���� ���� ���ϱ�. �̶� 1.125�� �� 
      							;. �Ǵ� ,�� �ִ� string�� dimtxtoreal�� ����
      (setq atl (djdg_dimtxtoreal (car spgol)))        ;@�� ���� ��� ���� �߰�. 
    );if
    (setq totl (+ totl atl))   ;�ѱ���(totl)�� ����� ����(atl) ���ϱ�.
  );foreach  
);defun


;******************************************
; Program : F_DH
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29
;******************************************
; ����ġ������ �Լ��� ó�����ش�.
; �Ѿ���� ����
;        SP : ������
;       DST : �Ÿ�
;         N : �ݺ�����
;        UD : �Ǽ��϶�: Up/DOWN (���밪�� LEVEL)
;	      list�� �� : ������ dimenxion point�� ��.
; �����ִ� �� - ���� ��ǥ
;******************************************
; 05/12/29 UD�� list�� ��쿡�� �� ���� dimension point�� ��.
(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;���� ũ�� ����

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (setq next (* dst n))                                     ;���������� �������� �Ÿ�

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep ��ġ���

;  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;ġ���� ��ġ.
  (if (listp ud)					;ġ������ġ UD�� list�̸� point�� ����.
    (progn
      (setq dxy ud)
      (setq dy (- (cadr ud) (cadr sp)))
      (if (minusp dy) (setq sgn -1) (setq sgn 1))
      (setq dy (abs dy))
    );progn  
    (progn
      (if (> ud 0)                                              ;�� �Ʒ�
        (setq sgn 1)
        (setq sgn -1)
      ) ;of if
      (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;ġ���� ��ġ ��� (���밪)
      (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;ġ���� ��ġ.
    );progn  
  );if  

  (setq dx (distance sp ep))                          ;�Ÿ� ���

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dx < 1000)

  (if (> (abs n) 1)                                           ;����� �ɼ��� ���
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos (abs n) 2 0))                          ;���� ���� ���
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000�̸��� ��
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000�̻��� ��
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text��ü����
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;ġ�������� ���� text �ȵ���
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;�� �Ʒ� ���ٷ� ������
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds 2.5)) 0.0)))

          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)

          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM��� ����	  
        ) ;of progn THEN
        (progn                                 ;ġ�������� ���� text ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM��� ����
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*********************************
; Function : F_DV
;           Fuction Dimension Vertical
;           Jong-Suk Yi
;           96/7/1
;*********************************
; 05/12/29 LR�� point�� ��� ������ dimension point��...

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th 2.5                                        ;textũ�� = 2.5
        dim_gap 10.0)                                 ;ġ���� ����
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (setq next (* dst n))                                 ;�������� �Ÿ�

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;������ ����

;  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ
  (if (listp LR) ; LR�� list�ΰ��(point�ΰ��)
    (progn
      (setq dxy LR)
      (setq dx (- (car dxy) (car sp)))
      (if (minusp dx) (setq sgn -1) (setq sgn 1))
      (setq dx (abs dx))
    );progn
    (progn
      (if (> lr 0)                                        ;����/������
        (setq sgn 1)
        (setq sgn -1)
      ) ;of if
      (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))
      (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ
    );progn
  );if  

  (setq dy (distance sp ep))                          ;�� ���� �Ÿ�

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dy < 1000)

  (if (> (abs n) 1)
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos (abs n) 2 0))                          ;���� �������
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;������ ���̰� 1000�̸���
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;������ ���̰� 1000�̻��
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dy)
        (progn                                  ;text�� ������ ���� �ȵ���
          (setq dtxt1 (strcat divn "@" divl))   ;���ٷ� ����
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list (- (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds 2.5)) 0.0 0.0)))
;          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn THEN
        (progn                                  ;text�� ������ ���� ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                    ;�����Է½� �� text�� ��
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM��� ����
    ) ;of progn ELSE
  ) ;of if
  ep
) ;defun


;******************************************
; Program : F_DA
;           Function Dimension Aligned
;           Jong-Suk Yi
;           04/04/03
;******************************************
; ������ġ������ �Լ��� ó�����ش�.
; �Ѿ���� ����
;        SP : ����
;        ANG : ����
;       DST : �Ÿ�
;         N : �ݺ�����
;        UD : Up/DOWN (���밪�� LEVEL)
;      TXT1 : ���� ���� text, ���̸� scale�� �ν��Ѵ�.
; �����ִ� �� - ���� ��ǥ
;******************************************

(defun F_DA(SP ANG DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;���� ũ�� ����

  (setq ds (getvar "DIMSCALE"))                             ;scale factor
  
  (if (listp ud)			;ud�� point�� ���
    (if (minusp (dang ang (angle sp ud)))	;ġ����������.. ������~dimpoint���� ��.
      (setq sgn -1)
      (setq sgn 1)
    );if      
    (if (> ud 0)                                              ;�� �Ʒ�
      (setq sgn 1)
      (setq sgn -1)
    ) ;if
  );if    
  
  (setq ang1 ang)
;  (setq a2sgn ud)
  (setq a2sgn sgn)
  (setq tsgn 1)
  (setq w4 (which4 ang1))
  (cond                                       ;�ؽ�Ʈ�� ���� (����� �ɼ�)    
    ((or (= w4 1) (= w4 4)) (setq tang ang1))    
    ((or (= w4 2) (= w4 3)) (setq tang (- ang1 pi)))    
  ) ;of cond    
  
  (if (and (or (= w4 2) (= w4 3)) (= a2sgn 1)) (setq tsgn -1))    
  (if (and (or (= w4 1) (= w4 4)) (= a2sgn -1)) (setq tsgn -1))

  (setq next (* dst n))                                     ;���������� �������� �Ÿ�
  (setq ep (polar sp ang next))              ;ep ��ġ���

  (if (listp ud)				;ġ���� defpoint���� ġ���������� �Ÿ�
    (setq dmdst (distance ud (inters sp ep ud (polar ud (v_angle sp ep ud) 10) nil)))
    (setq dmdst (* ds (+ 20 (* dim_gap (1- (abs ud))))))
  );if

  
  (if (listp ud)
    (setq dxy ud)
    (setq dxy (polar ep (+ ang (* 0.5 pi)) (* dmdst sgn)))  ;ġ���� ��ġ
  );if  

  (setq dx (distance sp ep))                          ;�Ÿ� ���

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;����� �ɼ��� ���
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos n 2 0))                          ;���� ���� ���
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000�̸��� ��
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000�̻��� ��
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text��ü����
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;ġ�������� ���� text �ȵ���
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;�� �Ʒ� ���ٷ� ������
          (setq dtxt2 (strcat "=" txt))
;          (setq dtxt1p (mapcar '+ (mid-point sp ep)
;                                  (list 0.0 (+ (* dmdst sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (polar (mid-point sp ep)    
                              (+ ang1 (* pi 0.5 a2sgn))    
                              (- dmdst (* th ds tsgn))))
	  
          (command "TEXT" "M" dtxt2p (* th ds) (rtod tang) dtxt2)
          (command "DIM1" "ALI" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn THEN
        (progn                                 ;ġ�������� ���� text ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "ALI" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "ALI" sp ep dxy txt1)             ;DIM��� ����
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun

;******************************************
; Program : F_DATEXT
;		F_DA�� ����ϳ� ġ������ �׸��� �ʰ� TEXT�� ó���Ѵ�.
;           Function Dimension Aligned TEXT only
;           Jong-Suk Yi
;           06/08/05
;******************************************
; ������ġ������ �Լ��� ó�����ش�.
; �Ѿ���� ����
;        SP : ����
;        ANG : ����
;       DST : �Ÿ�
;         N : �ݺ�����
;        UD : side point ;  Up/DOWN (���밪�� LEVEL) 06.08.05 ����
;      TXT1 : ���� ���� text, ���̸� scale�� �ν��Ѵ�.
; �����ִ� �� - ���� ��ǥ
;******************************************

(defun F_DATEXT(SP ANG DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;���� ũ�� ����

  (setq ds (getvar "DIMSCALE"))                             ;scale factor
  
  (if (listp ud)			;ud�� point�� ���
    (if (minusp (dang ang (angle sp ud)))	;ġ����������.. ������~dimpoint���� ��.
      (setq sgn -1)
      (setq sgn 1)
    );if      
    (if (> ud 0)                                              ;�� �Ʒ�
      (setq sgn 1)
      (setq sgn -1)
    ) ;if
  );if    
  
  (setq ang1 ang)
;  (setq a2sgn ud)
  (setq a2sgn sgn)
  (setq tsgn 1)
  (setq w4 (which4 ang1))
  (cond                                       ;�ؽ�Ʈ�� ���� (����� �ɼ�)    
    ((or (= w4 1) (= w4 4)) (setq tang ang1))    
    ((or (= w4 2) (= w4 3)) (setq tang (- ang1 pi)))    
  ) ;of cond    
  
  (if (and (or (= w4 2) (= w4 3)) (= a2sgn 1)) (setq tsgn -1))    
  (if (and (or (= w4 1) (= w4 4)) (= a2sgn -1)) (setq tsgn -1))

  (setq next (* dst n))                                     ;���������� �������� �Ÿ�
  (setq ep (polar sp ang next))              ;ep ��ġ���

  (if (listp ud)				;defpoint���� ġ���������� �Ÿ�
    (setq dmdst (distance ud (inters sp ep ud (polar ud (v_angle sp ep ud) 10) nil)))
    (setq dmdst (* ds (+ 20 (* dim_gap (1- (abs ud))))))
  );if

  
;  (if (listp ud)
;    (setq dxy ud)
;    (setq dxy (polar ep (+ ang (* 0.5 pi)) (* dmdst sgn)))  ;ġ���� ��ġ
;  );if  

  (setq dx (distance sp ep))                          ;�Ÿ� ���

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;����� �ɼ��� ���
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos n 2 0))                          ;���� ���� ���
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000�̸��� ��
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000�̻��� ��
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text��ü����
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;ġ�������� ���� text �ȵ���
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;�� �Ʒ� ���ٷ� ������
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt2p (polar (mid-point sp ep)    
                              (+ ang1 (* pi 0.5 a2sgn))    
                              (- dmdst (* th ds tsgn))))
	  

	  (djdg_wtxtonline1 sp ep ud dtxt1 (* th ds) (* th ds))
          (djdg_wtxtonline1 sp ep ud dtxt2 (* th ds) (* th ds -1))    ; "=1.200"
        ) ;of progn THEN
        (progn                                 ;ġ�������� ���� text ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
	  (djdg_wtxtonline1 sp ep ud dtxt1 (* th ds) (* th ds))
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;�����Է½� �� text�� ��
      	 (djdg_wtxtonline1 sp ep ud txt1 (* th ds)  (* th ds))
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun

;----------------------
; function : djdg_splitprenum
;            split prefix and number
;            S1�� ���� ����+��ȣ �� ����(prefix)�� ���ڷ� �и��Ѵ�.
;            Yi Suk Jong
;            05/08/08
;----------------------
(defun djdg_splitprenum( numstr / lstr i nonum return ch)
  (setq lstr (strlen numstr)) ;string length
  (setq i lstr)				;�ǵޱ��ں���
  (setq nonum 0)
  (while (and (> i 0) (= nonum 0))
    (setq ch (ascii (substr numstr i 1)))
    (if (or (< ch 48) (> ch 57)) (setq nonum i))
    (setq i (1- i))
  );repeat 
  (if (= lstr nonum)
    (setq return nil)
    (progn
      (if (= 0 nonum)
	(setq pre "")
	(setq pre (substr numstr 1 nonum))
      );if
      (setq num (substr numstr (1+ nonum) (- lstr nonum)))
      (setq return (list pre num))
    );progn
  );if
  return
);defun

; -------------------------------------
; function : mk_vertlist
; LwPolyline�� vertex list�� ������ش�.
; �μ�: vname  : vertext entity name
;                 (car (entsel)) ���·� �Ѿ�;��Ѵ�.
; return: (vertext list �� nvert�� list�� ��� ������)
; -------------------------------------

  (defun mk_vertlist (vname
  /  count nvert index tmp vert_list pt1
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

;---------------------------------
;function : djdg_splitstr
;           string�� �־��� ���ڿ��� split�Ѵ�.
;           ex)  (djdg_splitstr "ab+123" "+") --> ("ab" "123")
;           Yi Suk Jong
;           05/12/20
;---------------------------------
(defun djdg_splitstr(str ptstr
		     / lstr lptstr n i lastsearch return startp nn spstr tail)
  (setq lstr (strlen str)
        lptstr (strlen ptstr))
  (setq n (- lstr lptstr -1))   ;��ü����-patternstr����+1
  (setq i 1
	lastsearch 0
	return nil)
  (repeat n
    (if (= (substr str i lptstr) ptstr)
      (progn
	(if (= lastsearch 0)
	  (setq startp 1)
	  (setq startp (+ lastsearch lptstr)) ;������ġ.
	);if  
	(setq nn (- i startp))  ;�������.
	(setq spstr (substr str startp nn))  ;����� string
;	(if (and (/= spstr "") (/= spstr "\t"))
	(if (/= spstr "")	
          (setq return (append  return (list spstr)))  ;����� sring�߰�.""�϶� �߰����� ����.
	);if  
	(setq lastsearch i)
      );progn  
    );if  
    (setq i (1+ i))
  );repeat
  (setq tail (substr str (+ lastsearch lptstr) (- lstr lastsearch)))  ;������ �κ�.
  (if (/= tail "")
    (setq return (append  return (list tail)))
  );
  return
);defun

;---------------------
; function : djdg_attwrite
;            attribute�� ���Ե� insert�� att�� �������ش�.
;		Yi Suk Jong
;	02/12/20
;---------------------
;(djdg_attwrite ename tvlist)
;  ename : entity name
; tvlist : tag and value list (("MARK1" "A1") ("DIA" "D29") ...)
(defun djdg_attwrite (#blk_nm #_lst2 / #_rpt #_tag #_val eea_lst #_chk #_blk)
;
  (setq #_rpt 0)
  (repeat (length #_lst2)
    (setq #_tag   (car  (nth #_rpt #_lst2))
          #_val   (cadr (nth #_rpt #_lst2))
          #_blk   #blk_nm
          #_chk   t)
    (while #_chk
      (setq #_blk   (entnext #_blk)
            eea_lst (entget  #_blk) ) ;
      (if (= (cdr (assoc 2 eea_lst)) #_tag)
        (progn
          (setq eea_lst (subst (cons 1 #_val) (assoc 1 eea_lst) eea_lst))
          (entmod  eea_lst)
          (setq #_chk nil)
        ) ; p
      ) ; i
    ) ; w
    (setq #_rpt (+ #_rpt 1))
  ) ; r
;
  (entupd #blk_nm)
;
) ; de_fun


;------------------------
; function : djdg_gettag
;            get tag
;            Yi Suk Jong
;            05/04/15
;-----------------------
; ��ƼƼ ���������� �ָ� ������ ���� tag�� ���� �����ش�.
;-----------------------
; argument : etsel : (entsel)�� return��.
; return : (tag value)
(defun djdg_gettag(etsel / return spnt ename einf ipnt txt tbox tbox1 tbox2 pnt2 xspnt yspnt tag)
  
;  (setq sel (entsel "\nSelect Att: "))  ;att����.
  (setq return nil)
  (setq spnt (cadr etsel))  ;selection point
  (setq ename (car etsel))  ;entity name
  (while (= (cdr (assoc 0 (entget (setq ename (entnext ename))))) "ATTRIB")
    (setq einf (entget ename))
    (setq ipnt (cdr (assoc 10 einf)))
    (setq txt (cdr (assoc 1 einf)))  ;text����.
    (setq tbox (textbox einf))
    (setq tbox1 (car tbox)
	  tbox2 (cadr tbox))
    (setq pnt2 (list (+ (car ipnt) (car tbox2))
		     (+ (cadr ipnt) (cadr tbox2))))
    (setq xspnt (car spnt)
	  yspnt (cadr spnt))
    (setq tag (cdr (assoc 2 einf)))
;    (command "circle" (list xspnt yspnt))
    (if (and (and (<= xspnt (car pnt2)) (>= xspnt (car ipnt)))
	     (and (<= yspnt (cadr pnt2)) (>= yspnt (cadr ipnt))))
      (progn
;	(command "rectang" ipnt pnt2)
	(setq return (list (cdr (assoc 2 einf)) (cdr (assoc 1 einf))))
      );progn
    );  
  );while
  return
);defun  


;------------------------
; function : djdg_getattv
;		attribute�� ��ü ������ �о� list�� ���� ��ȯ�Ѵ�.
;	Yi Suk Jong
;	05/12/20
;------------------------
; (djdg_getattv entn) --> (("Mark1" . "A") ("Len" . "L=12.330"))
;   entn: entity name
(defun djdg_getattv( entn / en return ei)
  (setq en entn)
  (setq return nil)
   (while (/= (cdr (assoc 0 (entget (setq en (entnext en))))) "SEQEND")
    (setq ei (entget en))
    (if (= (cdr (assoc 0 ei)) "ATTRIB")
      (setq return (append return (list (cons (cdr (assoc 2 ei)) (cdr (assoc 1 ei))))))
    );if
  );while
  return
);defun


;-------------------
; function : djdg_cdimdan
;            ġ������ �ܼ��� �־��� ���ڸ�ŭ �ø��ų� ���δ�.
;      	Yi Suk Jong
;	05/12/27
;------------------
; (djdg_cdimdan ename cdan)
;   ename: dimension entity name
;   cdan : ���� ex;1 --> 1�� ���� -1-->1�� ����.
(defun djdg_cdimdan(ename cdan / deni as10 as14 ang nas10 ndeni)
  (setq deni (entget ename))		;dimension����
  (setq as10 (cdr (assoc 10 deni)) 	;dimension point
        as14 (cdr (assoc 14 deni)))
  (setq ang (angle as14 as10))		;ġ�������� ����.
  (setq nas10 (polar as10 ang (* (getvar "DIMDLI") (getvar "DIMSCALE") cdan))) ;�� dimpoint���ϱ�
  (setq ndeni (subst (cons 10 nas10) (assoc 10 deni) deni))	;�������� ��ġ��
  (entmod ndeni)	;����
);defun  


;-----------------------
; function : djdg_angofdimline
; 	�־��� ġ���� ġ����(dimension line) ����.
; 	Yi Suk Jong
;	05/12/30
;-----------------------
; (djdg_angofdimline( ename )
(defun djdg_angofdimline( ename / enti as70 return)
  (setq enti (entget ename))  ;entity ����
  (setq as70 (cdr (assoc 70 enti)))   ;dim type aligned or Horizontal
  (if (= (- as70 32) 0)
    (setq return (cdr (assoc 50 enti)))	;horizontal �̳� vertivcal�� ���
    (setq return (angle (cdr (assoc 13 enti)) (cdr (assoc 14 enti))))
  );if   
);defun


;****************************************
; Function : CROSS
;            CROSS point of arc & line
;            By Suk-Jong Yi
;            1995/6/26
;****************************************
;�Լ�: ȣ�� ������ ������ ã��
;     �μ�: ARC entity list(entget����), ������ ù�� , ������ ����
;     ���: ������ ARC�� ������

(defun CROSS(aent sp ep /
aent    sp      ep      a       b       r       sa      ea      x1      x2
y1      y2      c       d       a1      b1      c1      x1      x2      y1
y2      ang1    ang2
)

;(push-env)
(setq a (car (cdr (assoc 10 aent))))      ; ARC entity�� �߽��� x��ǥ
(setq b (cadr (cdr (assoc 10 aent))))     ; ARC entity�� �߽��� y��ǥ
(setq r (cdr (assoc 40 aent)))            ; ARC entity�� ������
(setq sa (cdr (assoc 50 aent)))           ; ARC entity�� ���� ����
(setq ea (cdr (assoc 51 aent)))           ; ARC entity�� �� ����

(setq x1 (car sp))                        ; LINE entity�� ������ x��ǥ
(setq x2 (car ep))                        ; LINE entity�� ���� x��ǥ
(setq y1 (cadr sp))                       ; LINE entity�� ������ y��ǥ
(setq y2 (cadr ep))                       ; LINE entity�� ���� y��ǥ
(if (= (- x1 x2) 0)
  (progn                                    ;x�� constant�� ��
    (setq c x1
          a1 1                              ;y�� ���� 2���������� a
          b1 (* -2 b)                       ;y�� ���� 2���������� b
          c1 (+ (* c c) (* -2 a c) (* a a) (* b b) (* -1 r r)) ;y�� ���� 2���������� c
          y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;�� 1
          y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;�� 2
    );setq
  );progn
  (progn                                    ; y�� x�� �Լ��� ��
    (setq c (/ (- y1 y2) (- x1 x2)))          ; y=cx+d���� c
    (setq d (- y2 (* c x2)))                  ; y=cx+d���� d
    (setq a1 (+ 1 (* c c)))                   ; x�� ���� ������������ a
    (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))   ;x�� ���� ������������ b
    (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))  ;���� �������� c
    (setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;�� 1
    (setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;�� 2
    (setq y1 (+ (* c x1) d))                  ;�� 1�� �� y��
    (setq y2 (+ (* c x2) d))                  ;�� 2�� �� y��
  );progn
)

(setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))   ;����1�� ���밢(��������)
(setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))   ;����2�� ���밢(��������)

(if (inang sa ea ang1)
  (list x1 y1 0.0)         ;����1�� ȣ�� ���۰��� ���� ���̿� ������ ���� ������
  (if (inang sa ea ang2)   ;����2�� ȣ�� ���۰��� ���� ���̿� ������ ���� ������
    (list x2 y2 0.0)
    nil                    ;���� 1�� 2�� ��� �� ������ ��� ��� nil������
  ) ;of if
) ;of if
)

;************************************************************
; Function : INANG
;            a angle is IN the range of ANGle-1 and angle-2 ?
;            By Suk-Jong Yi
;            1995/6/26
;*************************************************************
;� ���� �־��� �ΰ�(ang1, ang2) ���̿� �ִ°�?
; �ΰ� ���̿� �ִ� ��� �ΰ��� ���̸� �����ְ�
; �ΰ� ���̿� ���� ���� nil�� �����ش�.
;*************************************************************

(defun inang(a1 a2 a3 /             ;�μ� ����
a1 a2 a3                            ;�������� ����
)
(if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))   ;ù���� �ι�° ������ ũ�� +360��
(if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;�־��� ���� �ΰ����̿� ������
                                nil)         ; �ΰ��� ���̸� ������
)                                            ; �ΰ� ���̿� ������ nil������



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


;--------------------------------------------------------
; function : ali_Pdim
;            Aligned Point List dim
;		point list�� �̿��Ͽ� dimension �׸���
;             Yi Suk Jong
;            06/08/04
;--------------------------------------------------------
; ���: point list�� �޾Ƽ� dimension�Ǵ� text�� �׸���.
; �̶� �ɼ���
; defpoint : definition point ġ�����϶��� defpoint, text�� ���� ���ؼ� ��ġ�̴�.
; dirpoint : direction point ġ���� �������
; sidepoint: side point ġ������ ��������� ��ġ��ų ���ΰ��� �����ϴ� ��
; textout : text�� ����� ���ΰ�? nil�̸� dimension���� ���
; factor : point list�� �̷���� �Ÿ��� ������ ���� skew�� R�� ����ϱ� ���� �ɼ�
; delta : ���������� �Ÿ����� ���ų� ������ ����
; prefix, postfix : �ؽ�Ʈ �յڷ� �� ����
; oridist : �������� ���뿩�� nil�̸� ������(�� factor�� delta�� �����) ���̷� �׸���
; gol : @(�����) ���뿩�� nil�̸� �� ġ���� ǥ����
;--------------------------------------------------------
; �˰���
; 1. ġ�����������(����)�� side����(����)�� ���Ѵ�.
; 2. �������� ���������� �Ÿ����� ���Ѵ�.
; 3. �Ÿ��������� sort�Ѵ�.
; 4. �ι�°������ ������ ������ ġ������ �ۼ��Ѵ�.
;  4.1 ���Ÿ��� text�� ��ȯ�Ѵ�.
;  4.2 �հŸ��� ������ ������ ī��Ʈ �ϳ��� �ø��� �׸��� �ʴ´�
;  4.3 �հŸ��� �ٸ��� ��ġ�� ���� �̹� ġ���� ���´�.
; 5. 4�� �ݺ�
;--------------------------------------------------------

(defun ali_pdim( plist defpnt dirpnt sidepnt textout factor delta prefix postfix oridist gol
		/ dirang sideang npnt dplist pnt dist sdplist rdefpnt index dstlst dfdstlst
		  dst ndst nsame idst ipnt dsti dfdsti dstxt dimtxt ppdimtxt odimtxt odst odstxt
		)
;test�� ������(������ ����.)
;(setq plist nil
;      plist ;'((0 1) (1 2) (2 1) (4 5))
;            (repeat 5 (setq plist (append plist (list (getpoint "\nPoint: ")))))
;       defpnt (getpoint "\ndefpoint: ")
;      dirpnt (getpoint defpnt "\ndirpoint: ")
;       sidepnt (getpoint "\nsidepoint: ")
;      textout T
;       factor 1.0
;      delta 100.0
;       prefix "("
;      postfix ")"
;       oridist T
;      gol T
;);setq 
  
; 1. ġ�����������(����)�� side����(����)�� ���Ѵ�.
      (setq dirang (angle defpnt dirpnt) 		;ġ���� �������)
	    sideang (v_angle defpnt dirpnt sidepnt))	;ġ���� ��ġ����
  
; 2. �������� ���������� �Ÿ����� ���Ѵ�.
      (setq npnt (length plist))			;point ����
      (setq dplist nil)			;distance and point list((�Ÿ� (x y z)) (�Ÿ� (x y z)) ... )
      (foreach pnt plist
	(setq dist (djdg_angdist defpnt pnt dirang))		;����������ΰŸ�
	(setq dplist (append dplist (list (list dist  pnt))))
      );repeat	

; 3. �Ÿ��������� sort�Ѵ�.
      (setq sdplist (vl-sort dplist
		       		'(lambda (s1 s2)
    				(< (car  s1) (car s2)))))  ;ù��° ��Ҹ� �������� sort
  
   ;----- ���� def���� ���Ѵ�. �Է�def������ ���ఢ���� ù�Ÿ���ŭ ��������.
  (setq rdefpnt (polar defpnt dirang (car (nth 0 sdplist))))	;real def point

  
; 4. �ι�°������ ������ ������ ġ������ �ۼ��Ѵ�.
  ;----- ������ list�θ����.
      (setq index 1
	    dstlst nil)				;���� list (3 2 4)
      (repeat (1- npnt)
	(setq dstlst (append dstlst (list (- (car (nth index sdplist))
					     (car (nth (1- index) sdplist))))))
	(setq index (1+ index))
      );repeat
  

   ;----- ����list�� factor
  (setq fdstlst nil)
  (foreach dst dstlst
    (setq fdstlst (append fdstlst (list (* dst factor)))) ;factored dstlst
  );foreach
  
  ;----- ���� list�� delta�� �����Ѵ�.
  (setq dfdstlst nil)
  (foreach dst fdstlst
    (setq dfdstlst (append dfdstlst (list (+ dst delta)))) ;factored dstlst
  );foreach

  ; ���ݰ�����ŭ ġ���� or text�׸���
  (setq ndst (length dstlst))		;���ݰ���
  (setq nsame 1)
  (setq idst 0)
  (setq ipnt rdefpnt)			;������
  (repeat ndst
    (setq dsti (nth idst dstlst)	;�����Ÿ�
	  dfdsti (nth idst dfdstlst))	;�����Ÿ�
    
    (if (= oridist nil)			;���� �Ÿ��� �׸��� ���ִ� ��� 
      (setq dsti dfdsti)		;�̹� �Ÿ� �̾Ƴ���(�����Ÿ�, factor, delta ������)
    );if

    (setq dstxt dfdsti)			;text������ �Ÿ�(text������ �Ÿ��� ������ �����Ÿ��� ��)
    (setq dimtxt (rto_dimtxt dfdsti))	;�Ÿ��� dimension text(�����Ÿ��� ��)
    
    (if (> idst 0)			;�ι�° �Ÿ����� ��
      (if (/= gol nil)			;@�ɼ��� ���ִ� ���
	(progn
          (if (/= dimtxt odimtxt)		;�� ġ���ϰ� �ٸ����
	    (progn				;�ٸ���쿡�� ġ���� �׸���
	      (setq ppdimtxt (strcat prefix (goltotaltxt nsame odstxt) postfix)) ;prefix,postfix����
	      (if textout  ;textout option�� ���ִ� ��쿣 text���
		(setq ipnt (f_datext ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  text
	        (setq ipnt (f_da ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  dimension
	      );;if	
	      (setq nsame 1)				;���� ���� �ʱ�ȭ
	    );progn
	    (setq nsame (1+ nsame))				;���� ��쿡�� ī��Ʈ 1 �ø���
          );if
        );progn
	(progn					;����� �ɼ��� ���ִ� ��� ��� ǥ��
	  (setq ppdimtxt (strcat prefix (goltotaltxt nsame odstxt) postfix))	;prefix,postfix����
          (if textout  ;textout option�� ���ִ� ��쿣 text���
   	    (setq ipnt (f_datext ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  text
	    (setq ipnt (f_da ipnt dirang odst nsame sidepnt ppdimtxt))
	  );if  
	  (setq nsame 1)
	);progn
      );if
    );if
    (setq odimtxt dimtxt	;������ ������ old�����... 
	  odst dsti
	  odstxt dstxt) 	;old dimtext, old dist ����
    (setq idst (1+ idst))		;���� ����
  );repeat
  (setq ppdimtxt (strcat prefix (goltotaltxt nsame odstxt) postfix))	;prefix,postfix����
  (if textout  ;textout option�� ���ִ� ��쿣 text���
    (setq ipnt (f_datext ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  text
    (setq ipnt (f_da ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw horizontal dimension
  );if  
);defun


;-------------------------
; function : djdg_angdist
;            distance angle
;            � ���� �־��� ������������ �Ÿ�
;            Yi Suk Jong
;-------------------------
(defun djdg_angdist(bp tp ang / deltaang dist)
  (setq deltaang (dang ang (angle bp tp)))
  (setq dist (* (cos deltaang) (distance bp tp)))
);defun


;---------------------
; function : goltotaltxt
;            �־��� ������ ������ �̿��� ��ü�渦 ���Ѵ�.
;             ex) (djdg_makegoltotal 2 1250) --> "2@1.250=2.500")
;		Yi Suk Jong
;		06/08/04
;---------------------
;
(defun goltotaltxt(n d / )
  (if (< n 2)
    (setq return (rto_dimtxt d))
    (setq return (strcat (rtos n 2 0) "@"
			 (rto_dimtxt d) "="
			 (rto_dimtxt (* n d))))
  );if
  return
);defun

;---------------------------
; textboxm	: textbox middle
;		: �־��� text�� �߾��� ã��.
;		Yi Suk Jong
;		06/08/06
;---------------------------
; argument: entity name
; �˰���
; insertion point���ϱ� > ���� ���ϱ� > ȸ������ textbox bound���ϱ� > ȸ����Ű�� > ��������ϱ�
(defun textboxm(ename / eni ipnt tb tb1 tb2 mpnt)
        (setq eni (entget ename))
        (setq ipnt (cdr (assoc 10 eni))		;entity���� insert point
	      ang (cdr (assoc 50 eni)))		;angle	
        (setq tb (textbox eni))		;
        (setq tb1 (mapcar '+ (car tb) ipnt))	;insert point�� textbox���ϱ�
        (setq tb2 (mapcar '+ (cadr tb) ipnt))

        (setq tb11 (rotatepnt tb1 ipnt ang)
	      tb22 (rotatepnt tb2 ipnt ang))
  
        (setq mpnt (mid-point tb11 tb22))
  mpnt
);defun textboxm


;-----------------------------
; function : rotatepnt
;            ��¾���� base pont�� �������� ȸ���� ��ǥ�� ���Ѵ�.
; 	Yi Suk Jong
; 	06/08/06
;-----------------------------
; argument: p1   : ��� ��ǥ'(1 1 1)
;	    bpnt : ȸ��������'(1 1 1)
;	    ang  : ȸ������ (radian)
;�˰���
; �����ǥ�� ������ǥ�� x,y,z�� �и� > �� ��ǥ�� ���� ���� > ȸ�� > ��������ǥ����

(defun rotatepnt(p1 bpnt ang / x y z x0 y0 z0 xx yy zz x1 y1 z1)
  (setq x  (nth 0 p1) 				;ȸ����� �� x,y,z
	y  (nth 1 p1) 
	z  (nth 2 p1)) 
  (setq x0  (nth 0 bpnt) 			;������ x,y,z
	y0  (nth 1 bpnt) 
	z0  (nth 2 bpnt))
  (setq xx (- x x0)				;�������� �������� �� ������ǥ��
	yy (- y y0)
	zz (- z z0))
  (setq x1 (- (* xx (cos ang)) (* yy (sin ang)))  ;������ǥ�迡�� ȸ��
	y1 (+ (* xx (sin ang)) (* yy (cos ang)))
	z1 z)
  (list (+ x1 x0) (+ y1 y0) (+ z1 z0))		;��������ǥ�� ���Ͽ� return
);defun


;--------------------------------------
; Function: farest
;           seek farest 2 points
;           Yi Suk Jong
;           00/5/20
;--------------------------------------
; arguments :
;   points : point list
;--------------------------------------
(defun farest(points
         / points index1 far npoints point1 index2 point2 dist12 pnt1 pnt2)	      
  (setq index1 0
        far 0
	npoints (length points))
  (repeat npoints
    (setq point1 (nth index1 points))
    (setq index2 0)
    (repeat npoints
      (setq point2 (nth index2 points))
      (setq dist12 (distance point1 point2))
      (if (>= dist12 far)
	(progn
	  (setq far dist12)
	  (setq pnt1 point1
		pnt2 point2)
	);progn	 
      )
      (setq index2 (1+ index2))
    );repeat  
    (setq index1 (1+ index1))
  )
  (list pnt1 pnt2)
);defun



;--------------------------------
; function : crossent
;		�� entity�� ������ �����ش�.
;		Yi Suk Jong
;		06/08/13
;--------------------------------
; argument: ent1 : entity name - 1
;	    ent2 : entity name - 2
;	    opt  : 0 ;�Ѵ� Ȯ����� 1: ù��° Ȯ�� 2:�ι�° Ȯ�� 3: �Ѵ�Ȯ��
; return: ((x y z) (x y z))  ������ list
;--------------------------------
(defun crossent( ent1 ent2 opt / pts aobj1 aobj2 ipts )
    (setq pts nil)
    (setq aObj1 (vlax-ename->vla-object ent1))
       (setq aObj2 (vlax-ename->vla-object ent2)
            iPts  (vla-intersectwith ; Find intersections of Objects
                    aObj1
                    aObj2
                    opt
                  )
                    ; variant result
            iPts  (vlax-variant-value iPts)
      )
                    ; Variant array has values?
      (if (> (vlax-safearray-get-u-bound iPts 1) 0)
        (progn      ;array holds values, convert it
          (setq iPts ;to a list.
                 (vlax-safearray->list iPts)
          )
                    ;Loop through list constructing points
          (while (> (length iPts) 0)
            (setq Pts  (cons (list (car iPts) (cadr iPts) (caddr iPts)) Pts)
                  iPts (cdddr iPts)
            );setq 
          );while
        );progn
      );if
  Pts
);defun                   ;return list of points found


;-------------------------
; function : djdg_readdatafile
; 		read data file
;            data file�� �о��ش�. �̶� ";"���Ĵ� �����ϰ� �����ش�.
;            Yi Suk Jong
;            05/03/20
;------------------------
; usage : (djdg_readdatafile fn)
; arguments: fn : file name
; return: data���� ����  (";"���Ĵ� ���õ� ����� list�� ��� �����ش�.
;------------------------
(defun djdg_readdatafile( fn
			 / return opf ch divstr
			 )
  (setq return nil)
  (setq opf (open fn "r"))   ; open file
  (if opf
    (progn
      (while (setq ch (read-line opf))
	(setq ch (vl-string-subst "" "\t" ch)) ;tab���� ����
	(setq divstr (divide_str ch ";"))   ; ";"�� �и�.
	(setq ch (car divstr)) 			;�� string������.
	(if (> (strlen ch) 0)				;string ���̰� 0�� �ƴѰ�츸 ����.
	  (progn
	    (setq return (append return (list ch)))     ;string �߰�
	  );progn  
	);if  
      );while
      (close opf)        ;close file
      return
    );progn
  );if opf  
);defun

;--------------------------
; function : point list�� Ư�� ����(x,y,z,�Ÿ�)�� �������� sort
; 		06/08/11
;--------------------------
; ptlist : point list
; idx : assoc���� �ڵ�..(ex: �����̸� 0:x, 1:y, 2:z 3:�Ÿ�)
; ipt : ������(idx�� 3�϶��� ���ȴ�) nil�̸� ����������� �������κ��� �Ÿ��� sort
;--------------------------
(defun point-sort(plist idx ipt
		   / return farpnt ipt pnt dstlist p dst dstplist sdstplist dp )
;  (setq ne (length plist))

  (cond
    ((< idx 3)
     (setq return (vl-sort plist '(lambda (s1 s2)
				    (< (nth idx  s1) (nth idx s2)))))
				    
    );subcond
    ((= idx 3)
      (setq farpnt (farest plist))		;���� �� �� ã��
      (if (= ipt nil)			;���� �������� �־����� ������.
        (setq pnt (car farpnt))		;����� �� �ΰ��� �տ����� ����������...
        (setq pnt ipt)			
      );  
      (setq dstplist nil)
      (foreach p plist
        (setq dst (distance pnt p))
        (setq dstplist (append dstplist (list (list dst p))))
      );foreach

      (setq sdstplist (vl-sort dstplist '(lambda (s1 s2)	;�������������� �Ÿ��� sort
    				(< (car  s1) (car s2)))))
      (setq return nil)
      (foreach dp sdstplist
	(setq return (append return (list (cadr dp))))
      );foreach	
    );subcond
    
  );cond 
);