;-----------------------------------
; DJDG Function library
;------ 일반 -----------------------
; djdg_issamepnt : is there same point
;------ 기하 -----------------------
; v_angle(p1 p2 p3) : p1과 p2가 이루는 직선에 직각이며 p3를 지나는 각도
; asin(a) 	: arcsin값 return(radian)
; cross		: CROSS point of arc & line
; crossent	: 두 entity의 교점을 구해준다.
; cp_line_pline : cross point of line and polyline
; INANG		: a angle is IN the range of ANGle-1 and angle-2 ?(어떤각이 두각 사이에 있는가?)
; djdg_angdist 	: distance angle : 어떤 점의 주어진 각도방향으로 거리
; textboxm	: textbox middle : 주어진 text의 중앙점 찾기.
; function 	: rotatepnt	 : 어떤점의 회전후 좌표를 구한다.
; Function	: farest 	: seek farest 2 points
;------ entity관련 -----------------
; mk_vertlist 		: LwPolyline의 vertex list를 만들어준다.
; djdg_getpoints 	: get points of entity
; rib_dim(str)
; getLwVert (vlist tmpctr )
;------ Draw 관련 -----------------------
; djdg_insertarw1(p1 p2) 		: insert half arrow
; djdg_insertblk(bname p1 p2 dimscl) 	: insert block
; djdg_insertblkas(bname p1 p2) 	: insert block with angle & scale
; djdg_wtxtonline(p1 p2 text th gap) 	: write text on the line
; djdg_wtxtonline1(p1 p2 p3 text th gap): 기준선과 방향점을 이용하여 텍스트 쓰기
; djdg_rect(p1 ang l t opt) : draw rectanlular
; djdg_sldimage : draw sldimage
; djdg_mtredraw(ss mode) :  multi redraw
;-------- 문자열 관련 ----------------------------
; data-in :
; str_position(str1 str2)
; SP-TRUNC : 문자열의 앞뒤의 공백을 삭제해준다.
; IS-NUM : 숫자인지확인해준다.
; djdg_splitprenum 	: S1과 같은 문자+번호 를 문자(prefix)와 숫자로 분리한다.
; djdg_splitstr	; string을 주어진 문자열로 split한다.
; divide_str(arg1 arg2) : string arg1을 arg2기준으로 나눔
;--------  dimension 관련 ------------------------
; djdg_splitdimtxt	; dimtext를 split한다.; ex) 10@1.250=12.500  --> ("10" "1.250" "12.500")
; djdg_splitdimtxtnum; dimtxt를 잘라서 숫자로 넘겨준다.
; 				 ex)10@1.250=12.500  --> (10 1250 12500) "1.250" --> 1250과 같이 숫자화(dimlfac참조)
; djdg_divlen	; 길이(len)를 pitch로 나누고 몫과 나머지를 돌려줌 ;            ex) dst=1250 pitch=150 --> mok = 8, re = 50 --> (8 50)
; rto_dimtxt(l)   	; real to dimtxt;  ex)  (rto_dimtxt 1000)  --> "1.000" (rto_dimtxt 999)  --> "999"
; djdg_dimtxtoreal	; dimtext를 실제 값으로 바꿔준다. ex) "1.250" --> 1250
; djdg_getdimlen  	; get dim length; dimension string의 전체길이를 구해줌.
;				  ex) "150+2@250+150" --> 800
; F_DH		; Function Dimension Horizontal
; F_DV		; Fuction Dimension Vertical
; F_DA		; Function Dimension Aligned
; djdg_angofdimline  ; 주어진 치수의 치수선(dimension line) 각도.
; djdg_makegoltotal  ; 주어진 개수와 간격을 이용해 전체길를 구한다.
;				  ex) (djdg_makegoltotal 2 1250) --> "2@1.250=2.500")
;--- sort function
; point-srot : sort points using base point or x, y

;------ Attribute관련
; djdg_attwrite	; attribute가 포함되 insert의 att를 수정해준다.
; djdg_gettag	; get tag ; 엔티티 선택정보를 주면 선택한 점의 tag와 값을 돌려준다.
; djdg_getattv	; attribute의 전체 값들을 읽어 list로 묶어 반환한다.

;------ file 관련
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
; asin(a) : arcsin값 return(radian)
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
; 이 함수는 주어진 string을 기준이 되는 문자로 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************
(defun divide_str(arg1 arg2
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt    arg1     arg2 
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs arg2) (= subs ""))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (setq rslt (list lst))                  ;돌림값이 비었을때
               (setq rslt (append rslt (list lst)))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC



; -------------------------------------
; function : getLwVert
; LwPolyline의 Vertex를 척아
; 인수: vlist  : vertext list
;       tmpctr : 접근할 vertext 번호 0,1,2
; -------------------------------------

  (defun getLwVert (vlist tmpctr / count tmp)
;    (setq vlist (entget (car (entsel))))                       ;실

    (setq count 0)                                      ;첫 vertex 찾아감
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
  (setq ds (getvar "DIMSCALE"))                             ;스케일 값  
  (setq ang (angle p1 p2)                                   ;두점이 이루는 각
        w4  (which4 ang))                                   ;몇사분면인가?
  (if (or (= w4 1) (= w4 4))                                ;1,4분면일경우
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
  (setq ds (getvar "DIMSCALE"))                             ;스케일 값  
  (if (= dimscl nil) (setq ds 1.0))			;
  (if (= p2 nil)
    (setq ang 0)
    (setq ang (angle p1 p2))                                   ;두점이 이루는 각
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
  (setq scl (distance p1 p2))                             ;스케일 값  
  (if (= p2 nil)
    (setq ang 0)
    (setq ang (angle p1 p2))                                   ;두점이 이루는 각
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
;            기준선과 방향점을 이용하여 텍스트 쓰기
;            wtxtonline의 경우 기준선의 위아래를 gap으로 조절하기 때문에 컨트롤이 어려움
;            write text on the line
;            Yi Suk Jong
;            06/08/05
;---------------------------------------
; arguments
;   p1 : point1(시작점)
;   p2 : point2(끝점)
;   p3 : side point
;   text : text(string)
;   th   : text height
;   gap  : line ~ middle of text (항상+값임)
;---------------------------------------
(defun djdg_wtxtonline1(p1 p2 p3 text th gap
		       / ta mp tp)
  (setq ta (ang4text p1 p2))    ;text angle
  (setq va (v_angle p1 p2 p3))  ;vertical angle(기분선으로부터 text위치로의 각도)
  (setq mp (mid-point p1 p2))   ;mid point of line
  
  (setq tp (polar mp va gap))  ;text삽입점 구하기
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
    (start_image tilename)                                  ;image 보이기
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
;	lnend : T/nil (line의 중심으로 또는 끝으로 할것인가?)
; return : list of point(block:insert point, line: midpoint or end, circle/donut : center point)
; ex) djdg_getpoints(ss)
;     --->  ((0 0 0) (1 0 0) (2 0 0))
;------------------------------------
(defun djdg_getpoints(entlst lnend)

;  (setq mpnt (getpoint "Pick marking point: "))   ;마킹 원을 그릴 위치선택
  (setq nent (sslength entlst))                   ;마킹대상 엔티티갯수

  (setq plst nil)                 ;plst: 마킹대상 entity들의  포인트리스트
  (setq npnt 0)       ;포인트 갯수 (마킹대상 엔티티갯수 /= 마킹대상포인트갯수)
  (setq index 0)
  (repeat nent                                    ;대상 엔티티갯수만큼 반복
    (setq ent (entget (ssname entlst index)))
    (setq entype (cdr (assoc 0 ent)))             ;엔티티타입 구함
    (cond
      ((= entype "LINE")                          ;엔티티가 라인인경우
        (setq sp (cdr (assoc 10 ent)))            ;라인의 시작점
        (setq ep (cdr (assoc 11 ent)))            ;라인의 ?{점
        (if lnend
	  (progn
	    (setq plst (append plst (list sp)))
	    (setq plst (append plst (list ep)))	  
	  );progn
	  (progn				;중심점추가
            (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;라인중간점 X
                       (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y
            (setq plst (append plst (list mp)))       ;중간점을 마킹포인트에 추가
	  );progn
	);if  
        (setq npnt (1+ npnt))                     ;마킹포인트의 갯수 증가
      ) ;of entype="LINE"
      ((= entype "POLYLINE")                            ;폴리라인인 경우(철근을 도나스로 그린 경우)
        (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;첫점정보
        (if (/= (abs (cdr (assoc 42 vtx1))) 0)         ;폴리라인이 아크형인가?
          (progn
            (setq vtx2 (entget (setq nxt2 (entnext nxt1))))   ;두번째점 정보
            (setq vtx1p (cdr (assoc 10 vtx1)))                ;첫점 포인트
            (setq vtx2p (cdr (assoc 10 vtx2)))                ;둘째점 포인트
            (setq cenp (list (/ (+ (car vtx1p) (car vtx2p)) 2.0)     ;센타포인트 X
                             (/ (+ (cadr vtx1p) (cadr vtx2p)) 2.0))) ;           Y
            (setq plst (append plst (list cenp)))             ;포인트리스트에 추가
            (setq npnt (1+ npnt))                             ;포인트 갯수 증가
          ) ;of progn
        ) ;of if
      ) ;of entype="PLINE"
      ((= entype "LWPOLYLINE")      ;LW폴리라인인 경우(철근을 도나스로 그린 경우)
;        (princ "LWPOLYLINE")
        (setq pnt1 (getLwVert ent 0))
        (setq pnt2 (getLwVert ent 1))
        (setq cp (mid-point pnt1 pnt2))
        (setq plst (append plst (list cp)))
        (setq npnt (1+ npnt))
      ) ;of entype="LWPLOLYLINE"
      ((= entype "CIRCLE")                                ;엔티티가 서클인 경우
        (setq cp (cdr (assoc 10 ent)))                    ;서클의 센타포인트
        (setq plst (append plst (list cp)))               ;포인트리스트에 추가
        (setq npnt (1+ npnt))                             ;포인트 갯수 증가
      ) ;of entype="CIRCLE"
      ((= entype "INSERT")
        (setq cp (cdr (assoc 10 ent)))
        (setq plst (append plst (list cp)))
        (setq npnt (1+ npnt))
      ); entype="INSERT"
    ) ;of cond
    (setq index (1+ index))                                 ;index=엔티티번호
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
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************

(defun DATA-IN(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs ",") (= subs ""))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (setq rslt (list lst))                  ;돌림값이 비었을때
               (setq rslt (append rslt (list lst)))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; 입력문자열의 앞,뒤에 있는 빈칸을 짤라낸다.
; 리턴값은
; (짤라낸 문자열,
;  첫 문자 나오는 위치,
;  마지막 문자 나오는 위치,
;  숫자인가?)
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
; 문자열이 숫자인가?를 판단해준다.
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
;           dimtext를 split한다.
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
;            dimtxt를 잘라서 숫자로 넘겨준다.
;            ex)  10@1.250=12.500  --> (10 1250 12500) "1.250" --> 1250과 같이 숫자화(dimlfac참조)
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
;            dimtext를 실제 값으로 바꿔준다.
;            ex) "1.250" --> 1250
;            dimlfac값 참조
;---------------------------------
(defun djdg_dimtxtoreal(dimtxt)
  (setq dt (vl-string-subst "." "," dimtxt))  ;","가 있는 경우 "."로 바꿔서 시작한다 ","의 경우 atof함수에서 인식하지 못하므로...
  (if (vl-string-search "." dt)               ;"."이 있는 경우 atof의 결과에 1000을 곱한다.
    (progn
      (setq realen (* (atof dt) 1000 ))
    );progn
    (setq realen (* (atof dt) ))		;"."이 없는 경우엔 atof만 적용한다.
  );if
  realen
);defun

    
;---------------------------------
; function : djdg_divlen
;            길이(len)를 pitch로 나누고 몫과 나머지를 돌려줌
;            ex) dst=1250 pitch=150 --> mok = 8, re = 50 --> (8 50)
;            Yi Suk Jong
;            05/02/21
;---------------------------------
(defun djdg_divlen(dst pitch)
  (setq re (rem dst pitch))    ;remain(나머지)
  (setq mok (fix (/ dst pitch)))  ;mok(몫)
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
;            dimension string의 전체길이를 구해줌.
;            ex) "150+2@250+150" --> 800
;            Yi Suk Jong
;            05/02/26
;------------------------------
(defun djdg_getdimlen(dmstr / dmstr)
  (setq sp (divide_str dmstr "+"))   ;+기준으로 분리함.("150" "2@250" "150")
  
  (setq totl 0)   ;총길이 = 0
  
  (foreach atm sp  ;각 요소에 대해서 길이 구해서 더하기.
    (setq spgol  (divide_str atm "@")) ;골뱅이기준으로 나누기.
    (if (= (length spgol) 2)    ;골뱅이기준으로 나눈 리스트가 2이상일때 즉 골뱅이가 있을 때.
      (setq atl (* (atof (nth 0 spgol))
		   (djdg_dimtxtoreal (nth 1 spgol))))  ;atom length = 골뱅이 전후 숫자 곱하기. 이때 1.125와 이 
      							;. 또는 ,가 있는 string은 dimtxtoreal을 적용
      (setq atl (djdg_dimtxtoreal (car spgol)))        ;@가 없는 경우 길이 추가. 
    );if
    (setq totl (+ totl atl))   ;총길이(totl)에 요소의 길이(atl) 더하기.
  );foreach  
);defun


;******************************************
; Program : F_DH
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29
;******************************************
; 수평치수선을 함수로 처리해준다.
; 넘어오는 변수
;        SP : 시작점
;       DST : 거리
;         N : 반복갯수
;        UD : 실수일때: Up/DOWN (절대값은 LEVEL)
;	      list일 때 : 그점을 dimenxion point로 함.
; 돌려주는 값 - 끝점 좌표
;******************************************
; 05/12/29 UD가 list인 경우에는 그 점을 dimension point로 함.
(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep 위치계산

;  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치.
  (if (listp ud)					;치수선위치 UD가 list이면 point로 인정.
    (progn
      (setq dxy ud)
      (setq dy (- (cadr ud) (cadr sp)))
      (if (minusp dy) (setq sgn -1) (setq sgn 1))
      (setq dy (abs dy))
    );progn  
    (progn
      (if (> ud 0)                                              ;위 아래
        (setq sgn 1)
        (setq sgn -1)
      ) ;of if
      (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;치수선 위치 계산 (절대값)
      (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치.
    );progn  
  );if  

  (setq dx (distance sp ep))                          ;거리 계산

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (if (> (abs n) 1)                                           ;골뱅이 옵션일 경우
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos (abs n) 2 0))                          ;나눈 갯수 계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000이상일 때
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds 2.5)) 0.0)))

          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)

          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림	  
        ) ;of progn THEN
        (progn                                 ;치수보조선 내에 text 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM명령 내림
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
; 05/12/29 LR이 point인 경우 그점을 dimension point로...

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th 2.5                                        ;text크기 = 2.5
        dim_gap 10.0)                                 ;치수선 간격
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (setq next (* dst n))                                 ;끝점까지 거리

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;수정된 끝점

;  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치
  (if (listp LR) ; LR이 list인경우(point인경우)
    (progn
      (setq dxy LR)
      (setq dx (- (car dxy) (car sp)))
      (if (minusp dx) (setq sgn -1) (setq sgn 1))
      (setq dx (abs dx))
    );progn
    (progn
      (if (> lr 0)                                        ;왼쪽/오른쪽
        (setq sgn 1)
        (setq sgn -1)
      ) ;of if
      (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))
      (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치
    );progn
  );if  

  (setq dy (distance sp ep))                          ;두 점의 거리

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dy < 1000)

  (if (> (abs n) 1)
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos (abs n) 2 0))                          ;나눈 갯수계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;나누는 길이가 1000미만시
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;나누는 길이가 1000이상시
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dy)
        (progn                                  ;text가 보조선 내에 안들어가면
          (setq dtxt1 (strcat divn "@" divl))   ;두줄로 나눔
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list (- (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds 2.5)) 0.0 0.0)))
;          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                  ;text가 보조선 내에 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                    ;리턴입력시 옛 text를 씀
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM명령 내림
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
; 기울어진치수선을 함수로 처리해준다.
; 넘어오는 변수
;        SP : 시점
;        ANG : 각도
;       DST : 거리
;         N : 반복갯수
;        UD : Up/DOWN (절대값은 LEVEL)
;      TXT1 : 쓰고 싶은 text, 값이면 scale로 인식한다.
; 돌려주는 값 - 끝점 좌표
;******************************************

(defun F_DA(SP ANG DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor
  
  (if (listp ud)			;ud가 point인 경우
    (if (minusp (dang ang (angle sp ud)))	;치수선각도와.. 시작점~dimpoint각도 비교.
      (setq sgn -1)
      (setq sgn 1)
    );if      
    (if (> ud 0)                                              ;위 아래
      (setq sgn 1)
      (setq sgn -1)
    ) ;if
  );if    
  
  (setq ang1 ang)
;  (setq a2sgn ud)
  (setq a2sgn sgn)
  (setq tsgn 1)
  (setq w4 (which4 ang1))
  (cond                                       ;텍스트의 각도 (골뱅이 옵션)    
    ((or (= w4 1) (= w4 4)) (setq tang ang1))    
    ((or (= w4 2) (= w4 3)) (setq tang (- ang1 pi)))    
  ) ;of cond    
  
  (if (and (or (= w4 2) (= w4 3)) (= a2sgn 1)) (setq tsgn -1))    
  (if (and (or (= w4 1) (= w4 4)) (= a2sgn -1)) (setq tsgn -1))

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리
  (setq ep (polar sp ang next))              ;ep 위치계산

  (if (listp ud)				;치수선 defpoint에서 치수선까지의 거리
    (setq dmdst (distance ud (inters sp ep ud (polar ud (v_angle sp ep ud) 10) nil)))
    (setq dmdst (* ds (+ 20 (* dim_gap (1- (abs ud))))))
  );if

  
  (if (listp ud)
    (setq dxy ud)
    (setq dxy (polar ep (+ ang (* 0.5 pi)) (* dmdst sgn)))  ;치수선 위치
  );if  

  (setq dx (distance sp ep))                          ;거리 계산

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;골뱅이 옵션일 경우
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수 계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000이상일 때
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다
          (setq dtxt2 (strcat "=" txt))
;          (setq dtxt1p (mapcar '+ (mid-point sp ep)
;                                  (list 0.0 (+ (* dmdst sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (polar (mid-point sp ep)    
                              (+ ang1 (* pi 0.5 a2sgn))    
                              (- dmdst (* th ds tsgn))))
	  
          (command "TEXT" "M" dtxt2p (* th ds) (rtod tang) dtxt2)
          (command "DIM1" "ALI" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                 ;치수보조선 내에 text 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "ALI" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "ALI" sp ep dxy txt1)             ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun

;******************************************
; Program : F_DATEXT
;		F_DA와 비슷하나 치수선을 그리지 않고 TEXT만 처리한다.
;           Function Dimension Aligned TEXT only
;           Jong-Suk Yi
;           06/08/05
;******************************************
; 기울어진치수선을 함수로 처리해준다.
; 넘어오는 변수
;        SP : 시점
;        ANG : 각도
;       DST : 거리
;         N : 반복갯수
;        UD : side point ;  Up/DOWN (절대값은 LEVEL) 06.08.05 수정
;      TXT1 : 쓰고 싶은 text, 값이면 scale로 인식한다.
; 돌려주는 값 - 끝점 좌표
;******************************************

(defun F_DATEXT(SP ANG DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor
  
  (if (listp ud)			;ud가 point인 경우
    (if (minusp (dang ang (angle sp ud)))	;치수선각도와.. 시작점~dimpoint각도 비교.
      (setq sgn -1)
      (setq sgn 1)
    );if      
    (if (> ud 0)                                              ;위 아래
      (setq sgn 1)
      (setq sgn -1)
    ) ;if
  );if    
  
  (setq ang1 ang)
;  (setq a2sgn ud)
  (setq a2sgn sgn)
  (setq tsgn 1)
  (setq w4 (which4 ang1))
  (cond                                       ;텍스트의 각도 (골뱅이 옵션)    
    ((or (= w4 1) (= w4 4)) (setq tang ang1))    
    ((or (= w4 2) (= w4 3)) (setq tang (- ang1 pi)))    
  ) ;of cond    
  
  (if (and (or (= w4 2) (= w4 3)) (= a2sgn 1)) (setq tsgn -1))    
  (if (and (or (= w4 1) (= w4 4)) (= a2sgn -1)) (setq tsgn -1))

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리
  (setq ep (polar sp ang next))              ;ep 위치계산

  (if (listp ud)				;defpoint에서 치수선까지의 거리
    (setq dmdst (distance ud (inters sp ep ud (polar ud (v_angle sp ep ud) 10) nil)))
    (setq dmdst (* ds (+ 20 (* dim_gap (1- (abs ud))))))
  );if

  
;  (if (listp ud)
;    (setq dxy ud)
;    (setq dxy (polar ep (+ ang (* 0.5 pi)) (* dmdst sgn)))  ;치수선 위치
;  );if  

  (setq dx (distance sp ep))                          ;거리 계산

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;골뱅이 옵션일 경우
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수 계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000이상일 때
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt2p (polar (mid-point sp ep)    
                              (+ ang1 (* pi 0.5 a2sgn))    
                              (- dmdst (* th ds tsgn))))
	  

	  (djdg_wtxtonline1 sp ep ud dtxt1 (* th ds) (* th ds))
          (djdg_wtxtonline1 sp ep ud dtxt2 (* th ds) (* th ds -1))    ; "=1.200"
        ) ;of progn THEN
        (progn                                 ;치수보조선 내에 text 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
	  (djdg_wtxtonline1 sp ep ud dtxt1 (* th ds) (* th ds))
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      	 (djdg_wtxtonline1 sp ep ud txt1 (* th ds)  (* th ds))
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun

;----------------------
; function : djdg_splitprenum
;            split prefix and number
;            S1과 같은 문자+번호 를 숫자(prefix)와 문자로 분리한다.
;            Yi Suk Jong
;            05/08/08
;----------------------
(defun djdg_splitprenum( numstr / lstr i nonum return ch)
  (setq lstr (strlen numstr)) ;string length
  (setq i lstr)				;맨뒷글자부터
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
; LwPolyline의 vertex list를 만들어준다.
; 인수: vname  : vertext entity name
;                 (car (entsel)) 상태로 넘어와야한다.
; return: (vertext list 와 nvert를 list로 묶어서 돌려줌)
; -------------------------------------

  (defun mk_vertlist (vname
  /  count nvert index tmp vert_list pt1
                     );of local variable

    (setq vlist (entget vname))                          ;엔티티 정보

    (setq count 0)                                      ;첫 vertex 찾아감
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )                                                   ;첫째 vertex 위치


    (setq nvert (cdr (assoc 90 vlist)))                 ;vertext수

    (setq vert_list nil)                                 ;빈 list만들기
    (setq index 0)                                      ;첫vertex부터

    (repeat nvert
      (setq tmp (nth (+ count (* index 4)) vlist))     ;(10 x y)
      (setq tmp (append tmp (list (cdr (assoc 38 vlist)))))  ;z좌표추가
      (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))  ;ucs좌표로 치환
      (setq vert_list (append vert_list (list pt1)))         ;vertexlist에추가
      (setq index (1+ index))                                      ;다음 vertext로
    ); repeat

     (setq vert_list (list vert_list nvert))
  ) ;of defun

;---------------------------------
;function : djdg_splitstr
;           string을 주어진 문자열로 split한다.
;           ex)  (djdg_splitstr "ab+123" "+") --> ("ab" "123")
;           Yi Suk Jong
;           05/12/20
;---------------------------------
(defun djdg_splitstr(str ptstr
		     / lstr lptstr n i lastsearch return startp nn spstr tail)
  (setq lstr (strlen str)
        lptstr (strlen ptstr))
  (setq n (- lstr lptstr -1))   ;전체길이-patternstr갯수+1
  (setq i 1
	lastsearch 0
	return nil)
  (repeat n
    (if (= (substr str i lptstr) ptstr)
      (progn
	(if (= lastsearch 0)
	  (setq startp 1)
	  (setq startp (+ lastsearch lptstr)) ;시작위치.
	);if  
	(setq nn (- i startp))  ;추출길이.
	(setq spstr (substr str startp nn))  ;추출된 string
;	(if (and (/= spstr "") (/= spstr "\t"))
	(if (/= spstr "")	
          (setq return (append  return (list spstr)))  ;추출된 sring추가.""일땐 추가하지 않음.
	);if  
	(setq lastsearch i)
      );progn  
    );if  
    (setq i (1+ i))
  );repeat
  (setq tail (substr str (+ lastsearch lptstr) (- lstr lastsearch)))  ;마지막 부분.
  (if (/= tail "")
    (setq return (append  return (list tail)))
  );
  return
);defun

;---------------------
; function : djdg_attwrite
;            attribute가 포함되 insert의 att를 수정해준다.
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
; 엔티티 선택정보를 주면 선택한 점의 tag와 값을 돌려준다.
;-----------------------
; argument : etsel : (entsel)의 return값.
; return : (tag value)
(defun djdg_gettag(etsel / return spnt ename einf ipnt txt tbox tbox1 tbox2 pnt2 xspnt yspnt tag)
  
;  (setq sel (entsel "\nSelect Att: "))  ;att선택.
  (setq return nil)
  (setq spnt (cadr etsel))  ;selection point
  (setq ename (car etsel))  ;entity name
  (while (= (cdr (assoc 0 (entget (setq ename (entnext ename))))) "ATTRIB")
    (setq einf (entget ename))
    (setq ipnt (cdr (assoc 10 einf)))
    (setq txt (cdr (assoc 1 einf)))  ;text정보.
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
;		attribute의 전체 값들을 읽어 list로 묶어 반환한다.
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
;            치수선의 단수를 주어진 숫자만큼 늘리거나 줄인다.
;      	Yi Suk Jong
;	05/12/27
;------------------
; (djdg_cdimdan ename cdan)
;   ename: dimension entity name
;   cdan : 정수 ex;1 --> 1단 증가 -1-->1단 감소.
(defun djdg_cdimdan(ename cdan / deni as10 as14 ang nas10 ndeni)
  (setq deni (entget ename))		;dimension정보
  (setq as10 (cdr (assoc 10 deni)) 	;dimension point
        as14 (cdr (assoc 14 deni)))
  (setq ang (angle as14 as10))		;치수보조건 각도.
  (setq nas10 (polar as10 ang (* (getvar "DIMDLI") (getvar "DIMSCALE") cdan))) ;새 dimpoint구하기
  (setq ndeni (subst (cons 10 nas10) (assoc 10 deni) deni))	;새정보로 고치기
  (entmod ndeni)	;갱신
);defun  


;-----------------------
; function : djdg_angofdimline
; 	주어진 치선의 치수선(dimension line) 각도.
; 	Yi Suk Jong
;	05/12/30
;-----------------------
; (djdg_angofdimline( ename )
(defun djdg_angofdimline( ename / enti as70 return)
  (setq enti (entget ename))  ;entity 정보
  (setq as70 (cdr (assoc 70 enti)))   ;dim type aligned or Horizontal
  (if (= (- as70 32) 0)
    (setq return (cdr (assoc 50 enti)))	;horizontal 이나 vertivcal인 경우
    (setq return (angle (cdr (assoc 13 enti)) (cdr (assoc 14 enti))))
  );if   
);defun


;****************************************
; Function : CROSS
;            CROSS point of arc & line
;            By Suk-Jong Yi
;            1995/6/26
;****************************************
;함수: 호와 직선의 교차점 찾기
;     인수: ARC entity list(entget상태), 직선의 첫점 , 직선의 끝점
;     결과: 직선과 ARC의 교차점

(defun CROSS(aent sp ep /
aent    sp      ep      a       b       r       sa      ea      x1      x2
y1      y2      c       d       a1      b1      c1      x1      x2      y1
y2      ang1    ang2
)

;(push-env)
(setq a (car (cdr (assoc 10 aent))))      ; ARC entity의 중심점 x좌표
(setq b (cadr (cdr (assoc 10 aent))))     ; ARC entity의 중심점 y좌표
(setq r (cdr (assoc 40 aent)))            ; ARC entity의 반지름
(setq sa (cdr (assoc 50 aent)))           ; ARC entity의 시작 각도
(setq ea (cdr (assoc 51 aent)))           ; ARC entity의 끝 각도

(setq x1 (car sp))                        ; LINE entity의 시작점 x좌표
(setq x2 (car ep))                        ; LINE entity의 끝점 x좌표
(setq y1 (cadr sp))                       ; LINE entity의 시작점 y좌표
(setq y2 (cadr ep))                       ; LINE entity의 끝점 y좌표
(if (= (- x1 x2) 0)
  (progn                                    ;x가 constant일 때
    (setq c x1
          a1 1                              ;y에 대한 2차방정식의 a
          b1 (* -2 b)                       ;y에 대한 2차방정식의 b
          c1 (+ (* c c) (* -2 a c) (* a a) (* b b) (* -1 r r)) ;y에 대한 2차방정식의 c
          y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;근 1
          y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;근 2
    );setq
  );progn
  (progn                                    ; y가 x의 함수일 때
    (setq c (/ (- y1 y2) (- x1 x2)))          ; y=cx+d에서 c
    (setq d (- y2 (* c x2)))                  ; y=cx+d에서 d
    (setq a1 (+ 1 (* c c)))                   ; x에 대한 이차방정식의 a
    (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))   ;x에 대한 이차방정식의 b
    (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))  ;이차 방정식의 c
    (setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;근 1
    (setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;근 2
    (setq y1 (+ (* c x1) d))                  ;근 1일 때 y값
    (setq y2 (+ (* c x2) d))                  ;근 2일 때 y값
  );progn
)

(setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))   ;교점1의 절대각(원점에서)
(setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))   ;교점2의 절대각(원점에서)

(if (inang sa ea ang1)
  (list x1 y1 0.0)         ;교점1이 호의 시작각과 끝각 사이에 있으면 교점 돌려줌
  (if (inang sa ea ang2)   ;교점2가 호의 시작각과 끝각 사이에 있으면 교점 돌려줌
    (list x2 y2 0.0)
    nil                    ;교점 1과 2가 모두 각 범위를 벗어날 경우 nil돌려줌
  ) ;of if
) ;of if
)

;************************************************************
; Function : INANG
;            a angle is IN the range of ANGle-1 and angle-2 ?
;            By Suk-Jong Yi
;            1995/6/26
;*************************************************************
;어떤 각이 주어진 두각(ang1, ang2) 사이에 있는가?
; 두각 사이에 있는 경우 두각의 차이를 돌려주고
; 두각 사이에 없는 경우는 nil을 돌려준다.
;*************************************************************

(defun inang(a1 a2 a3 /             ;인수 정의
a1 a2 a3                            ;지역변수 정의
)
(if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))   ;첫각이 두번째 각보다 크면 +360도
(if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;주어진 각이 두각사이에 있으면
                                nil)         ; 두각의 차이를 돌려줌
)                                            ; 두각 사이에 없으면 nil돌려줌



;------------------------------------
; function : cp_line_pline
;            cross point of line and polyline
;            Yi suk jong
;            99/10/8
;------------------------------------
; 기능 : line과 pline의 교점을 찾아준다
;
; 넘어오는 값
;     pnt1 : 라인의 첫째점
;     pnt2 : 라인의 둘째점
;     plist: polyline의 point list
;
; 넘어가는 값 : 교점들을 list로 묶어서 넘김
;-----------------------------------------------

(defun cp_line_pline(pnt1 pnt2 plist
/ pnt1 pnt2 plist n z pnt12d pnt22d cplist index pp1
  )
  (setq n (length plist))               ;polyline의 point갯수
  (setq z (nth 2 (nth 0 plist)))         ;polyline의 z좌표

  (setq pnt12d (list (car pnt1) (cadr pnt1))   ;xy좌표축출
        pnt22d (list (car pnt2) (cadr pnt2)))  ;xy좌표축출

  (setq cplist nil) ;교점list비우기
  (setq index 1)

  (repeat (1- n)
    (setq pp1 (reverse (cdr (reverse (nth (1- index) plist))))  ;xy좌표축출
          pp2 (reverse (cdr (reverse (nth     index  plist))))) ;xy좌표축출
    (setq cp (inters pnt12d pnt22d pp1 pp2))        ;교차점 찾기
    (if cp
      (progn
        (setq cp (append cp (list z)))                ;z좌표추가
        (setq cplist (append cplist (list cp))) ;교차점이 존재할 때 더하기
      );progn
    );if
    (setq index (1+ index))                     ;다음구간으로
  );repeat
    (if cplist cplist)                          ;교차점이 존재하면 리턴

);defun  


;--------------------------------------------------------
; function : ali_Pdim
;            Aligned Point List dim
;		point list를 이용하여 dimension 그리기
;             Yi Suk Jong
;            06/08/04
;--------------------------------------------------------
; 기능: point list를 받아서 dimension또는 text를 그린다.
; 이때 옵션은
; defpoint : definition point 치수선일때는 defpoint, text일 때는 기준선 위치이다.
; dirpoint : direction point 치수선 진행방향
; sidepoint: side point 치수선을 어느쪽으로 위치시킬 것인가를 결정하는 점
; textout : text로 출력할 거인가? nil이며 dimension으로 출력
; factor : point list로 이루어진 거리에 곱해질 숫자 skew나 R을 고려하기 이한 옵션
; delta : 최종적으로 거리에서 빼거나 더해질 숫자
; prefix, postfix : 텍스트 앞뒤로 들어갈 문자
; oridist : 원본길이 적용여부 nil이면 수정된(즉 factor와 delta가 적용된) 길이로 그리기
; gol : @(골뱅이) 적용여부 nil이면 각 치수로 표현됨
;--------------------------------------------------------
; 알고리듬
; 1. 치수선진행방향(각도)와 side방향(각도)를 구한다.
; 2. 각점들의 진행방향상의 거리들을 구한다.
; 3. 거리기준으로 sort한다.
; 4. 두번째점부터 마지막 점까지 치수선을 작성한다.
;  4.1 각거리를 text로 변환한다.
;  4.2 앞거리와 같으면 같은수 카운트 하나만 올리고 그리지 않는다
;  4.3 앞거리와 다르면 앞치수 적고 이번 치수도 적는다.
; 5. 4를 반복
;--------------------------------------------------------

(defun ali_pdim( plist defpnt dirpnt sidepnt textout factor delta prefix postfix oridist gol
		/ dirang sideang npnt dplist pnt dist sdplist rdefpnt index dstlst dfdstlst
		  dst ndst nsame idst ipnt dsti dfdsti dstxt dimtxt ppdimtxt odimtxt odst odstxt
		)
;test용 데이터(지우지 말것.)
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
  
; 1. 치수선진행방향(각도)와 side방향(각도)를 구한다.
      (setq dirang (angle defpnt dirpnt) 		;치수선 진행방향)
	    sideang (v_angle defpnt dirpnt sidepnt))	;치수선 위치방향
  
; 2. 각점들의 진행방향상의 거리들을 구한다.
      (setq npnt (length plist))			;point 갯수
      (setq dplist nil)			;distance and point list((거리 (x y z)) (거리 (x y z)) ... )
      (foreach pnt plist
	(setq dist (djdg_angdist defpnt pnt dirang))		;진행방향으로거리
	(setq dplist (append dplist (list (list dist  pnt))))
      );repeat	

; 3. 거리기준으로 sort한다.
      (setq sdplist (vl-sort dplist
		       		'(lambda (s1 s2)
    				(< (car  s1) (car s2)))))  ;첫번째 요소를 기준으로 sort
  
   ;----- 실제 def점을 구한다. 입력def점에서 진행각도로 첫거리만큼 떨어진곳.
  (setq rdefpnt (polar defpnt dirang (car (nth 0 sdplist))))	;real def point

  
; 4. 두번째점부터 마지막 점까지 치수선을 작성한다.
  ;----- 간격을 list로만든다.
      (setq index 1
	    dstlst nil)				;간격 list (3 2 4)
      (repeat (1- npnt)
	(setq dstlst (append dstlst (list (- (car (nth index sdplist))
					     (car (nth (1- index) sdplist))))))
	(setq index (1+ index))
      );repeat
  

   ;----- 간격list에 factor
  (setq fdstlst nil)
  (foreach dst dstlst
    (setq fdstlst (append fdstlst (list (* dst factor)))) ;factored dstlst
  );foreach
  
  ;----- 간격 list에 delta를 적용한다.
  (setq dfdstlst nil)
  (foreach dst fdstlst
    (setq dfdstlst (append dfdstlst (list (+ dst delta)))) ;factored dstlst
  );foreach

  ; 간격개수만큼 치수선 or text그리기
  (setq ndst (length dstlst))		;간격개수
  (setq nsame 1)
  (setq idst 0)
  (setq ipnt rdefpnt)			;시작점
  (repeat ndst
    (setq dsti (nth idst dstlst)	;원래거리
	  dfdsti (nth idst dfdstlst))	;수정거리
    
    (if (= oridist nil)			;원래 거리로 그리기 꺼있는 경우 
      (setq dsti dfdsti)		;이번 거리 뽑아내기(수정거리, factor, delta 적용후)
    );if

    (setq dstxt dfdsti)			;text만들기용 거리(text만들기용 거리는 무조건 수정거리로 함)
    (setq dimtxt (rto_dimtxt dfdsti))	;거리의 dimension text(수정거리로 함)
    
    (if (> idst 0)			;두번째 거리부터 비교
      (if (/= gol nil)			;@옵션이 켜있는 경우
	(progn
          (if (/= dimtxt odimtxt)		;앞 치수하고 다른경우
	    (progn				;다른경우에는 치수선 그리기
	      (setq ppdimtxt (strcat prefix (goltotaltxt nsame odstxt) postfix)) ;prefix,postfix적용
	      (if textout  ;textout option이 켜있는 경우엔 text출력
		(setq ipnt (f_datext ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  text
	        (setq ipnt (f_da ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  dimension
	      );;if	
	      (setq nsame 1)				;같은 숫자 초기화
	    );progn
	    (setq nsame (1+ nsame))				;같은 경우에는 카운트 1 올리기
          );if
        );progn
	(progn					;골뱅이 옵션이 꺼있는 경우 모두 표시
	  (setq ppdimtxt (strcat prefix (goltotaltxt nsame odstxt) postfix))	;prefix,postfix적용
          (if textout  ;textout option이 켜있는 경우엔 text출력
   	    (setq ipnt (f_datext ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  text
	    (setq ipnt (f_da ipnt dirang odst nsame sidepnt ppdimtxt))
	  );if  
	  (setq nsame 1)
	);progn
      );if
    );if
    (setq odimtxt dimtxt	;현재의 값들을 old값들로... 
	  odst dsti
	  odstxt dstxt) 	;old dimtext, old dist 설정
    (setq idst (1+ idst))		;다음 간격
  );repeat
  (setq ppdimtxt (strcat prefix (goltotaltxt nsame odstxt) postfix))	;prefix,postfix적용
  (if textout  ;textout option이 켜있는 경우엔 text출력
    (setq ipnt (f_datext ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw  text
    (setq ipnt (f_da ipnt dirang odst nsame sidepnt ppdimtxt))   ;draw horizontal dimension
  );if  
);defun


;-------------------------
; function : djdg_angdist
;            distance angle
;            어떤 점의 주어진 각도방향으로 거리
;            Yi Suk Jong
;-------------------------
(defun djdg_angdist(bp tp ang / deltaang dist)
  (setq deltaang (dang ang (angle bp tp)))
  (setq dist (* (cos deltaang) (distance bp tp)))
);defun


;---------------------
; function : goltotaltxt
;            주어진 개수와 간격을 이용해 전체길를 구한다.
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
;		: 주어진 text의 중앙점 찾기.
;		Yi Suk Jong
;		06/08/06
;---------------------------
; argument: entity name
; 알고리듬
; insertion point구하기 > 각도 구하기 > 회전전의 textbox bound구하기 > 회전시키기 > 가운데점구하기
(defun textboxm(ename / eni ipnt tb tb1 tb2 mpnt)
        (setq eni (entget ename))
        (setq ipnt (cdr (assoc 10 eni))		;entity정보 insert point
	      ang (cdr (assoc 50 eni)))		;angle	
        (setq tb (textbox eni))		;
        (setq tb1 (mapcar '+ (car tb) ipnt))	;insert point와 textbox더하기
        (setq tb2 (mapcar '+ (cadr tb) ipnt))

        (setq tb11 (rotatepnt tb1 ipnt ang)
	      tb22 (rotatepnt tb2 ipnt ang))
  
        (setq mpnt (mid-point tb11 tb22))
  mpnt
);defun textboxm


;-----------------------------
; function : rotatepnt
;            어쩐점의 base pont를 기준으로 회전후 좌표를 구한다.
; 	Yi Suk Jong
; 	06/08/06
;-----------------------------
; argument: p1   : 대상 좌표'(1 1 1)
;	    bpnt : 회전기준점'(1 1 1)
;	    ang  : 회전각도 (radian)
;알고리듬
; 대상좌표와 기준좌표를 x,y,z로 분리 > 두 좌표의 차이 구함 > 회전 > 기준점좌표더함

(defun rotatepnt(p1 bpnt ang / x y z x0 y0 z0 xx yy zz x1 y1 z1)
  (setq x  (nth 0 p1) 				;회전대상 점 x,y,z
	y  (nth 1 p1) 
	z  (nth 2 p1)) 
  (setq x0  (nth 0 bpnt) 			;기준점 x,y,z
	y0  (nth 1 bpnt) 
	z0  (nth 2 bpnt))
  (setq xx (- x x0)				;기준점을 기준으로 한 국부좌표계
	yy (- y y0)
	zz (- z z0))
  (setq x1 (- (* xx (cos ang)) (* yy (sin ang)))  ;국부좌표계에서 회전
	y1 (+ (* xx (sin ang)) (* yy (cos ang)))
	z1 z)
  (list (+ x1 x0) (+ y1 y0) (+ z1 z0))		;기준점좌표를 더하여 return
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
;		두 entity의 교점을 구해준다.
;		Yi Suk Jong
;		06/08/13
;--------------------------------
; argument: ent1 : entity name - 1
;	    ent2 : entity name - 2
;	    opt  : 0 ;둘다 확장안함 1: 첫번째 확장 2:두번째 확장 3: 둘다확장
; return: ((x y z) (x y z))  교차점 list
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
;            data file을 읽어준다. 이때 ";"이후는 무시하고 돌려준다.
;            Yi Suk Jong
;            05/03/20
;------------------------
; usage : (djdg_readdatafile fn)
; arguments: fn : file name
; return: data파일 내용  (";"이후는 무시된 결과를 list로 묶어서 돌려준다.
;------------------------
(defun djdg_readdatafile( fn
			 / return opf ch divstr
			 )
  (setq return nil)
  (setq opf (open fn "r"))   ; open file
  (if opf
    (progn
      (while (setq ch (read-line opf))
	(setq ch (vl-string-subst "" "\t" ch)) ;tab문자 제거
	(setq divstr (divide_str ch ";"))   ; ";"로 분리.
	(setq ch (car divstr)) 			;앞 string만취함.
	(if (> (strlen ch) 0)				;string 길이가 0이 아닌경우만 실행.
	  (progn
	    (setq return (append return (list ch)))     ;string 추가
	  );progn  
	);if  
      );while
      (close opf)        ;close file
      return
    );progn
  );if opf  
);defun

;--------------------------
; function : point list를 특정 성분(x,y,z,거리)을 기준으로 sort
; 		06/08/11
;--------------------------
; ptlist : point list
; idx : assoc기준 코드..(ex: 시점이면 0:x, 1:y, 2:z 3:거리)
; ipt : 기준점(idx가 3일때만 사용된다) nil이면 가장먼점중의 한점으로부터 거리로 sort
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
      (setq farpnt (farest plist))		;가장 먼 점 찾기
      (if (= ipt nil)			;만일 기준점이 주어지지 않으면.
        (setq pnt (car farpnt))		;가장먼 점 두개중 앞에것을 기준점으로...
        (setq pnt ipt)			
      );  
      (setq dstplist nil)
      (foreach p plist
        (setq dst (distance pnt p))
        (setq dstplist (append dstplist (list (list dst p))))
      );foreach

      (setq sdstplist (vl-sort dstplist '(lambda (s1 s2)	;기준점에서부터 거리로 sort
    				(< (car  s1) (car s2)))))
      (setq return nil)
      (foreach dp sdstplist
	(setq return (append return (list (cadr dp))))
      );foreach	
    );subcond
    
  );cond 
);