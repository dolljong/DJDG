;------------------------
; Program : Truss
;           Draw Truss
;           Yi Suk Jong
;           04/09/06
;------------------------
; draw truss referencing upper chord center line and lower chord center line


(defun c:TRUSS( / uc ucl lc lcu sp ep gap ddia ucpl lcpl uclpl lcupl
	          ucbound lcbound uclbound lcubound xsp xep tx ngap
	          up_vl lp_vl ucrsp lcrsp uplowflag i x x1 yu yl
	          ucrsp1 lcrsp1 spnt epnt dang dang1 dang2 ddia2
	          spleft epleft spright epright crsleftu crsleftl crsrightu crsrightl)


  (djdg_loadltype "center")
  
  (setq uc (car (entsel "\n Select Center Line of Upper Chord: " )))
  (redraw uc 3)
  (setq ucl (car (entsel "\n Select Lower Line of Upper Chord: " )))
  (redraw ucl 3)
  (setq lc (car (entsel "\n Select Center Line of Lower Chord: " )))
  (redraw lc 3)
  (setq lcu (car (entsel "\n Select Upper Line of Lower Chord: " )))
  (redraw lcu 3)

  
  (setq sp (getpoint "\nPick Start Point: "))
  (setq ep (getpoint "\nPick End point: "))
  (setq gap (getreal "\nDistant of Node: "))
  (setq ddia (getreal "\nDepth of Diagonal Member: "))

  (redraw uc 4)(redraw ucl 4)(redraw lc 4)(redraw lcu 4)

  (setq ucpl (mk_vertlist uc))
  (setq lcpl (mk_vertlist lc))
  
  (setq ucpl (car ucpl))
  (setq lcpl (car lcpl))

  (setq uclpl (mk_vertlist ucl))
  (setq lcupl (mk_vertlist lcu))
  
  (setq uclpl (car uclpl))
  (setq lcupl (car lcupl))

  (setq ucbound (djdg_plbound ucpl)
        lcbound (djdg_plbound lcpl)
	uclbound (djdg_plbound uclpl)
	lcubound (djdg_plbound lcupl))

  (setq ucboundy (cadr (cadr ucbound))
        lcboundy (cadr (car lcbound))
	uclboundy (cadr (cadr uclbound))
	lcuboundy (cadr (car lcubound)))

  
  (setq xsp (car sp))  ; x of start point
  (setq xep (car ep))  ; x of end point
  (setq tx (- xep xsp)) ;total x
  (setq ngap (fix (/ tx gap))) ;number of gap

  (setq up_vl (list xsp (+ ucboundy 100)) ;upper point of vertical line
	lp_vl (list xsp (- lcboundy 100))) ;lower point of vertical line

  (setq ucrsp (car (cp_line_pline up_vl lp_vl ucpl)))  ;cross point upper chord and first vertical line
  (setq lcrsp (car (cp_line_pline up_vl lp_vl lcpl)))  ;cross point upper chord and first vertical line

  (if (< (distance ucrsp sp) (distance lcrsp sp))  ; upper & lower flag 1: upper 0: lower
    (setq uplowflag 1)
    (setq uplowflag 0)
  )  

  (setq i 0)
  (repeat ngap
    (setq x (+ xsp (* i gap)))   ;start point x
    (setq x1 (+ xsp (* (1+ i) gap)))  ;end point x
    (setq yu (+ ucboundy 100)    
	  yl (- lcboundy 100))
    (setq ucrsp (car (cp_line_pline (list x yu) (list x yl) ucpl)))
    (setq lcrsp (car (cp_line_pline (list x yu) (list x yl) lcpl)))
    (setq ucrsp1 (car (cp_line_pline (list x1 yu) (list x1 yl) ucpl)))
    (setq lcrsp1 (car (cp_line_pline (list x1 yu) (list x1 yl) lcpl)))
    (cond
      ((= uplowflag 1)
       (push-os)(celtype "center")(cecolor "1")
       (command "line" ucrsp lcrsp1 "")
       (popcolor)(pop-ltype)(pop-os)  ;center line upper --> lower
       (setq spnt ucrsp
	     epnt lcrsp1)
       (setq uplowflag 0) 
      );subcond
      ((= uplowflag 0)
       (push-os)(celtype "center")(cecolor "1")
       (command "line" lcrsp ucrsp1 "")
       (popcolor)(pop-ltype)(pop-os) ;center line lower --> upper
       (setq spnt lcrsp
	     epnt ucrsp1)
       (setq uplowflag 1) 
      );subcond
    );cond
    
    ;
    ; Draw left and right diagonal line
    ;
    (setq dang (angle spnt epnt)) ;angle of diagonal
    (setq dang1 (+ dang (* 0.5 pi))  ;angle + 90 degree
          dang2 (- dang (* 0.5 pi))) ;angle - 90 degree
    (setq ddia2 (/ ddia 2))          ;half of depth of diagonal member
    (setq spleft (polar spnt dang1 ddia2)     ;left line point
          epleft (polar epnt dang1 ddia2))
    (setq spright (polar spnt dang2 ddia2)    ;right line
          epright (polar epnt dang2 ddia2))
       
    (setq crsleftu (car (cp_line_pline spleft epleft uclpl)))
    (setq crsleftl (car (cp_line_pline spleft epleft lcupl)))
    (setq crsrightu (car (cp_line_pline spright epright uclpl)))
    (setq crsrightl (car (cp_line_pline spright epright lcupl)))
    (push-os)(command "line" crsleftu crsleftl "")   ;
             (command "line" crsrightu crsrightl "")  (pop-os)
    (setq i (1+ i))   ;next seg
  );repeat  
);defun


; -------------------------------------
; function : mk_vertlist
; LwPolyline의 vertex list를 만들어준다.
; 인수: vname  : vertext entity name
;                 (car (entsel)) 상태로 넘어와야한다.
; -------------------------------------

  (defun mk_vertlist (vname
  /  vlist count nvert tmp vert_list pt1
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


; -------------------------------------
; function : mk_vertlistp
; Polyline의 vertex list를 만들어준다.
; 인수: vname  : vertext entity name
;                (car (entsel)) 상태로 넘어와야한다.
; -------------------------------------
(defun mk_vertlistp(vname
/
)

  (setq vert_list nil                             ;리스트 초기화
        count 0                                   ;절점 갯수
        nxt vname)                                ;첫 엔티티 이름

  (while (setq nxt (entnext nxt))
    (progn
      (setq ent (entget nxt))                        ;엔티티정보축출
      (if (= (cdr (assoc 0 ent)) "VERTEX")           ;절점일때만
        (setq vert_list (append vert_list (list (cdr (assoc 10 ent))))
              count (1+ count))                      ;갯수추가
      );if
    );progn
  );while

  (setq vert_list (list vert_list count))            ;결과리턴

);defun




;------------------------------------------
; program : djdg_plbound
;           poly line bound
;           Yi Suk Jong
;           04/09/06
;------------------------------------------
; Arguments
;   pl ; point list '((0 0 0) (1 2 0) (3 0 0))
; Return
;   '((0 0 0) (3 2 0))
;------------------------------------------
(defun djdg_plbound(pl / )
  (setq minx (car (nth 0 pl))
	maxx minx
	miny (cadr (nth 0 pl))
	maxy miny)
  (setq n (length pl))
  (setq index 1)
  (repeat (1- n)
    (setq c (nth index pl))
    (if (< (car c) minx) (setq minx (car c)))
    (if (> (car c) maxx) (setq maxx (car c)))
    (if (< (cadr c) miny) (setq miny (cadr c)))
    (if (> (cadr c) maxy) (setq maxy (cadr c)))
    (setq index (1+ index))
  );repeat  
  (list (list minx miny) (list maxx maxy))
);defun  