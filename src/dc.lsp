;*****************************************    
; Program : DC    
;           Dimension Circle    
;           By Jong-Suk Yi    
;           96/6/7    
;****************************************    
; 호의 길이를 표시해줍니다.1
;****************************************    

;------------------------
; Program: DIMA
;          DIMension Arc
;          Yi Suk Jong
;          04/03/29
;------------------------


(defun C:DC(/    
ds elst cen rc sp sang ep eang seang    
cl txt dlst dcen rd dsp sep sgnpnt    
sgnr sgn dlevel mang dpnt wh4 tsgn    
tang txtpnt txtxy ltxt    
)    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
  (setvar "BLIPMODE" 0)    
  (setvar "CMDECHO" 0)    
;  (push-env)                                                ;환경변수 대피    
    
  (setq dim_gap (getvar "DIMDLI")                            ;치수선이 구조물에서 떨어진 거리    
        ds (getvar "DIMSCALE")                                  ;스케일값    
        th (getvar "DIMTXT"))                                                 ;text크기    
    
  (setq elst (entget (car (entsel "\nSelect Arc or Circle: "))))  ;길이의 기준호    
  (setq cen (cdr (assoc 10 elst))                               ;중심    
        rc  (cdr (assoc 40 elst)))                              ;반지름    
    
  (setq dlst (entget (car (entsel "Select baseline circle: ")))) ;치수선 기준 Arc    
  (setq dcen (cdr (assoc 10 dlst))                    ;치수선 기준 Arc의 중심    
        rd   (cdr (assoc 40 dlst)))                   ;치수선 기준 Arc의 반지름    
    
  (setq sgnpnt (getpoint "\nPick Side: "))            ;치수선의 위치 (위,아래?)    
  (setq sgnr (distance cen sgnpnt))                   ;치수선 위치 부호    
  (if (> sgnr rd)    
    (setq sgn 1)    
    (setq sgn -1)    
  ) ;of if    
    
; 치수선의 위치    
  (setq dlevel (getint "\nDim Line level <1>: "))    
  (if (= dlevel nil) (setq dlevel 1))    
    
  (setq sp (getpoint "Pick start point: "))                     ;시작점입력    
  (setq sang (angle cen sp))                                    ;중심점과 시작점이 이루는 각    
    
  (setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;끝점까지 거리    
  (cond                                               ;점을 입력하려면 리턴입력    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))                     ;끝점을 입력    
      (setq epang (angle cen ep))                               ;끝점의 각도    
      (if (< epang sang) (setq lsgn 1) (setq lsgn -1))          ;치수선 진행방향    
    ) ;cond(next=nil)    
    ((numberp next)                                             ;dx가 숫자인 경우    
       (setq ep (polar cen (- sang (/ next rc)) rc))             ;끝점    
       (if (> next 0) (setq lsgn 1) (setq lsgn -1))             ;치수선 진행방향    
    ) ;cond(next=number)    
  ) ;of cond    
    
    
  (while (/= ep nil)    
    
    (setq eang (angle cen ep))                                    ;중심점과 끝점이 이루는 각    
    (setq seang (abs (dang sang eang)))                           ;시작점에서 끝점 각    
    
    (setq cl (/ (* 2 pi rc seang) (* 2 pi)))                    ;Arc의 길이    
    (if (> cl 1000.0)    
;      (setq txt (rtos (* cl 0.001) 2 3))    
      (setq txt (rtos_dimdsep (* cl 0.001) 3))          
      (setq txt (rtos cl 2 0))    
    ) ;of if    
    
    (setq dsp (polar cen sang rd)                       ;치수보조선 시작점    
          dep (polar cen eang rd))                      ;치수보조선 끝점    
    
    (setq mang (/ (+ sang eang) 2.0))                   ;시작각과 끝각의 중간각    
    (setq dpnt (polar cen mang (+ rd (* 20 ds sgn) (* (- dlevel 1) dim_gap ds sgn))))    
                                                            ;Dimension line의 위치    
    
    (setq wh4 (which4 mang))    
    (if (or (= wh4 1) (= wh4 2))    
        (setq tsgn 1)    
        (setq tsgn -1)    
    ) ;of if    
    (setq tang (rtod (+ mang (* (/ PI 2.0) tsgn -1))))    
    (setq txtpnt (polar dpnt mang (* 1.1 th ds tsgn)))         ;text위치 (X,Y,Z)    
    (setq txtxy (list (car txtpnt) (cadr txtpnt)))          ;text위치 (X,Y)    
    (princ "\nText=<")    
    (princ txt)    
    (setq txt1 (getstring ">: "))    
    
  (if (= txt1 "@")    
    (progn    
      (setq divl (getint "\nDivision length: "))            ;나누는 길이 입력    
      (setq divn (rtos (/ cl divl) 2 0))                    ;나눈 갯수계산    
      (if (< divl 1000.)    
        (setq divl (rtos divl 2 0))                         ;1000미만일 때    
;        (setq divl (rtos (* 0.001 divl) 2 3))               ;1000이상일 때    
        (setq divl (rtos_dimdsep (* 0.001 divl) 3))               ;1000이상일     
	) ;of if    
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이    
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
      (if (>= txtlen cl)                       ;치수보조선 내에 text 안들어가면    
        (progn    
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다(첫줄)    
          (setq dtxt2 (strcat "=" txt))             ;                     (둘째줄)    
          (setq dtxt1p (polar dpnt mang (* th ds tsgn)))    
          (setq dtxt2p (polar dpnt mang (* -1 th ds tsgn)))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "TEXT" "M" dtxt1p (* th ds) tang dtxt1)    
          (command "TEXT" "M" dtxt2p (* th ds) tang dtxt2)    
          (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" " " dpnt txtxy)    
          (setvar "OSMODE" oldosmode)    
	) ;of progn    
        (progn                                 ;치수보조선 내에 text 들어가면    
          (setq dtxt1 (strcat divn "@" divl "=" txt))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" dtxt1 dpnt txtxy)    
          (setvar "OSMODE" oldosmode)    
	) ;of progn ELSE    
      ) ;of IF    
    ) ;of progn THEN    
    (progn    
      (if (= txt1 "") (setq txt1 txt))                  ;리턴입력시 옛 text를 씀    
        (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
        (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" txt1 dpnt txtxy)    
        (setvar "OSMODE" oldosmode)    
    ) ;of progn ELSE    
  ) ;of if(txt1=@)    
    
    (setq sp ep)                                            ;끝점을 첫점으로    
    (setq sang (angle cen sp))                              ;중심점과 시작점이 이루는 각    
    
    (initget "eXit Undo")    
    (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;끝점까지 거리    
    (cond                                                   ;점을 입력하려면 리턴입력    
      ((= next nil)    
        (setq ep (getpoint "\nPick point: "))               ;끝점을 입력    
      ) ;cond(next=nil)    
      ((= next "eXit")                                      ;eXit입력시 ep=nil    
        (setq ep nil)    
      ) ;cond(next="eXit")    
      ((numberp next)                                       ;dx가 숫자인 경우    
        (setq ep (polar cen (- sang (/ next rc lsgn)) rc))  ;끝점    
      ) ;cond(next=number)    
    ) ;of cond    
    
  ) ;of while    
    
  (pop-env)                                                 ;환경변수 복귀    
  (setq *error* oer seterr nil)    
  (princ)    
    
) ;of defun    
    
    
;-------------------------------------------------------    
; function : rtos_dimdsep    
;            rtos (change . to dimdsep)    
;            Yi suk jong    
;            00/5/10    
;-------------------------------------------------------    
; argument    
;       real : real to be converted    
;  precision : decimail precision    
;-------------------------------------------------------    
(defun rtos_dimdsep(real precision / real precision txt txtlen count)      
  (setq txt (rtos real 2 precision)    
	txtlen (strlen txt)    
	count 1)      
      
  (while (and (/= (substr txt count 1) ".") (<= count (1+ txtlen)))    
    (setq count (1+ count))    
  );while    
  (if (> count txtlen)    
    txt    
    (strcat (substr txt 1 (1- count)) (getvar "DIMDSEP") (substr txt (1+ count) (- txtlen count)))    
  )    
);defun        


;------------------------
; Program: DIMA
;          DIMension Arc
;          Yi Suk Jong
;          04/03/29
;------------------------
(defun c:DIMA()
  (setq dimgap (getvar "DIMDLI")
	ds     (getvar "DIMSCALE"))
  
  (setq aent (entget (car (entsel "\nSelect ARC: "))))
  (setq cpnt (cdr (assoc 10 aent)) ;center point
	r (cdr (assoc 40 aent))   ;radius
	sang (cdr (assoc 50 aent))     ; start ang
	eang (cdr (assoc 51 aent)))    ; end ang
  (setq pnt (getpoint cpnt "\nPick original point: "))
  (if (> sang eang)
    (setq eang (+ eang (* 2 pi)))
  );if
    
  (setq avang (* 0.5 (+ sang eang)))  ;average of angle

  (setq ro (distance cpnt pnt))   ; radius of original point
  (setq spnt (polar cpnt sang ro)
	epnt (polar cpnt eang ro))
  (setq cl (* r (abs (dang eang sang))))
  (if (< cl 1000)
    (setq lentxt (rtos cl 2 0))
    (setq lentxt (rtos cl 2 3))
  );if  
  (setq txtpnt (polar cpnt avang (+ ro (* 20 ds))))
  (push-os)
  (command "DIM1" "ANG" "" cpnt spnt epnt txtpnt lentxt "")
  (pop-os)
	
)


