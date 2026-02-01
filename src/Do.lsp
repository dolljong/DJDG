;*******************************************    
; Program : DO    
;           Dimension Oblique    
;           Jong-Suk Yi    
;           96/4/16    
;*******************************************    
; Vertical DIM을 OBLIQUE시켜준다.    
; 제약조건 - 수직 DIM에만 해당된다.    
;          - OBLIQUE각은 30도로 정해져 있다.    
;*******************************************    
    
(defun C:DO(/    
divl    divn    dp      ds      dsel    dtx     dtxt1   dtxt1p  dtxt2    
dtxt2p  dx      dxy     dy      ep      fst     lstdim  next    pnt1    
pnt2    ppnt    sent    sgn     sp      th      txt     txt1    txtlen    
)    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
  (setq th (getvar "DIMTXT")                          ;text크기 = dimtxt    
        dim_gap (getvar "DIMDLI"))                                 ;치수선 간격    
  (setq ds (getvar "DIMSCALE"))                       ;scale factor    
    
  (setvar "BLIPMODE" 0)    
  (setvar "CMDECHO" 0)    
      
;  (push-env)                                          ;환경변수값 대피    
    
  (initget "Object")    
  (setq sp (getpoint "\nPick first point/Object: "))  ;기존 치수선 참조=Object    
  (if (= sp "Object")    
    (progn    
      (setq dsel (entsel "\nSelect Dimension Entity: "))   ;기존 치수선 선택    
      (setq sent (entget (car dsel)))                      ;치수선 entity    
      (setq pnt1 (cdr (assoc 13 sent)))                    ;보조선 시작점    
      (setq pnt2 (cdr (assoc 14 sent)))                    ;보조선 끝점    
      (setq ppnt (cadr dsel))                              ;선택시 pick point    
      (if (> (distance ppnt pnt1) (distance ppnt pnt2))    ;pick point에 가까운    
        (setq sp pnt2) (setq sp pnt1))                     ;쪽 점을 sp로    
    ) ;of progn THEN    
  ) ;of IF(sp=Object)    
    
  (setq dp (getpoint "\nPick Dimension side: "))          ;치수선이 위치할 방향    
    
  (setq dtx (- (car dp) (car sp)))    
  (setq sgn (/ dtx (abs dtx)))                            ;왼쪽 오른쪽 부호    
    
  (setq fst (getint "\nDimension line LEVEL <1>: "))      ;치수선 level입력    
  (if (= fst nil) (setq fst 1))    
  (setq dx (* ds (+ 15 (* dim_gap (- fst 1)))))           ;찍은점과 치수선의거리    
    
  (setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;끝점까지 거리    
  (cond                                                ;점을 입력하려면 리턴입력    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))               ;끝점을 입력    
      (setq ep (list (car sp) (cadr ep)))                 ;수정된 끝점    
    ) ;cond(next=nil)    
    ((numberp next)                                       ;dx가 숫자인 경우    
      (setq ep (list (car sp) (+ (cadr sp) next)))        ;ep 위치계산    
    ) ;cond(next=number)    
  ) ;of cond    
    
    
  (while (/= ep nil)                                  ;ep가 nil이 아닌동안 반복    
    
    (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치    
    
    (setq dy (distance sp ep))                          ;두 점의 거리    
    (if (< dy 1000.0)    
      (setq txt (rtos dy 2 0))                          ;1000미만일 때    
;      (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때    
      (setq txt (rtos_dimdsep (* dy 0.001) 3))                ;1000이일      
    ) ;of if(dy < 1000)    
    
    (princ "\nDimension text <")                        ;Dimension text표시    
    (princ txt)    
    (setq txt1 (getstring T ">: "))                     ;새로운 dimension text입력    
    (if (= (substr txt1 1 1) "@")    
      (progn    
;        (setq divl (getint "\nDivision length: "))      ;나누는 길이 입력    
        (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))    
        (setq divn (rtos (/ dy divl) 2 0))              ;나눈 갯수계산    
        (if (< divl 1000.)    
          (setq divl (rtos divl 2 0))                   ;나누는 길이가 1000미만시    
;          (setq divl (rtos (* divl 0.001) 2 3))) ;of if  나누는 길이가 1000이상시    
          (setq divl (rtos_dimdsep (* divl 0.001) 3))) ;of if  나누는 길이가 1000이상    
	  (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds    
                     (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
        (if (>= txtlen dy)    
          (progn                                  ;text가 보조선 내에 안들어가면    
            (setq dtxt1 (strcat divn "@" divl))   ;두줄로 나눔    
            (setq dtxt2 (strcat "=" txt))    
            (setq dtxt1p (mapcar '+ (mid-point sp ep)    
                                    (list (- (* dx sgn) (* ds th))  ;x위치    
                                          (* dx (/ (sin (/ pi 6)) (cos (/ pi 6))))    
                                          0.0)))                     ;z위치    
            (setq dtxt2p (mapcar '+ (mid-point sp ep)    
                                    (list (+ (* dx sgn) (* ds th))  ;x위치    
                                          (* dx (/ (sin (/ pi 6)) (cos (/ pi 6))))    
                                          0.0)))                     ;z위치    
            (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)    
	    (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)    
            (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)    
            (command "DIM1" "VER" sp ep dxy " ")              ;DIM명령 내림    
            (setvar "OSMODE" oldosmode)    
	  ) ;of progn THEN    
          (progn                                  ;text가 보조선 내에 들어가면    
            (setq dtxt1 (strcat divn "@" divl "=" txt))    
            (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)    
	    (command "DIM1" "VER" sp ep dxy dtxt1)            ;DIM명령 내림    
            (setvar "OSMODE" oldosmode)    
	  ) ;of progn ELSE    
        ) ;of IF    
      ) ;of progn THEN    
      (progn    
        (if (= txt1 "") (setq txt1 txt))                      ;리턴입력시 옛 text를 씀    
        (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)    
	(command "DIM1" "VER" sp ep dxy txt1)             ;DIM명령 내림    
        (setvar "OSMODE" oldosmode)    
      ) ;of progn ELSE    
    ) ;of if(txt1=@)    
    
    (setq lstdim (entlast))                               ;방금 만들어진 dim선택    
    (setq oldexo (getvar "DIMEXO"))    
    (setvar "DIMEXO" 3)    
    (command "DIM1" "OBL" lstdim "" (* sgn 30))           ;30도만큼 돌려줌    
    (command "DIM1" "UPDATE" lstdim "")    
    (setvar "DIMEXO" oldexo)    
    
    (setq sp ep)                    ;끝점을 첫점으로    
    (initget "eXit Undo")    
    (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;끝점까지 거리    
    (cond                                               ;점을 입력하려면 리턴입력    
      ((= next nil)    
        (setq ep (getpoint "\nPick point: "))                 ;끝점을 입력    
        (setq ep (list (car sp) (cadr ep)))                   ;수정된 끝점    
      ) ;cond(next=nil)    
      ((= next "eXit")                                        ;eXit입력시 ep=nil    
        (setq ep nil)    
      ) ;cond(next="eXit")    
      ((numberp next)                                         ;dx가 숫자인 경우    
        (setq ep (list (car sp) (+ (cadr ep) next)))          ;ep 위치계산    
      ) ;cond(next=number)    
    ) ;of cond    
    
  ) ;of while    
    
  ;(pop-env)    
  (setq *error* oer seterr nil)    
  (prin1)    
) ;defun    
    
    
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
