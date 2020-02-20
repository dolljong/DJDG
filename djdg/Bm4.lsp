;**************************************    
; Program : BM4    
;           Rebar Marking-4    
;           Suk-Jong Yi    
;           1995. 3. 17    
;**************************************    
; 철근마킹중 아래의 모양을 만들어준다.    
;    -----------①    
;  /  /  /  /    
;**************************************    
    
(defun C:BM4(/    
plst crsp-lst    
)    
    
  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
(push-env)    
    
(setq ds (getvar "DIMSCALE")    
      th (getvar "DIMTXT"))    
    
;(setq cr (* ds 3.5))    
;(setq th (* ds 2.5))    
(setq cr (* ds 4.5))            ;마킹원의 크기    
(setq th (* ds th))             ;text의 크기    
    
(setq entlst (ssget))                           ;마킹대상 entity선택    
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
      (setq ep (cdr (assoc 11 ent)))            ;라인의 끝점    
      (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;라인중간점 X    
                     (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y    
      (setq plst (append plst (list mp)))       ;중간점을 마킹포인트에 추가    
      (setq npnt (1+ npnt))                     ;마킹포인트의 갯수 증가    
    ) ;of entype="LINE"    
    ((= entype "POLYLINE")      ;폴리라인인 경우(철근을 도나스로 그린 경우)    
      (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;첫점정보    
      (if (= (abs (cdr (assoc 42 vtx1))) 1.0)         ;폴리라인이 아크형인가?    
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
;      (princ "LWPOLYLINE")    
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
  ) ;of cond    
(setq index (1+ index))                                 ;index=엔티티번호    
) ; of repeat    
    
;* base line 시작과 끝포인트 입력    
(setq blpnt1 (getpoint "\nPick base line first point: "))    
(setq blpnt2 (getpoint blpnt1 "\nPick base line second point: "))    
(setq blang (angle blpnt2 blpnt1))          ;베이스라인의 각도(마키원에서부터)    
    
;*첫번째 포인트와 베이스라인과 만나는 점    
(setq fpnt (nth 0 plst))    
(setq fang (angle blpnt2 fpnt))                                 ;첫째포인트의 각    
(setq dtasgn (/ (dang blang fang) (abs (dang blang fang))))     ;첫각의 부호    
(setq ll-ang (+ blang (/ pi 4.0 dtasgn) pi))              ;리더라인의 각    
(setq tmppnt (polar fpnt ll-ang 1.0))    
(setq fcrs-pnt (inters blpnt1 blpnt2 fpnt tmppnt nil)) ;첫번째 크로스포인트    
    
    
;* 마킹원에서 가장 멀리 떨어진 포인트 찾기    
(setq fdst (distance blpnt2 fcrs-pnt))          ;첫점의 거리을 구한다    
(setq maxdst fdst)                              ;최대거리를 첫점의 거리로 한다    
(setq index 0)    
(repeat npnt                                    ;포인트 갯수만큼 반복    
  (setq pnt (nth index plst))                   ;현재 점    
  (setq tmppnt (polar pnt ll-ang 1.0))          ;임시점    
  (setq crs-pnt (inters blpnt1 blpnt2 pnt tmppnt nil))   ;크로스포인트    
  (setq crsp-lst (append crsp-lst (list crs-pnt)))  ;크로스 포인트 리스트에 추가    
  (cecolor "1")
  (command "LINE" pnt crs-pnt "")               ;리더선 그기  
  (popcolor)  
  (setq dst (distance blpnt2 crs-pnt))          ;마킹원의 포인트와 교차점의 거리    
  (if (>= dst maxdst)                           ;최대 거리 포인트인가?    
    (progn    
      (setq maxpnt crs-pnt)    
      (setq maxdst dst)    
    ) ;of progn    
  ) ;of if    
  (setq index (1+ index))                       ;포인트갯수만큼 반복    
) ;of repeat    
    
;최소 거리 포인트 찾기    
(setq maxdst 0)    
(setq index 0)    
(repeat npnt    
  (setq pnt (nth index crsp-lst))    
  (setq dst (distance maxpnt pnt))    
  (if (>= dst maxdst)    
    (progn    
      (setq minpnt pnt)    
      (setq maxdst dst)    
    ) ;of progn    
  ) ;of if    
  (setq index (1+ index))    
) ;of repeat    
    
    
;* 베이스라인 체크    
(setq mx-bl2dst (distance maxpnt blpnt2))    
(setq mx-mindst (distance maxpnt minpnt))    
(if (<= mx-bl2dst mx-mindst)    
  (setq blpnt2 (polar minpnt (+ blang pi) (* ds 7.0))))    
    
;* 마킹원의 중심점    
(setq ccen (polar blpnt2 (+ blang pi) cr))    
    
;* 베이스 라인 마킹원 및 철근 지름 표시    
(cecolor "1")
(command "LINE" maxpnt blpnt2 "")                       ;베이스라인 그리기    
(command "CIRCLE" ccen cr)                              ;마킹원 그리기    
(popcolor)
(setq mk (getstring "\nEnter Marking: "))               ;마킹명칭 입력    
(txtinc mk ccen 0.0)                                    ;마킹명칭 쓰기    
(setq dia (getstring "\nEnter Rebar Dia: "))            ;철근 다이아 입력    
(setq diaxy (list (+ (car ccen) (* 4 ds)) (- (cadr ccen) (* 4 ds)) 0.0))
(cecolor "bylayer")
(command "TEXT" diaxy th "0" (strcase dia))          ;철근다이아 기  
(popcolor)
    
(pop-env)    
  (setq *error* oer seterr nil)    
(princ)    
    
) ;of defun BM4    
    
;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; 원 안찌 철근번호를 기입해준다.
; 넘어오는 값
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ds)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))               ;text콜기=캬쩠콜기

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
	(progn
	  (cecolor "bylayer")
          (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
	  (popcolor)
	);progn  
        (progn
	  (cecolor "bylayer")
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
	  (popcolor)
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (progn
      (cecolor "bylayer")
      (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
      (popcolor)
    );progn  
  ) ;of IF
) ;of DEFUN

    
; -------------------------------------    
; function : getLwVert    
; LwPolyline의 Vertex를 척아냄    
; 인수: vlist  : vertext list    
;       tmpctr : 접근할 vertext 번호 0,1,2    
; -------------------------------------    
    
  (defun getLwVert (vlist tmpctr / count tmp)    
;    (setq vlist (entget (car (entsel))))                       ;실험용    
    
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
    
