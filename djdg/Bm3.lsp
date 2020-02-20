;**************************************
; Program : BM3
;           reBar Mark 3
;           Suk-Jong Yi
;           1995. 3. 16~17, 5/30
;**************************************
;철근마킹중 아래의 모양을 만들어준다.(밑줄없음)
;            ①
;           /|\
;         /  |  \
;**************************************

(defun C:BM3(/
plst oldclr ds entlst mpnt nent           ;지역변수 정의
index ent entype sp ep mp npnt
vtx1 nxt1 vtx2 vtx1p vtx2p cenp
cp ang minang maxang dtang mang
ccen mk dia diaxy
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(push-env)   ;환경변수값 대피

(setq ds (getvar "DIMSCALE")
      th (getvar "DIMTXT"))

;(setq cr (* ds 3.5))
;(setq th (* ds 2.5))
(setq cr (* ds 4.5))       ;마킹원의 크기
(setq th (* ds th))        ;text의 크기

(setq entlst (ssget))                           ;마킹대상 entity선택
(setq mpnt (getpoint "Pick marking point: "))   ;마킹 원을 그릴 위치선택
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


(setq ang (angle mpnt (nth 0 plst)))        ;첫점의 각을 구한다
(setq minang ang)                           ;최대각과 최소각을 첫점의 각으로한다
(setq maxang ang)
(setq index 0)
(repeat npnt                                ;포인트 갯수만큼 반복
  (setq ang (angle mpnt (nth index plst)))  ;마킹원의 포인트와 점의 각을 구한다
  (if (<= ang minang) (setq minang ang))    ;최대각과 최소각 찾기
  (if (>= ang maxang) (setq maxang ang))
  (cecolor "1")
  (command "LINE" mpnt (nth index plst) "") ;포인트에서 마킹원점까지 선을그린다
  (popcolor)
  (setq index (1+ index))                   ;포인트갯수만큼 반복
) ;of repeat

(setq dtang (dang minang maxang))           ;최대각과 최소각의 차이
(setq mang (+ minang (/ dtang 2.0)))        ;최대각과 최소각의 중간각
(setq ccen (polar mpnt (+ mang pi) cr))         ;마킹원의 센타점
(cecolor "1")
(command "CIRCLE" ccen cr)                      ;마킹원을 그린다
(popcolor)
(setq mk (getstring "\nEnter Marking: "))               ;마킹명칭 입력
(txtinc mk ccen 0.0)
(setq dia (getstring "\nEnter Rebar Dia: "))            ;철근 다이아 입력
(setq diaxy (list (+ (car ccen) (* 4 ds)) (- (cadr ccen) (* 4 ds)) 0.0))
(cecolor "bylayer")
(command "TEXT" diaxy th "0" (strcase dia))          ;철근다이아 쓰기
(popcolor)
(pop-env)   ;환경변수값 복귀

  (setq *error* oer seterr nil)
(princ)


); of defun

;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; 원 안에 철근번호를 기입해준다.
; 넘어오는 값
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ds)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))               ;text크기=치수크기

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
