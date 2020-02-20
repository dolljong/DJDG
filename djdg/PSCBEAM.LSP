;********************************************
; Program : PSCBEAM
;           PSC BEAM
;           Suk-Jong Yi
;           96/5/10
;********************************************
; PSC BEAM 단면 일반도를 그려준다.
;********************************************

(defun C:PSCBEAM()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)
  (setq ds (getvar "DIMSCALE"))                                 ;스케일값
  (setq b (* 1000.0 (getreal "\nB(m): ")))                      ;교량폭
  (setq bn (getint "\nNumber of beam: "))                       ;beam갯수
  (setq pb (* 1000.0 (getreal "\nPich of beam(m): ")))          ;beam간격
  (setq ls (* 0.01 (getreal "\nLeft slop(%): ")))               ;좌측 slop
  (setq rs (* 0.01 (getreal "\nRight slop(%): ")))              ;좌측 slop
  (setq ta (getreal "\nThickness of asphalt(mm): "))            ;포장두께
  (setq ts (getreal "\nThickness of slab(mm): "))               ;slab두께
  (setq p (getpoint "\nPick Insert point: "))                   ;삽입점 (중앙상단)

  (setq b2 (/ b 2.0))                                           ;교량폭의 반
  (setq bb (* pb (- bn 1)))                                     ;beam양측 간격

  (setq pl (list (- (car p) (- b2 456))
                 (+ (cadr p) (* (- b2 456) ls)) 0.0))           ;포장왼쪽
  (setq pr (list (+ (car p) (- b2 456))
                 (+ (cadr p) (* (- b2 456) rs)) 0.0))           ;포장오른쪽

  (setq sct (list (car p) (- (cadr p) ta))                      ;슬라브중앙상단
        scb (list (car p) (- (cadr sct) ts)))                   ;슬라브중앙하단

  (setq slte (list (- (car p) b2) (+ (cadr sct) (* ls b2)) 0.0))    ;슬라브왼쪽상단끝
  (setq srte (list (+ (car p) b2) (+ (cadr sct) (* rs b2)) 0.0))    ;슬라브오른쪽상단끝

  (setq slbe (list (car slte) (- (cadr slte) ts) 0.0))              ;슬라브왼쪽하단끝
  (setq srbe (list (car srte) (- (cadr srte) ts) 0.0))              ;슬라브오른쪽하단끝

  (command "LINE" sct slte slbe scb srbe srte sct "")               ;슬라브그리기

  (barrier 0 slte (* ls 100.0))                                               ;왼쪽방호벽
  (barrier 1 srte (* rs 100.0))                                               ;오른쪽방호벽

  (command "LINE" pl p pr "")                                       ;포장선 그리기

  (if (= (rem bn 2) 1)
    (progn                                                          ;빔 갯수가 홀수 일때
      (setq ly (+ (cadr scb) (* 350 ls))                            ;중앙빔 좌측 y
            ry (+ (cadr scb) (* 350 rs)))                           ;중앙빔 우측 y
      (if (<= ly ry)
        (setq iy ly)
        (setq iy ry))                                           ;낮은 쪽을 insert y로
      (draw_beam (list (car p) iy))                             ;중앙빔 그리기
      (setq bn2 (/ (- bn 1) 2))                                 ;한쪽 빔 갯수
      (setq cnt 1)                                              ;왼쪽첫번째 빔
      (repeat bn2
        (setq btp (list (- (car p) (* cnt pb))                  ;빔 상단점
                        (- (+ (cadr scb) (* cnt pb ls)) (* (abs ls) 350.0))
                        0.0))
        (draw_beam btp)                                         ;왼쪽빔 그리기
        (setq cnt (1+ cnt))                                     ;왼쪽 다음 빔
      ) ;of repeat
      (setq cnt 1)                                              ;오른쪽첫번째 빔
      (repeat bn2
        (setq btp (list (+ (car p) (* cnt pb))                  ;빔 상단점
                        (- (+ (cadr scb) (* cnt pb rs)) (* (abs rs) 350.0))
                        0.0))
        (draw_beam btp)                                         ;왼쪽빔 그리기
        (setq cnt (1+ cnt))                                     ;왼쪽 다음 빔
      ) ;of repeat
    ) ;of progn
    (progn                                                      ;빔 갯수가 짝수 일때
      (setq bn2 (/ bn 2))                                       ;한쪽 빔의 갯수
      (setq cnt 1)                                              ;첫 왼쪽빔부터
      (repeat bn2
        (if (= cnt 1)
          (setq dx (* pb 0.5))                                  ;중앙에서 빔중앙까지 거리
          (setq dx (+ (* pb 0.5) (* (1- cnt) pb)))
        ) ;of IF
        (setq btp (list (- (car p) dx)                          ;빔 상단점
                        (- (+ (cadr scb) (* dx ls)) (* (abs ls) 350.0))
                        0.0))
        (draw_beam btp)                                         ;빔 그리기
        (setq cnt (1+ cnt))                                     ;왼쪽 다음 빔
      ) ;of repeat
      (setq cnt 1)                                              ;첫 오른쪽빔부터
      (repeat bn2
        (if (= cnt 1)
          (setq dx (* pb 0.5))                                  ;중앙에서 빔중앙까지 거리
          (setq dx (+ (* pb 0.5) (* (1- cnt) pb)))
        ) ;of IF
        (setq btp (list (+ (car p) dx)                          ;빔 상단점
                        (- (+ (cadr scb) (* dx rs)) (* (abs rs) 350.0))
                        0.0))
        (draw_beam btp)                                         ;빔 그리기
        (setq cnt (1+ cnt))                                     ;왼쪽 다음 빔
      ) ;of repeat
    ) ;of progn
  ) ;of IF

;******  치수선 기입 (아래)
  (setq canl (/ (- b (* (1- bn) pb)) 2.0))                 ;캔틸레버 길이
  (setq dpy1 (- (cadr slbe) 2000.0))                       ;치수선y값
  (setq dp1 (list (car slbe) dpy1 0.0))                    ;치수선 시작점
  (setq dpy2 (- dpy1 (* ds 20)))                           ;치수선y값
  (setq dp11 (list (car slbe) dpy2 0.0))                   ;치수선위치
  (setq pp (f_dh dp1 canl 1 -1 nil))                       ;왼쪽 캔틸레버 치수
  (setq pp (f_dh pp pb (1- bn) -1 nil))                    ;beam간격치수
  (setq pp (f_dh pp canl 1 -1 nil))                        ;오른쪽 캔틸레버 치수
  (setq pp (f_dh dp1 b 1 -2 nil))                          ;총폭 치수

;****** 치수선 기입 (위)
  (setq pp1 (list (- (car p) (/ b 2)) (+ (cadr p) (* ls b 0.5) 1000)))   ;왼쪽방호벽 상단점
  (setq pp (f_dh pp1 450 1 1 nil))                          ;왼쪽 연석 치수
  (setq pp (f_dh pp (- b 900) 1 1 nil))                     ;포장면 치수
  (setq pp (f_dh pp 450 1 1 nil))                           ;오른쪽 연석 치수
  (setq pp (f_dh pp1 b 1 2 nil))                            ;전체 폭 치수

;****** slop 표시
  (setq pnt (list (- (car p) (/ b 4)) (+ (cadr p) (* (/ b 4) ls) ds)))
  (f_slop pnt -1 (* ls 100))
  (setq pnt (list (+ (car p) (/ b 4)) (+ (cadr p) (* (/ b 4) rs) ds)))
  (f_slop pnt 1 (* rs 100))


;****** 마무리

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of DEFUN


;*******************************************
; Function : DRAW_BEAM
;            DRAW psc BEAM
;            Suk-Jong Yi
;            96/5/11
;*******************************************
; PSC BEAM(단면)을 그려준다
;*******************************************

(defun DRAW_BEAM( ipnt / ipnt fp)
  (setq fp (list (- (car ipnt) 350) (cadr ipnt) 0.0))           ;시작점
  (command "LINE" fp "@700,0" "@0,-180" "@-250,-100" "@0,-1270"
                     "@230,-220" "@0,-230" "@-660,0" "@0,230"
                     "@230,220" "@0,1270" "@-250,100" "C")
) ;of defun

;********************************************
; Function : BARRIER
;            DRAW BARRIER
;            Suk-Jong Yi
;            96/7/3
;********************************************
;방호책을 그려준다.
; 넘어오는 값
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun BARRIER( LR ip SL / LR ip SL)
  (if (= LR 0)                                      ;왼쪽연석
    (progn
      (setq inpo (list (+ (car ip) 30)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;바깥쪽아래
      (setq inpi (list (+ (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;안쪽아래
      (command "PLINE" inpo "@0,1080" "@230,0" "@70,-700"
                           "@120,-175" inpi "")                             ;연석그리기
    ) ;of PROGN
    (progn
      (setq inpo (list (- (car ip) 30.0)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;바깥쪽아래
      (setq inpi (list (- (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;안쪽아래
      (command "PLINE" inpo "@0,1080" "@-230,0" "@-70,-700"
                           "@-120,-175" inpi "")                            ;연석그리기
    ) ;of progn
  ) ;of IF
) ;of defun

;***********************************
; Program : F_SLOP
;           Function SLOP mark
;           Suk-Jong Yi
;           96/7/3
;***********************************
;노면의 slop을 표시해준다. (%)
; 넘어오는 값
;     MP : Mid Point 화살표의 중간점
;   SGNX : (화살표의 진행방향)
;     SL : SLop값 (%)
;***********************************

(defun F_SLOP(MP SGNX SL
/
        ds      mp      sp      sgnx    sl      ent     spent
        epent   dx      sgn     tmp     dy      ang     angl
        sgnt    sltxt   getxt   lsttxt  txtlen  txtl    blsp
        blep    ap1ang  ap1     ap2ang  ap2     txtp    txtang  clr
)

  (setq ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))

  (setq ang (atan (/ sl 100.0)))
  (cond                                    ;화살표가 왼쪽으로 가는가 오른쪽으로
    ((= sgnx -1) (setq angl (- pi ang)))   ;가는가에 따라서 라인의 각도를 계산
    ((= sgnx 1) (setq angl (+ 0.0 ang)))   ;왼쪽으로 갈 경우 180+ang
  ) ;of cond                               ;오른쪽으로 갈 경우 0+ang


;기울기의 부호를 스트링으로 만든다
  (if (> sl 0.0)
    (setq sgnt "S=+")
    (setq sgnt "S=")
  )               ;양의 slop인 경우 +기호 추가

  (if (> (abs (rem sl 1)) 0)
    (setq slt (rtos sl 2 3))
    (setq slt (rtos (float sl) 2 1))
  ) ;of if
  (setq sltxt (strcat sgnt slt "%"))   ;부호 + slop + "%"
  (setq lsttxt sltxt)
  (setq txtlen (* (strlen lsttxt) th ds))                    ;전체 text의 길이
  (setq txtl (+ txtlen (* ds 2 th)))  ;text길이에다 2개의 글자를 추가한 길이

;화살표 베이스라인의 시작점과 끝점을 구한다
  (setq blsp (polar mp (- angl pi) (/ txtl 2.0)))
  (setq blep (polar mp    angl     (+ (/ txtl 2.0) (* ds th)))) ;화살표쪽을 길게
  (setq ap1ang (+ angl pi (* (dtor 15.0) sgnx -1)))
  (setq ap1 (polar blep ap1ang (* ds 2.0)))     ;화살표 사길이는 2.0mm
  (setq ap2ang (+ ap1ang (* (dtor 105.0) sgnx)))
  (setq ap2 (polar ap1 ap2ang (* (sin (dtor 15.0)) (* ds 2.0))))


  (setq txtp (polar mp (* (+ angl (/ pi 2.0)) sgnx) (* ds th)))  ;텍스트의 insert point
  (if (< sgnx 0)
    (setq txtang (- angl pi))          ;왼쪽방향일 경우 text각 구함
    (setq txtang angl)                 ;오른쪽 방향일 경우 베이스라인 각과 같다
  )
  (setq clr (getvar "CECOLOR"))
  (setvar "CECOLOR" "1")
  (command "PLINE" blsp blep ap1 ap2 "")                      ;화살표 그리기
  (setvar "CECOLOR" clr)
  (command "TEXT" "J" "M" txtp (* ds th) (rtod txtang) lsttxt) ;텍스트 쓰기

) ;of defun

;*******************************************
; Program : F_DO
;           Function Dimension Oblique
;           Jong-Suk Yi
;           96/7/3
;*******************************************
; Vertical DIM을 OBLIQUE시켜준다.
; 제약조건 - 수직 DIM에만 해당된다.
;          - OBLIQUE각은 30도로 정해져 있다.
;*******************************************

(defun F_DO(SP DST N LR TXT1
/
divl    divn    dp      ds      dsel    dtx     dtxt1   dtxt1p  dtxt2
dtxt2p  dx      dxy     dy      ep      fst     lstdim  next    pnt1
pnt2    ppnt    sent    sgn     sp      th      txt     txt1    txtlen
)

  (setq th (getvar "DIMTXT")                          ;text크기 =dimtxt
        dim_gap 10.0)                                 ;치수선 간격

  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (> lr 0)                                              ;왼쪽 오른쪽 선택
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 15 (* dim_gap (- (abs lr) 1)))))        ;찍은점과 치수선의거리

  (setq next (* dst n))

  (setq ep (list (car sp) (+ (cadr sp) next)))        ;ep 위치계산

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치

  (setq dy (distance sp ep))                          ;두 점의 거리

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                 ;나누는 길이 입력
      (setq divn (rtos n 2 0))              ;나눈 갯수계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;나누는 길이가 1000미만시
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if  나누는 길이가 1000이상시
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
            (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
            (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
            (command "DIM1" "VER" sp ep dxy " ")              ;DIM명령 내림
          ) ;of progn THEN
          (progn                                  ;text가 보조선 내에 들어가면
            (setq dtxt1 (strcat divn "@" divl "=" txt))
            (command "DIM1" "VER" sp ep dxy dtxt1)            ;DIM명령 내림
          ) ;of progn ELSE
        ) ;of IF
      ) ;of progn THEN
      (progn
        (if (= txt1 "") (setq txt1 txt))                      ;리턴입력시 옛 text를 씀
        (command "DIM1" "VER" sp ep dxy txt1)             ;DIM명령 내림
      ) ;of progn ELSE
    ) ;of if(txt1=@)

    (setq lstdim (entlast))                               ;방금 만들어진 dim선택
    (setq oldexo (getvar "DIMEXO"))
    (setvar "DIMEXO" 3)
    (command "DIM1" "OBL" lstdim "" (* sgn 30))           ;30도만큼 돌려줌
    (command "DIM1" "UPDATE" lstdim "")
    (setvar "DIMEXO" oldexo)

  ep                                                        ;끝점을 돌려보낸다.

) ;defun
