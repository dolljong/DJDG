;**********************************
; Program: EARTH
;          EARTH Volum
;          Yi Suk-Jong
;          96/6/28
;**********************************
; 토공량 계산 프로그램 (그림만..)
;**********************************

(defun C:EARTH(
/ s_tosa    s_ripi  s_balp  max ds  h_list  s_list  ipnt    b       b0      w
  toth      ix      by      bty     b1x     b2x     hn      count   vx      pp
  level     h       s       wstr    x1      x2      y       oldc    mp      mp1
)


  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq s_tosa 1.0                                          ;토사 기울기
        s_ripi 0.5                                          ;리핑암 기울기
        s_balp 0.3                                          ;발파암 기울기
        max    6000)                                        ;경계높이

  (setq ds (getvar "DIMSCALE")                             ;스케일값
        th (getvar "DIMTXT"))                           ;text크기
  (setq h_list nil
        s_list nil)

  (setq ipnt (getpoint "\nInsert (MID)point: "))            ;footing저면 가운데 점
  (setq b (* (getreal "\nB(m): ") 1000))                    ;폭
  (setq b0 b)
  (setq w (* (getreal "\nW(m): ") 1000))                    ;너비
  (setq h_tosa (* 1000 (getreal "\nHeight of TOSA(m): ")))  ;토사 높이
  (if (/= h_tosa 0)                                         ;토사 높이가 0이 아니면
    (if (> h_tosa max)                                      ;토사높이가 max를 넘을 경우
      (progn
        (setq h_list (append h_list (list max)))               ;max위 높이  (4m)
;        (setq h_list (append h_list (list 4000)))               ;max위 높이  (4m)
        (setq s_list (append s_list (list s_tosa)))
        (setq h_list (append h_list (list (- h_tosa max))))    ;max아래 높이
;        (setq h_list (append h_list (list (- h_tosa 4000))))    ;max아래 높이
        (setq s_list (append s_list (list s_tosa)))
      ) ;of progn
      (progn                                                ;토사높이가 max를 넘지 않을 경우
        (setq h_list (append h_list (list h_tosa)))         ;max위 높이  (4m)
        (setq s_list (append s_list (list s_tosa)))
      ) ;of progn
    ) ;of if
  ) ;of if
  (setq h_ripi (* 1000 (getreal "\nHeight of RIPPING(m): ")))   ;피핑암 높이
  (if (/= h_ripi 0)
    (if (and (> (+ h_tosa h_ripi) max) (> max h_tosa))
      (progn
;        (setq h_list (append h_list (list (- 4000 h_tosa))))    ;max위 높이  (4m)
        (setq h_list (append h_list (list (- max h_tosa))))    ;max위 높이  (4m)
        (setq s_list (append s_list (list s_ripi)))
;        (setq h_list (append h_list (list (- (+ h_ripi h_tosa) 4000)))) ;max아래 높이
        (setq h_list (append h_list (list (- (+ h_ripi h_tosa) max)))) ;max아래 높이
        (setq s_list (append s_list (list s_ripi)))
      ) ;of progn
      (progn
        (setq h_list (append h_list (list h_ripi)))
        (setq s_list (append s_list (list s_ripi)))
      ) ;of if
    ) ;of if
  ) ;of if
  (setq h_balp (* 1000 (getreal "\nHeight of BALPA(m): ")))     ;발파암 높이
  (if (/= h_balp 0)
    (if (and (> (+ h_tosa h_ripi h_balp) max) (> max (+ h_tosa h_ripi)))
      (progn
;        (setq h_list (append h_list (list (- 4000 h_tosa h_ripi))))     ;max위 높이  (4m)
        (setq h_list (append h_list (list (- max h_tosa h_ripi))))     ;max위 높이  (4m)
        (setq s_list (append s_list (list s_balp)))
;        (setq h_list (append h_list (list (- (+ h_balp h_ripi h_tosa) 4000))))  ;max아래 높이
        (setq h_list (append h_list (list (- (+ h_balp h_ripi h_tosa) max))))  ;max아래 높이
        (setq s_list (append s_list (list s_balp)))
      ) ;of progn
      (progn
        (setq h_list (append h_list (list h_balp)))
        (setq s_list (append s_list (list s_balp)))
      ) ;of if
    ) ;of if
  ) ;of if

  (setq toth (+ h_tosa h_ripi h_balp))                          ;총높이

  (setq ix (car ipnt)                                           ;삽입점 x좌표
        by (cadr ipnt)                                          ;삽입점 y좌표
        bty by)

  (setq b1x (- ix (/ b 2) 500)                              ;왼쪽 바닥 x좌표
        b2x (+ ix (/ b 2) 500))                             ;오른쪽 바닥 x좌표


;******** 수직 치수선 넣기
   (setq hn (length h_list)                                 ;층의 갯수
         h_list (reverse h_list)
         s_list (reverse s_list))

   (setq count 0
         vx b1x)
   (repeat hn
     (setq vx (- vx (* (nth count h_list) (nth count s_list))))
     (setq count (1+ count))
   ) ;of repeat

   (setq pp (list vx bty))

   (setq count 0)

   (repeat hn                                               ;각 층의 높이 치수
     (setq pp (f_dv pp (nth count h_list) 1 -1 nil))
     (setq count (1+ count))
   ) ;of repeat

   (setq pp (f_dv (list vx bty) toth 1 -2 nil))            ;총높이 치수

  (setq count 1
        level 0)

  (repeat hn
    (setq h (nth (1- count) h_list)
          s (nth (1- count) s_list))

    (if (/= h 0)                                        ;발파암
      (progn
        (setq wstr (rtos (* w 0.001) 2 3))              ;폭을 string으로
        (setq x1 (- b1x (* s h)))
        (setq x2 (+ b2x (* s h)))
        (setq y (+ by h))
        (command "LINE" (list b1x by) (list x1 y) "")
        (command "LINE" (list b2x by) (list x2 y) "")
        (if (= level 0)                                 ;수평 치수선 그리기
          (progn                                        ;처음 레벨일때
            (command "LINE" (list b1x bty) (list b2x bty) "")
            (setq pp (f_dh (list b1x bty) 500 1 -1 nil))
            (setq pp (f_dh pp (- b2x b1x 1000) 1 -1
                       (strcat (rtos (* b 0.001) 2 3) "X" wstr)))
            (setq pp (f_dh pp 500 1 -1 nil))
            (setq w (+ w 1000))
            (setq pp (f_dh (list b1x bty) (- b2x b1x) 1 -2
                       (strcat (rtos (* (+ b 1000) 0.001) 2 3) "X"
                               (rtos (* w 0.001) 2 3))))
            (setq pp (f_dh (list x1 bty) (- b1x x1) 1 -2 nil))
            (setq pp (f_dh (list x2 bty) (- b2x x2) 1 -2 nil))
            (setq w (+ w (* 2 s h)))
            (setq b (+ b 1000 (* 2 s h)))
            (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "1")
            (command "LINE" (list x1 y) (list x2 y) "")
            (setvar "CECOLOR" oldc)
            (setq pp (f_dh (list x1 bty) (- x2 x1) 1 -3
                       (strcat (rtos (* b 0.001) 2 3) "X"
                               (rtos (* w 0.001) 2 3))))
            (setq level -3)
          ) ;of progn
          (progn
            (setq w (+ w (* 2 s h)))
            (setq level (1- level))                         ;치수선 레벨 증가
            (setq pp (f_dh (list x1 bty) (- b1x x1) 1 (1+ level) nil))
            (setq pp (f_dh (list b2x bty) (- x2 b2x) 1 (1+ level) nil))
            (setq pp (f_dh (list x1 bty) (- x2 x1) 1 level
                       (strcat (rtos (* (- x2 x1) 0.001) 2 3) "X"
                               (rtos (* w 0.001) 2 3))))
            (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "1")
            (command "LINE" (list x1 y) (list x2 y) "")
            (setvar "CECOLOR" oldc)
          ) ;of progn
        ) ;of IF
        (setq b1x x1
              b2x x2
              by y)
      ) ;of PROGN
    ) ;of IF
    (setq count (1+ count))
  ) ;of REPEAT

  (if (/= h_balp 0)
    (progn
      (setq mp (list (+ ix (/ b0 2) 500 (* s_balp (/ h_balp 2)))
                     (+ bty (/ h_balp 2))))
      (setq mp1 (polar mp (- (atan (/ 1 s_balp)) (/ pi 2)) (* th ds)))
      (command "TEXT" "M" mp1 (* th ds) (rtod (atan (/ 1 s_balp)))
               (strcat "1:" (rtos s_balp 2 1)))
    ) ;of progn
  ) ;of if
  (if (/= h_ripi 0)
    (progn
      (setq mp (list (+ ix (/ b0 2) 500 (* s_balp h_balp) (* s_ripi (/ h_ripi 2)))
                     (+ bty h_balp (/ h_ripi 2))))
      (setq mp1 (polar mp (- (atan (/ 1 s_ripi)) (/ pi 2)) (* th ds)))
      (command "TEXT" "M" mp1 (* th ds) (rtod (atan (/ 1 s_ripi)))
               (strcat "1:" (rtos s_ripi 2 1)))
    ) ;of progn
  ) ;of if
  (if (/= h_tosa 0)
    (progn
      (setq mp (list (+ ix (/ b0 2) 500 (* s_balp h_balp) (* s_ripi h_ripi) (* s_tosa (/ h_tosa 2)))
                     (+ bty h_balp h_ripi (/ h_tosa 2))))
      (setq mp1 (polar mp (- (atan (/ 1 s_tosa)) (/ pi 2)) (* th ds)))
      (command "TEXT" "M" mp1 (* th ds) (rtod (atan (/ 1 s_tosa)))
               (strcat "1:" (rtos s_tosa 2 1)))
    ) ;of progn
  ) ;of if
  (setq *error* oer seterr nil)
  (princ)
) ;of defun


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
;        UD : Up/DOWN (절대값은 LEVEL)
; 돌려주는 값 - 끝점 좌표
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th (getvar "DIMTXT)
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (if (> ud 0)                                              ;위 아래
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;치수선 위치 계산 (절대값)

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep 위치계산

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치

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
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds th)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds th)) 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)
          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM명령 내림
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

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th (getvar "DIMTXT")                          ;text크기 = dimtxt
        dim_gap 10.0)                                 ;치수선 간격
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (> lr 0)                                        ;왼쪽/오른쪽
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))

  (setq next (* dst n))                                 ;끝점까지 거리

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;수정된 끝점

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치

  (setq dy (distance sp ep))                          ;두 점의 거리

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수계산
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
                                  (list (- (* dx sgn) (* ds th)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds th)) 0.0 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy " ")               ;DIM명령 내림
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
