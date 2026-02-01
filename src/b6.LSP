;*************************************                
;     Program : B6                
;               reBar marking                
;               By Suk-Jong Yi                
;               96/7/29                
;*************************************                
; 철근마킹중 아래 모양을 만들어줍니다.                
;          ①                
;           |                
; /---/---/-+-/---/                
;*************************************                
                
(defun C:B6(/                                          ;지역변수 정의                
            oldclr      index       nnsent      entl        entype                
            crsxy       spexy       epexy       crsxy       slp         elp                
            sp          ep          sslst       enum        index       enum                
)                
                
  (defun SETERR(s)                             ;내장에러루틴 정의
    (if (/= s "Function cancelled")                
        (princ (strcat "\nError: " s))                
    ); of If                
    (setq *error* oer seterr nil)                
    (princ)                
  ); of SETERR                
                
  (setq oer *error* *error* seterr)             ;내장에러루틴 가동                
                
  (push-env)                                    ;환경변수 대피                
                
  (setvar "CMDECHO" 0)                        ;환경변수 셋팅 명령메아리 끄기                
  (setvar "BLIPMODE" 0)                       ;BLIP MODE 끄기                
  (setq ds (getvar "DIMSCALE"))               ;스케일값                
  (setq rc (* ds 5.0)                         ;마킹원의 반지름                
        diaxg 6                               ;원의 중심부터 직경삽입점까지 x거리  
        th (getvar "DIMTXT"))                 ;text크기는 치수크기                
                
  (princ "\nSelect objects: ")                ;마킹 대상 엔티티 선택                
  (setq sslst (ssget))                
                
  (setq enum (sslength sslst))                ;seclection set의 엔티티 갯수                
                
                
  ;**** 대상 엔티티 강조, 대상엔티티 갯수 파악 (LINE과 ARC만)                
  (setq index 0                               ;엔티티 갯수                
        nssent 0)                             ;line이거나 arc인 엔티티 갯수                
                
  (repeat enum                  ;엔티티 갯수 만큼 반복                
    (setq entl (entget (ssname sslst index)))      ;엔티티 리스트                
    (setq entype (cdr (assoc 0 entl)))             ;엔티티 타입                
    (if (or (= entype "LINE") (= entype "ARC"))    ;엔티티가 line이거나 arc인경우                
      (progn                
        (redraw (ssname sslst index) 3)            ;엔티티 강조                
        (if (= nssent 0) (setq ssent (ssadd (ssname sslst index)))                
                         (setq ssent (ssadd (ssname sslst index) ssent))                
        ) ;of if                    ; line이거나 arc엔티티모은 slection set 만들기                
        (setq nssent (1+ nssent))                     ;대상 엔티티 갯수 count up                
      ) ;of progn                
    ) ; of if                
    (setq index (1+ index))                           ;다음 엔티티로                
  ) ;of repeat                
  (print nssent)                
  (princ "Entity Found")                              ;LINE이나 ARC가 몇개인가?                
                
  (setq sp (getpoint "\nPick start point: "))         ;보조선의 첫점                
  (setq ep (getpoint sp "\nPick end point: "))        ;보조선의 끝점                
  (setq seang (angle sp ep))                          ; 보조선의 도  
  (cecolor "1")              
  (command "LINE" sp ep "")                           ;보조선 그기  
  (popcolor)              
                
  (setq index 0)                
  (repeat nssent                                  ;ARC이거나 LINE인 엔티티만큼                
     (setq entl (entget (ssname ssent index)))    ;엔티티 리스트 구하기                
     (cond                
       ((= (cdr (assoc 0 entl)) "ARC")            ;마킹 대상물이 ARC인 경우                
         (setq crsxy (cross entl sp ep))            ;ARC와 보조선의 교차점찾기                
       ) ;of entity=ARC                
       ((= (cdr (assoc 0 entl)) "LINE")           ; 마킹 대상물이 LINE인 경우                
         (setq spexy (cdr (assoc 10 entl))          ;보조선의 시작과 끝점                
               epexy (cdr (assoc 11 entl)))                
         (setq crsxy (inters spexy epexy sp ep))    ;보조선과 LINE의 교차점 찾기                
       ) ;of entity=LINE                
     ) ;of cond                
     (setq slp (polar crsxy (+ seang (* pi 0.25)) (* 1.3 (getvar "DIMSCALE"))))                
     (setq elp (polar crsxy (+ seang (* pi 1.25)) (* 1.3 (getvar "DIMSCALE"))))
     (cecolor "1")
     (command "LINE" slp elp "")                  ; Tick line그리기 /
     (popcolor)
     (redraw (ssname ssent index) 4)              ; 강조된 엔티티 원상복구                
     (setq index (1+ index))                      ; 다음 엔티티로                
  ) ; repeat                
                
                
                
  (setq sp (getpoint "\nPick start point: "))                
  (setq ep (getpoint sp "\nPick end point: "))
  (cecolor "1")
  (command "LINE" sp ep "")
  (popcolor)
                
  (setq p3 (getpoint ep "\nPick base line: "))              ;base line point                
                
  (setq dx (- (car p3) (car ep)))                
  (if (< dx 0)                                          ;base line의 x방향인식                
    (setq xsgn -1)                
    (setq xsgn 1)                
  ) ;of if                
                
  (setq dy (- (cadr p3) (cadr ep)))                
  (if (<  dy 0)                                         ;base line의 y방향인식                
    (setq ysgn -1)                
    (setq ysgn  1)                
  ) ;of if                
                
  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line의 길이                
                
  (if (> (abs dx) (abs dy))                                             ;누워있나 서있나?                
    (progn                
      (setq p4 (list (+ (car ep) (* blen xsgn)) (cadr ep)))
      (cecolor "1")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dx 0)                
        (setq ip p4)                
        (setq ip ep)                
      ) ;of if                
      (setq cp (list (+ (car ip) rc)                
                     (+ (cadr ip) rc)))                
      (setq diaxy (list (+ (car cp) (* diaxg ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;dia표시 위치                
      (setq txtrot 0)                                   ;text회전각                
    ) ;of progn                
    (progn                
      (setq p4 (list (car ep) (+ (cadr ep) (* blen ysgn))))
      (cecolor "1")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dy 0)                
        (setq ip p4)                
        (setq ip ep)                
      ) ;of if                
      (setq cp (list (- (car ip) rc)                
                     (+ (cadr ip) rc)))                
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* diaxg ds)) 0.0))      ;dia표시 위치                
      (setq txtrot 90)                
    ) ;of progn                
  ) ;of if                
                
                
  (cecolor "1")
  (command "CIRCLE" cp rc)
  (popcolor)
                

  (setq mk (getstring "\nEnter Marking: "))                
  (txtinc mk cp txtrot)                
  (setq dia (getstring "\nEnter Rebar Dia: "))
  (cecolor "bylayer")  
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (popcolor)
                
  (pop-env)                
                
  (setq *error* oer seterr nil)                
  (princ)                
) ; defun                
                
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
                
;****************************************                
; Function : CROSS                
;            CROSS point of arc & line                
;            By Suk-Jong Yi                
;            1995/6/26                
;****************************************                
;함쩠: 호와 직선의 쏩췽초 찾기                
;     인쩠: ARC entity list, 직선의 첫초 , 직선의 씨초                
;     써과: 직선과 ARC의 쏩췽초                
                
(defun CROSS(aent sp ep /                
aent    sp      ep      a       b       r       sa      ea      x1      x2                
y1      y2      c       d       a1      b1      c1      x1      x2      y1                
y2      ang1    ang2                
)                
                
(push-env)                
(setq a (car (cdr (assoc 10 aent))))      ; ARC entity의 중쫠초 x좌킨                
(setq b (cadr (cdr (assoc 10 aent))))     ; ARC entity의 중쫠초 y좌킨                
(setq r (cdr (assoc 40 aent)))            ; ARC entity의 반지쟎                
(setq sa (cdr (assoc 50 aent)))           ; ARC entity의 시작 각옷                
(setq ea (cdr (assoc 51 aent)))           ; ARC entity의 씨 각옷                
                
(setq x1 (car sp))                        ; LINE entity의 시작초 x좌킨                
(setq x2 (car ep))                        ; LINE entity의 씨초 x좌킨                
(setq y1 (cadr sp))                       ; LINE entity의 시작초 y좌킨                
(setq y2 (cadr ep))                       ; LINE entity의 씨초 y좌킨                
(if (= (- x1 x2) 0)                
  (progn                                    ;x가 constant일 때                
    (setq c x1                
          a1 1                              ;y찌 얾한 2췽방정식의 a                
          b1 (* -2 b)                       ;y찌 얾한 2췽방정식의 b                
          c1 (+ (* c c) (* -2 a c) (* a a) (* b b) (* -1 r r)) ;y찌 얾한 2췽방정식의 c                
          y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;근 1                
          y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;근 2                
    );setq                
  );progn                
  (progn                                    ; y가 x의 함쩠일 때                
    (setq c (/ (- y1 y2) (- x1 x2)))          ; y=cx+d찌서 c                
    (setq d (- y2 (* c x2)))                  ; y=cx+d찌서 d                
    (setq a1 (+ 1 (* c c)))                   ; x찌 얾한 이췽방정식의 a                
    (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))   ;x찌 얾한 이췽방정식의 b                
    (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))  ;이췽 방정식의 c                
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
);defun

;************************************************************    
; Function : INANG    
;            a angle is IN the range of ANGle-1 and angle-2 ?    
;            By Suk-Jong Yi    
;            1995/6/26    
;*************************************************************    
    
;어떤 각이 주어진 두각(ang1, ang2) 사이에 있는가?    
; 두각 사이에 있는 경우 두각의 차이를 돌려주고    
; 두각 사이에 없는 경우는 nil을 돌려준다.    
    
(defun inang(a1 a2 a3 /             ;인수 정의    
a1 a2 a3                            ;지역변수 정의    
)    
(if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))   ;첫각이 두번째 각보다 크면 +360도    
(if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;주어진 각이 두각사이에 있으면    
                                nil)         ; 두각의 차이를 돌려줌    
)                                            ; 두각 사이에 없으면 nil돌려줌    