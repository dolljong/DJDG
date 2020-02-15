;****************************    
; Program : BOR    
;           Boring    
;           Yi Suk-Jong    
;           96/8/6    
;****************************    
; 보링 주상도 그리기 프로그램
;****************************    
    
(defun C:BOR( /    
              ds ip ys iy ix od edep ety h ip txt tip    
              nx1 nx2 count ny nl1 nl2 ntp txt    
            )    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
  (push-env)                                            ;환경변수 대피    
  (setvar "MEASUREMENT" 0)    
    
  (setq ds (getvar "DIMSCALE")                          ;scale 값    
        BB  6.0                                          ;주상도 폭    
        ip (getpoint "\nInsert point: ")                ;삽입점    
        ys (getreal "\nY-Scale: ")                      ;y-scale    
        iy (cadr ip)                                    ;insert Y    
        ix (car ip)                                     ;insert X    
        od 0)                                           ;old depth    
    
  (while (/= (setq edep (getreal "\nDepth(m): ")) nil)  ;지층깊이 입력(m)    
    (command "VSLIDE" "DJDG(BOR_REG)")                    ;범례 보여주기    
    (setq ety (getint "\nEarth type(1~11): ")           ;지층종류 입력    
          h   (* (- edep od) ys 1000.0)                 ;지층의 두께    
          ip (list ix (- iy (* ys od 1000.0)))          ;지층 상단 중앙 점    
          txt (rtos edep 2 1)                           ;지층 깊이 text    
;          tip (list (- ix (* 3.5 0.5 ds) (* 2.0 ds))    ;깊이 text insert point    
          tip (list (- ix (* BB 0.5 ds) (* 2.0 ds))    ;깊이 text insert point    
                    (- (cadr ip) h))    
;          lp1 (list (- ix (* 3.5 0.5 ds)) (- (cadr ip) h))  ; 1mm line pnt-1    
          lp1 (list (- ix (* BB 0.5 ds)) (- (cadr ip) h))  ; 1mm line pnt-1    
          lp2 (list (- (car lp1) (* 1.0 ds)) (cadr lp1)))   ; 1mm line pnt-2    
    
    (command "TEXT" "MR" tip (* 2.0 ds) "0" txt)            ;깊이 text쓰기    
;    (rect ip (* 3.5 ds) h)                                  ;사각형 그리기    
    (rect ip (* BB ds) h)                                  ;사각형 그리기    
    (setvar "CECOLOR" "GREEN")                              ;초록색으로    
    (command "LINE" lp1 lp2 "")                             ;1mm line그리기    
    
    (cond                                                   ;지층 종류별로..    
      ((= ety 1) (b_1 ip h ds))                             ; 1-점토 섞인 실트    
      ((= ety 2) (b_2 ip h ds))                             ; 2-실트 섞인 점토    
      ((= ety 3) (b_3 ip h ds))                             ; 3-모래 섞인 실트    
      ((= ety 4)                                            ; 4-실트 섞인 모래    
        (progn    
          (b_1 ip h ds)    
          (b_4 ip h ds)    
        ) ;of progn    
      ) ;of ety=4    
      ((= ety 5)    
        (progn    
          (redraw)    
          (b_4 ip h ds)                                     ; 5-자갈 섞인 모래    
        ) ;of progn    
      ) ;of sub_cond    
      ((= ety 6)                                            ; 6-실트질 모래 섞인 자갈    
        (progn    
          (b_1 ip h ds)    
          (b_5 ip h ds)    
        ) ;of progn    
      ) ;of ety=6    
      ((= ety 7)                                            ; 7-모래 섞인 자갈    
        (progn    
          (redraw)    
          (b_4 ip h ds)    
          (b_5 ip h ds)    
        ) ;of progn    
      ) ;of ety=7    
      ((= ety 8)                                            ; 8-풍화암    
        (progn    
          (b_6 ip h ds)    
          (b_7 ip h ds)    
        ) ;of progn    
      ) ;of ety=7    
      ((= ety 9)                                            ; 9-연  암    
        (progn    
          (b_2 ip h ds)    
          (b_7 ip h ds)    
        ) ;of progn    
      ) ;of ety=7    
      ((= ety 10) (b_7 ip h ds))                            ;10-경  암    
    ) ;of cond    
    
    (setq od edep)                                      ;현재 깊이를 old 깊이로    
    (setvar "CECOLOR" "WHITE")    
  ) ;of while                                           ;다음 지층 계속    
    
    
;  (setq nx1 (+ ix (* 3.5 0.5 ds))                       ;N값 표시 위치선 x값    
  (setq nx1 (+ ix (* BB 0.5 ds))                       ;N값 표시 위치선 x값    
        nx2 (+ nx1 (* 1.0 ds)))    
  (setq firstNy (getreal "\nFirst N-Value depth(m): "))    
  (setq count 0)                                        ;처음 N값부터    
  (princ "N-Value (") (princ firstny) (princ "m): ")    
  (while (/= (setq nv (getstring T)) "")                ;return 입력시 종료    
    (setq ny (- iy  (* (+ (* 1500 count) (* firstNy 1000)) ys))                   ;N값 기입 y좌표    
          nl1 (list nx1 ny)                             ;Tick    
          nl2 (list nx2 ny)    
          ntp  (list (+ nx2 (* ds 1.0)) ny))            ;N값 text삽입점    
    (setvar "CECOLOR" "GREEN")    
    (command "LINE" nl1 nl2 "")    
    (setvar "CECOLOR" "WHITE")    
    (command "TEXT" "ML" ntp (* 2.0 ds) "0.0" nv)       ;N값 표기    
    (setq count (1+ count))                             ;다음 N값으로    
    (setq txt (rtos (+ firstNy (* 1.5 count)) 2 1))    
    (princ "N-value (") (princ txt) (princ "m): ")      ;메세지 보여줌    
  ) ;of while    
    
  (pop-env)                                             ;환경변수 되돌림    
  (setq *error* oer seterr nil)    
  (princ)    
    
) ;of defun    
    
(defun b_1( IP H sc /    
            ip rec    
          )    
;  (rect ip (* 3.5 sc) h)    
  (rect ip (* BB sc) h)    
  (setq rec (entlast))    
    
  (command "HATCH" "LINE" (* sc (/ 5 3.5) BB) 90 rec "")    
    
  (command "ERASE" rec "")    
  (command "REDRAW")    
    
) ;of defun    
    
(defun b_2( IP H sc /    
            ip rec    
          )    
    
;  (rect ip (* 3.5 sc) h)    
  (rect ip (* BB sc) h)    
  (setq rec (entlast))    
    
  (command "HATCH" "LINE" (* sc (/ 5 3.5) BB) 45 rec "")    
    
  (command "ERASE" rec "")    
  (command "REDRAW")    
    
) ;of defun    
    
(defun b_3( IP H sc /    
            ip rec    
          )    
    
;  (rect ip (* 3.5 sc) h)    
  (rect ip (* BB sc) h)    
    
  (setq rec (entlast))    
    
  (command "HATCH" "LINE" (* sc (/ 2.5 3.5) BB) 90 rec "")    
    
  (command "ERASE" rec "")    
  (command "REDRAW")    
    
) ;of defun    
    
    
(defun b_4( IP H SC /    
            ip h sc b dd ix b4 iy x1 x2 nh count p1 p2    
          )    
    
;  (setq b (* 3.5 sc)    
  (setq b (* BB sc)    
        dd (/ b 20.0)    
        ix (car ip)    
        b4 (/ b 4.0)    
        iy (+ (cadr ip) (/ b4 2.0))    
        x1 (- ix b4)    
        x2 (+ ix b4)    
        nh (fix (/ h b4))    
        count 1)    
    
  (repeat nh    
    (if (= (rem count 2) 1)    
      (progn    
        (setq p1 (list x1 (- iy (* count b4)))    
              p2 (list x2 (cadr p1)))    
        (command "DONUT" "0.0" dd p1 p2 "")    
      ) ;of progn    
      (progn    
        (setq  p1 (list ix (- iy (* count b4))))    
        (command "DONUT" "0.0" dd p1 "")    
      ) ;of progn    
    ) ;of IF    
    (setq count (1+ count))    
  ) ;of repeat    
    
) ;of defun    
    
(defun b_5( IP H SC /    
            b dd ix b4 iy x1 x2 nh count p1 p2    
          )    
    
;  (setq b (* 3.5 sc)    
  (setq b (* BB sc)    
        dd (/ b 5.0)    
        ix (car ip)    
        b4 (/ b 4.0)    
        iy (- (cadr ip) (/ b4 2.0))    
        x1 (- ix b4)    
        x2 (+ ix b4)    
        nh (fix (/ (- h (/ b4 2.0)) b4))    
        count 1)    
    
  (repeat nh    
    (if (= (rem count 2) 1)    
      (progn    
        (setq p1 (list x1 (- iy (* count b4)))    
              p2 (list x2 (cadr p1)))    
        (command "DONUT" "0.0" dd p1 p2 "")    
      ) ;of progn    
      (progn    
        (setq  p1 (list ix (- iy (* count b4))))    
        (command "DONUT" "0.0" dd p1 "")    
      ) ;of progn    
    ) ;of IF    
    (setq count (1+ count))    
  ) ;of repeat    
    
) ;of defun    
    
(defun b_6( IP H sc /    
            ip rec    
          )    
    
;  (rect ip (* 3.5 sc) h)    
  (rect ip (* BB sc) h)    
    
  (setq rec (entlast))    
    
  (command "HATCH" "ANSI32" (* sc (/ 5 3.5) BB) 0 rec "")    
    
  (command "ERASE" rec "")    
  (command "REDRAW")    
    
) ;of defun    
    
(defun b_7( IP H sc /    
            ip rec    
          )    
    
;  (rect ip (* 3.5 sc) h)    
  (rect ip (* BB sc) h)    
    
  (setq rec (entlast))    
    
  (command "HATCH" "CROSS" (* sc (/ 5 3.5) BB) 0 rec "")    
    
  (command "ERASE" rec "")    
  (command "REDRAW")    
    
) ;of defun    
    
(defun rect( IP B H /    
             ix iy p1 p2 p3 p4 ip b h    
           )    
    
  (setq ix (car ip)    
        iy (cadr ip))    
    
  (setq p1 (list (- ix (/ b 2.0)) iy)    
        p2 (list (+ (car p1) b) iy)    
        p3 (list (car p2) (- iy h))    
        p4 (list (car p1) (cadr p3)))    
  (command "PLINE" p1 p2 p3 p4 "C")    
) ;of defun    
