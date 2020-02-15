;*******************************
; Program : STBOX
;           STeel BOX
;           Yi Suk-Jong
;           96/8/5
;*******************************
; Steel box ´Ü	ô·©£Õµµ ±×¸®±â
;*******************************
;
(defun C:STBOX()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

;  (setq ip (getpoint "\nPick Insert point: ")
;        lu (getreal "\nLU: ")
;        tu (getreal "\nTU: ")
;        ll (getreal "\nll: ")
;        tl (getreal "\ntl: ")
;        hw (getreal "\nhw: ")
;        tw (getreal "\ntw: ")
;        bw (getreal "\nbw: "))

  (setq ip (getpoint "\nPick Insert point: ")

        ix (car ip)
        iy (cadr ip)

        lu 2800             ; »óÇÃ·»Áö Æø
        tu 30               ; »óÇÃ·»Áö µÎ²²

        ll 2800             ; ÇÏÇÃ·»Áö Æø
        tl 30               ; ÇÏÇÃ·»Áö µÎ²²

        hw 2500             ; À¥ ³ôÀÌ
        tw 20               ; À¥ µÎ²²
        bw 2600             ; À¥ °£°Ý(center~center)

        sl -3                   ;³ë¸éÀÇ slop(%)
        sh 3                    ;slop of hunch 1:3
        tp 80                   ;thickness of pavement(mm)

        b1 12400                ;±³·®Æø
        b2 1100                 ;ÁÂÃø cantilever ³¡ ~ ÁÂÃø web center
        b3 4500                 ;¹Ú½º ¼ø°£°Ý(³»Ãøweb CL °£°Ý)
        bs 400                  ;¹Ú½º »ó´ÜÀÇ ½½¶óºê Á¢ÃË¸é Æø

        h1 300                  ;webÀ§ÂÊ ½½¶óºê µÎ²²(ÇåÄ¡ºÎ)
        h2 250                  ;ÀÏ¹ÝºÎ ½½¶óºê µÎ²²
        h3 200                  ;ÄµÆ¿·¹¹ö ³¡´Ü µÎ²²

        stlu 300                ; stringer »óÇÃ·»Áö Æø
        sttu 12                 ;                   t
        sthw 800                ;          Web ³ôÀÌ
        sttw 9                  ;                 t
        stll 300                ;          ÇÏÇÃ·»Áö Æø
        sttl 12 )               ;                    t

  (setq str (list (+ ix b1) (+ iy (* b1 sl 0.01 ))) ;slab top right
        sbl (list ix (- (cadr ip) h3))              ;slab bottom left
        sbr (list (car str) (- (cadr str) h3))             ;slab bottom right
        sh2l (list ix (- (cadr ip) h2))             ;slab h2 left
        sh2r (list (car str) (- (cadr str) h2))     ;slab h2 right
        b4 (- b1 b2 (* 2 bw) b3))                   ;¿ìÃø cantilever ±æÀÌ

  (if (> sl 0)
    (progn
      (setq ip1 (list (+ ix b2 (/ bw 2))                ;ÁÂÃø box insert point
                      (- (+ iy (* b2 sl 0.01)) h1))
            ip2 (list (+ ix b2 bw b3 (/ bw 2))          ;¿ìÃø box insert point
                      (- (+ iy (* (+ b2 bw b3) sl 0.01)) h1)))
    ) ;of progn
    (progn
      (setq ip1 (list (+ ix b2 (/ bw 2))                ;ÁÂÃø box insert point
                      (- (+ iy (* (+ b2 bw) sl 0.01)) h1))
            ip2 (list (+ ix b2 bw b3 (/ bw 2))          ;¿ìÃø box insert point
                      (- (+ iy (* (+ b2 bw b3 bw) sl 0.01)) h1)))
    ) ;of progn
  ) ;of if

  (setq stip (list (+ ix b2 bw (/ b3 2.0))
                   (+ iy (* (+ b2 bw (/ b3 2.0)) 0.01 sl) (- h1)))) ;stringerÀÇ »ðÀÔÁ¡

  (setq s1 (list (- (car ip1) (/ lu 2)) (cadr ip1))
        s2 (list (+ (car s1) bs) (+ (cadr s1) tu))
        a1 (atan (/ 1.0 sh))                            ;¿À¸¥ÂÊ hunch°¢
        a2 (- pi a1)                                    ;¿ÞÂÊ hunch°¢
        s3 (inters s2 (polar s2 a1 100) sh2l sh2r nil)
        s5 (list (+ (car ip1) (/ bw 2.0) (- bs)) (+ (cadr ip1) tu))
        s4 (inters s5 (polar s5 a2 100) sh2l sh2r nil)
        s6 (list (+ (car ip1) (/ lu 2.0)) (cadr ip1))
        s7 (inters s6 (polar s6 a1 100) sh2l sh2r nil)
        s9 (list (- (car stip) (/ stlu 2.0)) (cadr stip))
        s8 (inters s9 (polar s9 a2 100) sh2l sh2r nil)
        s10 (list (+ (car stip) (/ stlu 2.0)) (cadr stip))
        s11 (inters s10 (polar s10 a1 100) sh2l sh2r nil)
        s13 (list (- (car ip2) (/ lu 2.0)) (cadr ip2))
        s12 (inters s13 (polar s13 a2 100) sh2l sh2r nil)
        s14 (list (+ (car s13) bs) (+ (cadr ip2) tu))
        s15 (inters s14 (polar s14 a1 100) sh2l sh2r nil)
        s17 (list (+ (car ip2) (/ lu 2.0) (- bs)) (cadr s14))
        s16 (inters s17 (polar s17 a2 100) sh2l sh2r nil)
        s18 (list (+ (car s17) bs) (cadr ip2)))

  (setq pl (list (+ ix 450) (+ iy (* sl 0.01 450) tp))    ;Æ÷Àå ¿ÞÂÊ
        pr (list (- (car str) 450)                        ;Æ÷Àå ¿À¸¥ÂÊ
                 (+ (cadr str) (* sl -0.01 450) tp)))

  (command "LINE" ip str "")
  (command "LINE" ip sbl s1 "")
  (command "LINE" s2 s3 s4 s5 "")
  (command "LINE" s6 s7 s8 s9 "")
  (command "LINE" s10 s11 s12 s13 "")
  (command "LINE" s14 s15 s16 s17 "")
  (command "LINE" s18 sbr str "")
  (command "LINE" pl pr "")                                 ;Æ÷Àå¼±

  (st_box ip1 lu tu ll tl hw tw bw)
  (st_box ip2 lu tu ll tl hw tw bw)
  (stringer stip stlu sttu sthw sttw stll sttl)
  (barrier 0 ip (- sl))
  (barrier 1 str sl)

  (setq *error* oer seterr nil)

) ;of defun


;******************************************
; Function : ST_BOX
;            draw STeel BOX
;            Yi Suk-Jong
;             96/8/5
;******************************************
(defun ST_BOX(IP LU TU LL TL HW TW BW /
              ix iy p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16
 )
  (setq ix (car ip)
        iy (cadr ip)
        p1 (list (- ix (/ lu 2)) (+ iy tu))
        p2 (list (+ ix (/ lu 2)) (cadr p1))
        p3 (list (car p1) iy)
        p4 (list (car p2) iy)
        p5 (list (- ix (/ ll 2)) (- iy hw))
        p6 (list (+ ix (/ ll 2)) (cadr p5))
        p7 (list (car p5) (- (cadr p5) tl))
        p8 (list (car p6) (cadr p7))
        p9 (list (- ix (/ bw 2) (/ tw 2)) iy)
        p10 (list (+ (car p9) tw) iy)
        p11 (list (car p9) (cadr p5))
        p12 (list (car p10) (cadr p5))
        p14 (list (+ ix (/ bw 2) (/ tw 2)) iy)
        p13 (list (- (car p14) tw) iy)
        p15 (list (car p13) (cadr p5))
        p16 (list (car p14) (cadr p5)))

  (command "LINE" p1 p2 p4 p3 "C")
  (command "LINE" p5 p6 p8 p7 "C")
  (command "LINE" p9 p11 "")
  (command "LINE" p10 p12 "")
  (command "LINE" p13 p15 "")
  (command "LINE" p14 p16 "")
) ;of defun

;******************************************
; Function : STRINGER
;            draw STRINGER
;            Yi Suk-Jong
;             96/8/5
;******************************************
;
(defun STRINGER( IP STLU STTU STHW STTW STLL STTL /
                 ix iy p1 p2 p3 p4 p5 p6 p7 p8 p9
                 p10 p11 p12
)
  (setq ix (car ip)
        iy (cadr ip)
        p1 (list (- ix (/ stlu 2.0)) (+ iy sttu))
        p2 (list (+ (car p1) stlu) (cadr p1))
        p3 (list (car p2) iy)
        p4 (list (- (car p3) stlu) iy)
        p5 (list (- ix (/ stll 2.0)) (- iy sthw))
        p6 (list (+ (car p5) stll) (cadr p5))
        p7 (list (car p6) (- (cadr p6) sttl))
        p8 (list (car p5) (cadr p7))
        p9 (list (- ix (/ sttw 2.0)) iy)
        p10 (list (car p9) (- iy sthw))
        p11 (list (+ (car p9) sttw) (cadr p9))
        p12 (list (car p11) (cadr p10)))

  (command "LINE" p1 p2 p3 p4 "C")
  (command "LINE" p9 p10 "")
  (command "LINE" p11 p12 "")
  (command "LINE" p5 p6 p7 p8 "C")

) ;of defun


;********************************************
; Function : BARRIER
;            DRAW BARRIER
;            Suk-Jong Yi
;            96/7/3
;********************************************
;¹æÈ£Ã¥À» ±×·ÁÁØ´Ù.
; ³Ñ¾î¿À´Â °ª
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun BARRIER( LR ip SL / LR ip SL)
  (if (= LR 0)                                      ;¿ÞÂÊ¿¬¼®
    (progn
      (setq inpo (list (+ (car ip) 30)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))    ;¹Ù±ùÂÊ¾Æ·¡
      (setq inpi (list (+ (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;¾ÈÂÊ¾Æ·¡
      (command "PLINE" inpo "@0,1080" "@230,0" "@70,-700"
                           "@120,-175" inpi "")                 ;¿¬¼®±×¸®±â
    ) ;of PROGN
    (progn
      (setq inpo (list (- (car ip) 30.0)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))    ;¹Ù±ùÂÊ¾Æ·¡
      (setq inpi (list (- (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;¾ÈÂÊ¾Æ·¡
      (command "PLINE" inpo "@0,1080" "@-230,0" "@-70,-700"
                           "@-120,-175" inpi "")                ;¿¬¼®±×¸®±â
    ) ;of progn
  ) ;of IF
) ;of defun

