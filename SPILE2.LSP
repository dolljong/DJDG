;***************************************
; Program : SPILE2
;           Steel PILE - 2
;           Suk-Jong Yi
;           96/5/8
;***************************************
; ˆw‰ÅÌa·©·i ‹aaº…”a. (¶A¡e·i ÎaÑe)
;***************************************
(defun C:SPILE2(/
    oldclt ds s-pnt pd rdo pt rdi pl tang ang tdsto tdsti e-pnt
    sl-ont0o sr-pnt0o sl-pnt0i sr-pnt0i sl-pnto sr-pnto sl-pnti sr-pnti
    el-pnto dr-pnto el-pnti er-pnti cl-pnt cr-pnt ll-pnt1 ll-pnt2 rl-pnt
    lient rient cclr plent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                          ;ÑÅ‰w¥e® ”Ï¡

  (setq oldclt (getvar "CELTYPE"))

  (setq ds (getvar "DIMSCALE"))

  (setq s-pnt (getpoint "\nPick start point: "))      ;»¡¸b¸ñ ·³b
  (setq pd (getdist "\nPile diameter (mm): "))             ;Ìa·© »¡Ÿq (¶AÃb)
  (setq rdo (/ pd 2.0))                               ;Ìa·© ¤e»¡Ÿq(¶AÃb)
;  (setq pt (getdist "\Pile thickness (mm): "))             ;Ìa·© –A
;  (setq rdi (- (/ pd 2.0) pt))                        ;Ìa·© ¤e»¡Ÿq(¶AÃb)
  (setq pl (getdist s-pnt "\nPile length (m): "))     ;Ìa·© ‹©·¡
  (setq pl (* pl 1000))                               ;mm ”e¶á¡ ÑÅ¬e
  (initget "Tilt Angle")
  (setq tang (getreal "\nAngle/<1:?>: "))       ;1:?¡ ·³bÐaa¡e "T"Ÿi ·³b
  (if (= tang "Angle")
    (progn
      (setq tang (getreal "\nAngle (Degree): "))      ;ˆb•¡ ·³b
      (setq ang (+ (dtor tang) (* pi (/ 3.0 2.0))))   ;radian·a¡ Ã¡ÑÅ
      (setq pl (/ pl (cos tang)))                     ;‰w¬aÌa·©‹©·¡
    ) ;of PROGN
    (if (= tang 0)
      (setq ang (* pi (/ 3.0 2.0)))                          ;radian·a¡ Ã¡ÑÅ
      (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang)))   ;‹¡¶‰‹¡ˆt·i ˆb•¡¡ ÑÅ¬e
            pl  (/ pl (cos (atan (/ 1.0 tang)))))            ;‰w¬aÌa·©‹©·¡
    ) ;of IF
  ) ;of if

  (setq tdsto (abs (/ rdo (sin ang))))        ;Ìa·©‰w¬aµA ˜aŸe Ìa·© ¤e‰w· ‹©·¡

  (setq e-pnt (polar s-pnt ang pl))                   ;Ìa·© º—¯± {¸ñ

  (setq sl-pnt0o (polar s-pnt (- ang (/ pi 2.0)) rdo)) ;Ìa·©· ¶AÃb ¹ÁÃb(ˆa¬w)¯¡¸ñ
  (setq sr-pnt0o (polar s-pnt (+ ang (/ pi 2.0)) rdo)) ;Ìa·©· ¶AÃb ¶Ãb(ˆa¬w)¯¡¸ñ
  (setq sl-pnto (list (- (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;¹Á¶AÃb ¯¡¸ñ
  (setq sr-pnto (list (+ (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;¶¶AÃb ¯¡¸ñ
  (setq el-pnto (polar sl-pnt0o ang pl))                   ;Ìa·© ¹Á¶AÃb {¸ñ
  (setq er-pnto (polar sr-pnt0o ang pl))                   ;Ìa·© ¶¶AÃb {¸ñ

  (setq cl-pnt (list (/ (+ (car e-pnt) (car el-pnto)) 2.0)
                     (/ (+ (cadr e-pnt) (cadr el-pnto)) 2.0) 0.0))
  (setq cr-pnt (list (/ (+ (car e-pnt) (car er-pnto)) 2.0)
                     (/ (+ (cadr e-pnt) (cadr er-pnto)) 2.0) 0.0))

  (setq ll-pnt1 (polar cl-pnt (+ ang pi) (* rdo 0.25))) ;Ìa·©{· ¸é”eÎa¯¡ ‹aŸ¡‹¡
  (setq ll-pnt2 (polar cl-pnt ang (* rdo 0.25)))        ;¶áÐe ¸ñ—i ŠÐa‹¡
  (setq rl-pnt (polar cr-pnt ang (* rdo 0.25)))

  (setvar "CELTYPE" "BYLAYER")          ;œa·¥Èa·³·i ¯©¬å·a¡
  (command "LINE" sl-pnto el-pnto "")   ;¶E½¢ ¤aŒ{¬å‰Á µ¡Ÿe½¢ ¤aŒ{¬å ‹aŸ¡‹¡
  (command "LINE" sr-pnto er-pnto "")
  (setq cclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "RED")
  (setvar "CELTYPE" "CENTER")           ;œa·¥ Èa·³·i ·©¸ñ­á¬å·a¡
  (command "LINE" s-pnt e-pnt "")       ;º—¯±¬å ‹aŸ¡‹¡
  (setvar "CECOLOR" cclr)

  (setvar "CELTYPE" oldclt)             ;¶¥œœa·¥ Èa·³·a¡ ¥¢Šá¯¡Ç¡‹¡

  (command "PLINE" er-pnto "A" "S" rl-pnt e-pnt       ;Ìa·©{”e· ¸é”eÎa¯¡
                               "S" ll-pnt1 el-pnto    ;Í©Ÿ¡œa·¥· ´aÇa‹¡“w¬a¶w
                               "S" ll-pnt2 e-pnt "")
  (setq plent (list (entlast) ll-pnt1))

  (setvar "CECOLOR" "GREEN")                            ;¬‚·i ‘¢¬‚·a¡
  (setq rr (/ rdo 2.0))                                 ;Àõ Round Radious
  (while (>= rr (* 0.25 DS))                            ;ÂA¶AÃb roundˆa 0.25¥¡”a Çe •·´e e
    (setq tdsti (abs (/ (- rdo rr) (sin ang))))                 ;Ìa·©‰w¬aµA ˜aœa ¥eÐa“e Ìa·© ¤e»¡Ÿq
    (setq sl-pnt0i (polar s-pnt (- ang (/ pi 2.0)) (- rdo rr))) ;œa¶…—a ¹ÁÃb(ˆa¬w)¯¡¸ñ
    (setq sr-pnt0i (polar s-pnt (+ ang (/ pi 2.0)) (- rdo rr))) ;œa¶…—a ¶Ãb(ˆa¬w)¯¡¸ñ
    (setq sl-pnti (list (- (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;œa¶…—a ¹ÁÃb ¯¡¸ñ
    (setq sr-pnti (list (+ (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;œa¶…—a ¶Ãb ¯¡¸ñ
    (setq el-pnti (polar sl-pnt0i ang (- pl (* rdo 0.3))))     ;œa¶…—a ¹ÁÃb {¸ñ
    (setq er-pnti (polar sr-pnt0i ang (- pl (* rdo 0.3))))     ;œa¶…—a ¶Ãb {¸ñ

;    (setvar "CELTYPE" "HIDDEN")                        ;œa·¥ Èa·³·i ¸ñ¬å·a¡
    (command "LINE" sl-pnti el-pnti "")                 ;¶E½¢ ´e¬å‰Á µ¡Ÿe½¢ ´e¬å ‹aŸ¡‹¡
    (setq lient (list (entlast) el-pnti))
    (command "LINE" sr-pnti er-pnti "")
    (setq rient (list (entlast) er-pnti))
    (command "EXTEND" plent "" lient rient "")
    (setq rr (/ rr 2.0))                                ;œa¶…—a ˆáŸ¡Ÿi ¤e·a¡
  ) ;of WHILE

  (setvar "CECOLOR" "WHITE")                            ;¬‚·i Ó…¬‚·a¡

  (pop-env)                                             ;ÑÅ‰w¥e® ¥¢Šá

  (setq *error* oer seterr nil)
  (princ)

) ;of defun
