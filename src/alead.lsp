;--------------------------------
; Program : ALEAD
;           Arc lead
;           Yi Suk Jong
;           04/03/24
;--------------------------------
(defun c:ALEAD(
	       / ds ip cent cp sang eang r angip endang
	         endpnt angip90 intpnt tang lenarw
	         ip1 cp1 crspnt tang1 mirr
	       )

  (setq ds (getvar "DIMSCALE"))
	
  (setq ip (getpoint "\nPick first point: "))
  (push-os)(cecolor "1")
  (command "arc" ip pause pause )
  (setq cent (entget (entlast)))
  (command "arc" "" pause)
  (pop-os)(popcolor)
  
  (setq cp (cdr (assoc 10 cent)))
  (setq sang (cdr (assoc 50 cent)))
  (setq eang (cdr (assoc 51 cent)))
  (setq r (cdr (assoc 40 cent)))
  (setq angip (angle cp ip))
  (if (<= (abs (- angip sang)) (abs (- angip eang)))
    (setq endang eang)
    (setq endang sang)
  );if
  
  (setq endpnt (polar cp endang r))
  (setq angip90 (+ angip (* pi 0.5)))
  (setq intpnt (inters ip cp endpnt (polar endpnt angip90 100) nil))
  (setq tang (angle intpnt endpnt))
  (setq lenarw (* ds 2.8978))
  (setq ip1 (polar ip tang lenarw))
  (setq cp1 (polar cp tang lenarw))
  (setq crspnt (cross cent ip1 cp1))
  (setq tang1 (angle ip crspnt))
  (if (> (dang tang (angle ip endpnt)) 0) (setq mirr -1) (setq mirr 1))
  (push-os)
  (command "insert" (strcat (prefix) "blocks/arw1") ip ds  (* ds mirr) (rtod tang1))
  (pop-os)
  (princ "OK")
  
);defun

;****************************************    
; Function : CROSS    
;            CROSS point of arc & line    
;            By Suk-Jong Yi    
;            1995/6/26    
;****************************************    
;ÇÔÂÃ: È£¿Í Á÷¼±ÀÇ ½õÃñÃÊ Ã£±â    
;     ÀÎÂÃ: ARC entity list, Á÷¼±ÀÇ Ã¹ÃÊ , Á÷¼±ÀÇ ¾¾ÃÊ    
;     ½á°ú: Á÷¼±°ú ARCÀÇ ½õÃñÃÊ    
    
(defun CROSS(aent sp ep /    
aent    sp      ep      a       b       r       sa      ea      x1      x2    
y1      y2      c       d       a1      b1      c1      x1      x2      y1    
y2      ang1    ang2    
)    
    
(push-env)    
(setq a (car (cdr (assoc 10 aent))))      ; ARC entityÀÇ ÁßÂÔÃÊ xÁÂÅ²    
(setq b (cadr (cdr (assoc 10 aent))))     ; ARC entityÀÇ ÁßÂÔÃÊ yÁÂÅ²    
(setq r (cdr (assoc 40 aent)))            ; ARC entityÀÇ ¹ÝÁöÀó    
(setq sa (cdr (assoc 50 aent)))           ; ARC entityÀÇ ½ÃÀÛ °¢¿Ê    
(setq ea (cdr (assoc 51 aent)))           ; ARC entityÀÇ ¾¾ °¢¿Ê    
    
(setq x1 (car sp))                        ; LINE entityÀÇ ½ÃÀÛÃÊ xÁÂÅ²    
(setq x2 (car ep))                        ; LINE entityÀÇ ¾¾ÃÊ xÁÂÅ²    
(setq y1 (cadr sp))                       ; LINE entityÀÇ ½ÃÀÛÃÊ yÁÂÅ²    
(setq y2 (cadr ep))                       ; LINE entityÀÇ ¾¾ÃÊ yÁÂÅ²    
(if (= (- x1 x2) 0)    
  (progn                                    ;x°¡ constantÀÏ ¶§    
    (setq c x1    
          a1 1                              ;yÂî ¾õÇÑ 2Ãñ¹æÁ¤½ÄÀÇ a    
          b1 (* -2 b)                       ;yÂî ¾õÇÑ 2Ãñ¹æÁ¤½ÄÀÇ b    
          c1 (+ (* c c) (* -2 a c) (* a a) (* b b) (* -1 r r)) ;yÂî ¾õÇÑ 2Ãñ¹æÁ¤½ÄÀÇ c    
          y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;±Ù 1    
          y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;±Ù 2    
    );setq    
  );progn    
  (progn                                    ; y°¡ xÀÇ ÇÔÂÃÀÏ ¶§    
    
    (setq c (/ (- y1 y2) (- x1 x2)))          ; y=cx+dÂî¼­ c    
    (setq d (- y2 (* c x2)))                  ; y=cx+dÂî¼­ d    
    (setq a1 (+ 1 (* c c)))                   ; xÂî ¾õÇÑ ÀÌÃñ¹æÁ¤½ÄÀÇ a    
    (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))   ;xÂî ¾õÇÑ ÀÌÃñ¹æÁ¤½ÄÀÇ b    
    (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))  ;ÀÌÃñ ¹æÁ¤½ÄÀÇ c    
    (setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;±Ù 1    
    (setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;±Ù 2    
    (setq y1 (+ (* c x1) d))                  ;±Ù 1ÀÏ ¶§ y°ª    
    (setq y2 (+ (* c x2) d))                  ;±Ù 2ÀÏ ¶§ y°ª    
  );progn    
)
(setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))   ;½õÃÊ1ÀÇ Àý¾õ°¢(¿øÃÊÂî¼­)    
(setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))   ;½õÃÊ2ÀÇ Àý¾õ°¢(¿øÃÊÂî¼­)

;(command "line" sp ep "")  
;(command "line" (list a b 0.0) (list x1 y1 0.0) "")
;(command "line" (list a b 0.0) (list x2 y2 0.0) "")  
  
    
(if (inang sa ea ang1)    
  (list x1 y1 0.0)         ;½õÃÊ1ÀÌ È£ÀÇ ½ÃÀÛ°¢°ú ¾¾°¢ »çÀÌÂî ÀÖÀ¸Àý ½õÃÊ ¿Ì·ÁÁÜ    
  (if (inang sa ea ang2)   ;½õÃÊ2°¡ È£ÀÇ ½ÃÀÛ°¢°ú ¾¾°¢ »çÀÌÂî ÀÖÀ¸Àý ½õÃÊ ¿Ì·ÁÁÜ    
    (list x2 y2 0.0)    
    nil                    ;½õÃÊ 1°ú 2°¡ ¸ðµÎ °¢ ¹üÃ¯¸¦ ¹þ¾î³¯ °æ¿ì nil¿Ì·ÁÁÜ    
  ) ;of if    
)
);defun  


;************************************************************    
; Function : INANG    
;            a angle is IN the range of ANGle-1 and angle-2 ?    
;            By Suk-Jong Yi    
;            1995/6/26    
;*************************************************************    
    
;¾î¶² °¢ÀÌ ÁÖ¾îÁø µÎ°¢(ang1, ang2) »çÀÌ¿¡ ÀÖ´Â°¡?    
; µÎ°¢ »çÀÌ¿¡ ÀÖ´Â °æ¿ì µÎ°¢ÀÇ Â÷ÀÌ¸¦ µ¹·ÁÁÖ°í    
; µÎ°¢ »çÀÌ¿¡ ¾ø´Â °æ¿ì´Â nilÀ» µ¹·ÁÁØ´Ù.    
    
(defun inang(a1 a2 a3 /             ;ÀÎ¼ö Á¤ÀÇ    
a1 a2 a3                            ;Áö¿ªº¯¼ö Á¤ÀÇ    
)    
(if (> a1 a2)
  (progn
    (if (or (<= a3 a2) (>= a3 a1)) (- (+ (* 2.0 pi) a2) ) nil)   ;Ã¹°¢ÀÌ µÎ¹øÂ° °¢º¸´Ù Å©¸é +360µµ    , 4»çºÐ¸é --> 1»çºÐ¸é
  )
  (progn
    (if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;ÁÖ¾îÁø °¢ÀÌ µÎ°¢»çÀÌ¿¡ ÀÖÀ¸¸é    
                                nil)         ; µÎ°¢ÀÇ Â÷ÀÌ¸¦ µ¹·ÁÁÜ    
  );                                            ; µÎ°¢ »çÀÌ¿¡ ¾øÀ¸¸é nilµ¹·ÁÁÜ       
);if
);defun