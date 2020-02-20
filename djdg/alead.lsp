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
;����: ȣ�� ������ ������ ã��    
;     ����: ARC entity list, ������ ù�� , ������ ����    
;     ���: ������ ARC�� ������    
    
(defun CROSS(aent sp ep /    
aent    sp      ep      a       b       r       sa      ea      x1      x2    
y1      y2      c       d       a1      b1      c1      x1      x2      y1    
y2      ang1    ang2    
)    
    
(push-env)    
(setq a (car (cdr (assoc 10 aent))))      ; ARC entity�� ������ x��Ų    
(setq b (cadr (cdr (assoc 10 aent))))     ; ARC entity�� ������ y��Ų    
(setq r (cdr (assoc 40 aent)))            ; ARC entity�� ������    
(setq sa (cdr (assoc 50 aent)))           ; ARC entity�� ���� ����    
(setq ea (cdr (assoc 51 aent)))           ; ARC entity�� �� ����    
    
(setq x1 (car sp))                        ; LINE entity�� ������ x��Ų    
(setq x2 (car ep))                        ; LINE entity�� ���� x��Ų    
(setq y1 (cadr sp))                       ; LINE entity�� ������ y��Ų    
(setq y2 (cadr ep))                       ; LINE entity�� ���� y��Ų    
(if (= (- x1 x2) 0)    
  (progn                                    ;x�� constant�� ��    
    (setq c x1    
          a1 1                              ;y�� ���� 2��������� a    
          b1 (* -2 b)                       ;y�� ���� 2��������� b    
          c1 (+ (* c c) (* -2 a c) (* a a) (* b b) (* -1 r r)) ;y�� ���� 2��������� c    
          y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;�� 1    
          y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))  ;�� 2    
    );setq    
  );progn    
  (progn                                    ; y�� x�� ������ ��    
    
    (setq c (/ (- y1 y2) (- x1 x2)))          ; y=cx+d� c    
    (setq d (- y2 (* c x2)))                  ; y=cx+d� d    
    (setq a1 (+ 1 (* c c)))                   ; x�� ���� ����������� a    
    (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))   ;x�� ���� ����������� b    
    (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))  ;���� �������� c    
    (setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;�� 1    
    (setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;�� 2    
    (setq y1 (+ (* c x1) d))                  ;�� 1�� �� y��    
    (setq y2 (+ (* c x2) d))                  ;�� 2�� �� y��    
  );progn    
)
(setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))   ;����1�� ������(�����)    
(setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))   ;����2�� ������(�����)

;(command "line" sp ep "")  
;(command "line" (list a b 0.0) (list x1 y1 0.0) "")
;(command "line" (list a b 0.0) (list x2 y2 0.0) "")  
  
    
(if (inang sa ea ang1)    
  (list x1 y1 0.0)         ;����1�� ȣ�� ���۰��� ���� ������ ������ ���� �̷���    
  (if (inang sa ea ang2)   ;����2�� ȣ�� ���۰��� ���� ������ ������ ���� �̷���    
    (list x2 y2 0.0)    
    nil                    ;���� 1�� 2�� ��� �� ��ï�� ��� ��� nil�̷���    
  ) ;of if    
)
);defun  


;************************************************************    
; Function : INANG    
;            a angle is IN the range of ANGle-1 and angle-2 ?    
;            By Suk-Jong Yi    
;            1995/6/26    
;*************************************************************    
    
;� ���� �־��� �ΰ�(ang1, ang2) ���̿� �ִ°�?    
; �ΰ� ���̿� �ִ� ��� �ΰ��� ���̸� �����ְ�    
; �ΰ� ���̿� ���� ���� nil�� �����ش�.    
    
(defun inang(a1 a2 a3 /             ;�μ� ����    
a1 a2 a3                            ;�������� ����    
)    
(if (> a1 a2)
  (progn
    (if (or (<= a3 a2) (>= a3 a1)) (- (+ (* 2.0 pi) a2) ) nil)   ;ù���� �ι�° ������ ũ�� +360��    , 4��и� --> 1��и�
  )
  (progn
    (if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;�־��� ���� �ΰ����̿� ������    
                                nil)         ; �ΰ��� ���̸� ������    
  );                                            ; �ΰ� ���̿� ������ nil������       
);if
);defun