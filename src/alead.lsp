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
  ;(setq ip1 (polar ip tang lenarw))
  ;(setq cp1 (polar cp tang lenarw))
  ;(setq crspnt (cross cent ip1 cp1))
  (setq crspnt (crossac cent ip lenarw))
  (if (null crspnt)
    (princ "\n리더 선이 너무 짧습니다.\n")
    (progn
      (setq tang1 (angle ip crspnt))
      (if (> (dang tang (angle ip endpnt)) 0) (setq mirr -1) (setq mirr 1))
      (push-os)
      (command "insert" (strcat (prefix) "blocks/arw1") ip ds  (* ds mirr) (rtod tang1))
      (pop-os)
      (princ "OK")
    )
  )
  
);defun

;****************************************
; Function : CROSSAC
;            CROSS point of Arc & Circle
;            두 원의 교점을 구한 후 호 범위 내의 점을 반환
;   인수: aent - ARC entity list
;         cp2  - 원의 중심점
;         r2   - 원의 반지름
;   리턴: 호 위의 교점 또는 nil
;****************************************

(defun CROSSAC(aent cp2 r2 /
  a b r sa ea px py
  AA BB CC m n
  a1 b1 c1 disc
  cx1 cx2 cy1 cy2
  ang1 ang2
)

  ; ARC 정보 추출
  (setq a  (car (cdr (assoc 10 aent))))
  (setq b  (cadr (cdr (assoc 10 aent))))
  (setq r  (cdr (assoc 40 aent)))
  (setq sa (cdr (assoc 50 aent)))
  (setq ea (cdr (assoc 51 aent)))

  ; 원의 중심
  (setq px (car cp2))
  (setq py (cadr cp2))

  ; 두 원의 방정식을 빼면 직선(근축)이 됨: AA*x + BB*y = CC
  (setq AA (* 2 (- px a)))
  (setq BB (* 2 (- py b)))
  (setq CC (+ (* px px) (* py py) (* r r)
              (- (* r2 r2)) (- (* a a)) (- (* b b))))

  (if (= BB 0)
    ;; BB=0: 근축이 수직선 → x = CC/AA
    (progn
      (setq cx1 (/ CC AA)
            cx2 (/ CC AA)
            a1  1
            b1  (* -2 b)
            c1  (+ (* cx1 cx1) (* -2 a cx1) (* a a) (* b b) (- (* r r)))
            disc (- (* b1 b1) (* 4 a1 c1))
      )
      (if (>= disc 0)
        (setq cy1 (/ (+ (* -1 b1) (sqrt disc)) (* 2 a1))
              cy2 (/ (- (* -1 b1) (sqrt disc)) (* 2 a1))
        )
      )
    )
    ;; 일반: y = m*x + n  (m = -AA/BB, n = CC/BB)
    (progn
      (setq m (/ (- AA) BB))
      (setq n (/ CC BB))
      (setq a1 (+ 1 (* m m)))
      (setq b1 (+ (* -2 a) (* 2 m (- n b))))
      (setq c1 (+ (* a a) (* (- n b) (- n b)) (- (* r r))))
      (setq disc (- (* b1 b1) (* 4 a1 c1)))
      (if (>= disc 0)
        (progn
          (setq cx1 (/ (+ (* -1 b1) (sqrt disc)) (* 2 a1)))
          (setq cx2 (/ (- (* -1 b1) (sqrt disc)) (* 2 a1)))
          (setq cy1 (+ (* m cx1) n))
          (setq cy2 (+ (* m cx2) n))
        )
      )
    )
  )

  ; 교점이 호의 각도 범위 안에 있는지 확인
  (if cx1
    (progn
      (setq ang1 (angle (list a b 0.0) (list cx1 cy1 0.0)))
      (setq ang2 (angle (list a b 0.0) (list cx2 cy2 0.0)))
      (if (inang sa ea ang1)
        (list cx1 cy1 0.0)
        (if (inang sa ea ang2)
          (list cx2 cy2 0.0)
          nil
        )
      )
    )
    nil
  )
);defun

;****************************************
; Function : CROSS
;            CROSS point of arc & line
;            By Suk-Jong Yi    
;            1995/6/26    
;****************************************    
;기능: 호와 직선의 교점    
;     인자: ARC entity list, 직선의 시작점 , 직선의 끝점   
;     반환: 직선과 ARC의 교점    
    
(defun CROSS(aent sp ep /    
aent    sp      ep      a       b       r       sa      ea      x1      x2    
y1      y2      c       d       a1      b1      c1      cx1     cx2     cy1
cy2     ang1    ang2    disc
)    
    
(push-env)    
(setq a (car (cdr (assoc 10 aent))))       
(setq b (cadr (cdr (assoc 10 aent))))        
(setq r (cdr (assoc 40 aent)))            
(setq sa (cdr (assoc 50 aent)))           
(setq ea (cdr (assoc 51 aent)))           
    
(setq x1 (car sp))                           
(setq x2 (car ep))                        
(setq y1 (cadr sp))                          
(setq y2 (cadr ep))                       
(if (= (- x1 x2) 0)    
  (progn                                   
    (setq c x1
          a1 1                             
          b1 (* -2 b)                       
          c1 (+ (* c c) (* -2 a c) (* a a) (* b b) (* -1 r r)) 
          disc (- (* b1 b1) (* 4 a1 c1))   ;판별식
    );setq
    (if (>= disc 0)
      (setq cx1 c
            cx2 c
            cy1 (/ (+ (* -1 b1) (sqrt disc)) (* 2 a1))  ;근 1
            cy2 (/ (- (* -1 b1) (sqrt disc)) (* 2 a1))  ;근 2
      )
    )
  );progn    
  (progn                                   
    
    (setq c (/ (- y1 y2) (- x1 x2)))
    (setq d (- y2 (* c x2)))
    (setq a1 (+ 1 (* c c)))
    (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))
    (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))
    (setq disc (- (* b1 b1) (* 4 a1 c1)))
    (if (>= disc 0)
      (progn
        (setq cx1 (/ (+ (* -1 b1) (sqrt disc)) (* 2 a1)))
        (setq cx2 (/ (- (* -1 b1) (sqrt disc)) (* 2 a1)))
        (setq cy1 (+ (* c cx1) d))
        (setq cy2 (+ (* c cx2) d))
      )
    )
  );progn
)
(pop-env)
(if cx1
  (progn
    (setq ang1 (angle (list a b 0.0) (list cx1 cy1 0.0)))
    (setq ang2 (angle (list a b 0.0) (list cx2 cy2 0.0)))
    (if (inang sa ea ang1)
      (list cx1 cy1 0.0)
      (if (inang sa ea ang2)
        (list cx2 cy2 0.0)
        nil
      )
    )
  )
  nil
)
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
    
(defun inang(a1 a2 a3 /               
a1 a2 a3                              
)    
(if (> a1 a2)
  (progn
    (if (or (<= a3 a2) (>= a3 a1)) (- (+ (* 2.0 pi) a2) ) nil)  ;첫각이 두번째 각보다 크면 +360도    , 4사분면 --> 1사분면 
  )
  (progn
    (if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)      ;주어진 각이 두각사이에 있으면  
                                nil)           ; 두각의 차이를 돌려줌
  );                                              ; 두각 사이에 없으면 nil돌려줌     
);if
);defun