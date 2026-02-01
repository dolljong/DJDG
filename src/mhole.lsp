;--------------------------------
; Program : MHOLE
;           Manhole
;           Yi Suk Jong
;           04/06/14
;--------------------------------
(defun c:mhole(
	       / ip ang h b th gap r p1 p2 p3 p4 pp1 pp2 pp3 pp4 pp5 pp6 pp7 pp8 bpl hpl
	       )
  (setq ip (getpoint "\nPick insert Point: "))
  (setq ang (getangle ip "\nAngle: "))

  (setq h (getint "\nHeight(mm): "))
  (setq b (getint "\nWidth(mm): "))
;  (setq r (getint "\nFillet Radius(mm): "))

  (setq th 10) ;thickness of plate
  (setq gap 20) ;
  (setq r 100) ; fillet radius
  
  (setq p1 (polar ip ang (+ gap (* 0.5 h))))  ;upper plate
  (setq p2 (polar ip (+ ang (* 0.5 pi)) (+ gap (* 0.5 b))))  ;left plate
  (setq p3 (polar ip (- ang (* 0.5 pi)) (+ gap (* 0.5 b))))  ;right plate 
  (setq p4 (polar ip (- ang pi) (+ gap (* 0.5 h))))          ;lower plate

  (setq pp1 (polar (polar ip ang (* 0.5 h)) (+ ang (* 0.5 pi)) (- (* 0.5 b)  r)))  ;upper left
  (setq pp8 (polar (polar ip ang (* 0.5 h)) (- ang (* 0.5 pi)) (- (* 0.5 b)  r)))  ;upper right
  
  (setq pp2 (polar (polar ip (+ ang (* 0.5 pi)) (* 0.5 b)) ang  (- (* 0.5 h) r)))  ;left upper
  (setq pp3 (polar (polar ip (+ ang (* 0.5 pi)) (* 0.5 b)) (+ ang pi) (- (* 0.5 h) r)))  ;left lower

  (setq pp4 (polar pp1 (+ ang pi) h))  ;lower left
  (setq pp5 (polar pp8 (+ ang pi) h))  ;lower right
  
  (setq pp6 (polar pp3 (- ang (* 0.5 pi)) b))  ;right lower
  (setq pp7 (polar pp2 (- ang (* 0.5 pi)) b))  ;rightleft lower

  (push-os)
  (command "pline" pp1 "a" "r" r pp2 "l" pp3
	               "a" "r" r pp4 "l" pp5
	               "a" "r" r pp6 "l" pp7
	               "a" "r" r pp8 "l" "c")
  (pop-os)

  (setq bpl (+ b (* 2 (+ gap th th))))   ;upper & lower plate 
  (setq hpl (+ h (* 2 gap)))  ;left & right plate
	
  (push-os)
  (djdg_rect p1 ang th bpl 0)  ;upper
  (djdg_rect p4 (+ ang pi) th bpl 0)  ;lower
  (djdg_rect p2 (+ ang (* 0.5 pi)) th hpl 0)  ;left
  (djdg_rect p3 (- ang (* 0.5 pi)) th hpl 0)  ;right
  (pop-os)
);defun  