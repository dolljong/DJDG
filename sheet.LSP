; Program : sheet
; function : djdg_drawsheet

;---------------------------------
; program : sheet (sheet pile)
;           yi suk jong
;           05/08/13
;---------------------------------
(defun c:sheet(
	       / shsizediv h b p1 ent enty plst p2 nline i  )
  (if (not #sheet_shsize) (setq #sheet_shsize "400*150"))
  (setq #sheet_shsize (getstringold #sheet_shsize "\n쉬트파일 규격 : "))
  (setq shsizediv (divide_str #sheet_shsize "*"))
  (setq h (atof (car shsizediv))
	b (atof (cadr shsizediv)))
  (setq p1 (getpoint "\n쉬트파일을 설치할 첫점을 찍으세요<Enter=Entity선택>: "))
  ;-- 엔터를 쳤을대는 물체를 선택하도록...
  (if (not p1)
    (progn			; entity선택
      (setq ent (entget (car (entsel "\n쉬트파일을 설치할 Entity를 선택하세요: "))))
      (setq enty (cdr (assoc 0 ent)))   ;entity type
      (cond
	((= enty "LINE")		; line인 경우
	  (setq plst (list (cdr (assoc 10 ent)) (cdr (assoc 11 ent))))
	);sub cond
	((= enty "LWPOLYLINE")		;pline인 경우
	  (setq plst (car (mk_vertlist (cdr (assoc -1 ent)))))
	);sub cond
      );cons	
    );progn
    ;-- 점을 찍었을 때는 둘재점 입력받기
    (progn				; 둘째점 점찍기
      (setq p2 (getpoint p1 "\n둘째점을 찍으세요: "))
      (setq plst (list p1 p2))
    );progn  
  );if
  (setq nline (1- (length plst)))  	;number of line
  (setq i 0)  
  (repeat nline
    (djdg_drawsheet (nth i plst) (nth (1+ i) plst) 400 150)
    (setq i (1+ i))
  );repeat  
);defun

;----------------------------------
; function : djdg_drawsheet
;            sheet pile그려주기 두점사이에 sheet pile을 그려준다.
;            Yi Suk Jong
;            05/08/13
;----------------------------------
(defun djdg_drawsheet( p1 p2 b h / ang sign ip p1 p2 )
  ;-----------------------------
  ; function : djdg_drawasheet
  ;            sheet pile한장 그리기
  ;            Yi Suk Jong
  ;-----------------------------
  (defun djdg_drawasheet(sp ang1 bb hh side / p1 p2 p3 dang deltab )
    ; sp : side point
    ; ang : angle
    ; bb : shett파일 폭
    ; hh : sheet파일 두께
    ; side : 좌측(1)/우측(-1)
    (setq dang 15.0)   ;쉬트파일 각도 = 15도
    (setq deltab (* (/ (sin (dtor dang)) (cos (dtor dang))) hh))  		;tan 15 * h
    (setq p1 (polar (polar sp ang1 deltab) (+ ang1 (* side 0.5 pi)) hh ))  	;첫째점
    (setq p2 (polar (polar sp ang1 (- bb deltab)) (+ ang1 (* side 0.5 pi)) hh)) 	;둘째점
    (setq p3 (polar sp ang1 bb))    						;끝점.
    (push-os)
    (command "pline" sp p1 p2 p3 "")
    (pop-os)
   );defun
  
  (setq ang (angle p1 p2))        ;방향각도
  (setq sign -1)		;부호
  (setq ip p1) ;시작점
  (while (>= (distance ip p2) b)  ;남은 거리가 폭보다 작으면 반복
    (setq sign (* sign -1))		;부호바꾸기
    (djdg_drawasheet ip ang b h sign)   ;쉬트파일 한개그리기 함수 호출
    (setq ip (polar ip ang b))		;새로운 삽입점
  );while
);defun  