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
  (setq #sheet_shsize (getstringold #sheet_shsize "\n��Ʈ���� �԰� : "))
  (setq shsizediv (divide_str #sheet_shsize "*"))
  (setq h (atof (car shsizediv))
	b (atof (cadr shsizediv)))
  (setq p1 (getpoint "\n��Ʈ������ ��ġ�� ù���� ��������<Enter=Entity����>: "))
  ;-- ���͸� ������� ��ü�� �����ϵ���...
  (if (not p1)
    (progn			; entity����
      (setq ent (entget (car (entsel "\n��Ʈ������ ��ġ�� Entity�� �����ϼ���: "))))
      (setq enty (cdr (assoc 0 ent)))   ;entity type
      (cond
	((= enty "LINE")		; line�� ���
	  (setq plst (list (cdr (assoc 10 ent)) (cdr (assoc 11 ent))))
	);sub cond
	((= enty "LWPOLYLINE")		;pline�� ���
	  (setq plst (car (mk_vertlist (cdr (assoc -1 ent)))))
	);sub cond
      );cons	
    );progn
    ;-- ���� ����� ���� ������ �Է¹ޱ�
    (progn				; ��°�� �����
      (setq p2 (getpoint p1 "\n��°���� ��������: "))
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
;            sheet pile�׷��ֱ� �������̿� sheet pile�� �׷��ش�.
;            Yi Suk Jong
;            05/08/13
;----------------------------------
(defun djdg_drawsheet( p1 p2 b h / ang sign ip p1 p2 )
  ;-----------------------------
  ; function : djdg_drawasheet
  ;            sheet pile���� �׸���
  ;            Yi Suk Jong
  ;-----------------------------
  (defun djdg_drawasheet(sp ang1 bb hh side / p1 p2 p3 dang deltab )
    ; sp : side point
    ; ang : angle
    ; bb : shett���� ��
    ; hh : sheet���� �β�
    ; side : ����(1)/����(-1)
    (setq dang 15.0)   ;��Ʈ���� ���� = 15��
    (setq deltab (* (/ (sin (dtor dang)) (cos (dtor dang))) hh))  		;tan 15 * h
    (setq p1 (polar (polar sp ang1 deltab) (+ ang1 (* side 0.5 pi)) hh ))  	;ù°��
    (setq p2 (polar (polar sp ang1 (- bb deltab)) (+ ang1 (* side 0.5 pi)) hh)) 	;��°��
    (setq p3 (polar sp ang1 bb))    						;����.
    (push-os)
    (command "pline" sp p1 p2 p3 "")
    (pop-os)
   );defun
  
  (setq ang (angle p1 p2))        ;���Ⱒ��
  (setq sign -1)		;��ȣ
  (setq ip p1) ;������
  (while (>= (distance ip p2) b)  ;���� �Ÿ��� ������ ������ �ݺ�
    (setq sign (* sign -1))		;��ȣ�ٲٱ�
    (djdg_drawasheet ip ang b h sign)   ;��Ʈ���� �Ѱ��׸��� �Լ� ȣ��
    (setq ip (polar ip ang b))		;���ο� ������
  );while
);defun  