;****************************************
; Program : Cutpipe1
;           Cutpipe
;           By Suk-Jong Yi
;           00/4/25
;****************************************
;�������� ������ ������� �߶��.  (no 3d)
;----------------------------------------

(defun C:cutpipe1(
   /
   p1 p2 p3 len ang1 ang2 a2sgn p40 p5 p60 pi2 len4 p4 p6 p7
   arc1 cen1 cen11 r sang1 arc2 cen2 cen22 sang2 angarc index
)

  (defun SETERR(s)                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  
  (setq oer *error* *error* seterr)         ;���忡����ƾ ����

  (push-env)                                ;ȯ�溯�� ����

  (setq p1 (getpoint  "\nPick First point: ")          ;���� ������
        p2 (getpoint p1 "\nPick Second point: ")       ;���� ����
        p3 (getpoint "\nPick Side: "))                 ;������

  (setq len (distance p1 p2)                           ;���ܱ���
        ang1 (angle p1 p2)                             ;���ܼ� ����
        ang2 (angle p1 p3))                            ;���ܹ��� ����
	 
  (if (minusp (dang ang1 ang2)) (setq a2sgn -1) (setq a2sgn 1))  ;���ܼ������� ���ܹ��Ⱒ���� ����

  (setq p40 (polar p1 ang1 (* len 0.25))               ;���ܱ����� 1/4��ġ
        p5 (polar p1 ang1 (* len 0.5))                 ;���ܱ����� 1/2��ġ
        p60 (polar p1 ang1 (* len 0.75)))              ;���ܱ����� 3/4��ġ

  (setq pi2 (/ pi 2.0)                                 ;90��
        len4 (* len 0.1))                              ;���ܱ����� 1/10

  (setq p4 (polar p40 (+ ang1 (* a2sgn pi2)) len4)     ;ȣ�� �׸������� ������
	p6 (polar p60 (- ang1 (* a2sgn pi2)) len4)
	p7 (polar p60 (+ ang1 (* a2sgn pi2)) len4))

  (command "arc" p1 p4 p5)
  (setq arc1 (entlast)                                 ;ùȣ�� entity name
        cen1 (cdr (assoc 10 (entget  arc1)))           ;ùȣ�� �߽���
        cen11 (polar cen1 (+ ang1 (* a2sgn pi2)) len4) ;ùȣ trim line�� center
        r (cdr (assoc 40 (entget (entlast))))          ;������
        sang1 (angle cen1 p1))                         ;���۰���
  
  (command "arc" p5 p6 p2)
  (setq arc2 (entlast)                                 ;��°ȣ
        cen2 (cdr (assoc 10 (entget  arc2)))
	cen22 (polar cen2 (+ ang1 (* a2sgn pi2)) len4)
	sang2 (angle cen2 p5))
  
;  (setq cen2 (cdr (assoc 10 (entget  (entlast)))))
  ;(command "pline" p1 "a" "s" p4 p5 "s" p6 p2 "")

;  (command "pline")
      
  (setq angarc (* 2 (asin (/ len 4.0 r))))              ;ȣ�� ����
  
  (command "TRIM" arc1 arc2 "" "F")                     ;trim fance
;  (command (polar cen11 (+ sang1 (* a2sgn (/ angarc 12) index)) r))
  (setq index -1)  
  (repeat 7
    (command (polar cen11 (- sang1 (* a2sgn (/ angarc 6) index)) r))
    (setq index (1+ index))
  );repeat

  (setq index 1)                        ;ȣ�� 6��� �� ������ �����
  (repeat 7
    (command (polar cen22 (+ sang2 (* a2sgn (/ angarc 6) index)) r))
    (setq index (1+ index))
  );repeat
  
  (command "" "")
  
;  (command "arc" p2 p7 p5)
  
) ;of defun

;------------------------------------------
; function : asin
;            arc sin
;            Yi Suk Jong
;            2000/4/25
;------------------------------------------
; asin(a) : arcsin�� return(radian)
;------------------------------------------
(defun asin(a / a)
  (cond
    ((= a 1) (/ pi 2.0))
    ((= a -1) (/ pi -2.0))
    (T (atan (/ a (sqrt (- 1 (* a a))))))
  );cond  
);defun  