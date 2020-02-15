;*************************************    
;     Program : BM5    
;               reBar Marking 5    
;               By Suk-Jong Yi    
;               96/5/7    
;*************************************    
; ö�� ��ŷ�� �Ʒ��� ����� �׷���.
;     /--/--/--��    
;*************************************    
(defun C:BM5(/                                              ;�������� ����    
        oldclr      index       nnsent      entl        entype    
        crsxy       spexy       epexy       crsxy       slp         elp    
        sp          ep          sslst       enum        index       enum    
)    
    
  (defun SETERR(s)                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
(push-env)    
    
(setvar "CMDECHO" 0)                        ;ȯ�溯�� ���� ���ɸ޾Ƹ� ����    
(setvar "BLIPMODE" 0)                       ;BLIP MODE ����    
(setq ds (getvar "DIMSCALE")                ;������    
      th (getvar "DIMTXT"))                 ;textũ��    
    
;(setq rc (* ds 3.5))                        ;��ŷ���� ������    
;(setq th (* ds 2.5))                        ;��ŷ���� ������    
(setq rc (* ds 4.5))                        ;��ŷ���� ������    
(setq th (* ds th))                         ;textũ��    
    
(princ "\nSelect objects: ")                ;��ŷ ��� ��ƼƼ ����    
(setq sslst (ssget))    
    
(setq enum (sslength sslst))                ;seclection set�� ��ƼƼ ����    
    
    
;**** ��� ��ƼƼ ����, ���ƼƼ ���� �ľ� (LINE�� ARC��)    
(setq index 0                               ;��ƼƼ ����    
      nssent 0)                             ;line�̰ų� arc�� ��ƼƼ ����    
    
(repeat enum                  ;��ƼƼ ���� ��ŭ �ݺ�    
  (setq entl (entget (ssname sslst index)))      ;��ƼƼ ����Ʈ    
  (setq entype (cdr (assoc 0 entl)))             ;��ƼƼ Ÿ��    
  (if (or (= entype "LINE") (= entype "ARC"))    ;��ƼƼ�� line�̰ų� arc�ΰ��    
    (progn    
      (redraw (ssname sslst index) 3)            ;��ƼƼ ����    
      (if (= nssent 0) (setq ssent (ssadd (ssname sslst index)))    
                       (setq ssent (ssadd (ssname sslst index) ssent))    
      ) ;of if                    ; line�̰ų� arc��ƼƼ���� slection set �����    
      (setq nssent (1+ nssent))                     ;��� ��ƼƼ ���� count up    
    ) ;of progn    
  ) ; of if    
  (setq index (1+ index))                           ;���� ��ƼƼ��    
) ;of repeat    
(print nssent)    
(princ "Entity Found")                              ;LINE�̳� ARC�� ��ΰ�?    
    
(setq sp (getpoint "\nPick start point: "))         ;�������� ù��    
(setq ep (getpoint sp "\nPick end point: "))        ;�������� ����    
(setq seang (angle sp ep))                          ; �������� ��  
(cecolor "red")    
(command "LINE" sp ep "")                           ;������ �ױ�  
(popcolor)    
    
(setq index 0)    
(repeat nssent                                  ;ARC�̰ų� LINE�� ��ƼƼ��ŭ    
   (setq entl (entget (ssname ssent index)))    ;��ƼƼ ����Ʈ ���ϱ�    
   (cond    
     ((= (cdr (assoc 0 entl)) "ARC")            ;��ŷ ����� ARC�� ���    
       (setq crsxy (cross entl sp ep))            ;ARC�� �������� ������ã��    
     ) ;of entity=ARC    
     ((= (cdr (assoc 0 entl)) "LINE")           ; ��ŷ ����� LINE�� ���    
       (setq spexy (cdr (assoc 10 entl))          ;�������� ���۰� ����    
             epexy (cdr (assoc 11 entl)))    
       (setq crsxy (inters spexy epexy sp ep))    ;�������� LINE�� ������ ã��    
     ) ;of entity=LINE    
   ) ;of cond    
   (setq slp (polar crsxy (+ seang (* pi 0.25)) (* 1.3 ds)))    
   (setq elp (polar crsxy (+ seang (* pi 1.25)) (* 1.3 ds)))
   (cecolor "red")
   (command "LINE" slp elp "")                  ; Tick line�׸��� /
   (popcolor)
   (redraw (ssname ssent index) 4)              ; ������ ��ƼƼ ���󺹱�    
   (setq index (1+ index))                      ; ���� ��ƼƼ��    
) ; repeat    
    
  (setq cp (polar ep seang rc))                   ;��ŷ�� �߽�    
  (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 4 ds)) 0.0))    
    
  (cecolor "red")
  (command "CIRCLE" cp rc)    
  (popcolor)
    
  (setq mk (getstring "\nEnter Marking: "))    
;  (command "TEXT" "J" "M" cp th "0" (strcase mk))    
  (txtinc mk cp 0.0)    
  (setq dia (getstring "\nEnter Rebar Dia: "))
  (cecolor "bylayer")
  (command "TEXT" diaxy th "" (strcase dia))    
  (popcolor)
    
  (pop-env)    
    
  (setq *error* oer seterr nil)    
  (princ)    
) ; defun    
    
    
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
    
(if (inang sa ea ang1)    
  (list x1 y1 0.0)         ;����1�� ȣ�� ���۰��� ���� ������ ������ ���� �̷���    
  (if (inang sa ea ang2)   ;����2�� ȣ�� ���۰��� ���� ������ ������ ���� �̷���    
    (list x2 y2 0.0)    
    nil                    ;���� 1�� 2�� ��� �� ��ï�� ��� ��� nil�̷���    
  ) ;of if    
) ;of if    
)    
    
    
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
(if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))   ;ù���� �ι�° ������ ũ�� +360��    
(if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;�־��� ���� �ΰ����̿� ������    
                                nil)         ; �ΰ��� ���̸� ������    
)                                            ; �ΰ� ���̿� ������ nil������    
    
    
;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; �� ���� ö�ٹ�ȣ�� �������ش�.
; �Ѿ���� ��
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ds)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))               ;text�ݱ�=ļ���ݱ�

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
	(progn
	  (cecolor "bylayer")
          (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
	  (popcolor)
	);progn  
        (progn
	  (cecolor "bylayer")
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
	  (popcolor)
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (progn
      (cecolor "bylayer")
      (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
      (popcolor)
    );progn
  ) ;of IF
) ;of DEFUN