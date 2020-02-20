;********************************
;   Program : B1
;             Bar marking-1
;             By Jong-Suk Yi
;             95/3/2, 96/5/7, 96/9/14
;******************************************************
; ö�ٸ�ŷ�� �Ʒ��� ����� ������ش�. (���ΰ��� ����)
;       ��____
;       |
;       |
;******************************************************

(defun C:B1(/
rc sp ep ang dst cp
) ;variable

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)

  (setq th (getvar "DIMTXT"))
  (setq ds (getvar "DIMSCALE"))

  (setq rc (* ds 5.0)
	diaxg  6 )                  ;������

  (setq sp (getpoint "\nPick start point: "))               ;������
  (setq ep (getpoint sp "\nPick end point: "))              ;����

;  (setq oldclr (getvar "CECOLOR"))
;  (setvar "CECOLOR" "1")
  (cecolor "1")
  (command "LINE" sp ep "")                                 ;���ü�
  (popcolor)
  
  (setq p3 (getpoint ep "\nPick base line: "))              ;base line point

  (setq dx (- (car p3) (car ep)))
  (if (< dx 0)                                          ;base line�� x�����ν�
    (setq xsgn -1)
    (setq xsgn 1)
  ) ;of if

  (setq dy (- (cadr p3) (cadr ep)))
  (if (<  dy 0)                                         ;base line�� y�����ν�
    (setq ysgn -1)
    (setq ysgn  1)
  ) ;of if

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line�� ����

  (if (> (abs dx) (abs dy))                                             ;�����ֳ� ���ֳ�?
    (progn
      (setq p4 (list (+ (car ep) (* blen xsgn)) (cadr ep)))
      (cecolor "1")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dx 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (+ (car ip) rc)
                     (+ (cadr ip) rc)))
      (setq diaxy (list (+ (car cp) (* diaxg ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 0)                                   ;textȸ����
    ) ;of progn
    (progn
      (setq p4 (list (car ep) (+ (cadr ep) (* blen ysgn))))
      (cecolor "1")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dy 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (- (car ip) rc)
                     (+ (cadr ip) rc)))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* diaxg ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 90)
    ) ;of progn
  ) ;of if

  (cecolor "1")
  (command "CIRCLE" cp rc)
  (popcolor)

  (setvar "CECOLOR" "7")
  (setq mk (getstring "\nEnter Marking: "))
  (txtinc mk cp txtrot)
  (setq dia (getstring "\nEnter Rebar Dia: "))
;  (command "TEXT" diaxy (* 2.5 ds) txtrot (strcase dia))
  (cecolor "7")
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (popcolor)

  (setq *error* oer seterr nil)
  (princ)
) ;of defun

;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; �� �ȿ� ö�ٹ�ȣ�� �������ش�.
; �Ѿ���� ��
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th)

  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))               ;textũ��=ġ��ũ��

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
	(progn
	  (cecolor "7")
          (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
	  (popcolor)
	);progn  
        (progn
	  (cecolor "7")
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
	  (popcolor)
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (progn
      (cecolor "7")
      (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
      (popcolor)
    )
  ) ;of IF
) ;of DEFUN
