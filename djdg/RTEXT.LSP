;*************************************
; Program : RTEXT
;           Round TEXT
;           By Suk-Jong Yi
;           1995/7/1
;*************************************
; ���� ���ڸ� �迭

(defun C:RTEXT(/

)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq cpnt (getpoint "\nPick center point: "))          ; ���� �߽���
(setq ipnt (getpoint cpnt "\nPick insert point: "))     ; ������
(setq th (getreal "\n Text height: "))                  ; text ����
(setq txt (getstring T "\nTEXT: "))                       ; text�Է�
(setq r (distance cpnt ipnt))                           ; ���߽�~������ �Ÿ�

(setq ang (angle cpnt ipnt))                               ; �ؽ�Ʈ��ġ�� ���밢
(setq tn (strlen txt))                                  ; ���� ����
(setq tw (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))) ; ������
(setq ta (* (/ (* th tw) (* 2.0 pi r)) 2.0 pi))         ;�ѱ��ڰ� �����ϴ� ����
(setq tta (* tn ta))                                    ; ��ü������ ����



(if (> ang pi) (setq sgn 1 tb 3) (setq sgn -1 tb 0))              ; ���� ���� ����
(setq sta (+ ang (/ tta -2.0 sgn) (* 0.5 ta sgn)))

(setq header (list '(0 . "BLOCK") '(2 . "*JONG") '(70 . 1) (cons 10 ipnt)))
(entmake header)    ;�������������ϱ�

(setq count 1)
(repeat tn
  (setq t1a (+ sta (* (1- count) ta sgn)))              ;�� ���� ���밢
  (setq ra (+ t1a (* pi 0.5 sgn)))                      ;������ ȸ������
  (setq ip (polar cpnt t1a r))                          ;������ ��ġ
  (setq txt1 (substr txt count 1))                      ;���� �ϳ� ����
                                                ;*** text ��ƼƼ �����
  (setq tent (list '(0 . "TEXT")                  ;��ƼƼ��
                    (cons 1 txt1)                 ;�ؽ�Ʈ��
                    (cons 7 (getvar "TEXTSTYLE")) ;�ؽ�Ʈ ��Ÿ��
                    (cons 10 ip)                  ;������
                    (cons 11 ip)                  ;������
                    (cons 40 th)                  ;�ؽ�Ʈ ����
                    (cons 50 ra)                  ;ȸ������
                    (cons 72 1)                   ;�߾�����
                    (cons 73 tb)))                ;���, �ϴ�
  (setq subent (entmake tent))                    ;block table�� ���� ���
;  (princ subent)
;  (command "TEXT" ip th (rtod ra) txt1)
  (setq count (1+ count))
) ;of repeat

(setq blknm (entmake '((0 . "ENDBLK"))))

(entmake (list '(0 . "INSERT") (cons 2 blknm) (cons 10 ipnt)))
;(princ blknm)
  (setq *error* oer seterr nil)
(prin1)
) ;of defun
