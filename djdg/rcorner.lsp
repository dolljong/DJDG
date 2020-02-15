;**************************************
; program : RCORNER
;           Rebar for Corner
;           Yi Suk Jong
;           03/10/25
;**************************************

(defun c:RCORNER( /
 l1s  l2s  lds1  seld ldpnt rdist nlayer dist l1  l2  ld1  l1_s  l1_e  ang_l1
 l2_s  l2_e  ang_l2  ld1_s  ld1_e  lng_ld1 ld1_sdst ld1_edst spnt ang  bpnt ang90 
 index vpnt1 vpnt2 rpnt1 rpnt2
)

  (defun SETERR(s)                              ;���忡����ƾ ��
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

  (push-env)                            ;ȯ������


  (setq l1s (car (entsel "\nPick Rebar Line-1: ")))       ;�ܰ���1 ����
  (redraw l1s 3)
  (setq l2s (car (entsel "\nPick Rebar Line-2: ")))       ;�ܰ���2 ����
  (redraw l2s 3)
  (setq seld (entsel "\nPick diagonal Line: ")) 	;�缱 ����
  (setq lds1 (car seld)					; �缱 ������	
        ldpnt (cadr seld))    				;�缱 ������ ����
  (redraw lds1 3)
  (setq rdist (getreal "\nEnter Distance Ratio: "))     ;������ �Ÿ� ���� �ޱ�
  (setq nlayer (getint "\nEnter Number of Layer: "))    ;���� �ޱ�
  (setq dist  (getreal "\nEnter Distance: "))           ;���ݹޱ�
   
  (setq l1 (entget l1s)                            ;���õ� 3�� ���� ��������
        l2 (entget l2s)
        ld1 (entget lds1))

  (setq l1_s (cdr (assoc 10 l1))            ;2���� line�� ������ ���� ����
        l1_e (cdr (assoc 11 l1))
        

        l2_s (cdr (assoc 10 l2))
        l2_e (cdr (assoc 11 l2))
        

        ld1_s (cdr (assoc 10 ld1))          ;�缱1�� ������
        ld1_e (cdr (assoc 11 ld1))          ;�缱1�� ����
        lng_ld1 (distance ld1_s ld1_e)      ;�μ��� �Ÿ�
        ld1_sdst (distance ld1_s ldpnt)     ;���������� ���������� �Ÿ�
        ld1_edst (distance ld1_e ldpnt)
  );setq
  (if (< ld1_sdst ld1_edst)		;������ ����(�������� ��������� ���������� ����)		
    (setq spnt ld1_s ang (angle ld1_s ld1_e))     ;������ ���������� �������� ���ɹ���
    (setq spnt ld1_e ang (angle ld1_e ld1_s))
  );if
  (setq 	
	bpnt (polar spnt ang (* lng_ld1 rdist));������
;	ld1_m (mid-point ld1_s ld1_e)       ;�缱1�� �߰���
;        ang (angle ld1_s ld1_e)             ;�缱1�� ����
        ang90 (+ ang (* 0.5 pi))            ;�缱1�� ����+90��

;        ld2_s (cdr (assoc 10 ld2))                              ;�缱2�� ������
;        ld2_e (cdr (assoc 11 ld2))                              ;�缱2�� ����
;        ld2_m (inters ld2_s ld2_e ld1_m (polar ld1_m ang90 100) nil)
        ;(polar~)�� ������
  );setq
  (setq bpnt1 (polar bpnt (+ ang pi) (* 0.5 (1- nlayer) dist)))          ;ö����
  (setq index 0)
  (repeat nlayer
    (setq vpnt1 (polar bpnt1 ang (* index dist)) 		;������1  (ö�ٰ��� �������� ã������ ��)
	  vpnt2 (polar vpnt1 ang90 100)		;������2
          rpnt1 (inters vpnt1 vpnt2 l1_s l1_e nil) 	;�ܰ���1���� ����
	  rpnt2 (inters vpnt1 vpnt2 l2_s l2_e nil)    ;�ܰ���2���� ����
    );setq
    (command "LINE" rpnt1 rpnt2 "")
    (setq index (1+ index))
  );repeat  

  (redraw l1s 4)                               ;��������
  (redraw l2s 4)
  (redraw lds1 4)

  (pop-env)                                    ;ȯ�溹��

  (setq *error* oer seterr nil)
  (prin1)
) ;of defun                                                         ]
