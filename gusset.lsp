;**************************************
; program : GUSSET
;           GUSSET plate
;           Yi Suk Jong
;           97/6/11
;**************************************

(defun c:GUSSET( /
 l1s  l2s  lds1  lds2  l1  l2  ld1  ld2  l1_s  l1_e  ang_l1
 l2_s  l2_e  ang_l2  ld1_s  ld1_e  ld1_m  ang  ang90  ld2_s  ld2_e  ld2_m
 d90  l1i_s  l1i_e  l2i_s  l2i_e  c_l1_ld1  c_l1_ld2  c_l2_ld1  c_l2_ld2
 mindst  min_p  dd  ll  le_1  le_2  lee_1  lee_2  b_1  b_2  g_1  g_2
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

  (if (= gap nil) (setq gap 20))
  (princ "\nGap(mm)<") (princ gap)
  (setq gap0 (getreal ">: "))            ;������ ���� �Է� (�ּ� 20mm)
  (if (/= gap0 nil) (setq gap gap0))

  (if (= lbolt nil)
    (setq lbolt (getreal "Length of bolting range: "))
    (progn
      (princ "\nLength of bolting range<") (princ lbolt)
      (setq lbolt0 (getreal ">: "))                      ;��Ʈ�������� �Է�
      (if (/= lbolt0 nil) (setq lbolt lbolt0))
    ) ;of progn
  ) ;of if

  (setq l1s (car (entsel "\nPick line-1: ")))       ;�ܰ���1 ����
  (redraw l1s 3)
  (setq l2s (car (entsel "\nPick line-2: ")))       ;�ܰ���2 ����
  (redraw l2s 3)
  (setq lds1 (car (entsel "\nPick diagonal: ")))    ;�缱1 ����
  (redraw lds1 3)
  (setq lds2 (car (entsel "\nPick diagonal: ")))    ;�缱2 ����
  (redraw lds2 3)

  (setq l1 (entget l1s)                            ;���õ� 4�� ���� ��������
        l2 (entget l2s)
        ld1 (entget lds1)
        ld2 (entget lds2))

  (setq l1_s (cdr (assoc 10 l1))            ;2���� line�� ������ ���� ����
        l1_e (cdr (assoc 11 l1))
        ang_l1 (angle l1_s l1_e)

        l2_s (cdr (assoc 10 l2))
        l2_e (cdr (assoc 11 l2))
        ang_l2 (angle l2_s l2_e)

        ld1_s (cdr (assoc 10 ld1))          ;�缱1�� ������
        ld1_e (cdr (assoc 11 ld1))          ;�缱1�� ����
        ld1_m (mid-point ld1_s ld1_e)       ;�缱1�� �߰���
        ang (angle ld1_s ld1_e)             ;�缱1�� ����
        ang90 (+ ang (* 0.5 pi))            ;�缱1�� ����+90��

        ld2_s (cdr (assoc 10 ld2))                              ;�缱2�� ������
        ld2_e (cdr (assoc 11 ld2))                              ;�缱2�� ����
        ld2_m (inters ld2_s ld2_e ld1_m (polar ld1_m ang90 100) nil)
        ;(polar~)�� ������
  )
  (if (minusp (dang ang_l1 (angle l1_s ld1_m)))   ;gap��ŭ �������� ���� ��
    (setq d90 (* -0.5 pi))
    (setq d90 (*  0.5 pi))
  )
  (setq l1i_s (polar l1_s (+ ang_l1 d90) gap)
        l1i_e (polar l1_e (+ ang_l1 d90) gap))
;  (command "LINE" l1i_s l1i_e "")

  (if (minusp (dang ang_l2 (angle l2_s ld1_m)))   ;gap��ŭ �������� ���� ��
    (setq d90 (* -0.5 pi))
    (setq d90 (*  0.5 pi))
  )
  (setq l2i_s (polar l2_s (+ ang_l2 d90) gap)
        l2i_e (polar l2_e (+ ang_l2 d90) gap))
;  (command "LINE" l2i_s l2i_e "")

  (setq c_l1_ld1 (inters l1i_s l1i_e ld1_s ld1_e nil)   ;l1i�� ld1�� ������
        c_l1_ld2 (inters l1i_s l1i_e ld2_s ld2_e nil)   ;l1i�� ld2�� ������

        c_l2_ld1 (inters l2i_s l2i_e ld1_s ld1_e nil)   ;l2i�� ld1�� ������
        c_l2_ld2 (inters l2i_s l2i_e ld2_s ld2_e nil)   ;l2i�� ld2�� ������
  )

  (setq mindst (distance ld1_m c_l1_ld1)           ;ld1_m~l1���������� �Ÿ�
        min_p c_l1_ld1
        dd 1
        ll 1)
  (if (< (setq dst (distance ld1_m c_l2_ld1)) mindst)
    (setq mindst dst
          min_p c_l2_ld1
          dd 1
          ll 2))                          ;ld1_m~l2���������� �Ÿ�
  (if (< (setq dst (distance ld2_m c_l1_ld2)) mindst)           ;ld1_m~l1���������� �Ÿ�
    (setq mindst dst
          min_p c_l1_ld2
          dd 2
          ll 1))
  (if (< (setq dst (distance ld2_m c_l2_ld2)) mindst)           ;ld1_m~l2���������� �Ÿ�
    (setq mindst dst
          min_p c_l2_ld2
          dd 2
          ll 2))

  (setq  le_1 (inters ld1_s ld1_e (polar min_p ang90 100) min_p nil)  ;����� ����1
         le_2 (inters ld2_s ld2_e (polar min_p ang90 100) min_p nil)) ;����� ����2

  (if (< (distance ld1_s le_1) (distance ld1_e le_1))
    (setq  lee_1 ld1_s)
    (setq lee_1 ld1_e))                                 ;�缱1�� ���� ���ϱ�
  (if (< (distance ld2_s le_2) (distance ld2_e le_2))
    (setq  lee_2 ld2_s)
    (setq lee_2 ld2_e))                                 ;�缱2�� ���� ���ϱ�

  (setq b_1 (polar le_1 (angle lee_1 ld1_m) lbolt)        ;��Ʈ��-1
        b_2 (polar le_2 (angle lee_2 ld2_m) lbolt))        ;��Ʈ��-1

  (command "LINE" le_1 le_2 "")                         ;����� ���� �׸���
  (command "LINE" b_1 b_2 "")                           ;��Ʈ�� �׸���
  (command "BREAK" (list lds1 le_1) lee_1)              ;���缱1 �� �ڸ���
  (command "BREAK" (list lds2 le_2) lee_2)              ;���缱2 �� �ڸ���

  (cond
    ((and (= dd 1) (= ll 1))                            ;�缱1�� ��1�� ������
      (setq g_1 (inters l1_s l1_e
                        b_1  (polar b_1 (+ ang_l1 (* 0.5 pi)) 100) nil))
      (setq g_2 (inters l2_s l2_e
                        b_2  (polar b_2 (+ ang_l2 (* 0.5 pi)) 100) nil)))

    ((and (= dd 1) (= ll 2))
      (setq g_1 (inters l2_s l2_e
                        b_1  (polar b_1 (+ ang_l2 (* 0.5 pi)) 100) nil))
      (setq g_2 (inters l1_s l1_e
                        b_2  (polar b_2 (+ ang_l1 (* 0.5 pi)) 100) nil)))

    ((and (= dd 2) (= ll 1))
      (setq g_2 (inters l1_s l1_e
                        b_2  (polar b_2 (+ ang_l1 (* 0.5 pi)) 100) nil))
      (setq g_1 (inters l2_s l2_e
                        b_1 (polar b_1 (+ ang_l2 (* 0.5 pi)) 100) nil)))

    ((and (= dd 2) (= ll 2))
      (setq g_2 (inters l2_s l2_e
                        b_2  (polar b_2 (+ ang_l2 (* 0.5 pi)) 100) nil))
      (setq g_1 (inters l1_s l1_e
                        b_1 (polar b_1 (+ ang_l1 (* 0.5 pi)) 100) nil)))
  ) ;of cond

  (command "LINE" b_1 g_1 "")                 ;gusset �� �׸���
  (command "LINE" b_2 g_2 "")

  (redraw l1s 4)                               ;��������
  (redraw l2s 4)

  (pop-env)                                    ;ȯ�溹��

  (setq *error* oer seterr nil)
  (prin1)
) ;of defun                                                         ]
