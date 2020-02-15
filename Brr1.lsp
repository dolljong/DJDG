;*******************************
; Program : BRR1
;           draw BaRRier
;           Yi Suk-Jong
;           00/7/28
;*******************************
; ��ȣ���� �׷��ش� (���ΰ���) H=1.27m
;*******************************

(defun C:BRR1(/
              pk pp ent p1 p2 dst1 dst2 ip dx dy        ;�������� ����
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq pk (entsel "\nSelect line: "))                  ;��������

  (setq pp (cadr pk))                                   ;������
  (setq ent (entget (car pk)))                          ;���� entity����
  (setq p1 (cdr (assoc 10 ent))
        p2 (cdr (assoc 11 ent)))

  (setq dst1 (distance pp p1)                           ;������ �������� �Ÿ�
        dst2 (distance pp p2))                          ;������ �������� �Ÿ�

  (if (< dst1 dst2)                                     ;����� ���� insert point��
    (setq ip p1
          dx (- (car p2) (car p1))                      ;x����
          dy (- (cadr p2) (cadr p1)))                   ;y����
    (setq ip p2
          dx (- (car p1) (car p2))
          dy (- (cadr p1) (cadr p2)))
  ) ;of if

  (if (> dx 0)
    (barrier1 0 ip (/ dy dx -0.01))                      ;���� ��ȣ�� ȣ��
    (barrier1 1 ip (/ dy dx 0.01))                       ;������ ��ȣ�� ȣ��
  ) ;of if

  (setq *error* oer seterr nil)

) ;of defun

;********************************************
; Function : BARRIER1
;            DRAW BARRIER
;            Suk-Jong Yi
;            96/7/3
;********************************************
;��ȣå�� �׷��ش�.
; �Ѿ���� ��
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun BARRIER1( LR ip SL / LR ip SL)
  (if (= LR 0)                                      ;���ʿ���
    (progn
      (setq inpo (list (+ (car ip) 30)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;�ٱ��ʾƷ�
      (setq inpi (list (+ (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;���ʾƷ�
      (command "PLINE" inpo "@0,1350" "@230,0" "@70,-970"
                           "@120,-175" inpi "")                             ;�����׸���
    ) ;of PROGN
    (progn
      (setq inpo (list (- (car ip) 30.0)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;�ٱ��ʾƷ�
      (setq inpi (list (- (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;���ʾƷ�
      (command "PLINE" inpo "@0,1350" "@-230,0" "@-70,-970"
                           "@-120,-175" inpi "")                            ;�����׸���
    ) ;of progn
  ) ;of IF
) ;of defun


