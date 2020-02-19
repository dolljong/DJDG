;*******************************
; Program : JUNG
;           JUNG bun dae
;           Yi Suk-Jong
;           98/1/22
;*******************************
; �߾Ӻи��븦 �׷��ش� (���ΰ���)
;*******************************

(defun C:JUNG(/
              pk pp ent p1 p2 dst1 dst2 ip dx dy        ;�������� ����
)
  (defun SETERR(s)                                      ;���� ������ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                     ;���� ������ƾ ����

  (setq pk (entsel "\nSelect line: "))                  ;����� ��ܼ� ����

  (setq pp (cadr pk))                                   ;������
  (setq ent (entget (car pk)))                          ;������ܼ��� �������� ����
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
    (jungbundae 0 ip (/ dy dx -0.01))                      ;���� �ߺд� ȣ��
    (jungbundae 1 ip (/ dy dx 0.01))                       ;������ �ߺд� ȣ��
  ) ;of if

  (setq *error* oer seterr nil)                         ;������ƾ ����

) ;of defun

;********************************************
; Function : JUNGBUNDAE
;            draw JUNBUNDAE
;            Suk-Jong Yi
;            98/1/22
;********************************************
;�ߺд븦 �׷��ִ� ���.
; �Ѿ���� ��
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun JUNGBUNDAE( LR ip SL / LR ip SL)
  (if (= LR 0)                                                  ;���ʿ���
    (progn
;      (setq inpo (list (+ (car ip) 30)
;                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;�ٱ��ʾƷ�
      (setq inpi (list (+ (car ip) 280.0)
                       (+ (cadr ip) (* sl -280.0 0.01)) 0.0))   ;���ʾƷ�
      (command "PLINE" ip "@0,890" "@95,0" "@60,-560"
                           "@125,-175" inpi "")                 ;�ߺд� �׸���
    ) ;of PROGN
    (progn
;      (setq inpo (list (- (car ip) 30.0)
;                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;�ٱ��ʾƷ�
      (setq inpi (list (- (car ip) 280.0)
                       (+ (cadr ip) (* sl -280.0 0.01)) 0.0))   ;���ʾƷ�
      (command "PLINE" ip "@0,890" "@-95,0" "@-60,-560"
                           "@-125,-175" inpi "")                ;�ߺд�׸���
    ) ;of progn
  ) ;of IF
) ;of defun


