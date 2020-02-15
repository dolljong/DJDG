;-------------------
; Program : BASEC
;           draw BASE Concrete
;           by Yi Suk-Jong
;           98/9/28
;---------------------------
;Base concrete�׸���
;---------------------------

(defun C:BASEC(
/ Bbase ent thick p1 p2 ang ang pt1 pt2 pt11 pt21
)

  (push-env)                                        ;ȯ�溯������

  (setq Bbase 100)                                  ;������ũ��Ʈ ��

  (initget "2p")
  (setq sel (entsel "\nSelect Line/<2p>: "))            ;line enity����
  (if (= sel "2p")                                      ;2������ �Է¿ɼ� ���ý�
    (progn
      (setq p1 (getpoint "\nPick first point: "))       ;��������ǥ
      (setq p2 (getpoint p1 "\nPick second point: "))   ;������ǥ
    );progn
    (progn
      (setq ent (entget (car sel)))                     ;���õ� ������
      (setq p1 (cdr (assoc 10 ent)))                    ;��������ǥ
      (setq p2 (cdr (assoc 11 ent)))                    ;������ǥ
    );progn
  );if
  	   
  (setq thick (getdist "\nThickness <100>: "))       ;base��ũ��Ʈ �β�
  (if (= thick nil) (setq thick 100))

  
  (setq ang (angle p1 p2))

  (setq pt1  (polar p1  (+ ang pi)  Bbase)             ;������ ������ġ
        pt2  (polar p2  ang         Bbase)             ;����   ������ġ
        pt11 (polar pt1 (* pi -0.5) thick)             ;������ ������ġ
        pt21 (polar pt2 (* pi -0.5) thick))            ;����   ������ġ

  (command "PLINE" p1 pt1 pt11 pt21 pt2 p2 "")          ;base concrete�� �׸���

  (pop-env)                                             ;ȯ�溯�� ����

  (princ)

) ;of defun
