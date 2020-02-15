;**********************************
; Program : NOTEBOX
;           insert NOTE mark and draw NOTE BOX
;           By Suk-Jong Yi
;           1999/1/28
;*************************************

(defun C:NOTEBOX(
/                gap_mark r_fillet t_shadow ds
                 p0 p1 p2 p3 p4 p5 p6 p2_1 p3_1 p4_1 p5_1)

  (defun SETERR(s)                                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;ȯ�溯�� ����

  ;---�ʱⰪ����
  (setq gap_mark 2                  ;note mark�� box�� ����
        r_fillet 3                  ;�����ϴ� fillet�� ������
        t_shadow 1)                 ;box������ ������ ���� �β�

  (setq ds (getvar "DIMSCALE"))     ;dimscale�� ����

  (setq gap_mark (* gap_mark ds)             ;�ʱⰪ�� dimscale�� ����
        r_fillet (* r_fillet ds)
        t_shadow (* t_shadow ds)
        t_shadow2 (* t_shadow 0.5))          ;�����β��� 1/2

  (setq p1 (getpoint "\nPick Upper_left corner: "))     ;������ ����������Է�
  (setq p6 (getcorner p1 "\nPick Lower_right corner: "))   ;������ �����ϴ����Է�

  (setq p1x (car p1)                ;������ x��ǥ
        p1y (cadr p1)               ;������ y��ǥ
        p6x (car p6)                ;�����ϴ� x��ǥ
        p6y (cadr p6))              ;�����ϴ� y��ǥ

  (setq p0 (list p1x              (+ p1y gap_mark) 0.0)
        p2 (list p1x              p6y              0.0)     ;�����ϴ�
        p3 (list (- p6x r_fillet) p6y              0.0)     ;�����ϴ�-1
        p4 (list p6x              (+ p6y r_fillet) 0.0)     ;�����ϴ�-2
        p5 (list p6x              p1y              0.0)     ;�������
        p2_1 (list (+ p1x t_shadow2) (- p6y t_shadow2) 0.0) ;���������ϴ�
        p3_1 (list (- p6x r_fillet) (- p6y t_shadow2) 0.0)  ;���������ϴ�-1
        p4_1 (list (+ p6x t_shadow2) (+ p6y r_fillet) 0.0)  ;���������ϴ�-2
        p5_1 (list (+ p6x t_shadow2) (- p1y t_shadow2) 0.0));�����������

  (command "PLINE" p1 p2 p3 "A" p4 "L" p5 "C")
  (command "PLINE" p2_1 "W" t_shadow "" p3_1 "A" p4_1 "L" p5_1 "W" "0" "" "")
  (command "INSERT" (strcat (prefix) "blocks/note") p0 ds "" "0")

  (pop-env)                                                 ;ȯ�溯�� ����

  (setq *error* oer seterr nil)                             ;������ƾ ����

  (prin1)

) ;of defun

