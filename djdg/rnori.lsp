;  Program : RNORI ;Round NORI
; function : 3PARC ; �߽ɰ�, ���۰�, ��ü��ũ��, �������� �̿��Ͽ� ȣ�� �׷���
; function : Polar_line ; �߽ɰ�, ����, ���۹�����, �� �������� �̿��Ͽ� polar���� ������ �׷���

;**********************************
;  Program : RNORI
;            Round NORI
;            By Jong-Suk Yi
;            98/8/27
;*********************************
; 05/08/15 : elevation�� �Է��Ͽ� �⸮���� ����

(defun c:RNORI(
	       / cen div_ang sodan danh sp ep rlist topel botel deltael num_r
	         deltael remh dh sang enag total_angle ra total_r count index r2 r1 
		)

  (defun SETERR(s)                                  ;���忡����ƾ
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)                 ;���忡����ƾ ����

  (push-env)                                        ;ȯ�溯�� ����

  (setq cen (getpoint "\nPick Center point: "))     ;�߽���ǥ

  (setq div_ang 6)                                  ;������
  (setq sodan 1000)                                 ;�Ҵ���
  (setq danh 6)					;�ܳ��� 6.0m

  (setq sp (getpoint cen "\nPick start point: "))   ;array ������
  (setq ep (getpoint cen "\nPick End point: "))     ;array ����

  (setq rlist nil)

;�븮 ���̸� ���� �Է¹���  
;  (setq r (getdist cen "\nù�� �븮����: "))        ;ù �븮���� �Է�
;
;  (setq count 2)
;  (while r
;    (setq rlist (append rlist (list r)))            ;�븮���� ����Ʈ�� �߰�
;    (princ "\n") (princ count)
;    (setq r (getdist " �� �븮����: "))            ;2�� ���� �븮���� �Է�
;    (setq count (1+ count))
;  );while
;  (setq num_r (length rlist))                       ;�븮�ܼ�
  
  ;elevation���� �Է¹���.
  (setq topel (getreal "\n�븮 �������� Elevation(����:m): "))
  (setq botel (getreal "\n�븮 ������ Elevation(����:m): "))
  (setq deltael (- topel botel))  ;elevation��

  (setq num_r (fix (/ deltael danh)))		; �� �� 
  (if (> (rem deltael danh) 0)			;�� ���̷� ������ �������� ������ �� �� �ϳ� ���ϱ� 
    (setq num_r (1+ num_r)))
  
  (setq remh deltael)
;  (setq i 0)
  (while (> remh 0);������ ���̰� ���� ��� �ݺ�
    (if (= rlist nil) (setq slop 1.5) (setq slop 1.8))  ;ù��° �� �Ҵ� ���� 1.5, �ƴϸ� 1.8
    (if (>= remh 6.0)  	;�������� 6���� ũ��
      (setq dh 6.0)  	;������ 6.0��
      (setq dh remh)	;6.0�ȵǸ� ���簪
    );if
    (setq remh (- remh dh))	;������ ���� ����
    (setq rlist (append rlist (list (* dh slop 1000)))) ; ���� ������ ���ϱ�
;    (setq 
  );while

	 
  (setq sang (angle cen sp))                        ;������ ���밢
  (setq eang (angle cen ep))                        ;���� ���밢

  (setq total_angle (dang sang eang))               ;��ü ����
  (setq delta_angle (/ total_angle div_ang))        ;delta angle


  ;--------------------- ARC �׸���

  (setq ra (nth 0 rlist))                           ;ù�� �븮����

  (3parc cen sang total_angle ra)

  (setq total_r ra)

  (setq count 1)
  (repeat (1- num_r)
    (setq total_r (+ total_r sodan))
    (3parc cen sang total_angle total_r)            ;�Ҵܳ��� �׸���
    (setq total_r (+ total_r (nth count rlist)))
    (3parc cen sang total_angle total_r)            ;�븮���� �׸���
    (setq count (1+ count))
  );repeat

  ;--------------------- �� �븮���׸���
  (setq index 1)

  (repeat (1- div_ang)
    (polar_line cen (+ sang (* index delta_angle)) 0 ra)

    (setq count 1
          r2    ra)
    (repeat (1- num_r)
      (setq r1 (+ r2 sodan))
      (setq r2 (+ r1 (nth count rlist)))
      (polar_line cen (+ sang (* index delta_angle)) r1 r2)
      (setq count (1+ count))
    );repeat
    (setq index (1+ index))                         ;���� ������
  ) ;repeat

  ;--------------------- ª�� �븮���׸���
  (setq index 0)

  (repeat  div_ang
    (polar_line cen (+ sang (/ delta_angle 2.0) (* index delta_angle)) ;ù�ܳ븮
                0 (/ ra 2.0))
    (setq count 1
          r2    ra)
    (repeat (1- num_r)                                      ;�ι���� �̻� �븮
      (setq r1 (+ r2 sodan))
      (setq r2 (+ r1 (/ (nth count rlist) 2)))
      (polar_line cen (+ sang (/ delta_angle 2.0) (* index delta_angle))
                  r1 r2)
      (setq r2 (+ r1 (nth count rlist)))
      (setq count (1+ count))
    );repeat

    (setq index (1+ index))                         ;���� ������
  ) ;repeat

  (pop-env)                                         ;ȯ�溯������

  (setq *error* oer seterr nil)                     ;���忡����ƾ

  (princ)

) ;;; End of program


;----------------------------
; function : 3PARC :
;            3 Pint ARC
;            By Yi Suk Jong
;            99/4/1
;----------------------------
; �߽ɰ�, ���۰�, ��ü��ũ��, �������� �̿��Ͽ� ȣ�� �׷���
; �Ѿ���� ��
;      center_point : �߽���
;      s_angle      : ���۰�
;      total_angle  : ��ü��
;      radius       : ������
;----------------------------

(defun 3PARC(center_point s_angle total_angle radius
/ center_point s_angle total_angle radius)
  (command "ARC" (polar center_point s_angle                       radius)
                 (polar center_point (+ s_angle (/ total_angle 2)) radius)
                 (polar center_point (+ s_angle total_angle)       radius))
);defun

;----------------------------
; function : Polar_line :
;            draw Polar line
;            By Yi Suk Jong
;            99/4/1
;----------------------------
; �߽ɰ�, ����, ���۹�����, �� �������� �̿��Ͽ� polar���� ������ �׷���
; �Ѿ���� ��
;      center_point : �߽���
;      angle        : ����
;      radius1      : ���۹�����
;      radius2      : ��������
;----------------------------

(defun Polar_line(center_point angle radius1 radius2
/ center_point angle radius1 radius2 )

  (command "LINE" (polar center_point angle radius1)
                  (polar center_point angle radius2)
                  "")

);defun

