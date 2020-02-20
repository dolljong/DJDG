;***********************************
; Program ; MTLEAD
;           MulTi LEADer line
;           Yi -Suk-Jong
;           99/11/9
;***********************************
; �������ü��� �׷��ش�.
;***********************************

(defun C:mtLEAD(/
ds p1 p2 ang w4 ys tp tbox tl p3                            ;�������� ����
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;ȯ�溯�� ����
  (setq ds (getvar "DIMSCALE")                             ;������ ��
        th (getvar "DIMTXT"))

  (setq plist nil)
  (while (/= nil (setq p1 (getpoint "\nFirst point: ")))                    ;ù°��
    (setq plist (append plist (list p1)))                                   ;�����ϱ�
  );while

  
  (setq nplist (length plist))            ;number of plist
	
  
  (setq p2 (getpoint "\nSecond point: "))                ;��°��
  (setq ang (angle p2 (nth 0 plist)))        ;ù���� ���� ���Ѵ�
  (setq minang ang)                           ;�ִ밢�� �ּҰ��� ù���� �������Ѵ�
  (setq maxang ang)
  
  (setq index 0)
  (repeat nplist
    (setq p1 (nth index plist))
    (setq ang (angle p1 p2)                                   ;������ �̷�� ��
        w4  (which4 ang))                                   ;���и鰡?
    (if (<= ang minang) (setq minang ang))    ;�ִ밢�� �ּҰ� ã��
    (if (>= ang maxang) (setq maxang ang))
    (if (or (= w4 1) (= w4 4))                                ;1,4�и��ϰ��
      (setq ys (* 1 ds))                                             ;y-scale =  1
      (setq ys (* -1 ds))                                            ;y-scale = -1
    ) ;of if
    (command "INSERT" (strcat (prefix) "blocks/arw1") p1 ds ys (rtod ang))
    (setvar "CECOLOR" "1")
    (command "LINE" p1 p2 "")                                 ;leader line 
    (setq index (1+ index))
  );repeat

  

  (setq dtang (dang minang maxang))           ;�ִ밢�� �ּҰ��� ����
  (setq mang (+ minang (/ dtang 2.0) pi))        ;�ִ밢�� �ּҰ��� �߰���
  (setq w4  (which4 mang))

  (setvar "CECOLOR" "7")

  (if (or (= w4 1) (= w4 4))
    (progn                                                  ; 1, 4��и��� �� ���ʿ��� ����������
      (setq tp (list (+ (car p2) (* ds th)) (+ (cadr p2) (* ds 1.25)) 0.0))
      (command "DTEXT" tp (* ds th) "0.0")
    ) ;of progn
    (progn                                                  ; 2 ,3��и��� �� �����ʿ��� ��������
      (setq tp (list (- (car p2) (* ds th)) (+ (cadr p2) (* ds 1.25)) 0.0))
      ;(command "DTEXT" "R" tp (* ds th) "0.0" /)            ; text �Է´��
      (command "DTEXT" "R" tp (* ds th) "0.0")            ; text�Է´�
      ) ;progn
  ) ;of if

(command)

  (setq tbox (textbox (entget (entlast))))                  ;������ ���� ũ�� ����
  (setq tl (- (car (nth 1 tbox)) (car (nth 0 tbox))))       ;������ ���� ũ��
  (setq p3 (list (+ (car p2) (* 5.0 ys) (* tl (/ (abs ys) ys)))     ;����
                 (cadr p2) 0.0))

  (setvar "CECOLOR" "1")                                  ;���� ����������
  (command "LINE" p2 p3 "")                                 ;line�׸���
                                  ;���� �������

  (pop-env)                                                 ;ȯ�溯�� ������
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
