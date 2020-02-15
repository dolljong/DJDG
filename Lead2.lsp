;***********************************
; Program ; LEAD2
;           LEADer line
;           Yi -Suk-Jong
;           04/03/19
;***********************************
; ���ü��� �׷��ش�.
;***********************************

(defun C:LEAD2(/
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

  (setq p1 (getpoint "\nFirst point: "))                    ;ù°��
  (setq p2 (getpoint p1 "\nSecond point: "))                ;��°��
  (setq ang (angle p1 p2)                                   ;������ �̷�� ��
        w4  (which4 ang))                                   ;���и��ΰ�?
  (if (or (= w4 1) (= w4 4))                                ;1,4�и��ϰ��
    (setq ys (* 1 ds))                                             ;y-scale =  1
    (setq ys (* -1 ds))                                            ;y-scale = -1
  ) ;of if
  (command "INSERT" (strcat (prefix) "blocks/arw2") p1 ds ys (rtod ang))
  (setvar "CECOLOR" "RED")
  (command "LINE" p1 p2 "")                                 ;leader line
  (setvar "CECOLOR" "WHITE")

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

  (setvar "CECOLOR" "RED")                                  ;���� ����������
  (command "LINE" p2 p3 "")                                 ;line�׸���
                                  ;���� �������

  (pop-env)                                                 ;ȯ�溯�� ������
  (setq *error* oer seterr nil)
  (princ)

) ;of defun