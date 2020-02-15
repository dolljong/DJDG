;*************************************
; Program : TXTR
;           TeXT Rotate
;           Yi Suk-Jong
;           1995/12/10
;*************************************
; Text�� � ������ ������ŭ ȸ�������ش�.
; ������ ���� �˸°� �����¿찡 ǥ�õȴ�.

(defun C:TXTR(/
               txt      ln          ip      sp      ep      ang     wh4
               new_ang  ent_type    c_ent   cen_pnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq txt (entget (car (setq txtsel (entsel "\nSelect Text: ")))))
  (setq ip (cdr (assoc 10 txt)))    ;text�� insert point

  (initget "2Point")
  (setq ent (entsel "\n2P <Select Line, Arc or Circle>: "))

  (if (= ent "2Point")
    (setq sp (getpoint "\nPick first point: ")       ;������ ����
          ep (getpoint sp "\nPick second point: "))
    (progn                                           ;entity�� �������� ��
      (setq ent_type (cdr (assoc 0 (entget (car ent)))))  ;entity type
      (cond
        ((or (= ent_type "CIRCLE") (= ent_type "ARC")) ;entity�� �� �Ǵ� ȣ�� ��
          (setq c_ent (entget (car ent)))
          (setq cen_pnt (cdr (assoc 10 c_ent)))
          (setq sp (cadr ent))
          (setq ep (polar sp (+ (angle cen_pnt sp) (/ pi 2)) 1))
        )
        ((= ent_type "LINE")                           ;entity�� ���� ��
          (setq ln (entget (car ent)))
          (setq sp (cdr (assoc 10 ln)))
          (setq ep (cdr (assoc 11 ln)))
        )
      ) ;of cond
    ) ;progn
  )  ;of IF

  (setq ang (angle sp ep))                             ;������ �̷�� ��

  (setq wh4 (which4 ang))                              ;���и鿡 �ִ°�?

  (cond                                                ;1~4��и鿡 ���� ��
     ((= wh4 1)
       (setq ang2 ang)
     )
     ((= wh4 2)
       (setq ang2 (- ang pi))
     )
     ((= wh4 3)
       (setq ang2 (- ang pi))
     )
     ((= wh4 4)
       (setq ang2 (- ang (* 2 pi)))
     )
  );of cond

  (setq new_ang (cons 50 ang2))                    ;���ο� text�� ����
  (setq txt (subst new_ang (assoc 50 txt) txt))
  (entmod txt)                                     ;entity����

  (command "MOVE" txtsel "" ip)                    ;text�ű��

  (setq *error* oer seterr nil)
) ;of defun
