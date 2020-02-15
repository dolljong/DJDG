;------------------------------
; program : CHELV
;           CHange ELeVation
;           Yi suk jong
;           99/10/11
;------------------------------
; elevation�� ���� ��������� elevation�� ������
; ������ pline������� ���� ����

(defun c:chelv(
/ el delta psel pent
)
  (setq el (getreal "\nElevation: "))      ;ù ����� elevation
  (setq delta (getreal "\nDelta Z: "))     ;������� ����

  (princ "\nCurrent Elevation: ")(princ el)

  (while (setq psel (entsel " Pick polyline: "))
    (redraw (car psel) 3)                       ;��ƼƼ ����
    (setq pent (entget (car psel)))
    (chelv_f pent el)
    (setq el (+ el delta))
    (princ "\nCurrent Elevation: ")(princ el)
  ); while

);defun chelv


;---------------------
; function : chelv_f
;            change elevation (function)
;            Yi suk jong
;            99/10/11
;----------------------
; �־��� polyline�� elevation�� �ٲپ���
; �Ѿ���� ��:
;    plent : pline entity���� (entget�� ����)
;        z : elevation��
;----------------------

(defun chelv_f(plent z
 / plent z o38 n38
  )
  (setq o38 (assoc 38 plent))
  (setq n38 (cons (car o38) z))
  (setq plent (subst n38 o38 plent))
  (entmod plent)
);defun
