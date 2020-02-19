;***************************************************************
; Program : JUMP
;           ���� ���˿� ��� ��鵵
;           Yi Suk-Jong
;           1999/7/13
;***************************************************************
; ��Ȳ�� ������ �ٲپ��ش�
; layer�� ���ؼ� ���� ������ ��쿡�� ���� �ٲپ��ش�.
;***************************************************************

(defun C:JUMP(
/               spnt epnt

)

  (defun SETERR(s)                                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)                         ;������ƾ ����

  (push-env)

  (setq jum_width 1000
        jum_gap    300)

  (setq spnt (getpoint "\nStart point: "))
  (setq epnt (getpoint spnt "\nEnd point: "))

  (setq k (f_jump   (list (car spnt) (cadr spnt))
                    (list (car epnt) (cadr epnt))
                    jum_width
                    jum_gap))

  (pop-env)

);defun

(defun f_jump( pnts pnte width gap
;/ s_point e_point width gap
 )

  (setq l (distance pnts pnte))              ;���˷� ��� ����
  (setq n (fix (/ l gap)))                                  ;���˷� ��� ����
  (setq ang (angle pnts pnte))                  ;������-��������
  (setq angp90 (+ ang (* 0.5 pi)))
  (setq angm90 (- ang (* 0.5 pi)))
  (setq half_width (* 0.5 width))

  (setq pnt1 (polar pnts angm90 half_width))
  (setq pnt2 (polar pnts angp90 half_width))
  (setq pnt3 (polar pnt1 ang (* n gap)))
  (setq pnt4 (polar pnt2 ang (* n gap)))

  (command "LINE" pnt1 pnt3 "")
  (command "LINE" pnt2 pnt4 "")

  (repeat (1+ n)
    (command "LINE" pnt1 pnt2 "")
    (setq pnt1 (polar pnt1 ang gap)
          pnt2 (polar pnt2 ang gap))
  );repeat

);defun
