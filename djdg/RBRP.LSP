;**************************************
; Program : RBRP
;           ReBar Round Plan
;           By Suk-Jong Yi
;           96/5/23
;**************************************
; ���w�a�� ��e�i ���e�e�a. (�w�e)
; ť�a���a ��, ����e�a�� �a�a���a.
;**************************************

(defun C:RBRP(/
    r       ctc     bn      loop    cenccen     oldc    dent
    cen     pdia    nlay    count   cover
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;�ŉw�e�� ��ϡ

  (setq cen (getpoint "\nPick Center point: "))                 ;��e ���e�� ����
  (setq pdia (* 1000 (getreal "\nPost DIA(m): ")))              ;post���w
  (command "CIRCLE" cen "D" pdia)                               ;post�a����

  (setq nlay (getint "\nNumber of layer: "))                    ;�y�e ��e���a?

  (setq count 1)                                                ;���弁 �e����
  (repeat nlay
    (princ "\n*** ") (princ count) (princ " LAYER")
    (setq cover (getreal " Cover(mm): "))                        ;�������A
    (setq r (- (/ pdia 2) cover))                               ;����e ���e �e���q

    ;(setq r (getreal "\nRadius: "))                            ;��e ���e�� �e���q

    (initget "Number")
    (setq ctc (getreal "\nNumber/<C.T.C(mm)>: "))               ; ctc ���b�h�q
    (if (= ctc "Number")
      (setq bn (getint "\nNumber of REBAR: "))                  ;��e ���� ���󷳝b
      (setq bn (fix (/ (* 2.0 pi r) ctc)))                      ; ��e ���� ���e
    ) ;of if
    (setq ctc (/ (* 2.0 pi r) bn))                              ; �����A ���e ctc���e

    (princ "\nNumber of rebar: ")                               ; ��e ���� ���a��
    (princ bn)
    (princ "  New C.T.C.(mm): ")                                ; ctc���a��
    (princ ctc)

    (initget "Num Ctc Ok")                                      ; ���� �a�� ���ᥱ
    (setq loop (getkword "\nchange Number/Ctc/<Ok>: "))
    (if (= loop nil) (setq loop "Ok"))

    (while (/= loop "Ok")                   ; ��e�e�b �� ���� �e��  �������b
      (cond
        ((= loop "Num")                                         ; ���� ����
          (setq bn (getreal "\nNumber of Rebar: "))             ;���� ���b
          (setq ctc (/ (* 2.0 pi r) bn))                        ;ctc���e
        ) ;of loop=Num
        ((= loop "Ctc")                                         ; ctc����
          (setq ctc (getreal "\nC.T.C.(mm): "))                 ;ctc���b
          (setq bn (fix (/ (* 2.0 pi r) ctc)))                  ;���� ���e
          (setq ctc (/ (* 2.0 pi r) bn))                        ;�����A ���e ctc���e
        ) ;of loop=Ctc
      ) ;of cond
      (princ "\nNumber of rebar: ")                             ; ���� �b
      (princ bn)
      (princ "  New C.T.C.(mm): ")                              ; ctc�b
      (princ ctc)
      (initget "Num Ctc Ok")                                    ; �a�� �����i �����a?
      (setq loop (getkword "\nchange Number/Ctc/Ok: "))
      (if (= loop nil) (setq loop "Ok"))                        ; return���b�� OK�� ��
    ) ;of while


    (setq ccen (polar cen (* 1.5 pi) r))                        ;�� ��e ��á(�a�e)

    (setq oldc (getvar "CECOLOR"))                              ;�e���� ��ϡ
    (setvar "CECOLOR" "YELLOW")                                 ;���i ���e���a��
    (command "CIRCLE" cen "D" (- pdia (* 2 (- cover 15))))      ;����e
    (command "DONUT" "0" "30" ccen "")    ;���a�a �a����
    (setvar "CECOLOR" oldc)                                     ;�e���� �A������

    (setq dent (ssget "L"))                                     ;���a�a �Eˡˡ ��Ȃ
    (command "ARRAY" (list (ssname dent 0) ccen) "" "P" cen (fix bn) "360" "Y")  ;array

    (setq count (1+ count))                                     ;�a�q �e�a��
  ) ;of repeat

  (pop-env)                                                     ;�ŉw�e���t �A������
  (setq *error* oer seterr nil)
  (prin1)

) ;of defun