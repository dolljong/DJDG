;****************************************************
; Program: SBLOCK.LSP
;          Shoe BLOCK
;          Yi Suk Jong
;          97/1/22
;****************************************************
;�����ϸ� ����ö�� �׸���

(defun C:SBLOCK(
		/
  ds sc x1 n1 p1 x2 n2 p3 a1 a2 a3 a4 b1 b2 ipnt ix iy
  l1 h1 l2 lx ly pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 count dpt
  pt11 pt12 pt13 pt14 sbx sby pt15 pt16 cnt scl2 pp vgap mx fpt1 fpt2
  mp ss wp1 wp2
)

  ;;
  ;; ���� error routine
  ;;
  (defun SETERR(s)                                      ;���� ������ƾ ����
  ;If an error (CTRL-C) occurs when this command is active.
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ "\n*cancel*")
        (princ (strcat "\nError " s))
      ) ;of if
    ); of If
  ;Restore previous error handler
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  ;;
  ;; Function: SB_DIALOG (Dialog box�� �Է¹ޱ�)
  ;;
  (defun SB_DIALOG (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                  ;dialogȣ��
    (if (not (new_dialog "SBLOCK" dcl_id)) (exit))

    (start_image "sblock")                                  ;image ���̱�
    (slide_image
                 0 0
                 (dimx_tile "sblock") (dimy_tile "sblock")
                 "djdg(dsblock)"
    )
    (end_image)

    (if (= #X1 nil) (setq #X1  50))
    (if (= #N1 nil) (setq #N1   8))                      ;�ʱ�ġ ����
    (if (= #P1 nil) (setq #P1 150))
    (if (= #X2 nil) (setq #X2 100))
    (if (= #N2 nil) (setq #N2   8))
    (if (= #P2 nil) (setq #P2 150))
    (if (= #N3 nil) (setq #N3   3))
    (if (= #P3 nil) (setq #P3 150))
    (if (= #A1 nil) (setq #A1  50))
    (if (= #A2 nil) (setq #A2  62))
    (if (= #A3 nil) (setq #A3  50))
    (if (= #A4 nil) (setq #A4 850))
    (if (= #B1 nil) (setq #B1 100))
    (if (= #B2 nil) (setq #B2 100))
    (if (= #M1 nil) (setq #M1 "S1"))
    (if (= #D1 nil) (setq #D1 "D16"))
    (if (= #M2 nil) (setq #M2 "S2"))
    (if (= #D2 nil) (setq #D2 "D16"))
    (if (= #M3 nil) (setq #M3 "S"))
    (if (= #D3 nil) (setq #D3 "D16"))
    (if (= #M4 nil) (setq #M4 "S"))
    (if (= #D4 nil) (setq #D4 "D16"))
    (if (= #SCALE nil) (setq #SCALE 20))


    (set_tile "x1" (rtos #X1 2 0))
    (set_tile "n1" (rtos #N1 2 0))                  ;�ʱ�ġ�� edit box setting
    (set_tile "p1" (rtos #P1 2 0))
    (set_tile "x2" (rtos #X2 2 0))
    (set_tile "n2" (rtos #N2 2 0))
    (set_tile "p2" (rtos #P2 2 0))
    (set_tile "n3" (rtos #N3 2 0))
    (set_tile "p3" (rtos #P3 2 0))
    (set_tile "a1" (rtos #A1 2 0))
    (set_tile "a2" (rtos #A2 2 0))
    (set_tile "a3" (rtos #A3 2 0))
    (set_tile "a4" (rtos #A4 2 0))
    (set_tile "b1" (rtos #B1 2 0))
    (set_tile "b2" (rtos #B2 2 0))
    (set_tile "m1" #M1)
    (set_tile "d1" #D1)
    (set_tile "m2" #M2)
    (set_tile "d2" #D2)
    (set_tile "m3" #M3)
    (set_tile "d3" #D3)
    (set_tile "m4" #M4)
    (set_tile "d4" #D4)
    (set_tile "scale" (rtos #SCALE 2 0))
    (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
    (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))


    (action_tile "x1" "(set_val $key)")
    (action_tile "n1" "(set_val $key)")                   ;dialog box Action
    (action_tile "p1" "(set_val $key)")
    (action_tile "x2" "(set_val $key)")
    (action_tile "n2" "(set_val $key)")
    (action_tile "p2" "(set_val $key)")
    (action_tile "n3" "(set_val $key)")
    (action_tile "p3" "(set_val $key)")
    (action_tile "a1" "(set_val $key)")
    (action_tile "a2" "(set_val $key)")
    (action_tile "a3" "(set_val $key)")
    (action_tile "a4" "(set_val $key)")
    (action_tile "b1" "(set_val $key)")
    (action_tile "b2" "(set_val $key)")
    (action_tile "m1" "(set_val $key)")
    (action_tile "d1" "(set_val $key)")
    (action_tile "m2" "(set_val $key)")
    (action_tile "d2" "(set_val $key)")
    (action_tile "m3" "(set_val $key)")
    (action_tile "d3" "(set_val $key)")
    (action_tile "m4" "(set_val $key)")
    (action_tile "d4" "(set_val $key)")
    (action_tile "scale" "(set_scale)")
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "x1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG

  ;;;
  ;;; dialog box���� �� �Է¹޾� ������ ����
  ;;;
  (defun SET_VAL (key / in value)              ;n1 edit_box�� �Է��� ������ ��
    (setq in (get_tile key))
    (cond
      ((= key "x1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #X1 value)
            (set_tile "error" "")
            (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=x1
      ((= key "n1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #N1 value)
            (set_tile "error" "")
            (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=n1
      ((= key "p1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #P1 value)
            (set_tile "error" "")
            (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p1
      ((= key "x2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #X2 value)
            (set_tile "error" "")
            (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=x2
      ((= key "n2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #N2 value)
            (set_tile "error" "")
            (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=n2
      ((= key "p2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #P2 value)
            (set_tile "error" "")
            (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p2
      ((= key "n3")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #N3 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=n3
      ((= key "p3")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #P3 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p3
      ((= key "a1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #A1 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=a1
      ((= key "a2")
        (setq value (atoi in))
;        (if (= value 0)
;          (progn
;            (do_error key in)
;            nil
;          ) ;of THEN
;          (progn
            (setq #A2 value)
            (set_tile "error" "")
            T
;          ) ;of ELSE
;        ) ;of if
      ) ;of key=a2
      ((= key "a3")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #A3 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=a3
      ((= key "a4")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #A4 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=a4
      ((= key "b1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B1 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b1
      ((= key "b2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B2 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b2
      ((= key "scale")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #SCALE value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p3
      ((= key "m1")
        (setq #M1 (get_tile "m1"))
        T
      ) ;of key=m1
      ((= key "d1")
        (setq #D1 (get_tile "d1"))
        T
      ) ;of key=d1
      ((= key "m2")
        (setq #M2 (get_tile "m2"))
        T
      ) ;of key=m2
      ((= key "d2")
        (setq #D2 (get_tile "d2"))
        T
      ) ;of key=d2
      ((= key "m3")
        (setq #M3 (get_tile "m3"))
        T
      ) ;of key=m3
      ((= key "d3")
        (setq #D3 (get_tile "d3"))
        T
      ) ;of key=d3
      ((= key "m4")
        (setq #M4 (get_tile "m4"))
        T
      ) ;of key=m4
      ((= key "d4")
        (setq #D4 (get_tile "d4"))
        T
      ) ;of key=d4
    ) ;of cond
  ) ;of defun


  ;;
  ;; FUNCTION : SET_SCALE (scale�� �Է�)
  ;;
  (defun SET_SCALE(/ in value)              ;scale edit_box�� �Է��� ������ ��
    (setq in (get_tile "scale"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "scale" in)
        nil
      ) ;of THEN
      (progn
        (setq #SCALE value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun


  ;;;
  ;;; ok��ư�� ������ ��
  ;;;
  (defun do_accept()           ;dialog box�� ������ ���� ��� �Է� ����Ÿ Ȯ��
    (if (and (set_val "x1") (set_val "n1") (set_val "p1")
             (set_val "x2") (set_val "n2") (set_val "p2")
             (set_val "a1") (set_val "a2") (set_val "a3") (set_val "a4")
             (set_val "b1") (set_val "b2") (set_val "n3") (set_val "p3")
             (set_val "m1") (set_val "d1")
             (set_val "m2") (set_val "d2")
             (set_val "m3") (set_val "d3")
             (set_val "m4") (set_val "d4")
             (set_val "scale"))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; �߸��� ���� �ԷV�� ��
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massageâ�� ����ǥ��

    (if (or (= tile "x1") (= tile "n1") (= tile "p1")
            (= tile "x2") (= tile "n2") (= tile "p2")
            (= tile "a1") (= tile "a2") (= tile "a3") (= tile "a4")
            (= tile "b1") (= tile "b2") (= tile "n3") (= tile "p3")
            (= tile "scale"))
      (progn
        (set_tile  tile value)
        (mode_tile tile 2)
      ) ;of THEN
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; Cancel ��ư�� ������ ���
  ;;;
  (defun do_cancel()
    (done_dialog)
    (exit)
  )

  ;;;
  ;;; MAIN ROUTINE
  ;;;

  (setq oer *error* *error* seterr)

  (push-env)                            ;ȯ�溯�� ����

  (setq ds (getvar "DIMSCALE"))         ;�����ϰ���Ƴ���

  (sb_dialog)                           ;dialog�ڽ��� �Է¹ޱ�

  (setq sc (/ ds #SCALE))
  (setq x1 (* #X1 sc)                   ;scale��ŭ Ű���ֱ�
        n1 #N1
        p1 (* #P1 sc)
        x2 (* #X2 sc)
        n2 #N2
        p2 (* #P2 sc)
        n3 #N3
        p3 (* #P3 sc)
        a1 (* #A1 sc)
        a2 (* #A2 sc)
        a3 (* #A3 sc)
        a4 (* #A4 sc)
        b1 (* #B1 sc)
        b2 (* #B2 sc))

  (setq ipnt (getpoint "\nPick insert point:")
        ix (car ipnt)
        iy (cadr ipnt))

  (setq l1 (+ (* 2.0 x1) (* n1 p1))       ;shoe block ����
        h1 (+ a1 a2 a3)                   ;shoe block ����
        l2 (+ (* 2.0 x2) (* n2 p2))       ;splitö�� ����
        lx (- ix (* 0.5 l2) x2)           ;������ x��ǥ
        ly (- iy h1))                     ;��ũ��Ʈ ��� y��ǥ

  (setq pt1 (list lx ly)                   ;shoe block ��
        pt2 (list (- ix (* 0.5 l1)) ly)
        pt3 (list (car pt2) iy)
        pt4 (list (+ ix (* 0.5 l1)) iy)
        pt5 (list (+ ix (* 0.5 l1)) ly)
        pt6 (list (+ ix (* 0.5 l2) x2) ly))

  (command "LINE" pt1 pt2 pt3 pt4 pt5 pt6 "")

  (setq pt7 (list lx (- ly b1))            ;��ö�ټ�(����)
        pt8 (list (car pt6) (cadr pt7)))

  (setvar "CELTYPE" "HIDDEN")
  (command "LINE" pt7 pt8 "")
  (setvar "CELTYPE" "BYLAYER")

  (setvar "CECOLOR" "YELLOW")                     ;���� ���������..
  (setq pt9  (list (+ (car pt3) x1) (- iy a1))    ;shoe block���ö��
        pt10 (list (- (car pt4) x1) (cadr pt9)))
  (command "LINE" pt9 pt10 "")

  (setq count 0)                                    ;��ö��
  (repeat (1+ n1)
    (setq dpt (list (+ (car pt9) (* count p1)) (+ (cadr pt10) 16)))
    (command "DONUT" "0" "32"  dpt "")
    (setq count (1+ count))
  ) ;of repeat

  (if (> a2 0)
    (progn
      (setq pt11  (list (car pt9) (- (cadr pt9) a2))    ;shoe block�ϴ�ö��
            pt12 (list (car pt10) (cadr pt11)))
      (command "LINE" pt11 pt12 "")

      (setq count 0)                                   ;��ö��
      (repeat (1+ n1)
        (setq dpt (list (+ (car pt11) (* count p1)) (+ (cadr pt12) 16)))
        (command "DONUT" "0" "32"  dpt "")
        (setq count (1+ count))
      ) ;of repeat
    ) ;of progn
  ) ;ofIF

  (setq pt13 (list (car pt9)  (- (cadr pt9) (+ a2 a3 a4))) ;��������
        pt14 (list (car pt10) (cadr pt13)))
  (command "LINE" pt9 pt13 "")
  (command "LINE" pt10 pt14 "")

  (setq sbx (- ix (* 0.5 l2))                       ;splitö�� ���� x��ǥ
        sby (- iy h1 b1 b2))                        ;splitö�� ��� y��ǥ

  (setq count 0)
  (repeat (1+ n3)
    (setq pt15 (list sbx (- sby (* count p3)))
          pt16 (list (+ sbx l2) (cadr pt15)))
    (command "LINE" pt15 pt16 "")
    (setq cnt 0)
    (repeat (1+ n2)
      (setq dpt (list (+ sbx x2 (* cnt p2)) (+ (cadr pt15) 16)))
      (command "DONUT" "0" "32" dpt "")
      (setq cnt (1+ cnt))
    ) ;of repeat
    (setq count (1+ count))
  ) ;of repeat

  (setvar "CECOLOR" "BYLAYER")

  ;
  ; ġ���� �۾�
  ;
  (setq scl2 (/ #SCALE ds))                 ;ġ����ȯ factor

  (setq pp (f_dh pt3 x1 1 1 scl2))          ;shoe block
  (setq pp (f_dh pp p1 n1 1 scl2))
  (setq pp (f_dh pp x1 1 1 scl2))
  (setq pp (f_dh pt3 l1 1 2 scl2))

  (setq pp (f_dv (list (car pt1) (cadr pt13)) a4 1 -1 scl2))  ;���� ����ġ����
  (setq pp (f_dv pp a3 1 -1 scl2))
  (if (> a2 0) (setq pp (f_dv pp a2 1 -1 scl2)))
  (setq pp (f_dv pp a1 1 -1 scl2))
  (setq pp (f_dv pt1 h1 1 -2 scl2))

  (setq pp (f_dv (list (car pt6) (cadr pt16)) p3 n3 1 scl2)) ;���� ����ġ����
  (setq pp (f_dv pp b2 1 1 scl2))                           ;���� ����ġ����
  (setq pp (f_dv pp b1 1 1 scl2))                           ;���� ����ġ����

  (setq pp (f_dh pt15 x2 1 -1 scl2))                        ;splitö�� ġ����
  (setq pp (f_dh pp p2 n2 -1 scl2))                         ;splitö�� ġ����
  (setq pp (f_dh pp x2 1 -1 scl2))                          ;splitö�� ġ����
  (setq pp (f_dh pt15 l2 1 -2 scl2))                        ;splitö�� ġ����

  ;
  ; MARKING�۾�
  ;

  ; MARKING-2

  (setq vgap (* ds 5))                          ;ö�ٰ� marking���� ��������

  (setq mx (+ (car pt9) (* (+ 0.5 (fix (* (/ n1 3.0) 2))) p1)))

  (setq fpt1 (list mx (- iy a1 a2))                    ;fence�� �ϴ�
        fpt2 (list mx (- iy (/ a1 2.0)))               ;fence�� ���
        mp (list mx (+ (cadr pt9) vgap a1)))      ;marking point - 1
  (setq ss (ssget "F" (list fpt1 fpt2)))

  (F_B5 ss fpt1 mp 1 #M2 #D2)

  ;MARKING-4

  (setq mx (+ x2 (car pt15) (* (+ 0.5 (fix (* (/ n2 3.0) 2))) p2)))

  (setq fpt1 (list mx sby)                    ;fence�� ��ǥ
        fpt2 (list mx (cadr pt16))
        mp (list mx (- (cadr pt16) vgap (* ds 7))))      ;marking point - 1
  (setq ss (ssget "F" (list fpt1 fpt2)))

  (F_B5 ss fpt1 mp 1 #M4 #D4)

  ;MARKING-1
  (setq wp1 (list (+ (car pt9) (* (- (fix (/ n1 3.0)) 0.5) p1))     ;����
                  (+ 32 (cadr pt9)))                                ;����
        wp2 (list (+ (car wp1) (* 2 p1))                            ;����
                  (- (cadr pt9) a2))                                ;�Ʒ���
        mp (list (/ (+ (car wp1) (car wp2)) 2.0)
                 (+ iy vgap)))

  (setq ss (ssget "W"  wp1 wp2))

  (f_b3 ss mp 3 #M1 #D1)

  ;MARKING-3
  (setq wp1 (list (+ sbx x2 (* (- (fix (/ n2 3.0)) 0.5) p2))           ;����
                  (+ 32 sby))                                       ;����
        wp2 (list (+ (car wp1) (* 2 p2))                            ;����
                  (cadr pt16))                                ;�Ʒ���
        mp (list (/ (+ (car wp1) (car wp2)) 2.0)
                 (- (cadr wp2) vgap (* ds 7))))

  (setq ss (ssget "W"  wp1 wp2))

  (f_b3 ss mp 3 #M3 #D3)

  (princ "O.K!")

  (pop-env)

) ;of defun

;******************************************
; Program : F_DH
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29, 12/12
;******************************************
; ����ġ������ �Լ��� ó�����ش�. (Scaleó�� ����)
; �Ѿ���� ����
;        SP : ������
;       DST : �Ÿ�
;         N : �ݺ�����
;        UD : Up/DOWN (���밪�� LEVEL)
;      TXT1 : �����Է��ϰ����� text(�����̸� scale)
; �����ִ� �� - ���� ��ǥ
; ex) (setq pp (f_dh SP DST N UD TXT1))
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th (getvar "DIMTXT")
        dim_gap 10.0)                                       ;���� ũ�� ����

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (if (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0)))  ;����/�Ǽ��̸�
    (setq sc txt1)
    (setq  sc 1)
  ) ;of if                                            ;factor��

  (if (> ud 0)                                              ;�� �Ʒ�
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;ġ���� ��ġ ��� (���밪)

  (setq next (* dst n))                                     ;���������� �������� �Ÿ�

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep ��ġ���

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;ġ���� ��ġ

  (setq dx (distance sp ep))                          ;�Ÿ� ���

  (if (< (* dx sc) 1000.0)
    (setq txt (rtos (* dx sc) 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dx 0.001 sc) 2 3))                ;1000�̻��� ��
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;����� �ɼ��� ���
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos n 2 0))                          ;���� ���� ���
      (if (< divl 1000.)
        (setq divl (rtos (* divl sc) 2 0))                   ;1000�̸��� ��
        (setq divl (rtos (* 0.001 divl sc) 2 3))) ;of if ;1000�̻��� ��
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text��ü����
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;ġ�������� ���� text �ȵ���
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;�� �Ʒ� ���ٷ� ������
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds th)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds th)) 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)
          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM���� ����
        ) ;of progn THEN
        (progn                                 ;ġ�������� ���� text ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM���� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (or (= txt1 nil)
              (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0))))
        (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM���� ����
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*******************************************************
; Function : F_DV
;           Fuction Dimension Vertical
;           Jong-Suk Yi
;           96/7/1, 96/12/12
;*******************************************************
; ����ġ������ �Լ��� ó�����ش�. (Scaleó�� ����)
; �Ѿ���� ����
;        SP : ������
;       DST : �Ÿ�
;         N : �ݺ�����
;        UD : Right/Left (���밪�� LEVEL)
;      TXT1 : �����Է��ϰ����� text
; �����ִ� �� - ���� ��ǥ
;******************************************************

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th (getvar "DIMTXT")                          ;textũ�� = dimtxt
        dim_gap 10.0)                                 ;ġ���� ����
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0)))  ;����/�Ǽ��̸�
    (setq sc txt1)
    (setq  sc 1)
  ) ;of if                                            ;factor��

  (if (> lr 0)                                        ;����/������
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))

  (setq next (* dst n))                                 ;�������� �Ÿ�

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;������ ����

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ

  (setq dy (distance sp ep))                          ;�� ���� �Ÿ�

  (if (< (* dy sc) 1000.0)
    (setq txt (rtos (* dy sc) 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dy 0.001 sc) 2 3))                ;1000�̻��� ��
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos n 2 0))                          ;���� �������
      (if (< divl 1000.)
        (setq divl (rtos (* divl sc) 2 0))                   ;������ ���̰� 1000�̸���
        (setq divl (rtos (* divl 0.001 sc) 2 3))) ;of if           ;������ ���̰� 1000�̻��
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dy)
        (progn                                  ;text�� ������ ���� �ȵ���
          (setq dtxt1 (strcat divn "@" divl))   ;���ٷ� ����
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list (- (* dx sgn) (* ds th)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds th)) 0.0 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy " ")               ;DIM���� ����
        ) ;of progn THEN
        (progn                                  ;text�� ������ ���� ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM���� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (or (= txt1 nil)
              (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0))))
        (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM���� ����
    ) ;of progn ELSE
  ) ;of if
  ep
) ;defun

;*************************************
;     Program : F_B5
;               Function reBar marking 5
;               By Suk-Jong Yi
;               96/12/13
;**************************************************************
; ö�� ��ŷ�� �Ʒ��� ��� (�Լ�)
;     /--/--/--��
;
; (F_B5 SSLST SP EP DIR MK DIA)
;       SSLST                 : ���� entity list
;             SP              : Start point, �������� ������
;                EP           : End point, �������� ����
;                   P3        : �ؼ��� ��ġ
;                      MK     : Marking
;                         DIA : ö�� ���� �� DIA
;**************************************************************
(defun F_B5(SSLST SP EP DIR MK DIA /                       ;�������� ����
        oldclr      index       nnsent      entl        entype
        crsxy       spexy       epexy       crsxy       slp         elp
        sp          ep          sslst       enum        index       enum
)

  (push-env)

  (setvar "CMDECHO" 0)                        ;ȯ�溯�� ���� ���ɸ޾Ƹ� ����
  (setvar "BLIPMODE" 0)                       ;BLIP MODE ����
  (setq oldclr (getvar "CECOLOR"))            ;������ ����ϱ�
  (setvar "CECOLOR" "RED")                    ;������ ����������
  (setq ds (getvar "DIMSCALE"))               ;������
  (setq rc (* ds 3.5))                        ;��ŷ���� ������

;  (princ "\nSelect objects: ")                ;��ŷ ��� ��ƼƼ ����
;  (setq sslst (ssget))

  (setq enum (sslength sslst))                ;seclection set�� ��ƼƼ ����


  ;**** ��� ��ƼƼ ����, ���ƼƼ ���� �ľ� (LINE�� ARC��)
  (setq index 0                               ;��ƼƼ ����
        nssent 0)                             ;line�̰ų� arc�� ��ƼƼ ����

  (repeat enum                  ;��ƼƼ ���� ��ŭ �ݺ�
    (setq entl (entget (ssname sslst index)))      ;��ƼƼ ����Ʈ
    (setq entype (cdr (assoc 0 entl)))             ;��ƼƼ Ÿ��
    (if (or (= entype "LINE") (= entype "ARC"))    ;��ƼƼ�� line�̰ų� arc�ΰ��
      (progn
        (redraw (ssname sslst index) 3)            ;��ƼƼ ����
        (if (= nssent 0) (setq ssent (ssadd (ssname sslst index)))
                         (setq ssent (ssadd (ssname sslst index) ssent))
        ) ;of if                    ; line�̰ų� arc��ƼƼ���� slection set �����
        (setq nssent (1+ nssent))                     ;��� ��ƼƼ ���� count up
      ) ;of progn
    ) ; of if
    (setq index (1+ index))                           ;���� ��ƼƼ��
  ) ;of repeat
;  (print nssent)
;  (princ "Entity Found")                              ;LINE�̳� ARC�� ��ΰ�?

;  (setq sp (getpoint "\nPick start point: "))         ;�������� ù��
;  (setq ep (getpoint sp "\nPick end point: "))        ;�������� ����
  (setq seang (angle sp ep))                          ; �������� ����
  (command "LINE" sp ep "")                           ;������ �׸���

  (setq index 0)
  (repeat nssent                                  ;ARC�̰ų� LINE�� ��ƼƼ��ŭ
     (setq entl (entget (ssname ssent index)))    ;��ƼƼ ����Ʈ ���ϱ�
     (cond
       ((= (cdr (assoc 0 entl)) "ARC")            ;��ŷ ����� ARC�� ���
         (setq crsxy (cross entl sp ep))            ;ARC�� �������� ������ã��
       ) ;of entity=ARC
       ((= (cdr (assoc 0 entl)) "LINE")           ; ��ŷ ����� LINE�� ���
         (setq spexy (cdr (assoc 10 entl))          ;�������� ���۰� ����
               epexy (cdr (assoc 11 entl)))
         (setq crsxy (inters spexy epexy sp ep))    ;�������� LINE�� ������ ã��
       ) ;of entity=LINE
     ) ;of cond
     (setq slp (polar crsxy (+ seang (* pi 0.25)) (* 1.3 ds)))
     (setq elp (polar crsxy (+ seang (* pi 1.25)) (* 1.3 ds)))
     (command "LINE" slp elp "")                  ; Tick line�׸��� /
     (redraw (ssname ssent index) 4)              ; ������ ��ƼƼ ���󺹱�
     (setq index (1+ index))                      ; ���� ��ƼƼ��
  ) ; repeat

;  (setq p3 (getpoint ep "\nPick base line: "))              ;base line point


   (cond                                                 ;Marking�� ����
     ((= dir 1) (setq xsgn 1))
     ((= dir 2) (setq ysgn 1))
     ((= dir 3) (setq xsgn -1))
     ((= dir 4) (setq ysgn -1))
   ) ;of cong

;  (setq dx (- (car p3) (car ep)))
;  (if (< dx 0)                                          ;base line�� x�����ν�
;    (setq xsgn -1)
;    (setq xsgn 1)
;  ) ;of if
;
;  (setq dy (- (cadr p3) (cadr ep)))
;  (if (<  dy 0)                                         ;base line�� y�����ν�
;    (setq ysgn -1)
;    (setq ysgn  1)
;  ) ;of if

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line�� ����

;  (if (> (abs dx) (abs dy))                                 ;�����ֳ� ���ֳ�?
  (if (or (= dir 1) (= dir 3))                                 ;�����ֳ� ���ֳ�?
    (progn
      (setq p4 (list (+ (car ep) (* blen xsgn)) (cadr ep)))
      (command "LINE" ep p4 "")
;      (if (< dx 0)
      (if (< xsgn 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (+ (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 0)                                   ;textȸ����
    ) ;of progn
    (progn
      (setq p4 (list (car ep) (+ (cadr ep) (* blen ysgn))))
      (command "LINE" ep p4 "")
;      (if (< dy 0)
      (if (< ysgn 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (- (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* 4 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 90)
    ) ;of progn
  ) ;of if


  (command "CIRCLE" cp rc)
  (setvar "CECOLOR" oldclr)

  (setvar "CECOLOR" "WHITE")
;  (setq mk (getstring "\nEnter Marking: "))
  (txtinc mk cp txtrot)
;  (setq dia (getstring "\nEnter Rebar Dia: "))
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (setvar "CECOLOR" oldclr)

  (pop-env)

  (princ)
) ; defun

;**************************************
; Program : F_B3
;           Bar Mark 3
;           Suk-Jong Yi
;           97/1/22
;**************************************
;ö�ٸ�ŷ�� �Ʒ��� ����� ������ش�.
;            ��
;           /|\
;         /  |  \
; (F_B3 SSLST  MP DIR MK DIA)
;       SSLST                : ���� entity list
;              MP            : Marking��ġ
;                 DIR        : �ؼ��� ��ġ
;                     MK     : Marking
;                        DIA : ö�� ���� �� DIA
;**************************************

(defun F_B3( SSLST MP DIR MK DIA /
       plst   oldclr   ds      entlst  mpnt    nent           ;�������� ����
       index  ent      entype  sp      ep      mp     npnt
       vtx1   nxt1     vtx2    vtx1p   vtx2p   cenp
       cp     ang      minang  maxang  dtang   mang
       ccen   mk       dia     diaxy
)

  (push-env)                                    ;ȯ�溯���� ����
  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "RED")

  (setq ds (getvar "DIMSCALE"))
  (setq rc (* 3.5 ds))                            ;��ŷ���� ������

;  (setq entlst (ssget))                           ;��ŷ��� entity����
   (setq entlst sslst)
;  (setq mpnt (getpoint "Pick marking point: "))   ;��ŷ ���� �׸� ��ġ����
   (setq mpnt mp)
  (setq nent (sslength entlst))                   ;��ŷ��� ��ƼƼ����

  (setq plst nil)                 ;plst: ��ŷ��� entity����  ����Ʈ����Ʈ
  (setq npnt 0)       ;����Ʈ ���� (��ŷ��� ��ƼƼ���� /= ��ŷ�������Ʈ����)
  (setq index 0)
  (repeat nent                                    ;��� ��ƼƼ������ŭ �ݺ�
    (setq ent (entget (ssname entlst index)))
    (setq entype (cdr (assoc 0 ent)))             ;��ƼƼŸ�� ����
    (cond
      ((= entype "LINE")                          ;��ƼƼ�� �����ΰ��
        (setq sp (cdr (assoc 10 ent)))            ;������ ������
        (setq ep (cdr (assoc 11 ent)))            ;������ ����
        (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;�����߰��� X
                       (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y
        (setq plst (append plst (list mp)))       ;�߰����� ��ŷ����Ʈ�� �߰�
        (setq npnt (1+ npnt))                     ;��ŷ����Ʈ�� ���� ����
      ) ;of entype="LINE"
      ((= entype "POLYLINE")      ;���������� ���(ö���� �������� �׸� ���)
        (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;ù������
        (if (/= (abs (cdr (assoc 42 vtx1))) 0)         ;���������� ��ũ���ΰ�?
          (progn
            (setq vtx2 (entget (setq nxt2 (entnext nxt1))))   ;�ι�°�� ����
            (setq vtx1p (cdr (assoc 10 vtx1)))                ;ù�� ����Ʈ
            (setq vtx2p (cdr (assoc 10 vtx2)))                ;��°�� ����Ʈ
            (setq cenp (list (/ (+ (car vtx1p) (car vtx2p)) 2.0)     ;��Ÿ����Ʈ X
                             (/ (+ (cadr vtx1p) (cadr vtx2p)) 2.0))) ;           Y
            (setq plst (append plst (list cenp)))             ;����Ʈ����Ʈ�� �߰�
            (setq npnt (1+ npnt))                             ;����Ʈ ���� ����
          ) ;of progn
        ) ;of if
      ) ;of entype="PLINE"
      ((= entype "CIRCLE")                                ;��ƼƼ�� ��Ŭ�� ���
        (setq cp (cdr (assoc 10 ent)))                    ;��Ŭ�� ��Ÿ����Ʈ
        (setq plst (append plst (list cp)))               ;����Ʈ����Ʈ�� �߰�
        (setq npnt (1+ npnt))                             ;����Ʈ ���� ����
      ) ;of entype="CIRCLE"
    ) ;of cond
  (setq index (1+ index))                                 ;index=��ƼƼ��ȣ
  ) ; of repeat


  (setq ang (angle mpnt (nth 0 plst)))        ;ù���� ���� ���Ѵ�
  (setq minang ang)                           ;�ִ밢�� �ּҰ��� ù���� �������Ѵ�
  (setq maxang ang)
  (setq index 0)
  (repeat npnt                                ;����Ʈ ������ŭ �ݺ�
    (setq ang (angle mpnt (nth index plst)))  ;��ŷ���� ����Ʈ�� ���� ���� ���Ѵ�
    (if (<= ang minang) (setq minang ang))    ;�ִ밢�� �ּҰ� ã��
    (if (>= ang maxang) (setq maxang ang))
    (command "LINE" mpnt (nth index plst) "") ;����Ʈ���� ��ŷ�������� �����׸���
    (setq index (1+ index))                   ;����Ʈ������ŭ �ݺ�
  ) ;of repeat

  ;-------------------------------
  ; base line�� making/dia ����
  ;-------------------------------

;  (setq p3 (getpoint mpnt "\nPick base line: "))              ;base line point

;  (setq dx (- (car p3) (car mpnt)))
;  (if (< dx 0)                                          ;base line�� x�����ν�
;    (setq xsgn -1)
;    (setq xsgn 1)
;  ) ;of if
;
;  (setq dy (- (cadr p3) (cadr mpnt)))
;  (if (<  dy 0)                                         ;base line�� y�����ν�
;    (setq ysgn -1)
;    (setq ysgn  1)
;  ) ;of if

   (cond                                                 ;Marking�� ����
     ((= dir 1) (setq xsgn 1))
     ((= dir 2) (setq ysgn 1))
     ((= dir 3) (setq xsgn -1))
     ((= dir 4) (setq ysgn -1))
   ) ;of cong

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line�� ����

;  (if (> (abs dx) (abs dy))                                             ;�����ֳ� ���ֳ�?
  (if (or (= dir 1) (= dir 3))                                 ;�����ֳ� ���ֳ�?
    (progn
      (setq p4 (list (+ (car mpnt) (* blen xsgn)) (cadr mpnt)))
      (command "LINE" mpnt p4 "")
;      (if (< dx 0)
      (if (< xsgn 0)
        (setq ip p4)
        (setq ip mpnt)
      ) ;of if
      (setq cp (list (+ (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 0)                                   ;textȸ����
    ) ;of progn
    (progn
      (setq p4 (list (car mpnt) (+ (cadr mpnt) (* blen ysgn))))
      (command "LINE" mpnt p4 "")
;      (if (< dy 0)
      (if (< ysgn 0)
        (setq ip p4)
        (setq ip mpnt)
      ) ;of if
      (setq cp (list (- (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* 4 ds)) 0.0))      ;diaǥ�� ��ġ
      (setq txtrot 90)
    ) ;of progn
  ) ;of if


  (command "CIRCLE" cp rc)
  (setvar "CECOLOR" oldclr)

  (setvar "CECOLOR" "WHITE")
;  (setq mk (getstring "\nEnter Marking: "))
  (txtinc mk cp txtrot)
;  (setq dia (getstring "\nEnter Rebar Dia: "))
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (setvar "CECOLOR" oldclr)

  (pop-env)   ;ȯ�溯���� ����

  (princ)


); of defun


;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; �� �ȿ� ö�ٹ�ȣ�� �������ش�.
; �Ѿ���� ��
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT)
  (setq txtl (strlen txt))
  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
        (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
        (progn
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
  ) ;of IF
) ;of DEFUN