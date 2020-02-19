;****************************************************
; Program: SB.LSP
;          Split Bar
;          Yi Suk Jong
;          96/12/12
;****************************************************

(defun C:SB(/
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
    (if (not (new_dialog "SB" dcl_id)) (exit))

    (start_image "sb")                                  ;image ���̱�
    (slide_image
                 0 0
                 (dimx_tile "sb") (dimy_tile "sb")
                 "djdg(ddsb)"
    )
    (end_image)

    (if (= #N1 nil) (setq #N1 13))                      ;�ʱ�ġ ����
    (if (= #P1 nil) (setq #P1 100))
    (if (= #N2 nil) (setq #N2 13))
    (if (= #P2 nil) (setq #P2 100))
    (if (= #EX nil) (setq #EX  50))
    (if (= #M1 nil) (setq #M1 "S"))
    (if (= #D1 nil) (setq #D1 "D16"))
    (if (= #M2 nil) (setq #M2 "S"))
    (if (= #D2 nil) (setq #D2 "D16"))
    (if (= #SCALE nil) (setq #SCALE 20))


    (set_tile "n1" (rtos #N1 2 0))                  ;�ʱ�ġ�� edit box setting
    (set_tile "p1" (rtos #P1 2 0))
    (set_tile "n2" (rtos #N2 2 0))
    (set_tile "p2" (rtos #P2 2 0))
    (set_tile "ex" (rtos #EX 2 0))
    (set_tile "m1" #M1)
    (set_tile "d1" #D1)
    (set_tile "m2" #M2)
    (set_tile "d2" #D2)
    (set_tile "total1" (rtos (+ (* #EX 2) (* #N1 #P1)) 2 0))
    (set_tile "total2" (rtos (+ (* #EX 2) (* #N2 #P2)) 2 0))
    (set_tile "scale" (rtos #SCALE 2 0))

    (action_tile "n1" "(set_n1)")                   ;dialog box Action
    (action_tile "p1" "(set_p1)")
    (action_tile "n2" "(set_n2)")
    (action_tile "p2" "(set_p2)")
    (action_tile "ex" "(set_ex)")
    (action_tile "m1" "(set_m1)")
    (action_tile "d1" "(set_d1)")
    (action_tile "m2" "(set_m2)")
    (action_tile "d2" "(set_d2)")
    (action_tile "scale" "(set_scale)")
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "n1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG

  ;;
  ;; FUNCTION : SET_N1 (n1�� �Է�)
  ;;
  (defun SET_N1 (/ in value)              ;n1 edit_box�� �Է��� ������ ��
    (setq in (get_tile "n1"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "n1" in)
        nil
      ) ;of THEN
      (progn
        (setq #N1 value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ (* #EX 2) (* #N1 #P1)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_P1 (p1�� �Է�)
  ;;
  (defun SET_P1(/ in value)              ;p1 edit_box�� �Է��� ������ ��
    (setq in (get_tile "p1"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "p1" in)
        nil
      ) ;of THEN
      (progn
        (setq #P1 value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ (* #EX 2) (* #n1 #p1)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_N2 (n2�� �Է�)
  ;;
  (defun SET_N2(/ in value)              ;n2 edit_box�� �Է��� ������ ��
    (setq in (get_tile "n2"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "n2" in)
        nil
      ) ;of THEN
      (progn
        (setq #N2 value)
        (set_tile "error" "")
        (set_tile "total2" (rtos (+ (* #N2 #P2) (* 2 #EX)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_P2 (P2�� �Է�)
  ;;
  (defun SET_P2(/ in vlaue)              ;p2 edit_box�� �Է��� ������ ��
    (setq in (get_tile "p2"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "p2" in)
        nil
      ) ;of THEN
      (progn
        (setq #P2 value)
        (set_tile "error" "")
        (set_tile "total2" (rtos (+ (* #N2 #P2) (* 2 #EX)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_EX (ex�� �Է�)
  ;;
  (defun SET_EX(/ in value)              ;g  edit_box�� �Է��� ������ ��
    (setq in (get_tile "ex"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "ex" in)
        nil
      ) ;of THEN
      (progn
        (setq #EX value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ (* #EX 2) (* #N1 #P1)) 2 0))
        (set_tile "total2" (rtos (+ (* #EX 2) (* #N2 #P2)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
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
        (setq #SCL value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_M1 (Mark-1�� �Է�)
  ;;
  (defun SET_M1(/ in value)              ;m1 edit_box�� �Է��� ������ ��
    (setq #M1 (get_tile "m1"))
    T
   ) ;of defun

  ;;
  ;; FUNCTION : SET_D1 (Dia-1�� �Է�)
  ;;
  (defun SET_D1(/ in value)              ;d1 edit_box�� �Է��� ������ ��
    (setq #D1 (get_tile "d1"))
    T
   ) ;of defun

  ;;
  ;; FUNCTION : SET_M2 (Mark-2�� �Է�)
  ;;
  (defun SET_M2(/ in value)              ;m2 edit_box�� �Է��� ������ ��
    (setq #M2 (get_tile "m2"))
    T
   ) ;of defun

  ;;
  ;; FUNCTION : SET_D2 (Dia-2�� �Է�)
  ;;
  (defun SET_D2(/ in value)              ;d2 edit_box�� �Է��� ������ ��
    (setq #D2 (get_tile "d2"))
    T
   ) ;of defun

  ;;;
  ;;; ok��ư�� ������ ��
  ;;;
  (defun do_accept()           ;dialog box�� ������ ���� ��� �Է� ����Ÿ Ȯ��
    (if (and (set_n1) (set_p1) (set_n2) (set_p2) (set_ex) (set_scale))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; �߸��� ���� �Է�?�� ��
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massageâ�� ����ǥ��

    (cond
      ((or (= tile "n1") (= tile "n2"))
        (progn
          (set_tile  tile value)
          (mode_tile tile 2)
        )
      )
      ((or (= tile "p1") (= tile "p2") (= tile "ex"))
        (progn
          (set_tile tile value)
          (mode_tile tile 2)
        )
      )
    ) ;of COND
  )


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

  (setq SCL #SCL                         ;Splitö���� ������
        EX  #EX                          ;��������
        N1  #N1                          ;���ι��� ����
        P1  #P1                          ;���ι��� ����
        N2  #N2                          ;���ι��� ����
        P2  #P2)                         ;���ι��� ����


  (setq scl1 (/ ds scl)                 ; �� scale�� ��(�׸�Ȯ��)
        ex (* ex scl1)                  ; ȯ�����
        p1 (* p1 scl1)                  ; ȯ�����
        p2 (* p2 scl1))                 ; ȯ�����

  (setq l1 (+ (* 2 ex) (* n1 p1))           ;���α���
        l2 (+ (* 2 ex) (* n2 p2)))          ;���α���

  (setq ip (getpoint "\nPick Insert Point"))  ;������ (�׸��� �������)

  (setq ix (car ip)                         ;������ x
        iy (cadr ip))                       ;������ y

  ;;; ����ö�ٹݺ�
  (setvar "CECOLOR" "YELLOW")
  (setq n 0)
  (repeat (1+ n1)
    (command "LINE" (list (+ ix ex (* n p1)) iy)    ;������(��)
                    (list (+ ix ex (* n p1)) (- iy (* 2 ex) (* n2 p2)))
                    "")
    (setq n (1+ n))                                 ;����ö��
  ) ;of repeat

  (if (= (rem n2 2) 0)
    (setq my (- iy (/ l2 2.0) (/ p2 2.0)))  ;����ö�� ��ŷ������ y��ġ
    (setq my (- iy (/ l2 2.0)))
  ) ;of if

  (setq hgap (* ds (+ 7 (* 2.5 3) 5)))      ;ö�ٰ� ��ŷ���� ���򰣰�
  (setq mp1 (list (- (+ ix l1) ex) my)      ;������ ������
        mp2 (list (- ix hgap) my))          ;������ ����(Marking��ġ)
  (setq ssv (ssget "F" (list mp1 mp2)))

  ;;; ����ö�ٹݺ�
  (setq n 0)
  (repeat (1+ n2)
    (command "LINE" (list ix (- iy ex (* n p2))) ;������(��)
                    (list (+ ix (* 2 ex) (* n1 p1)) (- iy ex (* n p2)))
                    "")
    (setq n (1+ n))                         ;����ö��
  ) ;of repeat

  (setvar "CECOLOR" "BYLAYER")

  (if (= (rem n1 2) 0)
    (setq mx (+ ix (/ l1 2.0) (/ p1 2.0)))  ;����ö�� ��ŷ������ x��ġ
    (setq mx (+ ix (/ l1 2.0)))
  ) ;of if

  (setq vgap (* ds 5))                      ;ö�ٰ� ��ŷ���� ��������
  (setq mp3 (list mx (+ (- iy l2) ex))      ;������ �Ʒ���
        mp4 (list mx (+ iy vgap)))           ;������ ����(Marking��ġ)
  (setq ssh (ssget "F" (list mp3 mp4)))


  ;; ����ġ���� �۾�
  (setq scl2 (/ scl ds))                    ;�� scale�� ��
  (setq hdim0 (list ix (- iy l2)))          ;����ġ���� ù��

  (setq pp (f_dh hdim0 ex 1 -1 scl2))
  (setq pp (f_dh pp p1 n1 -1 scl2))
  (setq pp (f_dh pp ex 1 -1 scl2))
  (setq pp (f_dh hdim0 l1 1 -2 scl2))

  ;����ġ���� �۾�
  (setq pp (f_dv (list (+ ix l1) (- iy l2)) ex 1 1 scl2))
  (setq pp (f_dv pp p2 n2 1 scl2))
  (setq pp (f_dv pp ex 1 1 scl2))
  (setq p0 (f_dv (list (+ ix l1) (- iy l2)) l2 1 2 scl2))

  ;����ö�� ��ŷ
  (F_B5 ssv mp1 mp2 1 #M2 #D2)
  (F_B5 ssh mp3 mp4 1 #M1 #D1)

  (setq *error* oer seterr nil)

  (pop-env)                             ;

) ;of defun


;******************************************
; Program : F_DHS
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
;      TXT1 : �����Է��ϰ���� text
; �����ִ� �� - ���� ��ǥ
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
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
                                  (list 0.0 (+ (* dy sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds 2.5)) 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)
          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM��� ����
        ) ;of progn THEN
        (progn                                 ;ġ�������� ���� text ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (or (= txt1 nil)
              (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0))))
        (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM��� ����
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*******************************************************
; Function : F_DVS
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
;      TXT1 : �����Է��ϰ���� text
; �����ִ� �� - ���� ��ǥ
;******************************************************

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th 2.5                                        ;textũ�� = 2.5
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
                                  (list (- (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy " ")               ;DIM��� ����
        ) ;of progn THEN
        (progn                                  ;text�� ������ ���� ����
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM��� ����
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (or (= txt1 nil)
              (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0))))
        (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM��� ����
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
;     /--/--/--Uc
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

  (setvar "CMDECHO" 0)                        ;ȯ�溯�� ���� ��ɸ޾Ƹ� ����
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
  (print nssent)
  (princ "Entity Found")                              ;LINE�̳� ARC�� ��ΰ�?

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

  (setq blen (+ (* ds 7) (* 4 ds 2.5)))                     ;base line�� ����

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
  (command "TEXT" diaxy (* 2.5 ds) txtrot (strcase dia))
  (setvar "CECOLOR" oldclr)

  (pop-env)

  (princ)
) ; defun

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
        (command "TEXT" "M" ipnt (* 2.5 ds) TXTROT (strcase txt))
        (progn
          (command "TEXT" "C" ipnt (* 2.5 ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* 2.5 ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (command "TEXT" "M" ipnt (* 2.5 ds) TXTROT (strcase txt))
  ) ;of IF
) ;of DEFUN


