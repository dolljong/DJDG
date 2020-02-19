;************************************
; Program : PIER
;           draw PIER
;           Suk-Jong Yi
;           96/7/18
;************************************
; ���� �Ϲݵ� �׸���
;************************************

(defun C:PIER(/
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
      ;; Function: SPLICE_DIALOG (Dialog box�� �Է¹ޱ�)
      ;;
      (defun SPLICE_DIALOG (/
                           dcl_id
      )
        (setq dcl_id (load_dialog "DJDG"))                ;dialogȣ��
        (if (not (new_dialog "PIER" dcl_id)) (exit))

        (start_image "dd_pier")                           ;image ���̱�
        (princ (dimx_tile "dd_pier"))
        (princ (dimy_tile "dd_pier"))
        (slide_image
                     0 0
                     (dimx_tile "dd_pier") (dimy_tile "dd_pier")
                     "djdg(ddpier)"
        )
        (end_image)

        (if (= #BC nil) (setq #BC 2.5))                       ;�ʱ�ġ ����
        (if (= #C1 nil) (setq #C1 2.5))                       ;�ʱ�ġ ����
        (if (= #C2 nil) (setq #C2 1.5))
        (if (= #PD nil) (setq #PD 2.5))
        (if (= #PL nil) (setq #PL 5.0))
        (if (= #FB nil) (setq #FB 8.0))
        (if (= #FH nil) (setq #FH 2.0))
        (if (= #YS nil) (setq #YS 1))
        (if (= #SCOPING nil) (setq #SCOPING 3))
        (if (= #RCOPING nil) (setq #RCOPING "0"))

        (set_tile "bc"  (rtos #BC 2 3))                      ;�ʱ�ġ�� edit box setting
        (set_tile "c1"  (rtos #C1 2 3))                      ;�ʱ�ġ�� edit box setting
        (set_tile "c2"  (rtos #C2 2 3))
        (set_tile "pd"  (rtos #PD 2 3))
        (set_tile "pl"  (rtos #PL 2 3))
        (set_tile "fb"  (rtos #FB 2 3))
        (set_tile "fh"  (rtos #FH 2 3))
        (set_tile "ys"  (rtos #YS 2 3))
        (set_tile "rcoping" #RCOPING)
        (set_tile "scoping"  (rtos #SCOPING 2 3))

        (action_tile "bc" "(set_bc)")                       ;dialog box Action
        (action_tile "c1" "(set_c1)")                       ;dialog box Action
        (action_tile "c2" "(set_c2)")
        (action_tile "pd" "(set_pd)")
        (action_tile "pl" "(set_pl)")
        (action_tile "fb" "(set_fb)")
        (action_tile "fh" "(set_fh)")
        (action_tile "ys" "(set_ys)")
        (action_tile "rcoping" "(set_rcoping)")
        (action_tile "scoping" "(set_scoping)")
        (action_tile "dim" "(set_dim)")
        (action_tile "accept" "(do_accept)")
        (action_tile "cancel" "(do_cancel)")
        (mode_tile "c1" 2)
        (if (= #RCOPING "0")
          (mode_tile "scoping" 1)
          (mode_tile "scoping" 0)
        ) ;of if
        (start_dialog)
        (unload_dialog dcl_id)
      ) ;of defun SPLICE_DIALOG

      ;;
      ;; FUNCTION : SET_C1 (c1�� �Է�)
      ;;
      (defun SET_C1 (/ in value)              ;n1 edit_box�� �Է��� ������ ��
        (setq in (get_tile "c1"))
        (setq value (atof in))                ;string�� ������
        (if (= value 0)                            ;0�� �ԷµǸ�
          (progn
            (do_error "c1" in)
            nil
          ) ;of THEN
          (progn
            (setq #C1 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_BC (bc�� �Է�)
      ;;
      (defun SET_BC (/ in value)              ;bc edit_box�� �Է��� ������ ��
        (setq in (get_tile "bc"))
        (setq value (atof in))                ;string�� ������
        (if (= value 0)                            ;0�� �ԷµǸ�
          (progn
            (do_error "bc" in)
            nil
          ) ;of THEN
          (progn
            (setq #BC value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_C2 (c2�� �Է�)
      ;;
      (defun SET_C2(/ in value)              ;p1 edit_box�� �Է��� ������ ��
        (setq in (get_tile "c2"))
        (setq value (atof in))              ;string�� ������
        (if (= value 0)                     ;���� 0�� ���
          (progn
            (do_error "c2" in)
            nil
          ) ;of THEN
          (progn
            (setq #C2 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_E1 (e1�� �Է�)
      ;;
      (defun SET_PD(/ in value)              ;e1 edit_box�� �Է��� ������ ��
        (setq in (get_tile "pd"))
        (setq value (atof in))
        (if (= vlaue 0)
          (progn
            (do_error "pd" in)
            nil
          ) ;of THEN
          (progn
            (setq #PD value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_PL2 (pl�� �Է�)
      ;;
      (defun SET_PL(/ in value)              ;n2 edit_box�� �Է��� ������ ��
        (setq in (get_tile "pl"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "pl" in)
            nil
          ) ;of THEN
          (progn
            (setq #PL value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_FB (FB�� �Է�)
      ;;
      (defun SET_FB(/ n vlaue)              ;p2 edit_box�� �Է��� ������ ��
        (setq in (get_tile "fb"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "fb" in)
            nil
          ) ;of THEN
          (progn
            (setq #FB value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_FH (e2�� �Է�)
      ;;
      (defun SET_FH(/ n value)              ;e2 edit_box�� �Է��� ������ ��
        (setq in (get_tile "fh"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "fh" in)
            nil
          ) ;of THEN
          (progn
            (setq #FH value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_YS (ys�� �Է�)
      ;;
      (defun SET_YS(/ n value)              ;ys edit_box�� �Է��� ������ ��
        (setq in (get_tile "ys"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "ys" in)
            nil
          ) ;of THEN
          (progn
            (setq #YS value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_SCOPING (scoping�� �Է�)
      ;;
      (defun SET_SCOPING(/ n value)              ;scoping edit_box�� �Է��� ������ ��
        (setq in (get_tile "scoping"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "scoping" in)
            nil
          ) ;of THEN
          (progn
            (setq #SCOPING value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;;round coping�ɼ��� �������� ���
      ;;
      (defun set_rcoping( / in )
        (setq in (get_tile "rcoping"))
        (if (= in "1")
          (mode_tile "scoping" 0)
          (mode_tile "scoping" 1)
        ) ;of if
        (setq #RCOPING in)
      ) ;of defun SET-RCOPING

      ;;
      ;;dim�ɼ��� �������� ���
      ;;
      (defun set_dim( / in )
        (mode_tile "dim" 2)
        (setq in (get_tile "dim"))
        (setq #DIM in)
      ) ;of defun SET-DIM

      ;;;
      ;;; ok��ư�� ������ ��
      ;;;
      (defun do_accept()           ;dialog box�� ������ ���� ��� �Է� ����Ÿ Ȯ��
        (if (and (set_c1) (set_c2) (set_pd) (set_pl) (set_fb) (set_fh))
          (done_dialog)
        ) ;of IF
      ) ;of defun


      ;;;
      ;;; �߸��� ���� �Է�?�� ��
      ;;;
      (defun do_error(tile value)
        (set_tile "error" "Invalid input")            ;error massageâ�� ����ǥ��
        (cond
          ((or (= tile "c1") (= tile "c2"))
            (progn
              (set_tile  tile value)
              (mode_tile tile 2)
            )
          )
          ((or (= tile "pd") (= tile "pl") (= tile "fb") (= tile "fh"))
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


      ;;
      ;; MAIN ROUTINE
      ;;

      (setq oer *error* *error* seterr)                 ;����error��ƾ ����

      (push-env)                                        ;ȯ�溯�� ����

      (setq ds (getvar "DIMSCALE"))                     ;�����ϰ�

      (graphscr)

      (splice_dialog )                                    ;dialog�� �Է¹���

      (setq ipnt (getpoint "\nPick first point: "))       ;ù�� �Է�
      (setq ix (car ipnt)                                 ;������ x��ǥ
            iy (cadr ipnt))                               ;������ y��ǥ

      (pier_side ipnt #BC #C1 #C2 #PD #PL #FB #FH #YS #SCOPING)            ;pier�׸���

      (pop-env)                                           ;ȯ�溯�� ����

      (setq *error* oer seterr nil)

      (princ)

) ;of defun



;************************************
; Function : PIER_SIDE
;            draw PIER SIDE view
;            Suk-Jong Yi
;            96/7/18
;************************************
; pier�� ���鵵 �׸���
; IP : Insert Point
; C1 : Coping �߾��� ����
; C2 : Coping ������ ����
; PD : POST DIA
; PL : POST LENGTH
; FB : Footing ��
; FH : Footing ����
; YS : Y-Scale
;************************************

(defun PIER_SIDE(IP BC C1 C2 PD PL FB FH YS SCOPING
/ ip bc c1 c2 pd pl fb fh ys scoping
)

  (setq BC (* BC 1000)
        C1 (* C1 YS 1000)
        C2 (* C2 YS 1000)
        PD (* PD 1000)
        PL (* PL YS 1000)
        FB (* FB 1000)
        FH (* FH YS 1000)
        BH (* 100 YS)              ;base Con'c �β�
        BE  100                    ;base Con'c ������
        SCOPING (* SCOPING YS))

  (if (>= bc pd)                    ;�������� ����Ʈ������ ū���� ���������
    (setq bt bc)
    (setq bt pd)
  ) ;of if
  (command "LINE" (list (- (car ip) (/ bt 2.0)) (cadr ip))
                  (list (+ (car ip) (/ bt 2.0)) (cadr ip)) "")

  (if (= #RCOPING "1") (copingr ip BC c1 c2 SCOPING)  ;coping �׸���
                       (coping  ip BC c1 c2))
  (if (>= bc pd)
    (post (list ix (- iy c1)) pd pl)                  ;post �׸���
    (progn                                            ;post�� ���κ��� ���� ��
      ; ��ռ��׸���
      (command "LINE" (list (- ix (/ pd 2.0)) iy)               ;���������
                      (list (- ix (/ pd 2.0)) (- iy c1 pl)) "")
      (command "LINE" (list (+ ix (/ pd 2.0)) iy)               ;��տ�����
                      (list (+ ix (/ pd 2.0)) (- iy c1 pl)) "")
      ;����߽ɼ�
      (setvar "CECOLOR" "RED")
      (setvar "CELTYPE" "CENTER")
      (command "LINE" (list ix (- iy c1))
                      (list ix (- iy c1 pl)) "")
      (setvar "CECOLOR" "BYLAYER")
      (setvar "CELTYPE" "BYLAYER")
      ;��ն���ǥ��
      (setvar "CECOLOR" "GREEN")
      (setq xsgn 1)
      (repeat 2                                     ;�¿�� �ι� ����
        (setq xsgn (* xsgn -1)
              dx   (/ pd 2.0)                       ;post�� ������
              x    ix)
        (while (>= dx (* ds 0.5))
          (setq dx (/ dx 2.0)
                x (+ x (* dx xsgn)))
          (if (>= (abs (- ix x)) (/ bc 2.0))
            (setq y iy)
            (setq y (- iy c1))
          ) ;of if
          (setq lp1 (list x y)
                lp2 (list (car lp1) (- iy c1 pl)))
          (command "LINE" lp1 lp2 "")
        ) ;of while
      ) ;of repeat
      (setvar "CECOLOR" "BYLAYER")
    ) ;of progn
  ) ;of if
  ; Ǫ�ñ׸���
  (footing (list ix (- iy c1 pl)) FB FH BH BE)      ;footing �׸���

) ;of defun


;------------------------------------------------------
; Function : COPING
;            draw COPING
;            Yi Suk Jong
;            97/2/11
;---------------------------------------------------------
; COPING(IP BC H1 H2)
;        IP            : insert point
;           BC         : coping ��
;              H1      : ��� ����
;                 H2   : �� ����
;---------------------------------------------------------
(defun COPING(IP BC H1 H2
              / IP BC H1 H2 ix iy bc2 p1 p2 p3 p4 oldc dy count y
)

  (setq ix (car ip)
        iy (cadr ip)
        bc2 (/ bc 2.0)                          ; �������� ��
        p1 (list (- ix bc2) iy)                ;�������
        p2 (list (car p1) (- iy h1))           ;�ϴ�����
        p3 (list (+ ix bc2) (cadr p2))         ;�ϴܿ���
        p4 (list (car p3) iy))                 ;��ܿ���

  (command "LINE" p1 p2 p3 p4 "")

  (command "LINE" (list (car p1) (- iy h2))      ;���γ��ܼ�
                  (list (car p4) (- iy h2)) "")

  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "GREEN")
  (setq dy (/ (- h1 h2) 6)
        count 1)

  (repeat 5
    (setq y (- iy h2 (* count dy)))
    (command "LINE"
             (list (- ix bc2) y)
             (list (+ ix bc2) y) "")
    (setq count (1+ count))
  ) ;of repeat
  (setvar "CECOLOR" oldc)

) ;of defun

;------------------------------------------------------
; Function : COPINGR
;            draw COPING Round
;            Yi Suk Jong
;            97/2/11
;---------------------------------------------------------
; COPINGR(IP BC H1 H2 S)
;        IP            : insert point
;           BC         : coping ��
;              H1      : ��� ����
;                 H2   : �� ����
;                    S : ���ΰ��(L/H)
;---------------------------------------------------------
(defun COPINGR(IP BC H1 H2 S
             / IP BC H1 H2 S ix iy bc2 p1 p2 p3 p4 h3 ap1 ap2
               ap3 aent xsgn dx x lp1 lp11 lp2 dy count y
)

  (setq ix (car ip)
        iy (cadr ip)
        bc2 (/ bc 2.0)                          ;�������� ��=���� R
        p1 (list (- ix bc2) iy)                 ;�������
        p2 (list (car p1) (- iy h1))            ;�ϴ�����
        p3 (list (+ ix bc2) (cadr p2))          ;�ϴܿ���
        p4 (list (car p3) iy)                   ;��ܿ���
        h3 (/ bc2 s))                           ;���γ��� ~ round����

  (command "LINE" p1 p2 p3 p4 "")               ;����4����

  (setq ap1 (list (+ ix bc2) (- iy h2 h3))      ;Arc�� 1
        ap2 (list ix (- iy h2))                 ;Arc�� 2
        ap3 (list (- ix bc2) (cadr ap1)))       ;Arc�� 3
  (command "ARC" ap1 ap2 ap3)
  (setq aent (entget (entlast)))                ;Arc ����

  (setvar "CELTYPE" "CENTER")
  (setvar "CECOLOR" "RED")
  (command "LINE" ip ap2 "")
  (setvar "CELTYPE" "BYLAYER")
  (setvar "CECOLOR" "BYLAYER")

  ;--------------------
  ; ������ ���� ǥ��
  ;--------------------
  (setvar "CECOLOR" "GREEN")
  (setq xsgn 1)
  (repeat 2                                     ;�¿�� �ι�
    (setq xsgn (* xsgn -1)
          dx   bc2
          x    ix)
    (while (>= dx (* ds 0.5))
      (setq dx (/ dx 2.0)
            lp1 (list (+ x (* dx xsgn)) iy)
            lp11 (list (car lp1) (- (cadr lp1) 100.0))
            lp2 (car (cross aent lp1 lp11)))
      (setq x (car lp1))
      (command "LINE" lp1 lp2 "")
    ) ;of while
  ) ;of repeat

  ;------------------------------
  ; coping slop�� ǥ��
  ;------------------------------

  (setq dy (/ (- h1 h2) 6)
        count 1)

  (repeat 5
    (setq y (- iy h2 (* count dy)))
    (if (> y (cadr ap1))
      (setq p1 (car (cross aent (list (car ap3) y) (list (car ap1) y)))
            p2 (cadr (cross aent (list (car ap3) y) (list (car ap1) y))))
      (setq p1 (list (- ix bc2) y)
            p2 (list (+ ix bc2) y))
    ) ;of if
    (command "LINE" p1 p2 "")
    (setq count (1+ count))
  ) ;of repeat
  (setvar "CECOLOR" "BYLAYER")

) ;of defun


;************************************
; Function : PIER_FRONT
;            draw PIER FRONT view
;            Suk-Jong Yi
;            96/7/18
;************************************
; pier�� ���鵵 �׸���
; IP : Insert Point
; CL : Coping ����
; C1 : Coping �߾��� ����
; C2 : Coping ������ ����
; PD : POST DIA
; PL : POST LENGTH
; FB : Footing ��
; FH : Footing ����
;************************************
(defun PIER_FRONT(IP CL C1 C2 PD PL FB FH
                / IP CL C1 C2 PD PL FB FH
                  ix iy cl2 pd2 )

  (setq ix (car ip)
        iy (cadr ip)
        cl2 (/ cl 2)
        pd2 (/ pd 2))

  (command "LINE"
           (list (- ix cl2) iy)
           (list (+ ix cl2) iy)
           (list (+ ix cl2) (- iy c2))
           (list (+ ix pd2) (- iy c1))
           (list (- ix pd2) (- iy c1))
           (list (- ix cl2) (- iy c2))
           "C")

  (post (list ix (- iy c1)) pd pl)   ;post �׸���
  (footing (list ix (- iy c1 pl)) FB FH BH BE)   ;footing �׸���

) ;of defun

;*********************************
; Function : FOOTING
;            draw FOOTING
;            Suk-Jong Yi
;            96/7/19
;*********************************
; footing�� �׷��ش�
; IP : Insert Point
; B  : ��
; H  : ����
; BH : Base con'c ����
; BE : Base con'c ��
;*********************************
(defun FOOTING(IP B H BH BE
             / IP B H BH BE
               ix iy b2 y1 y2 )

  (setq ix (car ip)
        iy (cadr ip)
        b2 (/ b 2)
        y1 (- iy h)
        y2 (- iy h BH))

  (command "LINE"
           (list (- ix b2) y1)
           (list (- ix b2) iy)
           (list (+ ix b2) iy)
           (list (+ ix b2) y1) "")

  (command "LINE"
           (list (- ix b2 BE) y1)
           (list (+ ix b2 BE) y1)
           (list (+ ix b2 BE) y2)
           (list (- ix b2 BE) y2)
           "C")
) ;of defun

;***********************************
; Function : POST
;            draw POST
;            Suk-Jong Yi
;            96/7/18
;***********************************
(defun POST(IP PD PL
          / IP PD PL
            ix iy pd2 p1s p1e p2s p2e p3s p3e oldlt oldc)

  (setq ix (car ip)
        iy (cadr ip)
        pd2 (/ pd 2))

  (setq p1s (list (- ix pd2) iy)
        p1e (list (- ix pd2) (- iy pl))
        p2s (list (+ ix pd2) iy)
        p2e (list (+ ix pd2) (- iy pl))
        p3s (list ix iy)
        p3e (list ix (- iy pl)))

  (command "LINE" p1s p1e "")
  (command "LINE" p2s p2e "")
  (setq oldlt (getvar "CELTYPE"))
  (setq oldc (getvar "CECOLOR"))
  (setvar "CELTYPE" "CENTER")
  (setvar "CECOLOR" "RED")
  (command "LINE" p3s p3e "")
  (setvar "CELTYPE" oldlt)
  (setvar "CECOLOR" oldc)

  (hround p3s p3e p1s p1e)
  (hround p3s p3e p2s p2e)

  (princ "POST")
) ;of defun

;**************************************
; Function : HROUND
;            Half ROUNDing
;            Suk-Jong Yi
;            1995. 3. 16
;**************************************

(defun HROUND(p1s p1e p2s p2e /             ;�� ������ ���ִ� Function
sdst edst lpnts lpnte ds p1s p1e oldclr     ;Argument�� �μ��� �������� ����
)

(setq ds (getvar "DIMSCALE"))         ;DIMSCALE�� �˾Ƴ���.

(setq ssdst (distance p1s p2s))     ;ù������ ���������� ��°������ �������Ÿ�
(setq sedst (distance p1s p2e))     ;ù������ ���������� ��°������ �����Ÿ�
(if (< sedst ssdst)                 ;���� �� �Ÿ��� ��������� �������� �ȴ�
  (progn
    (setq tmp p2s)
    (setq p2s p2e)
    (setq p2e tmp)
  ) ;of progn
) ; of if

(setq sdst (distance p1s p2s))    ;ù°���� ���������� ��°���� ���������� �Ÿ�
(setq edst (distance p1e p2e))    ;ù°���� �������� ��°���� �������� �Ÿ�

             ;�� ������ �Ÿ��� 2�̸��̻� �϶��� �� ���� �߰����� ���� �׸���
(setq oldclr (getvar "CECOLOR"))
(setvar "CECOLOR" "GREEN")
(while (or (> sdst (* ds 1.0)) (> edst (* ds 1.0)))
  (setq lpnts (list (/ (+ (car p1s) (car p2s)) 2.0)      ;�� �������� �߰��� X
                    (/ (+ (cadr p1s) (cadr p2s)) 2.0)))  ;                   Y
  (setq lpnte (list (/ (+ (car p1e) (car p2e)) 2.0)      ;�� ������ �߰���   X
                    (/ (+ (cadr p1e) (cadr p2e)) 2.0)))  ;                   Y
  (command "LINE" lpnts lpnte "")     ; �߰����� �׸���
  (setq p1s lpnts)             ;�߰����� �������� ù°���� ���������� �ٲ۴�
  (setq p1e lpnte)             ;�߰����� ������ ù°���� �������� �ٲ۴�
  (setq sdst (distance lpnts p2s))  ;�� ���������� �Ÿ��� ���Ѵ�
  (setq edst (distance lpnte p2e))  ;�� �������� �Ÿ��� ���Ѵ�
) ;of while
(setvar "CECOLOR" oldclr)
) ;of defun HROUND


;-----------------------------------------
; function : CROSS
;            CROSS point of line & circle
;-----------------------------------------
; CROSS(aent lent)
;       aent             : Arc entity����
;            lent        : Line entity����
;-----------------------------------------------
(defun CROSS(aent line_p1 line_p2 /
             a b r sa ea x1 x2 y1 y2 dx dy a1 b1 c1 ang1 ang2 crsp
)

  (setq a (car (cdr (assoc 10 aent))))      ;arc ������ x��ǥ
  (setq b (cadr (cdr (assoc 10 aent))))     ;arc ������ y��ǥ
  (setq r (cdr (assoc 40 aent)))            ;arc�� r
  (setq sa (cdr (assoc 50 aent)))           ;arc�� ���۰�
  (setq ea (cdr (assoc 51 aent)))           ;arc�� ����

  (setq x1 (car line_p1)                    ;line�� ù�� x��ǥ
        x2 (car line_p2)                    ;line�� ���� x��ǥ
        y1 (cadr line_p1)                   ;line�� ù�� y��ǥ
        y2 (cadr line_p2))                  ;line�� ���� y��ǥ

  (setq dx (- x1 x2)                        ;x��
        dy (- y1 y2))                       ;y��

  (cond
    ((= dx 0)                               ;line�� y�࿡ ������ ���
      (setq a1 1.0
            b1 (* -1.0 2.0 b)
            c1 (+ (- (expt (- x1 a) 2) (expt r 2)) (expt b 2))
            y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))
            y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
    ) ;of dx=0
    ((= dy 0)                               ;line�� x�࿡ �����Ѱ��
      (setq a1 1.0
            b1 (* -1.0 2.0 a)
            c1 (+ (- (expt (- y1 b) 2) (expt r 2)) (expt a 2))
            x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))
            x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
    ) ;of dy=0
    ( T                                     ;line�� ������ ���⸦ ���� ���
      (setq c (/ (- y1 y2) (- x1 x2)))
      (setq d (- y2 (* c x2)))

      (setq a1 (+ 1 (* c c)))
      (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))
      (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))

      (setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
      (setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
      (setq y1 (+ (* c x1) d))
      (setq y2 (+ (* c x2) d))
    ) ;of T
  ) ;of cond

  (setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))
  (setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))

  (setq crsp nil)
  (if (inang sa ea ang1)
    (setq crsp (append crsp (list (list x1 y1 0.0)))))

  (if (inang sa ea ang2)
    (setq crsp (append crsp (list (list x2 y2 0.0)))))

  crsp

) ;of defun

(defun inang(a1 a2 a3 /
  a1 a2 a3)

  (if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))
  (if (and (>= a3 a1) (<= a3 a2))
    (- a2 a1)
    nil)

) ;of defun

