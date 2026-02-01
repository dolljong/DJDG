;******************************
; Program : RWALL
;           ReWALL drawing
;           Suk-Jong Yi
;           98/10/12
;******************************
; 옹벽그리기
;******************************

(defun C:DDRWALL( /
  ds sc x1 n1 p1 x2 n2 p3 a1 a2 a3 a4 b1 b2 ipnt ix iy
  l1 h1 l2 lx ly pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 count dpt
  pt11 pt12 pt13 pt14 sbx sby pt15 pt16 cnt scl2 pp vgap mx fpt1 fpt2
  mp ss wp1 wp2
)

  ;;
  ;; error routine
  ;;
  (defun SETERR(s)                                      
  ;If an error (CTRL-C) occurs when this command is active.
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ "\n*cancel*")
        (princ (strcat "\nError " s))
      ) ;of if
    ); of If
  ;Restore previous error handler

;    (setq *error* oer seterr nil)

    (princ)
  ); of SETERR

  ;;
  ;; Function: RW_DIALOG (Input using Dialog box)
  ;;
  (defun RW_DIALOG (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                  ;dialog
    (if (not (new_dialog "RWALL" dcl_id)) (exit))

    (start_image "rwall")                                  ;image 
    (slide_image
                 0 0
                 (dimx_tile "rwall") (dimy_tile "rwall")
                 "djdg(ddrwall)"
    )
    (end_image)

    (if (= #B1 nil) (setq #B1 0.8))
    (if (= #B2 nil) (setq #B2 0.7))                      
    (if (= #B3 nil) (setq #B3 0.3))
    (if (= #B4 nil) (setq #B4 2.8))
    (if (= #B5 nil) (setq #B5 0.3))
    (if (= #H1 nil) (setq #H1 6.3))
    (if (= #H2 nil) (setq #H2 0.2))
    (if (= #H3 nil) (setq #H3 0.5))
    (if (= #FSLOP nil) (setq #FSLOP "1"))
    (if (= #DIM nil) (setq #DIM "1"))
    (if (= #YSCALE nil) (setq #YSCALE 1.00))


    (set_tile "b1" (rtos #B1 2 3))
    (set_tile "b2" (rtos #B2 2 3))                  ;edit box setting
    (set_tile "b3" (rtos #B3 2 3))
    (set_tile "b4" (rtos #B4 2 3))
    (set_tile "b5" (rtos #B5 2 3))
    (set_tile "h1" (rtos #H1 2 3))
    (set_tile "h2" (rtos #H2 2 3))
    (set_tile "h3" (rtos #H3 2 3))
    (set_tile "fslop" #FSLOP)
    (set_tile "dim" #DIM)
    (set_tile "yscale" (rtos #YSCALE 2 3))
    (set_tile "totalb" (strcat "Total Width: " (rtos (+ #b1 #b2 #b3 #b4) 2 3)))
    (set_tile "totalh" (strcat "Total Height: " (rtos (+ #h1 #h2 #h3) 2 3)))


    (action_tile "b1"     "(set_val $key)")
    (action_tile "b2"     "(set_val $key)")               ;dialog box Action
    (action_tile "b3"     "(set_val $key)")
    (action_tile "b4"     "(set_val $key)")
    (action_tile "b5"     "(set_val $key)")
    (action_tile "h1"     "(set_val $key)")
    (action_tile "h2"     "(set_val $key)")
    (action_tile "h3"     "(set_val $key)")
    (action_tile "fslop"  "(set_val $key)")
    (action_tile "yscale" "(set_val $key)")
    (action_tile "dim"    "(set_val $key)")
    (action_tile "dfile"  "(set_dfile)")
    (action_tile "reset"  "(do_reset)")
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "b1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun rw_dialog
  
  

  ;;;
  ;;; dialog box
  ;;;
  (defun SET_VAL (key / in value)              ;edit_box
    (setq in (get_tile key))
    (cond
      ((= key "b1")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B1 value)
            (set_tile "error" "")
            (set_tile "totalb" (strcat "     Toral Width: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b1
      ((= key "b2")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B2 value)
            (set_tile "error" "")
            (set_tile "totalb" (strcat "     Toral Width: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b2
      ((= key "b3")
        (setq value (atof in))
        (setq #B3 value)
        (set_tile "error" "")
        (set_tile "totalb" (strcat "     Toral Width: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
        T
      ) ;of key=b3
      ((= key "b4")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B4 value)
            (set_tile "error" "")
            (set_tile "totalb" (strcat "     Toral Width: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b4
      ((= key "b5")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B5 value)
            (set_tile "error" "")
;            (set_tile "totalb" (strcat "     �ѳ���: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b5
      ((= key "h1")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #H1 value)
            (set_tile "error" "")
            (set_tile "totalh" (strcat "     Toral Height: " (rtos (+ #H1 #H2 #H3) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=h1
      ((= key "h2")
        (setq value (atof in))
        (setq #H2 value)
        (set_tile "error" "")
        (set_tile "totalh" (strcat "     Toral Height: " (rtos (+ #H1 #H2 #H3) 2 3)))
        T
      ) ;of key=h2
      ((= key "h3")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #H3 value)
            (set_tile "error" "")
            (set_tile "totalh" (strcat "Toral Height: " (rtos (+ #H1 #H2 #H3) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=h3
      ((= key "yscale")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #YSCALE value)
            (set_tile "error" "")
;            (if (/= #TSCALE 1.0) (set_tile "dim 1))
            T
          ) ;of ELSE
        ) ;of if
      ) ;sub cond
     ((= key "dim")
       (mode_tile "dim" 2)
       (setq in (get_tile "dim"))
       (setq #DIM in)
     ) ;of subcond
     ((= key "fslop")
       (mode_tile "fslop" 2)
       (setq in (get_tile "fslop"))
       (setq #FSLOP in)
     ) ;of subcond
    ) ;of cond
  ) ;of defun

  

  (defun set_dfile()
    (if (= #DFILE nil)                                  ;���ϸ��� nil�̸� �Է¹���
      (setq #DFILE (getfiled "Data file" (getvar "DWGPREFIX") "" 2))
      (setq #DFILE (getfiled "Data file" (getpath #DFILE)     "" 2))
    )
    (if  (/= #DFILE nil)
      (progn
        (set_tile "path_name" #DFILE)
        (setq opf (open #DFILE "r"))                          ;file open
        (if opf
          (progn                                          ;file�� �ִ� ���
            (setq title (read-line opf))                  ;ù��(Ÿ��Ʋ)�� �д´�
            (setq ch (read-line opf))                     ;��°��(B)�� �д´�
            (setq inline (data-in ch))                    ;���� ������ ,�������� ����
            (setq #B1 (atof (nth 0 inline))
                  #B2 (atof (nth 1 inline))
                  #B3 (atof (nth 2 inline))
                  #B4 (atof (nth 3 inline))
                  #B5 (atof (nth 4 inline)))
            (setq ch (read-line opf))                     ;��°��(B)�� �д´�
            (setq inline (data-in ch))                    ;���� ������ ,�������� ����
            (setq #H1 (atof (nth 0 inline))
                  #H2 (atof (nth 1 inline))
                  #H3 (atof (nth 2 inline)))
            (close opf)                                           ;file close
          ) ;of progn
          (princ "\nFile not found")                          ;file�� ���� ���
        ) ;of if
        (set_tile "b1" (rtos #B1 2 3))
        (set_tile "b2" (rtos #B2 2 3))                  ;���� ������ edit box setting
        (set_tile "b3" (rtos #B3 2 3))
        (set_tile "b4" (rtos #B4 2 3))
        (set_tile "b5" (rtos #B5 2 3))
        (set_tile "h1" (rtos #H1 2 3))
        (set_tile "h2" (rtos #H2 2 3))
        (set_tile "h3" (rtos #H3 2 3))
      );progn
    );if
  );of defun

  

  ;;;
  ;;; ok��ư�� ������ ��
  ;;;
  (defun do_accept()           ;dialog box�� ������ ���� ��� �Է� ����Ÿ Ȯ��
    (if (and (set_val "b1") (set_val "h1")
             (set_val "b2") (set_val "h2")
             (set_val "b3") (set_val "h3")
             (set_val "b4")
             (set_val "b5")
             (set_val "yscale"))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; �߸��� ���� �ԷV�� ��
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massageâ�� ����ǥ��

    (if (or (= tile "b1") (= tile "b2") (= tile "b3")
            (= tile "b4") (= tile "b5") (= tile "h1")
            (= tile "h2") (= tile "h3")
            (= tile "yscale"))
      (progn
        (set_tile  tile value)
        (mode_tile tile 2)
      ) ;of THEN
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; 초기화 버튼을 눌렀을 때
  ;;;
  (defun do_reset()
    (setq #B1 0.8)
    (setq #B2 0.7)
    (setq #B3 0.3)
    (setq #B4 2.8)
    (setq #B5 0.3)
    (setq #H1 6.3)
    (setq #H2 0.2)
    (setq #H3 0.5)
    (setq #FSLOP "1")
    (setq #DIM "1")
    (setq #YSCALE 1.00)
    (set_tile "b1" (rtos #B1 2 3))
    (set_tile "b2" (rtos #B2 2 3))
    (set_tile "b3" (rtos #B3 2 3))
    (set_tile "b4" (rtos #B4 2 3))
    (set_tile "b5" (rtos #B5 2 3))
    (set_tile "h1" (rtos #H1 2 3))
    (set_tile "h2" (rtos #H2 2 3))
    (set_tile "h3" (rtos #H3 2 3))
    (set_tile "fslop" #FSLOP)
    (set_tile "dim" #DIM)
    (set_tile "yscale" (rtos #YSCALE 2 3))
    (set_tile "totalb" (strcat "Toral Width: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
    (set_tile "totalh" (strcat "Toral Height: " (rtos (+ #H1 #H2 #H3) 2 3)))
    (set_tile "error" "")
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

  ;(push-env)                            ;ȯ�溯�� ����

  (setq ds (getvar "DIMSCALE"))         ;�����ϰ���Ƴ���

  (rw_dialog)                           ;dialog�ڽ��� �Է¹ޱ�

 (setq p (getpoint "\ninsert point: ")) ;������ �Է� ����

 (f_rwall p
         #H1
         #H2
         #H3
         #B1
         #B2
         #B3
         #B4
         #B5
         (if (= #FSLOP "1") 0.02 0)    ;���� 1:0.02 slop����
         #YSCALE                        ;yscale
         #DIM)                          ;dimensionǥ�� ����

  (princ "O.K!")

  (pop-env)

) ;of defun


;---------------------------------
; PROGRAM : RWALL
;           draw ReWALL
;           Yi Suk jong
;           98/10/13
;---------------------------------
; 다이얼로그 박스나
; 해석프로그램 데이타 파일을 이용하여
; 옹벽단면도를 그려줌
;---------------------------------

(defun c:RWALL(
/ answ fn opf title ch inline B1 B2 B3 B4 H1 H2 H3)
;  (setq llist nil)                                      ;빈 line-list 만듬

  (initget "File Entity")
  (setq answ (getkword "\nFile/Entity <File>: "))

  (if (or (= answ nil) (= answ "File"))                 ;return입력이나 F입력시
    (progn
      (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name�Է�
      (setq opf (open fn "r"))                          ;file open
      (if opf
        (progn                                          ;file�� �ִ� ���
          (setq title (read-line opf))                  ;ù��(Ÿ��Ʋ)�� �д´�
          (setq ch (read-line opf))                     ;��°��(B)�� �д´�
          (setq inline (data-in ch))                    ;���� ������ ,�������� ����
          (setq B1 (atof (nth 0 inline))
                B2 (atof (nth 1 inline))
                B3 (atof (nth 2 inline))
                B4 (atof (nth 3 inline))
                B5 (atof (nth 4 inline)))
          (setq ch (read-line opf))                     ;��°��(B)�� �д´�
          (setq inline (data-in ch))                    ;���� ������ ,�������� ����
          (setq H1 (atof (nth 0 inline))
                H2 (atof (nth 1 inline))
                H3 (atof (nth 2 inline)))
          (close opf)                                           ;file close
        ) ;of progn
        (princ "\nFile not found")                          ;file�� ���� ���
      ) ;of if
    ) ;of progn THEN
    (progn                                                  ;Entity�� �Է��� ���
      (setq tent (ssget '((0 . "TEXT"))))                   ;text�� select
      (setq tn    (sslength tent)                           ;text����
            count               0)                          ;ù text����
      (repeat tn                                            ;text������ŭ �ݺ�
        (setq ch (cdr (assoc 1 (entget (ssname tent count)))))  ;text����
        (princ (chr 13))                                        ;�۾��߸޼���
        (princ (1+ count))
        (princ " Line Processing...")
        (setq inline (data-in ch))
        (setq lst (list                                     ;���� data�� ���� data��
                     (strcase (car (sp-trunc (nth 0 inline))))   ;ö�ٹ�ȣ
                     (strcase (car (sp-trunc (nth 1 inline))))   ;ö������
                     (atof (nth 2 inline))                       ;ö�ٱ���
                     (atoi (nth 3 inline))))                     ;ö�ٰ���
        (setq llist (append llist (list lst)))                   ;llist�� �߰�
        (setq count (1+ count))                                  ;���� text��
      ) ;of repeat
    ) ;of progn ELSE
  ) ;of IF

  (setq FSLOP (getreal "\n��ü���� slop <0.02>: "))           ;��ü���� slop
  (if (= FSLOP nil) (setq FSLOP 0.02))

  (setq YS (getreal "\nY���� SCALE <1.0>: "))                 ;Y���� SCALE
  (if (= YS nil) (setq YS 1.0))

  (initget "Yes No")
  (setq DIM (getkword "\nġ������ �����Ͻðڽ��ϱ�? <Y/n>: ")) ;ġ�������Կ���
  (if (or (= DIM nil) (= DIM "Yes")) (setq DIM "1") (setq DIM "0"))

 (setq p (getpoint "\ninsert point: "))             ;������(��ü������)

 (f_rwall p H1 H2 H3 B1 B2 B3 B4 B5 FSLOP YS DIM)   ;�˺��׸��� �Լ� ��

);defun

;-------------------
; test용 프로그램
;-------------------

(defun c:rw()
 (setq p (getpoint "\ninsert point: "))
 (f_rwall p
         6.3 ;H1
         0.2 ;H2
         0.5 ;H3
         0.8 ;B1
         0.7 ;B2
         0.0 ;B3
         3.1 ;B4
         0.3 ;B5
         0.02 ;FSLOP
         1.0
         "1")
) ;defun

;***********************************
; Function : F_RWALL
;            RWALL drawing
;            Suk-Jong Yi
;            98/10/12
;***********************************
(defun F_RWALL( IP H1 H2 H3
                   B1 B2 B3 B4 B5 FSLOP YS DIM
/               )

  (setq ix (car ip)
        iy (cadr ip))

  (setq h1 (* h1 ys 1000)  b1 (* b1 1000)
        h2 (* h2 ys 1000)  b2 (* b2 1000)
        h3 (* h3 ys 1000)  b3 (* b3 1000)
        bh (* 100 ys)      b4 (* b4 1000)
                           b5 (* b5 1000)
        ht (+ h1 h2 h3)
        bt (+ b1 b2 b3 b4))

  (setq p1 ip                                           ;��ü������
        p2 (list (- ix (/ (* h1 fslop) ys)) (- iy h1))         ;��ü�����ϴ�
        p3 (list (- (car p2) b1)     (- (cadr p2) h2))  ;�ձ�������
        p4 (list (car p3)            (- (cadr p3) h3))  ;�ձ������ϴ�
        p5 (list (+ (car p3) bt)     (cadr p4))         ;�ޱ��ĸ��ϴ�
        p6 (list (car p5)            (cadr p3))         ;�ޱ��ĸ���
        p7 (list (- (car p6) b4)     (cadr p2))         ;��ġ�ϴ�
        p8 (list (- (car p7) b3)     (+ (cadr p7) b3))  ;��ġ���
        p9 (list (+ ix b5)           iy)                ;��ü�����

        bp1 (list (- (car p4) 100) (cadr p4))           ;���ʾձ����
        bp2 (list (car bp1) (- (cadr bp1) bh))          ;���ʾձ��ϴ�
        bp3 (list (+ (car p5) 100) (cadr bp2))          ;���ʵޱ����
        bp4 (list (car bp3) (+ (cadr bp3) bh))          ;���ʵޱ��ϴ�
  ) ;setq

  (command "LINE" p1 p2 p3 p4 p5 p6 p7 p8 p9 ip "")     ;�˺���ü
  (command "LINE" p4 bp1 bp2 bp3 bp4 p5 "")             ;������ũ��Ʈ

  ;;;;; 치수선 처리
  (if (= DIM "1")
    (progn
      (if (> Fslop 0)                                    ;��ü������⼱
        (setq pp (f_dh (list (car p2) (cadr ip))
                       (- (car p1) (car p2)) 1 1 nil))
        (setq pp ip)
      );if
      (setq pp (f_dh pp b5 1 1 nil))                     ;��ü��� �β�
      (setq pp (f_dh pp (- (car p8) (car p9)) 1 1 nil))  ;��ü��� ����β�

      (setq pp (f_dh (list (car p3) (cadr p2)) b1 1 1 nil)) ;�ձ�
      (setq pp (f_dh pp b2 1 1 nil))                        ;��ü�β�
      (if (> b3 0)                                          ;��ġ
        (setq pp (f_dh pp b3 1 1 nil))
      );if
      (setq pp (f_dh pp b4 1 1 nil))                         ;�ޱ�

      (f_dh p4 bt 1 -1 nil)                                  ;������ü

      (setq pp (f_dv p4 h3 1 -1 nil))                        ;�ձ��ǵβ�
      (if (> h2 0)                                           ;�ձ�������
        (setq pp (f_dv pp h2 1 -1 nil))
      );if
      (setq pp (f_dv pp h1 1 -1 nil))                        ;��ü����
      (setq pp (f_dv p4 ht 1 -2 nil))                        ;��ü��ü����

 ;     (setq pp (f_dv pp h5 1 1 nil))
 ;     (f_dv pp h4 1 1 nil)
 ;     (f_dv p9 ht 1 2 nil)
 ;     (setq pp (f_dv p10 h3 1 -1 nil))
 ;     (setq pp (f_dv pp h2 1 -1 nil))
 ;     (f_dv pp h1 1 -1 nil)
 ;
     ) ;of progn
   ) ;of if

);of defun F_ABUT

;******************************************
; Program : F_DH
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29
;******************************************
; 수평치수선을 함수로 처리해준다.
; 넘어오는 변수
;        SP : 시작점
;       DST : 거리
;         N : 반복갯수
;        UD : Up/DOWN (절대값은 LEVEL)
; 돌려주는 값 - 끝점 좌표
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

  (if (> ud 0)                                              ;�� �Ʒ�
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;ġ���� ��ġ ��� (���밪)

  (setq next (* dst n))                                     ;���������� �������� �Ÿ�

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep ��ġ���

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;ġ���� ��ġ

  (setq dx (distance sp ep))                          ;�Ÿ� ���

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;����� �ɼ��� ���
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos n 2 0))                          ;���� ���� ���
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000�̸��� ��
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000�̻��� ��
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
      (if (= txt1 nil) (setq txt1 txt))                  ;�����Է½� �� text�� ��
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM���� ����
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*********************************
; Function : F_DV
;           Fuction Dimension Vertical
;           Jong-Suk Yi
;           96/7/1
;*********************************

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th (getvar "DIMTXT")                          ;textũ�� =dimtxt
        dim_gap 10.0)                                 ;ġ���� ����
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (> lr 0)                                        ;����/������
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))

  (setq next (* dst n))                                 ;�������� �Ÿ�

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;������ ����

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ

  (setq dy (distance sp ep))                          ;�� ���� �Ÿ�

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                   ;������ ���� �Է�
      (setq divn (rtos n 2 0))                          ;���� �������
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;������ ���̰� 1000�̸���
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;������ ���̰� 1000�̻��
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
      (if (= txt1 nil) (setq txt1 txt))                    ;�����Է½� �� text�� ��
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM���� ����
    ) ;of progn ELSE
  ) ;of if
  ep
) ;defun

;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************

(defun DATA-IN(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;�Ѿ�� ���ڿ�
   (setq strl (strlen arg1))                    ;�Ѿ�� ���ڿ��� ����
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;������� ��ġ
   (setq nchr 1)                                ;���⹮�� ����
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;���� �Ѱ�
      (if (or (= subs ",") (= subs ""))         ;���� ���ڰ� ,�̰ų� ���϶�
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;������ġ����
            (if (= rslt nil)
               (setq rslt (list lst))                  ;�������� �������
               (setq rslt (append rslt (list lst)))    ;���������� �߰�
            ) ;of if
            (setq nchr 0)                       ;���ⰹ�� �ٽ� 0����
            (setq strt (1+ count))              ;���� ��������� �������ڷ�
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;���� ���ڷ�
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;���� ���� �Ѱ� ����
   ) ;of repeat
   (setq arg1 rslt)                             ;������ ����
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; 입력문자열의 앞,뒤에 있는 빈칸을 짤라낸다.
; 리턴값은
; (짤라낸 문자열,
;  첫 문자 나오는 위치,
;  마지막 문자 나오는 위치,
;  숫자인가?)
;***************************************************************************

(defun SP-TRUNC(txt
/               txtl        frntn       backn       txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if

);of defun


;**************************************************************************
; Function : GETPATH
;            GET PATH
;            By Suk-Jong Yi
;            1999/3/6
;**************************************************************************
; 어떤 Fullpath filename에서 path만 축출해낸다.
; 넘어오는값  : fullpath file name
; 리턴값은    : path명
;***************************************************************************

(defun GETPATH(fpath / num_c count )
  (setq num_c (strlen fpath))                       ;글자수
  (setq count num_c)                                ;맨마지박 글자부터
  (while (and (> count 1)
              (/= (substr fpath count 1) "\\"))
    (setq count (1- count))
  ); while

  (substr fpath 1 count)

);defun
