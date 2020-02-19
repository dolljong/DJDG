;****************************************************************************
; Program : PLTA1
;           PLoT A1
;           By Suk-Jong Yi
;           1996/6/24
;****************************************************************************
; ���鳻�� �ִ� ��� Border�� ��� (A1��)
; Device  : ��ɳ����� ���� �̸� ����
; Scale   : Fit / Factor �ΰ��� �ɼ�
;           (Factor�� �������� �� DIMSCALE �����ڵ��ν�)
; plt�̸� : Border�� �Ѱ��� ��� - DWG name�� ����
;           Border�� �ΰ� �̻��� ��� - DWG���� �̸��� ������ ���ڸ� ��ȣ��
;****************************************************************************

(defun C:PLTA1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name�Է�
  (setq opf (open fn "r"))                          ;file open
  (if opf
    (progn
      (setq count 1)
      (while (setq dwgn (read-line opf))
        (command "OPEN" dwgn)
;        (PUSH-ENV)                                        ;ȯ�溯�� ����
        (setq ds (getvar "DIMSCALE"))                     ;scale ������

;        (setq dwgn (getvar "DWGNAME"))
        (setq f_list (list (cons 0 "INSERT") (cons 2 "BORDER*")))  ;filter list
        (setq ss_lst (ssget "X" f_list))                          ;entity ����
        (setq ss_num (sslength ss_lst))                   ;���õ� entity����

        (setq index 0)
        (repeat ss_num                        ;���õ� border ������ŭ �ݺ�
          (if (= index 0)
            (setq pltn dwgn)
            (progn
              (if (<= (strlen dwgn) 6)
                (setq pltn1 dwgn)
                (setq pltn1 (substr dwgn 1 6))
              ) ;of IF

              (if (<= index 9)
                (setq pltn (strcat pltn1 "0" (itoa index)))
                (setq pltn (strcar pltn1 (itoa index)))
              ) ;of IF
            ) ;of progn
          ) ;of IF
          (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity����
          (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
          (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border�� scale factor
          (setq up_left (list (- (car ipnt) (* 5 i_scale))        ;border�� ���� ����
                              (+ (cadr ipnt) (* 570 i_scale))))
          (setq low_right (list (+ (car ipnt) (* 800 i_scale))    ;border�� ���� �Ʒ���
                                (- (cadr ipnt) (* 5 i_scale))))
          (setvar "CMDDIA" 0)                                     ;command echo OFF
          (if (= index 0)
            (progn
              (setq pnts (strcat (rtos (car up_left) 2 3)
                             "," (rtos (cadr up_left) 2 3)
                             " " (rtos (car low_right) 2 3)
                             "," (rtos (cadr low_right) 2 3)))
              (setq scl (strcat "1=" (rtos ds 2 3)))
              (setq scr (open "a1.scr" "w"))
              (write-line "PLOT W" scr)
              (write-line pnts scr)
              (write-line "Y" scr)
              (write-line "N" scr)
              (write-line " " scr)
              (write-line " " scr)
              (write-line " " scr)
              (write-line " " scr)
              (write-line scl scr)
              (write-line pltn scr)
              (close scr)
              (command "SCRIPT" "A1")
            ) ;of THEN
            (command "PLOT" "W" up_left low_right "N" pltn)       ;change anything
          ) ;of IF
          (setvar "CMDDIA" 1)                                     ;command echo ON
          (princ pltn) (princ " is Plotted") (terpri)
          (setq index (1+ index))                                 ;���� border��
        ) ;of repeat
      ) ;of WHILE
    ) ;progn
  ) ;of IF

  (POP-ENV)                                                 ;ȯ�溯�� ����
  (princ)
) ;of defun



