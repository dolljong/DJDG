;**********************************
; Program : APLOT
;           Auto PLOT
;           By Suk-Jong Yi
;           1995/7/21, 7/24
;***********************************
; ¬åÈ‚–E border Ÿi Â‰b
;***********************************

(defun C:APLOT( /
                name_lst
)

  (setq bdr_B 815                                   ;border· Í¢
        bdr_H 570                                   ;border· ‘¼·¡
        bn    "BORDER"                             ;§iœâ ·¡Ÿq
        xg    -15                                    ;x gap
        yg    -5)                                   ;y gap

  (setq ds (getvar "DIMSCALE"))                     ;scale ˆtŠÐq

  (setq ss_lst (ssget))                             ;entity ¬åÈ‚
  (setq ss_num (sslength ss_lst))                   ;¬åÈ‚–E entityˆ•®

  (setq index 0)
  (setq bdr_num 0)

  (repeat ss_num                                    ;entityˆ•® eÇq ¤e¥¢
    (setq ent (entget (ssname ss_lst index)))         ;entity ¸÷¥¡
    (if (and (= (cdr (assoc 0 ent)) "INSERT")         ;border·¥ ‰w¶
             (= (substr (cdr (assoc 2 ent)) 1 (strlen bn)) bn))
      (progn
        (redraw (ssname ss_lst index) 3)              ;border¤e¸å
        (setq plt_name (getstring "\nPLT filename(No extension): ")) ;plt name ·³b(ÑÂ¸w¸a ´ô·q)
        (setq plt_name (strcat plt_name ".PLT"))                     ;·³b nameµA”a ÑÂ¸w¸a Âˆa
        (while (or (/= (findfile (strcat (getvar "DWGPREFIX") plt_name)) nil)
                   (is_member plt_name name_lst))     ;—¡BÉ¡Ÿ¡µÁ plt name listµA •··© ·¡Ÿq ¹¥¸ ÑÂ·¥
          (princ "File ") (princ plt_name) (princ " is already exist ")
          (setq plt_name (getstring "PLT filename(No extension): "))
          (setq plt_name (strcat plt_name ".PLT"))
        ) ;of WHILE                                   ;•··©·¡Ÿq ¹¥¸¯¡ ‰­¢ ¤e¥¢
        (if (= bdr_num 0)
          (progn                                      ;Àõ border·¥ ‰w¶
            (setq index_lst (list index))               ;¡y¤å¼ entity·¥ˆaŸi ‹¡´â
            (setq name_lst (list plt_name))             ;plt ·¡Ÿq·e ¢¥ˆaŸi ‹¡´â
          ) ;of THEN
          (progn                                      ;Àõ borderˆa ´a“¥‰w¶
            (setq index_lst (append index_lst (list index)))
            (setq name_lst (append name_lst (list plt_name)))
          ) ;of THEN
        ) ;of IF
        (setq bdr_num (1+ bdr_num))
        (redraw (ssname ss_lst index) 4)              ;¤e¸å–E borderŸi ¸÷¬w·a¡
      ) ;of progn
    ) ;of IF
    (setq index (1+ index))                           ;”a·q entity¡
  ) ;of repeat
  (princ bdr_num) (princ " Border(s) found")        ;¡yˆ· borderˆa ¬åÈ‚–V“e»¡ Îa¯¡

  (setq index 0)
  (repeat (length index_lst)                        ;¬åÈ‚–E border ˆ•® eÇq ¤e¥¢
    (setq bdr_ent (entget (ssname ss_lst (nth index index_lst))))  ;border entity¸÷¥¡
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border· insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border· scale factor
    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border· ¹ÁÃb ´aœ
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border· ¶Ãb ¶á
                         (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (command "PLOT" "W" low_left up_right "N" "N" (nth index name_lst))  ;plot¡ww
    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ (nth index name_lst)) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;”a·q border¡
  ) ;of repeat

  (princ)
) ;of defun



;;
;; atom·¡ list· ¶¥­¡·¥ˆa?
;;
(defun is_member(a lst / count)
  (setq count 0)
  (if (/= (length lst) 0)
    (progn
      (while (and (/= a (nth count lst)) (< count (length lst)))
        (setq count (1+ count))
      ) ;of WHILE
      (if (= count (length lst))
       nil
       count
      ) ;of IF
    ) ;of THEN
    nil
  ) ;of IF
) ;of defun
