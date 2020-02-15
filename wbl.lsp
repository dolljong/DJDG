;-------------------
; Program : WBl (WBlock using File NAME list)
;           YI Suk Jong
;           04/11/10
;-------------------
(defun c:wbl()
  (setq ss (ssget '((0 . "TEXT"))))                 ;text entity·³b
  (setq ns (sslength ss))                           ;selection set· entityˆ•®
;  (setq fname (strcat (getvar "dwgprefix") "/"))    ;filename·i ¤w·¡Ÿq·a¡
  (setq fname (getvar "dwgprefix"))    ;filename·i ¤w·¡Ÿq·a¡
  (setq index 0)
  (repeat ns
    (setq txt (cdr (assoc 1 (entget (ssname ss index))))) ;textˆt
    (if (> index 0)
      (setq fname (strcat fname "-" txt))   ;–¤å¼ text¦Èá“e text´|µA "-"Âˆa
      (setq fname (strcat fname txt))       ;Àõ¤å¼ text·¥ ‰w¶µE ¤w·¡ŸqµA ¦›·±
    );if
    (setq index (1+ index))
  );repeat

  (setq ipnt (getpoint "\nInsertion base point:")) ;¬s·³¸ñ ¬åÈ‚
  (setq sse (ssget))                        ;wblockÐi entity¬åÈ‚

  (setvar "FILEDIA" 0)                      ;¡ww»¥Ð—¯¡ dialog box›a»¡ ´g•¡¢

  (command "WBLOCK"          ;wblock¡ww ¯©Ð—
           fname             ;filename
           ""                ;block name
           ipnt              ;insert point
           sse               ;¬åÈ‚–E entity
           "")               ;end selection

  (setvar "FILEDIA" 1)                      ;

);defun


(defun get_fnlist()
  (setq fn (strcat (prefix) "djdg/wbl.dat"))      ;file nameÀÔ·Â
  (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;fileÀÌ ¾ø´Â °æ¿ì
        (progn
           (setq count 1)
	   (setq llist nil)
           (while (setq ch (read-line opf))             ;ÇÑÁÙÀ» ÀÐ´Â´Ù
              (princ (chr 13))                          ;ÀÔ·ÂÁß ¸Þ¼¼Áö Ãâ·Â
              (princ count)
              (princ " Line Processing...")
              (setq inline (data-in ch))
              (setq lst (cons                           ;¹®ÀÚ data¸¦ ¼ýÀÚ data·Î
			  (delsp (strcase (nth 1 inline)))   ;subject
			  (delsp (strcase (nth 0 inline))))) ;filename (number)
              (setq llist (append llist (list lst)))                   ;llist¿¡ Ãß°¡
              (setq count (1+ count))                   ;line¹øÈ£ Áõ°¡
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;fileÀÌ ¾ø´Â °æ¿ì
      ) ;of if
      (close opf)                                           ;file close
);defun


(defun delsp(str / )
  (setq return str)
  (while (vl-string-position (ascii " ") return)
    (setq return (vl-string-subst "" " " return))
  );while
  return
);defun  
  