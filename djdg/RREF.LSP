;-----------------------------
; Program : RREF
;           Rotation REFerance
;           By Yi Suk-Jong
;           1999/6/7
;------------------------------

(defun c:rref()
  (princ "\nSelect objects: ")
  (setq ent (ssget))                            ;Select objects:
  (setq bp (getpoint "\nBase point: "))         ;
  (setq ra (getangle "\nReference angle: "))    ;
  (setq na (getangle "\nNew angle: "))          ;

  (command "ROTATE"                                 ;Rotate command
           ent                                      ;select objects
           ""                                       ;select objects
           bp                                       ;Base point:
           "R"                                      ;<Rotation angle>/Reference:R
           (rtod ra)                                ;Reference angle <0>:
           (rtod na)                                ;Second point:
  );command
) ;defun
