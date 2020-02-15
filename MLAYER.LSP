;**********************************
;  Make Layer
;**********************************

(defun c:mlayer (/ newl ox yn ss clayer n i olist elist nlist)
  (setq ss (ssget))
  (setq newl (strcase (getstring "\nNew layer name or ? :")))
  (if (= newl "?") (listlayer) (makelayer))
)

(defun LISTLAYER()
  (command "'ddlmodes")
  (setq newl (strcase (getstring "New layer name :")))
  (MAKELAYER)
  (princ)
)

(defun MAKELAYER ()
  (setq ox (tblsearch "layer" newl))
  (setq yn "N")
  (if ox
     (progn
        (princ "\nThe layer already exists.")
        (setq yn (strcase (getstring "\n   Redefine it (Y/N)?")))
     ) ;; prog
  ) ;; if
  (if (or (= "Y" yn) (not ox))
    (progn
       (setq clayer (getvar "clayer"))
       (command "layer" "m" newl "")
       (setq n (sslength ss))
       (setq i 0)
       (repeat n
          (setq elist (entget (ssname ss i)))
          (setq olist (assoc 8 elist))
          (setq nlist (subst (cons 8 newl) olist elist))
          (entmod nlist)
          (setq i (1+ i))
       ) ; repeat
    ) ; progn                     
  ) ; if
  (command "layer" "s" clayer "")
  (princ)
) ;program


