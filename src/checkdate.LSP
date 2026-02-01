(defun c:checkdate ( / )
  (setq fn (getstring "Enter File Name: "))
  (setq ffn (strcat (getvar "DWGPREFIX") fn))
  (if (findfile ffn)
    (setq opf (open ffn "a"))  ;file ?? ?
    (setq opf (open ffn "w"))  ;file ?? ?
  );if
  (setq tdc (menucmd "m=$(edtime,$(getvar,tdcreate),YY/MO/DD HH:MM:SSSS)"))
  (setq tdu (menucmd "m=$(edtime,$(getvar,tdupdate),YY/MO/DD HH:MM:SSSS)"))
  (setq out (strcat (getvar "dwgname") " " tdc " " tdu))
  (write-line out opf)
  (close opf)
);defun  