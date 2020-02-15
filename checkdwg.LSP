(defun c:checkdate ( / )
  (setq fn (getstring "Enter File Name: "))
  (setq ffn (strcat (getvar "DWGPREFIX") fn))
);defun  