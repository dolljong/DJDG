(defun C:BB()
  (push-env)
  (setq ent (entsel "\nSelect Entity: "))
  (setq pt (getpoint "\nPick break point: "))
  (command "BREAK" ent "F" pt pt)
  (pop-env)
  (princ)
) ;of defun
