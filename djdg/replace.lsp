(defun str-rep (str pat rep)
  (while (vl-string-search pat str)
    (setq str (vl-string-subst rep pat str))
  )
  str
)