;**************************************
; Line End-End
;                Yi suk jong  1994,5
;**************************************

(defun c:lee(/ p1 p2)
  (setq oldos (getvar "osmode"))
  (setvar "osmode" 1)
  (setq p1 (getpoint "Pick Instersection Point: "))
  (setvar "osmode" 1)
  (setq p2 (getpoint "Enter Perpendicular: " p1))
  (setvar "osmode" oldos)
  (command "line" p1 p2 "")(princ)
)

