;**************************************
; Line Int-perpendiculer
;                Yi suk jong  1994,5
;**************************************

(defun c:lip(/ p1 p2)
  (setq oldos (getvar "osmode"))
  (setvar "osmode" 32)
  (setq p1 (getpoint "Pick Instersection Point: "))
  (setvar "osmode" 128)
  (setq p2 (getpoint "Enter Perpendicular: " p1))
  (setvar "osmode" 0)
  (command "line" p1 p2 "")(princ)
)
