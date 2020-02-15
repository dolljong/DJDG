(defun c:orip(/ opt pt)
(setq opt (getpoint "Pick Origin point"))
(setq pt (getpoint "Pick Last point" opt))
)
