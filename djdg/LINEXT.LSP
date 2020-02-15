(defun c:linext(/ ent a dis pt1 pt2 pt3 pt10 pt11)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

   (setvar "osmode" 512)
   (setq e (entsel "\nPick endpart of line : (nearest of) "))
   (setq ent (entget (car e)))
   (setvar "osmode" 0)
   (setq pt10 (cdr (assoc 10 ent)))
   (setq pt11 (cdr (assoc 11 ent)))
   (setq a (angle pt10 pt11))
   (setq pt1 (cadr e))
   (if (< (distance pt1 pt10) (distance pt1 pt11))
      (setq frpt pt10)
      (setq frpt pt11)
   ) ;; if
   (setq dis (getdist frpt "\nDistance to extend :"))
   (if (< (distance pt1 pt10) (distance pt1 pt11))
       (progn
          (setq pt2 (polar pt10 (+ a pi) dis))
          (setq ent1 (subst (cons 10 pt2) (assoc 10 ent) ent))
       ) ;;prog
       (progn
          (setq pt2 (polar pt11 a dis))
          (setq ent1 (subst (cons 11 pt2) (assoc 11 ent) ent))
       ) ;;prog
   ) ;;if
   (entmod ent1)
  (setq *error* oer seterr nil)
   (princ)
)
