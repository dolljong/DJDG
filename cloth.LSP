(defun Point_circle(x0 y0 theta R Lp )
  (setq taup (/ Lp R)
        x  (* R (sin taup))
        y  (* R (- 1.0 (cos taup))))
);defun
	