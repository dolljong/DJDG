(defun c:beam()

  (setq ent (entget (car (entsel "\nSelect CIRCLE or ARC: "))))  ;顶贰恆 选葌
  (setq cen (assoc 10 ent))                     ;顶穪 簵格
  (setq p1 (getpoint "\nPick start point: "))          ;竍格
  (setq p2 (getpoint "\n{ock end point: "))            ;弡格
  (setq ang1 (angle cen p1)                            ;竍格穪 坆
        ang2 (angle cen p2))                           ;弡格穪 坆
  (setq avang (/ (+ ang1 ang2) 2.0))                ;竍格 坆壛 弡格坆穪 蛍婨
  (setq dang (
) ;of defun
