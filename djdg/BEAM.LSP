(defun c:beam()

  (setq ent (entget (car (entsel "\nSelect CIRCLE or ARC: "))))  ;원이나 호선택
  (setq cen (assoc 10 ent))                     ;원의 중심점
  (setq p1 (getpoint "\nPick start point: "))          ;시작점
  (setq p2 (getpoint "\n{ock end point: "))            ;끝점
  (setq ang1 (angle cen p1)                            ;시작점의 각
        ang2 (angle cen p2))                           ;끝점의 각
  (setq avang (/ (+ ang1 ang2) 2.0))                ;시작점 각과 끝점각의 평균
  (setq dang (
) ;of defun
