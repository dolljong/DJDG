;**************************************
; Function : HROUND
;            Half ROUNDing
;            Suk-Jong Yi
;            1995. 3. 16
;**************************************

(defun HROUND(p1s p1e p2s p2e /             ;반 라운딩을 해주는 Function
sdst edst lpnts lpnte ds p1s p1e oldclr     ;Argument는 두선의 시작점과 끝점
)

(setq ds (getvar "DIMSCALE"))         ;DIMSCALE을 알아낸다.

(setq ssdst (distance p1s p2s))     ;첫라인의 시작점에서 두째라인의 시작점거리
(setq sedst (distance p1s p2e))     ;첫라인의 시작점에서 두째라인의 끝점거리
(if (< sedst ssdst)                 ;위의 두 거리중 가까운쪽이 시작점이 된다
  (progn
    (setq tmp p2s)
    (setq p2s p2e)
    (setq p2e tmp)
  ) ;of progn
) ; of if

(setq sdst (distance p1s p2s))    ;첫째라인 시작점에서 둘째라인 시작점까지 거리
(setq edst (distance p1e p2e))    ;첫째라인 끝점에서 둘째라인 끝점까지 거리

             ;두 선간의 거리가 2미리이상 일때만 두 선의 중간에다 선을 그린다
(setq oldclr (getvar "CECOLOR"))
(setvar "CECOLOR" "GREEN")
(while (or (> sdst (* ds 1.0)) (> edst (* ds 1.0)))
  (setq lpnts (list (/ (+ (car p1s) (car p2s)) 2.0)      ;두 시작점의 중간점 X
                    (/ (+ (cadr p1s) (cadr p2s)) 2.0)))  ;                   Y
  (setq lpnte (list (/ (+ (car p1e) (car p2e)) 2.0)      ;두 끝점의 중간점   X
                    (/ (+ (cadr p1e) (cadr p2e)) 2.0)))  ;                   Y
  (command "LINE" lpnts lpnte "")     ; 중간선을 그린다
  (setq p1s lpnts)             ;중간선의 시작점을 첫째선의 시작점으로 바꾼다
  (setq p1e lpnte)             ;중간선의 끝점을 첫째선의 끝점으로 바꾼다
  (setq sdst (distance lpnts p2s))  ;두 시작점간의 거리를 구한다
  (setq edst (distance lpnte p2e))  ;두 끝점간의 거리를 구한다
) ;of while
(setvar "CECOLOR" oldclr)
) ;of defun HROUND
