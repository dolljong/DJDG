;************************************
; Program : ROUND
;           ROUND lines
;           Suk-Jong Yi
;           1995. 3. 16, 5/25
;************************************

(defun C:ROUND(/
ds ent1 ent2 pnt1s pnt1e pnt2s pnt2e ans mpnts mpnte
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(push-env)  ;환경변수 대피

(setq ds (getvar "DIMSCALE"))

(setq ent1 (entget (car (entsel "\nSelect first line: ")))) ;라운딩할 두 선 선택
(setq ent2 (entget (car (entsel "\nSelect second line: "))))

(setq pnt1s (cdr (assoc 10 ent1)))              ;두 라인의 시작점과 끝점
(setq pnt1e (cdr (assoc 11 ent1)))
(setq pnt2s (cdr (assoc 10 ent2)))
(setq pnt2e (cdr (assoc 11 ent2)))
;
(setq ssd (distance pnt1s pnt2s))     ;첫라인의 시작점에서 두째라인의 시작점거리
(setq sed (distance pnt1s pnt2e))     ;첫라인의 시작점에서 두째라인의 끝점거리
(if (< sed ssd)                 ;위의 두 거리중 가까운쪽이 시작점이 된다
  (progn
    (setq tmp pnt2s)
    (setq pnt2s pnt2e)
    (setq pnt2e tmp)
  ) ;of progn
) ; of if

(initget "All Half")                            ;전체라운딩 또는 반 라운딩 물음
(setq ans (getkword "\nAll round/<Half round>: "))
(if (= ans nil) (setq ans "Half"))

(if (= ans "All")                       ;전체라운딩일 경우 중간라인을 구하고
  (progn                                ; 그 좌우로 라운딩 선 그림
    (setq mpnts (list (/ (+ (car pnt1s) (car pnt2s)) 2.0)
                      (/ (+ (cadr pnt1s) (cadr pnt2s)) 2.0)))
    (setq mpnte (list (/ (+ (car pnt1e) (car pnt2e)) 2.0)
                      (/ (+ (cadr pnt1e) (cadr pnt2e)) 2.0)))
    (setq oldclr (getvar "CECOLOR"))
    (setvar "CECOLOR" "1")
    (setq oldlt (getvar "CELTYPE"))
    (setvar "CELTYPE" "CENTER")
    (COMMAND "LINE"  mpnts mpnte "")
    (setvar "CELTYPE" oldlt)
    (setvar "CECOLOR" oldclr)
    (hround mpnts mpnte pnt1s pnt1e)
    (hround mpnts mpnte pnt2s pnt2e)
  ) ; of progn
  (hround pnt1s pnt1e pnt2s pnt2e)     ;반 라운딩일 경우 시작선에서 끝선으로
) ; of if                              ;라운딩 선 그림

(pop-env)                              ;대피시켜둔 환경변수값 복구
  (setq *error* oer seterr nil)

) ;of defun ROUND

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

(setq sdst (distance p1s p2s))    ;첫째라인 시작점에서 둘째라인 시작점까지 거리
(setq edst (distance p1e p2e))    ;첫째라인 끝점에서 둘째라인 끝점까지 거리

             ;두 선간의 거리가 2미리이상 일때만 두 선의 중간에다 선을 그린다
(setq oldclr (getvar "CECOLOR"))
(setvar "CECOLOR" "3")
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


