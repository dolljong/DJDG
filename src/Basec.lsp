;-------------------
; Program : BASEC
;           draw BASE Concrete
;           by Yi Suk-Jong
;           98/9/28
;---------------------------
;Base concrete그리기
;---------------------------

(defun C:BASEC(
/ Bbase ent thick p1 p2 ang ang pt1 pt2 pt11 pt21
)

  (push-env)                                        ;환경변수대피

  (setq Bbase 100)                                  ;기초콘크리트 폭

  (initget "2p")
  (setq sel (entsel "\nSelect Line/<2p>: "))            ;line enity받음
  (if (= sel "2p")                                      ;2점으로 입력옵션 선택시
    (progn
      (setq p1 (getpoint "\nPick first point: "))       ;시작점좌표
      (setq p2 (getpoint p1 "\nPick second point: "))   ;끝점좌표
    );progn
    (progn
      (setq ent (entget (car sel)))                     ;선택된 선정보
      (setq p1 (cdr (assoc 10 ent)))                    ;시작점좌표
      (setq p2 (cdr (assoc 11 ent)))                    ;끝점좌표
    );progn
  );if
  	   
  (setq thick (getdist "\nThickness <100>: "))       ;base콘크리트 두께
  (if (= thick nil) (setq thick 100))

  
  (setq ang (angle p1 p2))

  (setq pt1  (polar p1  (+ ang pi)  Bbase)             ;시작점 수평위치
        pt2  (polar p2  ang         Bbase)             ;끝점   수평위치
        pt11 (polar pt1 (* pi -0.5) thick)             ;시작점 수직위치
        pt21 (polar pt2 (* pi -0.5) thick))            ;끝점   수직위치

  (command "PLINE" p1 pt1 pt11 pt21 pt2 p2 "")          ;base concrete선 그리기

  (pop-env)                                             ;환경변수 복귀

  (princ)

) ;of defun
