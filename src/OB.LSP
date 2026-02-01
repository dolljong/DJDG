;*******************************************
; Program : OBLique
;           dimension Oblique
;           Jong-Suk Yi
;           97/7/30
;*******************************************
; Vertical DIM을 OBLIQUE시켜준다.
; 제약조건 - 수직 DIM에만 해당된다.
;          - OBLIQUE각은 30도로 정해져 있다.
;*******************************************

(defun C:OB(/
entdim  ents    sp      ep      ang     selpnt  lent    sp      ep
ang     deg     dxy     dy      ep
)

  (defun SETERR(s)                                  ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;내장에러루틴 가동

  (push-env)                                        ;환경변수 대피

  (princ "\nSelect Dimension: ")                    ;dimension 선택
  (setq entdim (ssget '((0 . "DIMENSION"))))

  (initget "2p")
  (setq ents (entsel "\nSelect line/2p: "))         ;라인 또는 두점 선택

  (if (= ents "2p")
    (progn
      (setq sp (getpoint "\nPick start point: "))   ;시작점 입력
      (setq ep (getpoint "\nPick end point: "))     ;끝점 입력
      (setq ang (angle sp ep))                      ;각도 계산
    ) ;of progn
    (progn
      (setq selpnt (car (cdr ents)))                ;라인선택점
      (setq lent (entget (car ents)))               ;line element정보
      (setq sp (cdr (assoc 10 lent)))               ;시작점 입력
      (setq ep (cdr (assoc 11 lent)))               ;끝점 입력
      (if (< (distance sp selpnt) (distance ep selpnt))
        (setq ang (angle ep sp))                    ;선택점이 시작점과 가까우면 각은 끝점~시작점
        (setq ang (angle sp ep))                    ;   "     끝점        "          시작점~끝점
      ) ;of if
    ) ;of progn
  ) ;of if

  (setq deg (rtod ang))             ;radian을 degree로

  (COMMAND "DIM1" "OBLIQUE"         ;oblique명령
           entdim ""                ;dimensio선택
           deg)                     ;각도입력

  (pop-env)                                         ;환경변수 복귀

  (setq *error* oer seterr nil)                     ;재장에러루틴 복귀

  (prin1)
) ;defun
