;***************************************
; Program : RBRF
;           ReBar Round Front
;           By Suk-Jong Yi
;           1995/6/30
;***************************************
; 기둥의 주철근 배근 (정면도)

(defun C:RBRF(/
l1 l2 l1s l1e l2s l2e
ah bn mls mle oldc
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq l1 (entget (car (entsel "\nSelect first line: "))))     ;첫번째 라인 입력
(setq l2 (entget (car (entsel "\nSelect second line: "))))    ;두번째 라인 입력

(setq l1s (cdr (assoc 10 l1))                   ;첫라인의 시작점
      l1e (cdr (assoc 11 l1)))                  ;    "      끝점
(setq l2s (cdr (assoc 10 l2))                   ;두번째라인의 시작점
      l2e (cdr (assoc 11 l2)))                  ;    "          끝점

(initget "All Half")
(setq ah (getkword "\nAll round/Half round <All round>: "))
(if (= ah nil) (setq ah "All"))
(cond
  ((= ah "All")
    (setq bn (getreal "\nNumber of Rebar: "))
    (setq mls (list (/ (+ (car l1s) (car l2s)) 2.0)
                    (/ (+ (cadr l1s) (cadr l2s)) 2.0)))
    (setq mle (list (/ (+ (car l1e) (car l2e)) 2.0)
                    (/ (+ (cadr l1e) (cadr l2e)) 2.0)))
    (setq oldc (getvar "CECOLOR"))
    (setvar "CECOLOR" "YELLOW")
    (command "LINE" mls mle "")
    (setvar "CECOLOR" oldc)
    (halfr mls mle l1s l1e (/ bn 2.0))
    (halfr mls mle l2s l2e (/ bn 2.0))
  ) ;of cond ah=All
  ((= ah "Half")
    (setq bn (getreal "\nNumber of Rebar: "))
    (halfr l1s l1e l2s l2e bn)
  ) ;of cond ah=Half
) ;of cond

  (setq *error* oer seterr nil)
) ;of defun

;****************************************
; Function : HALFR
;            HALF Round
;            By Suk-Jong Yi
;            1995/6/30
;****************************************
; 기둥 주철근 배근 (반원)

(defun HALFR(l1s l1e l2s l2e bn /
l1s l1e l2s l2e bn
)

(setq dst1 (distance (list (car l1s) (cadr l1s)) (list (car l2s) (cadr l2s))))
(setq dst2 (distance (list (car l1s) (cadr l1s)) (list (car l2e) (cadr l2e))))
(setq r dst1)
(if (< dst2 dst1)
  (progn
    (setq temp l2s)
    (setq l2s l2e)
    (setq l2e temp)
    (setq r dst2)
  ) ;of progn
) ;of if

(setq dx (- (car l2s) (car l1s)))
(setq xsgn (/ (abs dx) dx))

(setq da (/ pi bn))

(setq oldc (getvar "CECOLOR"))
(setvar "CECOLOR" "YELLOW")
(setq count 1)
(repeat (fix (/ bn 2.0))
  (setq l1s-1 (list (+ (car l1s) (* xsgn (sin (* count da)) r)) (cadr l1s)))
  (setq l1e-1 (list (+ (car l1e) (* xsgn (sin (* count da)) r)) (cadr l1e)))
  (command "LINE" l1s-1 l1e-1 "")
  (setq count (1+ count))
) ;of repeat
(setvar "CECOLOR" oldc)

) ;of defun

