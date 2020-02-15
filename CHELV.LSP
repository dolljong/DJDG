;------------------------------
; program : CHELV
;           CHange ELeVation
;           Yi suk jong
;           99/10/11
;------------------------------
; elevation이 없는 등고선들의 elevation을 정해줌
; 선택한 pline순서대로 지정 가능

(defun c:chelv(
/ el delta psel pent
)
  (setq el (getreal "\nElevation: "))      ;첫 등고선 elevation
  (setq delta (getreal "\nDelta Z: "))     ;등고선간 편차

  (princ "\nCurrent Elevation: ")(princ el)

  (while (setq psel (entsel " Pick polyline: "))
    (redraw (car psel) 3)                       ;엔티티 강조
    (setq pent (entget (car psel)))
    (chelv_f pent el)
    (setq el (+ el delta))
    (princ "\nCurrent Elevation: ")(princ el)
  ); while

);defun chelv


;---------------------
; function : chelv_f
;            change elevation (function)
;            Yi suk jong
;            99/10/11
;----------------------
; 주어진 polyline의 elevation을 바꾸어줌
; 넘어오는 값:
;    plent : pline entity정보 (entget된 상태)
;        z : elevation값
;----------------------

(defun chelv_f(plent z
 / plent z o38 n38
  )
  (setq o38 (assoc 38 plent))
  (setq n38 (cons (car o38) z))
  (setq plent (subst n38 o38 plent))
  (entmod plent)
);defun

