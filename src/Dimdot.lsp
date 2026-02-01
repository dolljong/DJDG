;------------------------------
; program : dimdot
;           modify dimmension dot
;           By Yi Suk Jong
;           2000/6/26
;------------------------------
; dot블럭의 donut크기를 바꾸어준다.
; dimstyle에서 바꿀 경우 치수의 위치가 home으로 가는 현상이
; 있어서 작업해둔 도면이 망가질 경우가 있으나 이 프로그램을
; 사용하면 그런 현상 없이 dot의 donut크기만 변경한다.
; 참고로 [다정다감]의 기본 donut지름은 1.0으로 설정되어있다.
;------------------------------
(defun c:dimdot()
  (setq dotdia (getreal "\nDot diameter:"))
  (setq blkhead (tblsearch "block" "dot"))
  (setq ent1 (cdr (assoc -2 blkhead)))
  (setq pent (entget ent1))
  (setq nlist (length pent))
  (setq count 0
        newpent nil
        flag 0)
  (repeat nlist
    (setq nthlst (nth count pent))
    (setq nth0 (car nthlst))
    (cond
      ((= nth0 10)
        (if (= flag 0)
          (setq flag 1
               nthlst (list 10 (* 0.25 dotdia) 0.0))
          (setq nthlst (list 10 (* -0.25 dotdia) 0.0))
        );if
      );subcond
      ((= nth0 43)
        (setq nthlst (cons nth0 (* 0.5 dotdia)))
      );subcond
    );cond
    (setq newpent (append newpent (list nthlst)))
    (setq count (1+ count))
  );repeat
  (entmod newpent)
  (entupd (cdr (assoc -1 newpent)))

)
