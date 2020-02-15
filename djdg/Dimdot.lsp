;------------------------------
; program : dimdot
;           modify dimmension dot
;           By Yi Suk Jong
;           2000/6/26
;------------------------------
; dot���� donutũ�⸦ �ٲپ��ش�.
; dimstyle���� �ٲ� ��� ġ���� ��ġ�� home���� ���� ������
; �־ �۾��ص� ������ ������ ��찡 ������ �� ���α׷���
; ����ϸ� �׷� ���� ���� dot�� donutũ�⸸ �����Ѵ�.
; ����� [�����ٰ�]�� �⺻ donut������ 1.0���� �����Ǿ��ִ�.
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
