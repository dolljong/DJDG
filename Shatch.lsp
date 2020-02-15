;----
; program : SHATCH (Solid Hatch)
;           Yi Suk Jong
;           1999.9.16
;------
; point���� �Է¹޾� polyline���� �����
; solid hatch���ش�.
;

(defun c:shatch()
 (setq pntlst nil)
 (setq pntnum 0)
 (setq pnt (getpoint "\nPick point: "))
 (setq firstpnt pnt)
 (while (/= pnt nil)
  (if (= pnt "Undo")
    (progn
;      (command "U")
      (setq pntlst (cdr (reverse pntlst)))
      (setq pntnum (1- pntnum))
      (setq pntlst (reverse pntlst))
    );progn
    (progn
      (setq pntlst (append pntlst (list pnt)))
      (setq pntnum (1+ pntnum))
    );progn
  );if
  (princ "\n")(princ pntlst)
  (initget "Undo")
  (setq pnt (getpoint firstpnt "\nPick point: "))
 );while

 (setq index 0)
 (command "pline")
 (repeat pntnum
   (command (nth index pntlst ))
   (setq index (1+ index))
 );repeat
 (command "c")
 (setq pl (entlast))
 (command "hatch"  ;��ġ ��� ����
          "s"      ;solid �ɼ�
          "l"      ;������ ������ entity
          ""       ;��ɳ�����
 );command
  (command "erase" pl "")
);defun

