;----
; program : SHATCH (Solid Hatch)
;           Yi Suk Jong
;           1999.9.16
;------
; point들을 입력받아 polyline으로 만들고
; solid hatch해준다.
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
 (command "hatch"  ;해치 명령 시작
          "s"      ;solid 옵션
          "l"      ;지난번 생성된 entity
          ""       ;명령끝내기
 );command
  (command "erase" pl "")
);defun

