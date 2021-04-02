;--------------------------
; command : PPL : draw lines connet two points groups
; 		2021/04/02
;--------------------------

(defun c:ppl( / ssent1 ssent2 bpnt ssum1 ssum2 plist1 plist2 i )
  (princ "Select first points group: ")
  (setq ssent1 (ssget '((0 . "POINT"))))                 ;ssget

  (princ "Select second points group: ")
  (setq ssent2 (ssget '((0 . "POINT"))))                 ;ssget

  (setq bpnt (getpoint "Pick base point: "))
  
  (setq ssnum1 (sslength ssent1))                         ;number of ssget
  (setq ssnum2 (sslength ssent2))                         ;number of ssget

  (print ssnum1)(print ":")
  (print ssnum2)

  (setq plist1 nil)

  (setq i 0 )
  (repeat ssnum1
    (setq plist1 (append plist1 (list (cdr (assoc 10 (entget (ssname ssent1 i))))))) 
    (setq i (+ i 1))
  );repeat

  (setq plist2 nil)

  (setq i 0 )
  (repeat ssnum2
    (setq plist2 (append plist2 (list (cdr (assoc 10 (entget (ssname ssent2 i))))))) 
    (setq i (+ i 1))
  );repeat

  (setq plist11 (point-sort plist1 3 bpnt))
  (setq plist22 (point-sort plist2 3 bpnt))

  (setq i 0)
  (repeat ssnum1
    (command "line" (nth i plist11) (nth i plist22) "")
    (setq i (+ i 1))
  );repeat  
      
);defun

