;-----------------------------------
; program : gsection
;           make gsection data
;           Yi Suk Jong
;           00/2/16
;-----------------------------------

(defun c:gsection(
   /
   pp  enbound  elbound  vlist  nnode  opnt  nmprj  scl  ox  oy
   fn  opf  strnnode  nodelst  count  nthnode  firstnode
)

  (initget "Select")
  (setq pp (getpoint "\nPick point or [Select polyline]: "))
  (if (= pp "Select")
    (setq enbound (car (entsel "\nSelect Polyline: ")))
    (setq enbound (bpoly pp))
  );if  
  
  (if (/= enbound nil)
    (progn
      (redraw enbound 3)
      (setq elbound (entget enbound))         ; entity list
      (if (= (cdr (assoc 70 elbound)) 1) 
        (progn
          (setq vlist (mk_vertlist enbound))
	  (setq nnode (car (cdr vlist)))
	  (princ "\n")(princ nnode)
	  (princ " node found")
	  (setq opnt (getpoint "\nPick Original point: "))
	  (setq nmprj (getstring T "\nEnter Title: "))
	  (setq scl (getreal "\nEnter scale factor: "))
	  (setq ox (car opnt)
		oy (cadr opnt))

	  (setq fn (getfiled "Save data file" "" "sec" 1))
	  (setq opf (open fn "w"))
	  (write-line nmprj opf)				;wirte project name
	  (setq strnnode (rtos (1+ nnode) 2 0))
	  (cond 
	    ((= (strlen strnnode) 1) (setq strnnode (strcat "  " strnnode)))
	    ((= (strlen strnnode) 2) (setq strnnode (strcat " " strnnode)))
	  );cond	
	  (write-line strnnode opf)				;write node-number
	  (setq nodelst (car vlist))
	  (setq count 0)
	  (repeat nnode
	    (setq nthnode (nth count nodelst))
	    (write-line  (strcat (rtos (1+ count) 2 0) ","
		                 (rtos (* scl (- (nth 0 nthnode) ox)) 2 3) ","
  		                 (rtos (* scl (- (nth 1 nthnode) oy)) 2 3) ",0"
	    	         );strcat
	    opf);write-line
	    (setq count (1+ count))
	  );repeat

	  (setq firstnode (nth 0 nodelst))
	  (write-line (strcat (rtos (1+ count) 2 0) ","
		              (rtos (* scl (- (nth 0 firstnode) ox)) 2 3) ","
  		              (rtos (* scl (- (nth 1 firstnode) oy)) 2 3) ",0"
		      );strcat	      
	  opf);write-line
	  (close opf)
	  (entdel enbound)
	  
        ); of progn
	(alert "Polyline must be closed")
        ;(princ "\n Polyline must be closed")
      );of if	
    );progn
    ;(princ "\nBoundary is not closed")
    (alert "Boundary is not closed")  
  );of if
  (redraw enbound 4)  
)


; -------------------------------------
; function : mk_vertlist
; LwPolyline의 vertex list를 만들어준다.
; 인수: vname  : vertext entity name
;                 (car (entsel)) 상태로 넘어와야한다.
; -------------------------------------

  (defun mk_vertlist (vname
  /  count nvert tmp vert_list pt1
                     );of local variable

    (setq vlist (entget vname))                          ;엔티티 정보

    (setq count 0)                                      ;첫 vertex 찾아감
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )                                                   ;첫째 vertex 위치


    (setq nvert (cdr (assoc 90 vlist)))                 ;vertext수

    (setq vert_list nil)                                 ;빈 list만들기
    (setq index 0)                                      ;첫vertex부터

    (repeat nvert
      (setq tmp (nth (+ count (* index 4)) vlist))     ;(10 x y)
      (setq tmp (append tmp (list (cdr (assoc 38 vlist)))))  ;z좌표추가
      (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))  ;ucs좌표로 치환
      (setq vert_list (append vert_list (list pt1)))         ;vertexlist에추가
      (setq index (1+ index))                                      ;다음 vertext로
    ); repeat

     (setq vert_list (list vert_list nvert))
  ) ;of defun 
