;-------------------
; Program : WBNAME (WBlock File NAME)
;           YI Suk Jong
;           99/9/9
;-------------------
(defun c:wb1()
  (setq ss (entsel "select text: "))
  (setq sn (car ss))  ;sname
  (setq ipnt (cadr ss))

  (setq fname (getvar "dwgprefix"))    ;filename을 방이름으로
  (setq dwgname (getvar "dwgname"))
  (setq fname (strcat fname (substr dwgname 1 (vl-string-search "_" dwgname))
		      "_apt_block_"
		      ))

  (setq txt (cdr (assoc 1 (entget sn)))) 
  (setq fname (strcat fname txt))
  

;  (setq ipnt (getpoint "\nInsertion base point:")) ;삽입점 선택
  (setq sse (ssget))                        ;wblock할 entity선택

  (setq sn (sslength sse))
  (setq index 0)
  (repeat sn
    (setq sget (entget (ssname  sse index)))
    (setq ty (cdr (assoc 0 sget)))  ;type
    (setq ly (cdr (assoc 8 sget)))  ;layer
    (if (and (= ty "LWPOLYLINE") (= ly "?≫c???"))
      (progn
        (setq p0 (getLwVert sget 0)
	    p1 (getLwVert sget 1)
	    p2 (getLwVert sget 2)
	    p3 (getLwVert sget 3))
        (setq minx (car p0)
	    maxx (car p0))
        (if (< (car p1) minx) (setq minx (car p1)) (if (> (car p1) maxx) (setq maxx (car p1))))
        (if (< (car p2) minx) (setq minx (car p2)) (if (> (car p2) maxx) (setq maxx (car p2))))
        (if (< (car p3) minx) (setq minx (car p3)) (if (> (car p3) maxx) (setq maxx (car p3))))
        (if (and (> (car ipnt) minx) (< (car ipnt) maxx))
	  (progn
	    (command "pedit" (ssname sse index) "W" "3" "X")
	    ;(setq oldwidth (assoc 41 ent))
	    ;(setq newwidth (cons 41 3.0))
	    ;((setq sget (subst newwidth oldwidth sget))
	  );progn  
        );if
      );progn    
    );if  
    (setq index (1+ index ))
  );  
  
  (setvar "FILEDIA" 0)                      ;명령진행시 dialog box뜨지 않도록

  (command "WBLOCK"          ;wblock명령 실행
           fname             ;filename
           ""                ;block name
           ipnt              ;insert point
           sse               ;선택된 entity
           "")               ;end selection
  
;  (command "oops")
  
  (setvar "FILEDIA" 1)                      ;

);defun




; -------------------------------------
; function : getLwVert
; LwPolyline의 Vertex를 찾음
; 인수: vlist  : vertext list
; tmpctr : 접근할 vertext 번호 0,1,2
; -------------------------------------

  (defun getLwVert (vlist tmpctr / count tmp)
;    (setq vlist (entget (car (entsel))))               ;

    (setq count 0)                                      ;첫 vertex 찾아감
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )
    ;; If the counter reaches the number of vertices,
    ;; reset ctr and tmpctr to zero again.
    (if (= tmpctr (cdr (assoc 90 vlist)))
        (progn
        (setq ctr 0)
        (setq tmpctr 0)
        )
    )
    (setq tmp (nth (+ count (* tmpctr 4)) vlist))
    (setq tmp (append tmp (list(cdr (assoc 38 vlist)))))
    (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))
;    (setq tmp (cons 10 pt1))
    (setq pt1 pt1)
  ) ;of defun