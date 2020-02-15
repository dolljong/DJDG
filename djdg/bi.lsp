;-----------------------------------
; program : bi
;           make Biaxial data
;           Yi Suk Jong
;           01/01/19
;-----------------------------------
	
(defun c:Bi(
   /
   pp  enbound  elbound  vlist  nnode  opnt  nmprj  scl  ox  oy
   fn  opf  strnnode  nodelst  count  nthnode  firstnode
)
  (setq nmprj (getstring T "\nEnter Title: "))
  (setq fck (getstring T "\nEnter fck of Concrete(kg/cm2): "))
  (setq fy  (getstring T "\nEnter fy(kg/cm2): "))
  
  (initget "Select")
  (setq pp (getpoint "\nPick point or [Select polyline]: "))
  (if (= pp "Select")
    (setq enbound (car (entsel "\nSelect Polyline: ")))
    (setq enbound (bpoly pp))
  );if
  
  (setq opnt (getpoint "\nPick Original point: "))
  (setq ox (car opnt)
	oy (cadr opnt))

;  (setq scl (getreal "\nEnter scale factor: "))
  (setq scl 0.1)
  
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
	  (setq concdata (mkconcdata (reverse (car vlist)) opnt scl))
        ); of progn
	(alert "Polyline must be closed")
        ;(princ "\n Polyline must be closed")
      );of if	
    );progn
    ;(princ "\nBoundary is not closed")
    (alert "Boundary is not closed")  
  );of if
  (redraw enbound 4)
  (princ "\nSelect Rebar: ")
  (setq rebarlist (getrebarlist))
  (setq nrebar (length rebarlist))
  (setq rebarcount 0
	rebardata nil)

  (repeat nrebar
    (setq nthrebar (nth rebarcount rebarlist))
    (setq rebarnum (rtos (1+ rebarcount) 2 0))
    (setq rebardata (append rebardata
		      (list  	    
			(strcat rebarnum "," rebarnum ",1,0," 
			        (rtos (AreaofRebar (nth 2 nthrebar) "cm") 2 3) ","
			        (rtos (* (- (car nthrebar) ox) scl) 2 3) ","
			        (rtos (* (- (cadr nthrebar) oy) scl) 2 3)
			);strcat
		      );list 	
		    );append
    );setq	  
    (setq rebarcount (1+ rebarcount))
  );repeat

  (setq fn (getfiled "Save BiAxial Data" "" "" 1))
  (setq opf (open fn "w"))
  (write-line nmprj opf)				;wirte project name
  (write-line "2,2,0,2,2,0,0,0,0,500" opf)		;wirte control data
  (write-line "1,0,400,0,80" opf)			;
  (write-line "2,90,100,200,0" opf)			;
;  (write-line (rtos nrebar 2 0) opf)
  (write-line (strcat (rtos (length concdata) 2 0) ",1,0," fck) opf)
  (setq count 0)
  (repeat (length concdata)
    (write-line (nth count concdata) opf)
    (setq count (1+ count))
  );repeat
  (write-line (strcat (rtos (length rebardata) 2 0) "," fy",2000000") opf)
  (setq count 0)
  (repeat (length rebardata)
    (write-line (nth count rebardata) opf)
    (setq count (1+ count))
  );repeat  
  (write-line "999" opf)
  (write-line "0" opf)
  (close opf)
  (princ "\nProgram terminate")
  (princ)
  
);defun


; -------------------------------------
; function : mk_vertlist
; LwPolyline�� vertex list�� ������ش�.
; �μ�: vname  : vertext entity name
;                 (car (entsel)) ���·� �Ѿ�;��Ѵ�.
; -------------------------------------

  (defun mk_vertlist (vname
  /  count nvert tmp vert_list pt1
                     );of local variable

    (setq vlist (entget vname))                          ;��ƼƼ ����

    (setq count 0)                                      ;ù vertex ã�ư�
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )                                                   ;ù° vertex ��ġ


    (setq nvert (cdr (assoc 90 vlist)))                 ;vertext��

    (setq vert_list nil)                                 ;�� list�����
    (setq index 0)                                      ;ùvertex����

    (repeat nvert
      (setq tmp (nth (+ count (* index 4)) vlist))     ;(10 x y)
      (setq tmp (append tmp (list (cdr (assoc 38 vlist)))))  ;z��ǥ�߰�
      (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))  ;ucs��ǥ�� ġȯ
      (setq vert_list (append vert_list (list pt1)))         ;vertexlist���߰�
      (setq index (1+ index))                                      ;���� vertext��
    ); repeat

     (setq vert_list (list vert_list nvert))
  ) ;of defun

;----------------------------------
; function : getRebarlist
;            Get donut point list
;            By Yi Suk Jong
;            2000/12/28
;----------------------------------
(defun GetRebarlist( / entlst nent plst npnt index ent entype)
  (setq entlst (ssget))                           ;��ŷ��� entity����            
  (setq nent (sslength entlst))                   ;��ŷ��� ��ƼƼ����            
            
  (setq plst nil)                 ;plst: ��ŷ��� entity����  ����Ʈ����Ʈ            
  (setq npnt 0)       ;����Ʈ ���� (��ŷ��� ��ƼƼ���� /= ��ŷ�������Ʈ����)            
  (setq index 0)            
  (repeat nent                                    ;��� ��ƼƼ������ŭ �ݺ�            
    (setq ent (entget (ssname entlst index)))            
    (setq entype (cdr (assoc 0 ent)))             ;��ƼƼŸ�� ����            
    (cond            
      ((= entype "POLYLINE")      ;���������� ���(ö���� �������� �׸� ���)            
        (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;ù������            
        (if (= (abs (cdr (assoc 42 vtx1))) 1.0)         ;���������� ��ũ���ΰ�?            
          (progn            
            (setq vtx2 (entget (setq nxt2 (entnext nxt1))))   ;�ι�°�� ����            
            (setq vtx1p (cdr (assoc 10 vtx1)))                ;ù�� ����Ʈ            
            (setq vtx2p (cdr (assoc 10 vtx2)))                ;��°�� ����Ʈ            
            (setq cenp (list (/ (+ (car vtx1p) (car vtx2p)) 2.0)     ;��Ÿ����Ʈ X            
                             (/ (+ (cadr vtx1p) (cadr vtx2p)) 2.0))) ;           Y            
            ;(setq dia (cdr (assoc 40 vt
	    (setq plst (append plst (list cenp)))             ;����Ʈ����Ʈ�� �߰�            
	    (setq npnt (1+ npnt))                             ;����Ʈ ���� ����            
          ) ;of progn            
        ) ;of if            
      ) ;of entype="PLINE"            
      ((= entype "LWPOLYLINE")      ;LW���������� ���(ö���� �������� �׸� ���)            
;        (princ "LWPOLYLINE")            
        (setq pnt1 (getLwVert ent 0))            
        (setq pnt2 (getLwVert ent 1))
        (setq dia (* 2 (cdr (assoc 40 ent))))  ;dia of donut
        (setq cp (mid-point pnt1 pnt2))            
        (setq plst (append plst (list (list (car cp) (cadr cp)  dia))))           
        (setq npnt (1+ npnt))            
      ) ;of entype="LWPLOLYLINE"            
      ((= entype "CIRCLE")                                ;��ƼƼ�� ��Ŭ�� ���            
        (setq cp (cdr (assoc 10 ent)))         	          ;��Ŭ�� ��Ÿ����Ʈ            
        (setq dia (* 2 (cdr (assoc 40 ent))))
        (setq plst (append plst (list (list (car cp) (cadr cp)  dia))))               ;����Ʈ����Ʈ�� �߰�            
        (setq npnt (1+ npnt))                             ;����Ʈ ���� ����            
      ) ;of entype="CIRCLE"            
    ) ;of cond            
  (setq index (1+ index))                                 ;index=��ƼƼ��ȣ            
  ) ; of repeat            
  (setq plst plst)  
);defun  

; -------------------------------------            
; function : getLwVert            
; LwPolyline�� Vertex�� ô��            
; �μ�: vlist  : vertext list            
;       tmpctr : ������ vertext ��ȣ 0,1,2            
; -------------------------------------            
            
(defun getLwVert (vlist tmpctr / count tmp)            
;    (setq vlist (entget (car (entsel))))                       ;��            
            
    (setq count 0)                                      ;ù vertex ã�ư�            
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


;------------------------------------------
; function : AreaofRebar
;            Area of Rebar    
;            By Yi Suk Jong (dolljong@dreamwiz.com)
;            2000/12/28 
;------------------------------------------
(defun areaofrebar(barname unit / barname unit )
  (if (numberp barname)
    (setq barname (rtos barname 2 0))
    (setq barname (substr barname 2 2))
  );if  
  (cond
    ((= barname "10") (setq area  0.713))
    ((= barname "13") (setq area  1.267))
    ((= barname "16") (setq area  1.986))
    ((= barname "19") (setq area  2.865))
    ((= barname "22") (setq area  3.871))
    ((= barname "25") (setq area  5.067))
    ((= barname "29") (setq area  6.424))
    ((= barname "32") (setq area  7.942))
    ((= barname "35") (setq area  9.566))
    ((= barname "38") (setq area 11.400))
    (T (setq area 0.0))    
;    (T (alert "Unknown Dia:")(setq area 0.0))
  );cond
  (cond
    ((= (strcase unit) "MM") (setq area (* area 100)))
    ((= (strcase unit) "CM") (setq area (* area 1)))
    ((= (strcase unit) "M") (setq area (/ area 10000)))     
  );cond
  (setq area area)
);


;---------------------------------
; function : rtosfw
;            rtos fixed width
;            By Yi Seok Jong (dolljong@dreamwiz.com)
;            2000/7/29
;---------------------------------
; �־��� ���ڸ� �������� ���� string���� ��ȯ�Ͽ� �������ش�.
; fortran���� f10.3�� ���� ������ �Ѵ�.
; ��½� ���� ���� �� �ʿ�
; ex) (rtosfw 3.456 10 3)  --> "     3.456")
;-----------------------------------
(defun rtosfw(num width dec / num width dec str1 lenstr errmsg)
  (setvar "DIMZIN" 0)
  (setq str1 (rtos num 2 dec))
  (setq lenstr (strlen str1))
  (if (> lenstr width)
    (progn
;      (setq errmsg (strcat "String too long (" (rtos lenstr 2 0) ") > " (rtos width 2 0) "(Width)"))
;      (alert errmsg)(exit)
      (setq str1 str1)
    );progn
    (progn
      (strcat (strcopy " " (- width lenstr)) str1)
    );progn
  );if
);defun rtosfw
  
;---------------------------
;function : strcopy
;           string copy
;           By Yi Seok Jong (dolljong@dreamwiz.com)
;---------------------------
; �־��� ���ڿ��� �־��� Ƚ����ŭ ���Ѵ�. �ڸ����� ���� �� ����
; ex) (strcopy " " 5)  --> "     "
;--------------------------------------------
(defun strcopy(str num / str num return)
  (setq return str)
  (if (> (1- num) 0)
    (repeat (1- num)
      (setq return (strcat return str))
    );repeat
    (setq rerurn str)
  );if  
);defun  



;------------------------------------------
; Function: mkconcdata
;           Make Concrete Data
;           Yi suk jong (dolljong@dreamwiz.com)
;           01/1/19
;------------------------------------------
;mkconcdata(vlist opnt scl)
; input
;   vlist : vertext list
;   opnt  : original point
;   scl   : scale factor
;return
;   ("1, 0.000, 0.00" "2, 1.000,0.000" ...)
;------------------------------------------
(defun mkconcdata(nodelst opnt scl / vlist scl)
;  (setq scl 0.01)
  (setq nnode (length nodelst))  ;number of node
  (setq ox (car opnt)
	oy (cadr opnt))
;  (setq strnnode (rtos (1+ nnode) 2 0))    ;number of node(str)
;  (cond 
;    ((= (strlen strnnode) 1) (setq strnnode (strcat "  " strnnode)))
;    ((= (strlen strnnode) 2) (setq strnnode (strcat " " strnnode)))
;  );cond	
;  (write-line strnnode opf)				;write node-number
;  (setq nodelst (car vlist))
  (setq datalist nil)
  (setq count 0)
  (repeat nnode
    (setq nthnode (nth count nodelst))
    (setq datalist (append datalist  (list (strcat (rtos (1+ count) 2 0) ","
	                                           (rtos (* scl (- (nth 0 nthnode) ox)) 2 3) ","
	                                           (rtos (* scl (- (nth 1 nthnode) oy)) 2 3) 
    	                                    );strcat
				     );list	   
		   );append
    );setq	  
    (setq count (1+ count))
  );repeat

  (setq firstnode (nth 0 nodelst))
  (setq datalist (append datalist (list (strcat (rtos (1+ count) 2 0) ","
	                                        (rtos (* scl (- (nth 0 firstnode) ox)) 2 3) ", "
	                                        (rtos (* scl (- (nth 1 firstnode) oy)) 2 3) 
	                                );strcat
			          );list     
		 );append
  );setq 	
  (setq datalist datalist)  ;return value
);defun mkconcdata