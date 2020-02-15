;-----------------------
; Program : DB24.lsp
;           make db-24 input file for 3D
;           Yi Suk Jong
;           2000/2/25
;-----------------------
(defun c:db24( / nnlist)

  (push-env)
  
  (setq startlc (getint "\nEnter Start Load Case: ")
        lchead "DB-24")
;	pr "-15.651"
;	pf " -3.913")
  (setq sele (getint "\nEnter Start Element Number: ")
	eele (getint "\nEnter End Element Number: ")
        ne (- eele sele -1)
	nn (1+ ne)
        pr (getreal "\nEnter Pr (ton): ")
	pf (* pr 0.25)
	pr (rtos (* pr -1) 2 3)
;	pf (getreal "\nEnter Pf (ton): ")
	pf (rtos (* pf -1) 2 3)
	title (getstring "\nEnter Title: " T))
  (setq ng (getint "\nEnter NUmber of girder: "))
  (if (> ng 1) (setq ginc "\nEnter increment: "))

(print pr) (print pf)
  (setq path (getvar "dwgprefix")
	outfilename (getfiled "Load 3D *.OUT file" path "out" 0))
  (setq opf (open outfilename "r"))
 ;(setq pr (getreal "\nEnter Pr: ")
 ; 	pf (getreal "\nEnter Ps: "))
  
  (setq ;nn 129     ;number of node
	;ne 128     ;number of element
	nlist nil  ;node list
        elist nil) ;element list
  
  (while (/= (substr (setq readline (read-line opf)) 2 33) "N O D A L   D E F I N I T I O N S") );
  ;(princ "\N O D A L   D E F I N I T I O N S")
  (setq nn (atoi (substr readline 61 5))) 
  (repeat 4 (read-line opf))
  (repeat nn
    (setq l (read-line opf))
    (setq nlist (append nlist (list (cons (atoi (substr l 1 6))
	                            (atof (substr l 15 10))))))
  );repeat
  
  ;(princ nlist)
  
  (while (/= (substr (setq readline (read-line opf)) 2 25) "BEAM ELEMENT INFORMATIONS") );
  (setq nel (atoi (substr readline 54 5)))   
  (princ "\nfound element")
  (repeat 4 (read-line opf))
  (repeat nel
    (setq l (read-line opf))
    (setq elist (append elist (list (cons (atoi (substr l  1 5))
					  (list (atoi (substr l  6 5))
					        (atoi (substr l 11 5)))))))
  );repeat
  
  (close opf)
  
;  (drawgirder nlist 0 0)
; (setq nnlist (mk_nlist nlist elist sele eele)) 
  (makedb24 nlist elist sele eele)
  
  (pop-env)
  
  
);defun

;------------------------------------------
; function : getelx
;            get element number and x from i-node
;            by Yi Suk Jong
;            2000/2/25
;------------------------------------------
(defun getelx(nlist elist x sele eele / nlist inode jnode elist x sele eele nmlist count x1 x2 return)
  
  (setq nmlist (length nlist)
        count sele)
  
  (setq return nil)
  
;  (princ x)
  (repeat (- eele sele -1)
    (setq inode (cadr (assoc count elist))     ;i-node number
	  jnode (caddr (assoc count elist)))    ;j-jode number
    (setq x1 (cdr (assoc inode nlist))
          x2 (cdr (assoc jnode nlist)))
    (if (and (>= x x1) (<= x x2))
          (if (= (- x x1) (- x2 x1))
	    (progn
              (setq return (list count (- x x1 0.001)))	      
	    );progn
            (setq return (list count (- x x1)))
          );if  
    );if	     
    (setq count (1+ count))
  );repeat
  (if (/= return nil) return)
);defun


;----------------------------------
; function drawgirder
;          by Yi Suk Jong
;          2000/2/25
;----------------------------------
; (drawgirder nlist x y)
;    nlist : node list
;    x     : x of insertion point
;    y     : y of insertion point
;----------------------------------

(defun drawgirder(nlist x y / nlist x y n x1 x2 count p1 p2 donutradius ipnt modelgap)
  (setq modelgap -5
        donutradius 0.2
        txtht 0.5
	count 0
	boxgap (* 0.25 txtht)
	nn (length nlist)
	lastx (cdr (nth (1- nn) nlist))  ;last point
	ipnt (list x y))    ;insertion point

(command "LINE" ipnt (list lastx y) "")   ;draw girder line
  
(command "donut" "0" donutradius ipnt "")                ;draw donut
(if (/= nil (tblsearch "BLOCK" "$node"))  			;make node block 
  (command "block" "$node" "Y" ipnt (entlast) "")         ; $node block aleady exist
  (command "block" "$node" ipnt (entlast) "")             ;  
);if  
  
  (repeat  (1- (length nlist))
    (setq x1 (cdr (nth count nlist))
	  x2 (cdr (nth (1+ count) nlist))
	  x3 (/ (+ x1 x2) 2)
	  y1 (- y (* txtht 1.5)))
     
    (setq p1 (list x1 y)
	  p2 (list x2 y)
	  txt (rtos (1+ count) 2 0))
          
;    (command "donut" "0" donutradius p1 "")                ;
    (command "INSERT" "$node" p1 "" "" "")   ;insert node block
    (command "text" "J" "BC" (list x1 y1)  txtht "" txt)   ;write node number
;    (command "LINE" p1 p2 "")      			   ;write element line
;    (command "text" "j" "bc" (list x3 y1) txtht "" txt)    ;write element number
    
    (setq tboxb (+ (* txtht (strlen txt))))
    (setq p1 (list (- x3 (/ (+ tboxb boxgap) 2)) (- y1 (/ boxgap 2)))
	  p2 (list (+ x3 (/ (+ tboxb boxgap) 2)) (+ y1 txtht boxgap)))
;    (command "rectangle" p1 p2)
    (setq count (1+ count))	   
  );repeat

  (command "INSERT" "$node" (list lastx y) "" "" "")    ;insert last node block
  (command "text" "J" "BC" (list lastx y1)  txtht "" (rtos (1+ count) 2 0))   ;write node number

  (command "zoom" "E")
  (setq modelents (ssget "C" ipnt (list lastx y1)))   ;get model entity
  (if (/= (tblsearch "BLOCK" "$model") nil)
    (command "BLOCK" "$model" "Y" ipnt modelents "")  ;aleady exist
    (command "BLOCK" "$model" ipnt modelents "")    
  );if

  (command "MINSERT" "$model" ipnt "" "" "" nn "" modelgap)    ;insert model block


  
);defun  


;-----------------------------------
;function : getpath
;           Yi Seok Jong
;           2000/4/20
;-----------------------------------
;
(defun getpath(fn / fn )
  (setq index (1- (strlen fn)))
	
  (while (/= (substr fn index 1) "\\")
    (setq index (1- index))
  );while
  (substr fn 1 index)
);defun

;-----------------------------------------------
; function makedb24
;          make db24 inputfile
;          by Yi Suk Jong
;          2000/2/25
;-----------------------------------------------
(defun makedb24(nlist elist sele eele / defaultfn nlist elist sele eele opf index firstx load1 load2 load3 )
    
  (setq defaultfn (strcat (getpath outfilename) "db24"))
  (setq opf (open (getfiled "OUTPUT 3D *.lin file" defaultfn "LIN" 1) "w"))
  (write-line "START" opf)
  (write-line (strcat "TITLE " title) opf)
  (write-line "" opf)
  
  (setq index 0
	enum sele)

;  (while (/= sele (car (nth index nlist))) (setq index (1+ index)))
  (setq firstnode (cadr (assoc sele elist)))
;  (setq firstnode index)
  (repeat (- eele sele -1)
    (setq firstx (cdr (assoc (cadr (assoc enum elist)) nlist)))
;    (setq firstx (cdr (nth index nlist))) 
    (setq load1 (getelx nlist elist firstx sele eele))
    (setq load2 (getelx nlist elist (+ firstx 4.2) sele eele))       ;find Pn
    (setq load3 (getelx nlist elist (+ firstx 8.4) sele eele))
   
    (write-line (strcat "LOAD C=" (itoa (+ startlc (- index firstnode)))
			"  H=<" lchead ">") opf)
    (if (/= load1 nil)
      (write-line (strcat (itoa (car load1)) "  T=cg p=0," pr
       			  "  d=" (rtos (cadr load1) 2 3)) opf))
    (if (/= load2 nil)
      (write-line (strcat (itoa (car load2)) "  T=cg p=0," pr
			  "  d=" (rtos (cadr load2) 2 3)) opf))
    (if (/= load3 nil)
      (write-line (strcat (itoa (car load3)) "  T=cg p=0," pf
			  "  d=" (rtos (cadr load3) 2 3)) opf))			
    (write-line "" opf)
    
    (setq index (1+ index))
    (setq enum (1+ enum))
  );repeat

  (setq lastnode (caddr (assoc eele elist)))
  (write-line (strcat "LOAD C=" (itoa (+ startlc (- index firstnode)))
			"  H=<" lchead ">") opf)
  (write-line (strcat (itoa lastnode) "  T=cn p=0," pr) opf)

  (write-line "" opf)

  (write-line "stop" opf)

  (close opf)
  
);defun   


;----------------------------------------
;function : mk_nlist
;           make node list from element data
;           By Yi Seok-Jong (dolljong@dreamwiz.com)
;----------------------------------------
(defun mk_nlist(nlist elist sele eele / nlist elist sele eele nnlist nele index nindex inode x)
  (setq nnlist nil)
  (setq nele (- eele sele))
  (setq index 0
	nindex 1)
  (while (/= sele (car (nth index elist))) (setq index (1+ index)))
  (repeat (1+ nele)
    (setq inode (nth 1 (nth index elist)))
    (setq x (cdr (assoc inode nlist)))
    (setq nnlist (append nnlist (list (cons nindex x))))
    (setq index (1+ index)
	  nindex (1+ nindex))				
  );repeat
  
  (setq inode (nth 2 (nth (1- index) elist)))
  (setq x (cdr (assoc inode nlist)))
  (setq nnlist (append nnlist (list (cons nindex x))))
  
  nnlist
  
) ;defun mk_nlist 