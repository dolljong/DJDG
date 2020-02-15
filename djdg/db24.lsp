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
  (setq sele (getint "\nEnter Start Element Number: ")
	eele (getint "\nEnter End Element Number: ")
        ne (- eele sele -1)
	nn (1+ ne)
	title (getstring "\nEnter Title: " T))
  (setq ng (getint "\nEnter NUmber of girder: "))
  (setq ginc (getint "\nEnter increment: "))
  (setq count 1
	prlist nil
	pflist nil)
  (repeat ng
    (princ "\nPr for Girder-")(princ count) (princ )
    (setq pr ( getreal": ")
	  pf (/ pr 4))
    (setq prlist (append prlist (list (rtosfw (* pr -1) 8 3)))
	  pflist (append pflist (list (rtosfw (* pf -1) 8 3))))
    (setq count (1+ count))
  )

(print pr) (print pf)
  (setq path (getvar "dwgprefix")
	outfilename (getfiled "Load 3D *.OUT file" path "out" 0))
  (setq opf (open outfilename "r"))
  
  (setq ;nn 129     ;number of node
	;ne 128     ;number of element
	nlist nil  ;node list
        elist nil) ;element list
  
  (while (/= (substr (setq readline (read-line opf)) 2 33) "N O D A L   D E F I N I T I O N S") );
  (setq nn (atoi (substr readline 61 5))) 
  (repeat 4 (read-line opf))
  (repeat nn
    (setq l (read-line opf))
    (setq nlist (append nlist (list (cons (atoi (substr l 1 6))
	                            (atof (substr l 15 10))))))
  );repeat
  
  (while (/= (substr (setq readline (read-line opf)) 2 25) "BEAM ELEMENT INFORMATIONS") );
  (setq nel (atoi (substr readline 54 5)))   
  ;(princ "\nfound element")
  (repeat 4 (read-line opf))
  (repeat nel
    (setq l (read-line opf))
    (setq elist (append elist (list (cons (atoi (substr l  1 5))
					  (list (atoi (substr l  6 5))
					        (atoi (substr l 11 5)))))))
  );repeat
  
  (close opf)
  
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
(defun makedb24(nlist elist sele eele /
		defaultfn nlist elist sele eele opf index firstx load1 load2 load3
		gcount )
    
  (setq defaultfn (strcat (getpath outfilename) "db24"))
  (setq opf (open (getfiled "OUTPUT 3D *.lin file" defaultfn "LIN" 1) "w"))
  (write-line "START" opf)
  (write-line (strcat "TITLE " title) opf)
  (write-line "" opf)
  
  (setq index 0
	enum sele)

  (setq firstnode (cadr (assoc sele elist)))

  (repeat (- eele sele -1)
    (setq firstx (cdr (assoc (cadr (assoc enum elist)) nlist)))
    (setq load1 (getelx nlist elist firstx sele eele))
    (setq load2 (getelx nlist elist (+ firstx 4.2) sele eele))       ;find Pn
    (setq load3 (getelx nlist elist (+ firstx 8.4) sele eele))
   
    (write-line (strcat "LOAD C=" (itoa (+ index startlc))
			"  H=<" lchead (rtosfw (car load1) 5 0) ">") opf)
    (setq gcount 0)
    (repeat ng
      (if (/= load1 nil)
        (write-line (strcat (rtosfw (+ (* gcount ginc) (car load1))  5 0)
			    "  T=cg p=0,"
			    (nth gcount prlist)
       	  		    "  d=" (rtosfw (cadr load1) 8 3)) opf))
      (if (/= load2 nil)
        (write-line (strcat (rtosfw (+ (* gcount ginc) (car load2)) 5 0)
			    "  T=cg p=0,"
			    (nth gcount prlist)
	  		    "  d=" (rtosfw (cadr load2) 8 3)) opf))
      (if (/= load3 nil)
        (write-line (strcat (rtosfw (+ (* gcount ginc) (car load3)) 5 0)
			    "  T=cg p=0,"
			    (nth gcount pflist)
  			    "  d=" (rtosfw (cadr load3) 8 3)) opf))
      (setq gcount (1+ gcount))
    );repeat  
    (write-line "" opf)
    
    (setq index (1+ index))
    (setq enum (1+ enum))
  );repeat

  (setq lastnode (caddr (assoc eele elist)))
  (write-line (strcat "LOAD C=" (itoa (+ startlc index))
  			"  H=<" lchead ">") opf)
  
  (setq gcount 0)
  (repeat ng
    (write-line (strcat (rtosfw (+ (* gcount ginc) lastnode)  5 0)
			"  T=cn p=0,"
			(nth gcount prlist)) opf)
    (setq gcount (1+ gcount))
  )  
  
  (write-line "" opf)

  (write-line "stop" opf)

  (close opf)
  
);defun   


;---------------------------------
; function : rtosfw
;            rtos fixed width
;            By Yi Seok Jong (dolljong@dreamwiz.com)
;            2000/7/29
;---------------------------------
; 주어진 숫자를 일정폭을 가진 string으로 변환하여 리턴해준다.
; fortran에서 f10.3과 같은 역할을 한다.
; 출력시 열을 맞출 때 필요
; ex) (rtosfw 3.456 10 3)  --> "     3.456")
;-----------------------------------
(defun rtosfw(num width dec / num width dec str1 lenstr errmsg)
  (setq str1 (rtos num 2 dec))
  (setq lenstr (strlen str1))
  (if (> lenstr width)
    (progn
      (setq errmsg (strcat "String too long (" (rtos lenstr 2 0) ") > " (rtos width 2 0) "(Width)"))
      (alert errmsg)(exit)
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
; 주어진 문자열을 주어진 횟수만큼 더한다. 자릿수를 맞출 때 유용
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
