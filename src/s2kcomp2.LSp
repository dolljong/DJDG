
;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************

(defun DATA-IN1(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs ",") (= subs "") (= subs " "))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (if (/= lst "") (setq rslt (list lst)))                  ;돌림값이 비었을때
               (if (/= lst "") (setq rslt (append rslt (list lst))))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC



(defun c:test( / )

  (setq elforce (read2k (getfiled "Open data file" "" "txt" 0)))
  
  (setq maxmin (compforce elforce '(1001 1002) 1002 nil))
  
;  (setq maxmin0 (compforce elforce '(1501 1504) 1003 nil))
;  (setq maxmin1 (appendcase maxmin maxmin0))
;  (factforce maxmin1 2.0)  
  (printresult1 maxmin '(1002) "kk")

;  (setq maxmin1 (compforce maxmin '(1002) 1004 maxmin0))  
  (princ)
)	

;------------------------------------------------------------
; Function : READ2K
;            read sap2000 output file (*.txt)
;            Yi seok jong
;            2000/8/23
;------------------------------------------------------------
; ex) read2k("c:\result.txt")
; return: ((1001 (1001 (i-node forces )( j-node forces ))
;		 (1002 (i-node forces )( j-node forces ))
;		 ....)
;          (1002 (1001 (i-node forces )( j-node forces ))
;                (1002 (i-node forces )( j-node forces ))
;		 ....)
;          ....)
(defun read2k(fn / fn opf forcelist ln ellist ellc inode jnode)
  (print "Read ")(princ fn)
  (setq opf (open fn "r"))
  (while (/= (substr (setq ln (read-line opf)) 1 40) " F R A M E   E L E M E N T   F O R C E S"))
  (repeat 3 (read-line opf))
  (setq forcelist nil)
  (setq ln (read-line opf))
  (setq ellist (list (atoi (car (data-in1 ln)))))
  (while (/= ln nil)
    (while (/= ln "   ")
      (setq ellc (data-in1 ln)) ;(print "ele: ")(princ ellc)
      (setq inode (s2kforce (read-line opf))) ;(print "i: ")(princ inode)
      (setq jnode (s2kforce (read-line opf))) ;(print "j: ")(princ jnode)
      (setq ellist (append ellist (list (list (atoi (cadr ellc)) inode jnode))))  ;(print caselist)
      (setq ln (read-line opf))
    );if
    (setq ln (read-line opf))
    (setq forcelist (append forcelist (list ellist)));(print ellist)
    (if (and (/= ln "   ") (/= ln nil)) (setq ellist (list (atoi (car (data-in1 ln))))))
  )  
  ;(alert "frame found")
;  (princ forcelist)
  
  (close opf)
  forcelist
);defun

;--------------------------------------------------
; Function : s2kforce
;            Yi Seok Jong
;            2000/8/23
;--------------------------------------------------
; ex) (s2kforce "                  0.00     -453.62       -4.64       -1.97  -9.973E-02   1.366E-01        0.00")
; return: (0.00 -453.62 -4.64 -1.97 -9.973E-2 1.366E-1 0.0)
;--------------------------------------------------
(defun s2kforce(ln / ln )
   (list (atof (substr ln 23 12))
	(atof (substr ln 35 12))
	(atof (substr ln 47 12))
	(atof (substr ln 59 12))
	(atof (substr ln 71 12))
	(atof (substr ln 83 12)))
);defun

;-------------------------------------------------------
; Function : compforce
;            compare element force of sap2000
;            Yi Suk Jong
;            2000/8/23
;-------------------------------------------------------
; ex) (compforce
;       ((1001 (1001 (i-node force) (j-node force))...)...)
;       (1001 1002 1003....)
;       ((1001 (i-node f1max) (j-node f1max) (i-node f2max) (j-node f2max)....) ...))
; return:
;       ((1001 (i-node f1max) (j-node f1max) (i-node f2max) (j-node f2max)....) ...))
;-------------------------------------------------------
(defun compforce(elforce caselist casenum opt
                 / elforce caselist casenum optcount
                   nel elcount cases ncase elnum ijforcenum firstcasei firstcasej
                   f1maxi f2maxi f3maxi f4maxi f5maxi f6maxi
                   f1mini f2mini f3mini f4mini f5mini f6mini
                   f1maxj f2maxj f3maxj f4maxj f5maxj f6maxj
                   f1minj f2minj f3minj f4minj f5minj f6minj maxmin
                   casecount nrepeat maxminf forcei forcej  )
 (setq nel (length elforce))
 (setq maxmin nil)
  
 (checkcase elforce caselist) 
  
 (setq elcount 0) 
 (repeat nel
   (setq cases (nth elcount elforce)
	 ncase (length caselist)
	 elnum (nth 0 cases))
   
;   (setq firstcasei (nth 1 (assoc (car caselist) (cdr cases)))
;	 firstcasej (nth 2 (assoc (car caselist) (cdr cases))))
   
   (cond
     ((= opt nil)
       (setq ijforcenum (length (assoc (car caselist) (cdr cases))))
       (cond 
         ((<= ijforcenum 3)
	   (setq firstcasei (nth 1 (assoc (car caselist) (cdr cases)))
	 	 firstcasej (nth 2 (assoc (car caselist) (cdr cases))))
	   (setq f1maxi firstcasei   f2maxi firstcasei   f3maxi firstcasei
		 f4maxi firstcasei   f5maxi firstcasei   f6maxi firstcasei

	 	 f1mini firstcasei   f2mini firstcasei   f3mini firstcasei
	 	 f4mini firstcasei   f5mini firstcasei   f6mini firstcasei

	 	 f1maxj firstcasej   f2maxj firstcasej   f3maxj firstcasej
	 	 f4maxj firstcasej   f5maxj firstcasej   f6maxj firstcasej

	 	 f1minj firstcasej   f2minj firstcasej   f3minj firstcasej
	 	 f4minj firstcasej   f5minj firstcasej   f6minj firstcasej
	 	 casecount 1
	 	 nrepeat (1- ncase))
	  );sub cond
	 ((> ijforcenum 3)
	   (setq firstcase (assoc (car caselist) (cdr cases)))
	   (setq f1maxi (nth  1 firstcase)   f2maxi (nth  3 firstcase)   f3maxi (nth  5 firstcase)
		 f4maxi (nth  7 firstcase)   f5maxi (nth  9 firstcase)   f6maxi (nth 11 firstcase)

	 	 f1mini (nth 13 firstcase)   f2mini (nth 15 firstcase)   f3mini (nth 17 firstcase)
	 	 f4mini (nth 19 firstcase)   f5mini (nth 21 firstcase)   f6mini (nth 23 firstcase)

	 	 f1maxj (nth  2 firstcase)   f2maxj (nth  4 firstcase)   f3maxj (nth  6 firstcase)
	 	 f4maxj (nth  8 firstcase)   f5maxj (nth 10 firstcase)   f6maxj (nth 12 firstcase)

	 	 f1minj (nth 14 firstcase)   f2minj (nth 16 firstcase)   f3minj (nth 18 firstcase)
	 	 f4minj (nth 20 firstcase)   f5minj (nth 22 firstcase)   f6minj (nth 24 firstcase)
	 	 casecount 1
	 	 nrepeat (1- ncase))	  
	 );sub cond
       ); cond
     );sub cond
     ((/= opt nil)
       (setq
         maxminf (cdr (nth 1 (nth elcount opt)))
         f1maxi (nth  0 maxminf)  f2maxi (nth  2 maxminf)  f3maxi (nth  4 maxminf)
	 f4maxi (nth  6 maxminf)  f5maxi (nth  8 maxminf)  f6maxi (nth 10 maxminf)

	 f1mini (nth 12 maxminf)  f2mini (nth 14 maxminf)  f3mini (nth 16 maxminf)
	 f4mini (nth 18 maxminf)  f5mini (nth 20 maxminf)  f6mini (nth 22 maxminf)

	 f1maxj (nth  1 maxminf)  f2maxj (nth  3 maxminf)  f3maxj (nth  5 maxminf)
	 f4maxj (nth  7 maxminf)  f5maxj (nth  9 maxminf)  f6maxj (nth 11 maxminf)

	 f1minj (nth 13 maxminf)  f2minj (nth 15 maxminf)  f3minj (nth 17 maxminf)
	 f4minj (nth 19 maxminf)  f5minj (nth 21 maxminf)  f6minj (nth 23 maxminf)
         casecount 0
	 nrepeat ncase)
      );sub cond
   );cond
;   (setq casecount 1)
   (repeat nrepeat
     (setq thisforce (assoc (nth casecount caselist) (cdr cases))
	   nthisforce (length thisforce))
     (cond
       ((> nthisforce 3)
	 (setq forcef1maxi (nth  1 thisforce) forcef2maxi (nth  3 thisforce) forcef3maxi (nth  5 thisforce)
	       forcef4maxi (nth  7 thisforce) forcef5maxi (nth  9 thisforce) forcef6maxi (nth 11 thisforce)
	       forcef1mini (nth 13 thisforce) forcef2mini (nth 15 thisforce) forcef3mini (nth 17 thisforce)
	       forcef4mini (nth 19 thisforce) forcef5mini (nth 21 thisforce) forcef6mini (nth 23 thisforce)
	       forcef1maxj (nth  2 thisforce) forcef2maxj (nth  4 thisforce) forcef3maxj (nth  6 thisforce)
	       forcef4maxj (nth  8 thisforce) forcef5maxj (nth 10 thisforce) forcef6maxj (nth 12 thisforce)
	       forcef1minj (nth 14 thisforce) forcef2minj (nth 16 thisforce) forcef3minj (nth 18 thisforce)
	       forcef4minj (nth 20 thisforce) forcef5minj (nth 22 thisforce) forcef6minj (nth 24 thisforce))
	       
         (if (> (nth 0 forcef1maxi) (nth 0 f1maxi))
           (setq f1maxi forcef1maxi))
	
         (if (< (nth 0 forcef1mini) (nth 0 f1mini))
	   (setq f1mini forcef1mini))
	
         (if (> (nth 0 forcef1maxj) (nth 0 f1maxj))
           (setq f1maxj forcef1maxj))
	
         (if (< (nth 0 forcef1minj) (nth 0 f1minj))
	   (setq f1minj forcef1minj))
     
         (if (> (nth 1 forcef2maxi) (nth 1 f2maxi))
           (setq f2maxi forcef2maxi))
	
         (if (< (nth 1 forcef2mini) (nth 1 f2mini))
	   (setq f2mini forcef2mini))
         
         (if (> (nth 1 forcef2maxj) (nth 1 f2maxj))
           (setq f2maxj forcef2maxj))
	
         (if (< (nth 1 forcef2minj) (nth 1 f2minj))
	   (setq f2minj forcef2minj))

         (if (> (nth 2 forcef3maxi) (nth 2 f3maxi))
           (setq f3maxi forcef3maxi))
	
         (if (< (nth 2 forcef3mini) (nth 2 f3mini))
	   (setq f3mini forcef3mini))
     
         (if (> (nth 2 forcef3maxj) (nth 2 f3maxj))
           (setq f3maxj forcef3maxj))
	
         (if (< (nth 2 forcef3minj) (nth 2 f3minj))
	   (setq f3minj forcef3minj))

         (if (> (nth 3 forcef4maxi) (nth 3 f4maxi))
           (setq f4maxi forcef4maxi))
	
         (if (< (nth 3 forcef4mini) (nth 3 f4mini))
	   (setq f4mini forcef4mini))
     
         (if (> (nth 3 forcef4maxj) (nth 3 f4maxj))
           (setq f4maxj forcef4maxj))
	
         (if (< (nth 3 forcef4minj) (nth 3 f4minj))
	   (setq f4minj forcef4minj))

         (if (> (nth 4 forcef5maxi) (nth 4 f5maxi))
           (setq f5maxi forcef5maxi))
	
         (if (< (nth 4 forcef5mini) (nth 4 f5mini))
	   (setq f5mini forcef5mini))
     
         (if (> (nth 4 forcef5maxj) (nth 4 f5maxj))
           (setq f5maxj forcef5maxj))
	
         (if (< (nth 4 forcef5minj) (nth 4 f5minj))
	   (setq f5minj forcef5minj))

         (if (> (nth 5 forcef6maxi) (nth 5 f6maxi))
           (setq f6maxi forcef6maxi))
	
         (if (< (nth 5 forcef6mini) (nth 5 f6mini))
	   (setq f6mini forcef6mini))
     
         (if (> (nth 5 forcef6maxj) (nth 5 f6maxj))
           (setq f6maxj forcef6maxj))
	
         (if (< (nth 5 forcef6minj) (nth 5 f6minj))
	   (setq f6minj forcef6minj))
       );subcond
       ((<= nthisforce 3)
         (setq forcei (nth 1 thisforce)
	       forcej (nth 2 thisforce))
         ;; i-node F1 max/min
         (if (> (nth 0 forcei) (nth 0 f1maxi))
           (setq f1maxi forcei)
           (if (< (nth 0 forcei) (nth 0 f1mini))
	     (setq f1mini forcei)
           );if
         );if

     
         ;; j-node F1 max/min
         (if (> (nth 0 forcej) (nth 0 f1maxj))
           (setq f1maxj forcej)
           (if (< (nth 0 forcej) (nth 0 f1minj))
	     (setq f1minj forcej)
           );if
         );if
     
         ;; i-node F2 max/min
         (if (> (nth 1 forcei) (nth 1 f2maxi))
           (setq f2maxi forcei)
           (if (< (nth 1 forcei) (nth 1 f2mini))
	     (setq f2mini forcei)
           );if
         );if
         
         ;; j-node F2 max/min
         (if (> (nth 1 forcej) (nth 1 f2maxj))
           (setq f2maxj forcej)
           (if (< (nth 1 forcej) (nth 1 f2minj))
	     (setq f2minj forcej)
           );if
         );if  

         ;; i-node F3 max/min
         (if (> (nth 2 forcei) (nth 2 f3maxi))
           (setq f3maxi forcei)
           (if (< (nth 2 forcei) (nth 2 f3mini))
	     (setq f3mini forcei)
           );if
         );if
     
         ;; j-node F3 max/min
         (if (> (nth 2 forcej) (nth 2 f3maxj))
           (setq f3maxj forcej)
           (if (< (nth 2 forcej) (nth 2 f3minj))
	     (setq f3minj forcej)
           );if
         );if  

         ;; i-node F4 max/min
         (if (> (nth 3 forcei) (nth 3 f4maxi))
           (setq f4maxi forcei)
           (if (< (nth 3 forcei) (nth 3 f4mini))
	     (setq f4mini forcei)
           );if
         );if
     
         ;; j-node F4 max/min
         (if (> (nth 3 forcej) (nth 3 f4maxj))
           (setq f4maxj forcej)
           (if (< (nth 3 forcej) (nth 3 f4minj))
	     (setq f4minj forcej)
           );if
         );if  

         ;; i-node F5 max/min
         (if (> (nth 4 forcei) (nth 4 f5maxi))
           (setq f5maxi forcei)
           (if (< (nth 4 forcei) (nth 4 f5mini))
	     (setq f5mini forcei)
           );if
         );if
     
         ;; j-node F5 max/min
         (if (> (nth 4 forcej) (nth 4 f5maxj))
           (setq f5maxj forcej)
           (if (< (nth 4 forcej) (nth 4 f5minj))
	     (setq f5minj forcej)
           );if
         );if  

         ;; i-node F6 max/min
         (if (> (nth 5 forcei) (nth 5 f6maxi))
           (setq f6maxi forcei)
           (if (< (nth 5 forcei) (nth 5 f6mini))
	     (setq f6mini forcei)
           );if
         );if
     
         ;; j-node F6 max/min
         (if (> (nth 5 forcej) (nth 5 f6maxj))
           (setq f6maxj forcej)
           (if (< (nth 5 forcej) (nth 5 f6minj))
	     (setq f6minj forcej)
           );if
         );if
       );subcond
     );cond  
     
     (setq casecount (1+ casecount))	   
	   
   );repeat  
;;;(print elnum)   (princ "i maxF1: ") (princ f1maxi)
;;;(print elnum)   (princ "j maxF1: ")  (princ f1maxj)
;;;(print elnum)   (princ "i minF1: ")  (princ f1mini)
;;;(print elnum)   (princ "j minF1: ")  (princ f1minj)
;;;(print elnum)   (princ "i maxF2: ") (princ f2maxi)
;;;(print elnum)   (princ "j maxF2: ") (princ f2maxj)
;;;(print elnum)   (princ "i minF2: ") (princ f2mini)
;;;(print elnum)   (princ "j minF2: ") (princ f2minj)
;;;(print elnum)   (princ "i maxF3: ") (princ f3maxi)
;;;(print elnum)   (princ "j maxF3: ") (princ f3maxj)
;;;(print elnum)   (princ "i minF3: ") (princ f3mini)
;;;(print elnum)   (princ "j minF3: ") (princ f3minj)
;;;(print elnum)   (princ "i maxF4: ") (princ f4maxi)
;;;(print elnum)   (princ "j maxF4: ") (princ f4maxj)
;;;(print elnum)   (princ "i minF4: ") (princ f4mini)
;;;(print elnum)   (princ "j minF4: ") (princ f4minj)
;;;(print elnum)   (princ "i maxF5: ") (princ f5maxi)
;;;(print elnum)   (princ "j maxF5: ") (princ f5maxj)
;;;(print elnum)   (princ "i minF5: ") (princ f5mini)
;;;(print elnum)   (princ "j minF5: ") (princ f5minj)
;;;(print elnum)   (princ "i maxF6: ") (princ f6maxi)
;;;(print elnum)   (princ "j maxF6: ") (princ f6maxj)
;;;(print elnum)   (princ "i minF6: ") (princ f6mini)
;;;(print elnum)   (princ "j minF6: ") (princ f6minj)
   (setq maxmin (append maxmin (list (list elnum   (list casenum f1maxi f1maxj
				                                 f2maxi f2maxj
				                   		 f3maxi f3maxj
				                   		 f4maxi f4maxj
				                   		 f5maxi f5maxj
				                   		 f6maxi f6maxj
				                   		 f1mini f1minj
				                   		 f2mini f2minj
				                   		 f3mini f3minj
				                   		 f4mini f4minj
			                   			 f5mini f5minj
				                   		 f6mini f6minj)))))
				           
   (setq elcount (1+ elcount))
 );repeat
 (setq maxmin maxmin) 
);defun


;---------------------------------------------------------------------------
; Function : GetInput
;            Get comp/comb input
;            Yi Seok Jong
;            2000/8/23
;---------------------------------------------------------------------------
; ex) (getinput "cmp")
; return : (("comp" "c=1002") ("1001" "1002" "fi=md") ("1501" "1504" "fi=md")...)
; input file format:
;     comp c=1002
;     1001,1002 fi=md
;     1501,1504 fi=md
;---------------------------------------------------------------------------
(defun getinput(ext / input opf ln fname ext)
  (setq input nil)
  (setq fname (getfiled "Open data file" "" ext 0))
  (setq opf (open fname "r"))
  (while (setq ln (read-line opf))
    (if (/= ln "") (setq input (append input (list (data-in1 ln)))))    
;    (setq input (append input (list (data-in1 ln))))
  );while
  (close opf)
  (setq input (list fname input ))

);defun


;----------------------------------------------------------------------
; program : main
;           By Yi Seok Jong
;           2000/8/24
;----------------------------------------------------------------------
(defun c:s2kcomp( / input1 inputfn input count path cmdcount casenum cmd cmdset ncmd results)
  (setq oldfn nil)
  (setq input1  (getinput "cmp")
	inputfn (car input1)
	path (getpath inputfn)
	input (cadr input1)
	cmdset (splitinput input)
	ncmd (length cmdset))
  (setq cmdcount 0)
  (repeat ncmd
    (setq cmd (strcase (caar (nth cmdcount cmdset))))
    (cond
      ((= cmd "COMP")
        (setq results (compcmd (nth cmdcount cmdset)))
       );sub cond
      ((= cmd "COMB")
        (setq results (combcmd (nth cmdcount cmdset)))
      );sub cond 
    );cond  
    (if (= cmdcount 0)
      (setq finalresults results) 
      (setq finalresults (appendcase finalresults results))
    );if  
    (setq cmdcount (1+ cmdcount))  
  );repeat
  
  (setq outfn (strcat (substr inputfn 1 (- (strlen inputfn) 4)) ".c2k"))
;  (printresult finalresults outfn)
  (printresult1 finalresults '(1002) outfn)	
);defun



;-----------------------------------
;function : getpath
;           Yi Seok Jong
;           2000/4/20
;-----------------------------------
;
(defun getpath(fn / fn index )
  (setq index (1- (strlen fn)))
	
  (while (/= (substr fn index 1) "\\")
    (setq index (1- index))
  );while
  (substr fn 1 index)
);defun

;----------------------------------
; function : str_tail
;            Yi Suk Jong
;            00/8/21
;----------------------------------
; str1 : long string
; str2 : short string
; ex) (str_tail "fi=md" "=")
; return: "md"
;----------------------------------
(defun str_tail(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
	len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1))
   nil
   (substr str1 (+ count len2) (- len1 count len2 -1))
 );if  
);defun str_position

;----------------------------------
; function : str_head
;            Yi Suk Jong
;            00/8/26
;----------------------------------
; str1 : long string
; str2 : short string
; ex) (str_tail "fi=md" "=")
; return: "fi"
;----------------------------------
(defun str_head(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
	len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (= count 1)
   nil
   (substr str1 1 (1- count))
 );if  
);defun str_position


;------------------------------------------------------------
; Function : fromtostep
;            By Yi Seok Jong
;            2000/8/23
;------------------------------------------------------------
; ex) (fromtostep 100 110 2)
; return : (100 102 104 106 108 110)
;------------------------------------------------------------
(defun fromtostep(from to step / numlist num from to step)
  (setq  numlist (list from)
         num from)
  (while (<= (setq num (+ num  step)) to)
    (setq numlist (append numlist (list num)))
  );while
  (setq numlist numlist)
);defun


;-----------------------------------------------------------------
; Function : SplitInput
;            By Yi Seok Jong
;            2000/8/23
;-----------------------------------------------------------------
; ex) (splitinput (("comp" "c=1002") ("1001" "1002" "fi=md") ("comp" "c=1002") ("1501" "1504" "fi=md")))
; return: ((("comp" "c=1002") ("1001" "1002" "fi=md")) (("comp" "c=1002") ("1501" "1504" "fi=md")))
;-----------------------------------------------------------------
(defun splitinput(input / input ninput count icount onecmd lastcmd aline inputs  )
  (setq ninput (length input)
	count 0
	inputs nil
	lastcmd 0)
  (repeat ninput
    (setq aline (nth count input))
    (if (and (/= 0 count) (or (= (strcase (car aline)) "COMP") (= (strcase (car aline)) "COMB")))
      (progn
	(setq icount lastcmd
	      onecmd nil)
	(while (< icount count)
	  (setq onecmd (append onecmd (list (nth icount input))))
	  (setq icount (1+ icount))
	);while
	(setq lastcmd count)
	(setq inputs (append inputs (list onecmd)))
      );progn	
    );if	
    (setq count (1+ count))
  );repeat
  (setq icount lastcmd
        onecmd nil)  
  (while (< icount ninput)
    (setq onecmd (append onecmd (list (nth icount input))))
    (setq icount (1+ icount))
  );while
  (setq inputs (append inputs (list onecmd)))
);


;------------------------------------------------------------------------
; function : compcmd
;            compare
;            Yi Seok Jong
;            2000/8/22
;------------------------------------------------------------------------
; (compcmd cmdset)
;    cmdset : (("1001" "1002" "fi=md") ("1501" "1504" "fi=md"))
;    return : ((1001 (f1maxi )(f1maxj )(f2maxi)(f2maxj )...( )( ))
;              (1002 (f1maxi )(f1maxj )(f2maxi)(f2maxj )...( )( ))
;              .....)
;------------------------------------------------------------------------
(defun compcmd(cmdset / cmdset count caselist lcnum cmdline fn)
  (setq count 1
	lcnum (atoi (str_tail (cadr (nth 0 cmdset)) "=")))
;  (print fn  )(princ ":oldfn")(princ oldfn)
  (repeat (1- (length cmdset))
    (setq cmdline (nth count cmdset))
    (print cmdline)
    (setq cmdline (commandl cmdline))	   
    (setq fn (cdr (assoc "FI" cmdline))
          caselist (fromtostep (cdr (assoc "FROM" cmdline))
			       (cdr (assoc "TO" cmdline))
			       (cdr (assoc "STEP" cmdline))))
    (if (= fn nil)
      (setq elforce finalresults
	    oldfn nil)
      (progn
	(if (/= fn oldfn)
          (setq elforce (read2k (strcat path fn ".txt"))	
     	        oldfn fn)
	);if
      );progn
    );if
	
    (if (= count 1)
      (setq maxmin (compforce elforce caselist lcnum nil))
      (setq maxmin (compforce elforce caselist lcnum maxmin))
    );if  
    (setq count (1+ count))
  );repeat
  maxmin
);defun

;------------------------------------------------------------------------
; function : combcmd
;            combination
;            Yi Seok Jong
;            2000/8/25
;------------------------------------------------------------------------
; (combcmd cmdset)
;    cmdset : (("1001" "1002" "fi=md") ("1501" "1504" "fi=md"))
;    return : ((1001 (f1maxi )(f1maxj )(f2maxi)(f2maxj )...( )( ))
;              (1002 (f1maxi )(f1maxj )(f2maxi)(f2maxj )...( )( ))
;              .....)
;------------------------------------------------------------------------
(defun combcmd(cmdset / cmdset count caselist lcnum sf lcnum cmdline fn combi)
  (setq count 1
	lcnum (atoi (str_tail (cadr (nth 0 cmdset)) "=")))
;  (print fn  )(princ ":oldfn")(princ oldfn)
  (repeat (1- (length cmdset))
    (setq cmdline (nth count cmdset))
    (print cmdline)
    (setq cmdline (commandl cmdline))
    (setq fn  (cdr (assoc "FI" cmdline))
          caselist (fromtostep (cdr (assoc "FROM" cmdline))
			       (cdr (assoc "TO"   cmdline))
			       (cdr (assoc "STEP" cmdline))))
    (if (= fn nil)
      (setq elforce finalresults
	    oldfn nil)
      (progn
	(if (/= fn oldfn)
          (setq elforce (read2k (strcat path fn ".txt"))	
   	        oldfn fn)
        );if
      );progn
    );if
    (if (= (assoc "M" cmdline) nil)
      (setq sf 1.0)
      (setq sf (atof (cdr (assoc "M" cmdline))))
    );if
    (if (= count 1)
      (setq combi (combforce elforce caselist lcnum sf nil))
      (setq combi (combforce elforce caselist lcnum sf combi))
    );if  
    (setq count (1+ count))
  );repeat
  combi
);defun


;----------------------------------------------------------------
; function : combforce
;            By Yi Seok Jong
;            2000/8/23
;----------------------------------------------------------------
; ex) (combforce elforce lclist 1.5 nil)
(defun combforce(elforce lclist lcnum sf opt 
                  / nlc nele result firstlc count force elforce lclist sf opt count elnum lcnum final
                    eleresult neleresult eforce lccount lcforce nlcforce addedforce )
  (checkcase elforce lclist)
  (setq nlc (length lclist)
	nele (length elforce))
  (if (= opt nil) 
    (progn
      (setq result nil
	    firstlc (car lclist)
;	    nele (length elforce)
	    count 0    )
      
      (repeat nele
	(setq force (nth count elforce))
	(setq elnum (car force))
	(setq result (append result (list (cons elnum (list (cons lcnum (cdr (assoc firstlc (cdr force)))))))))
	(setq count (1+ count))
      );repeat
      (setq result (factforce result sf))
      (setq nlc (1- nlc)
	    firstlc 1)
    );progn
    (progn
      (setq result nil
;	    nele (length opt)
	    count 0    )
      
      (repeat nele
	(setq force (nth count opt))
	(setq elnum (car force))
	(setq result (append result (list (cons elnum (list (cons lcnum (cdadr force)))))))
	(setq count (1+ count))
      );repeat
      (setq firstlc 0)
    );progn
  );if
  (setq final nil
        elecount 0)
  (repeat nele
    (setq elnum (car (nth elecount result))
          eleresult (cadr (nth elecount result))
	  neleresult (length eleresult)
          eforce (cdr (nth elecount elforce))
	  lccount firstlc)
    (repeat nlc
      (setq lcforce (assoc (nth lccount lclist) eforce)
;    (mapcar '(lambda(x) (mapcar '(lambda(y) (* y sf)) x)) '((1 2 3) (2 3 4)))
	    nlcforce (length lcforce)
	    neleresult (length eleresult))
      (cond
	((and (<= neleresult 3) (<= nlcforce 3))
;	  (setq addedeforce (list lcnum (mapcar '+ (nth 1 eleresult) (nth 1 lcforce))
	  (setq eleresult   (list lcnum (mapcar '+ (nth 1 eleresult) (factatom (nth 1 lcforce) sf))
				        (mapcar '+ (nth 2 eleresult) (factatom (nth 2 lcforce) sf))))
	);subcond
	((and (<= neleresult 3) (> nlcforce 3))
;          (setq addedeforce (list lcnum  (mapcar '+ (nth 1 eleresult) (nth  1 lcforce))
          (setq eleresult (list lcnum   (mapcar '+ (nth 1 eleresult) (factatom (nth  1 lcforce) sf))
	                                (mapcar '+ (nth 2 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth  3 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth  4 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth  5 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth  6 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth  7 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth  8 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth  9 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 10 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth 11 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 12 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth 13 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 14 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth 15 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 16 lcforce) sf))
				        (mapcar '+ (nth 1 eleresult) (factatom (nth 17 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 18 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth 19 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 20 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth 21 lcforce) sf))				 	
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 22 lcforce) sf))
				 	(mapcar '+ (nth 1 eleresult) (factatom (nth 23 lcforce) sf))
				 	(mapcar '+ (nth 2 eleresult) (factatom (nth 24 lcforce) sf))))
				 
				 
	);subcond
	((and (> neleresult 3) (<= nlcforce 3))
;          (setq addedeforce (list lcnum  (mapcar '+ (nth  1 eleresult) (nth  1 lcforce))
          (setq eleresult  (list lcnum  (mapcar '+ (nth  1 eleresult) (factatom (nth  1 lcforce) sf))	 
				        (mapcar '+ (nth  2 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth  3 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth  4 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth  5 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth  6 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth  7 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth  8 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth  9 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 10 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 11 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 12 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 13 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 14 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 15 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 16 eleresult) (factatom (nth  2 lcforce) sf))
				        (mapcar '+ (nth 17 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 18 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 19 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 20 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 21 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 22 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth 23 eleresult) (factatom (nth  1 lcforce) sf))
				 	(mapcar '+ (nth 24 eleresult) (factatom (nth  2 lcforce) sf))))	  
	);subcond
	((and (> neleresult 3) (> nlcforce 3))
;          (setq addedeforce (list lcnum  (mapcar '+ (nth  1 eleresult) (nth  1 lcforce))
          (setq eleresult  (list lcnum  (mapcar '+ (nth  1 eleresult) (factatom (nth  1 lcforce) sf))				  
				        (mapcar '+ (nth  2 eleresult) (factatom (nth  2 lcforce) sf))
				 	(mapcar '+ (nth  3 eleresult) (factatom (nth  3 lcforce) sf))
				 	(mapcar '+ (nth  4 eleresult) (factatom (nth  4 lcforce) sf))
				 	(mapcar '+ (nth  5 eleresult) (factatom (nth  5 lcforce) sf))
				 	(mapcar '+ (nth  6 eleresult) (factatom (nth  6 lcforce) sf))
				 	(mapcar '+ (nth  7 eleresult) (factatom (nth  7 lcforce) sf))
				 	(mapcar '+ (nth  8 eleresult) (factatom (nth  8 lcforce) sf))
				 	(mapcar '+ (nth  9 eleresult) (factatom (nth  9 lcforce) sf))
				 	(mapcar '+ (nth 10 eleresult) (factatom (nth 10 lcforce) sf))
				 	(mapcar '+ (nth 11 eleresult) (factatom (nth 11 lcforce) sf))
				 	(mapcar '+ (nth 12 eleresult) (factatom (nth 12 lcforce) sf))
				 	(mapcar '+ (nth 13 eleresult) (factatom (nth 13 lcforce) sf))
				 	(mapcar '+ (nth 14 eleresult) (factatom (nth 14 lcforce) sf))
				 	(mapcar '+ (nth 15 eleresult) (factatom (nth 15 lcforce) sf))
				 	(mapcar '+ (nth 16 eleresult) (factatom (nth 16 lcforce) sf))
				        (mapcar '+ (nth 17 eleresult) (factatom (nth 17 lcforce) sf))
				 	(mapcar '+ (nth 18 eleresult) (factatom (nth 18 lcforce) sf))
				 	(mapcar '+ (nth 19 eleresult) (factatom (nth 19 lcforce) sf))
				 	(mapcar '+ (nth 20 eleresult) (factatom (nth 20 lcforce) sf))
				 	(mapcar '+ (nth 21 eleresult) (factatom (nth 21 lcforce) sf))
				 	(mapcar '+ (nth 22 eleresult) (factatom (nth 22 lcforce) sf))
				 	(mapcar '+ (nth 23 eleresult) (factatom (nth 23 lcforce) sf))
				 	(mapcar '+ (nth 24 eleresult) (factatom (nth 24 lcforce) sf))))	  	  
	);subcond  
      )	
      (setq lccount (1+ lccount))
    );repeat
;    (setq final (append final (list (list elnum  addedeforce))))
    (setq final (append final (list (list elnum  eleresult))))   
    (setq elecount (1+ elecount))  
  );repeat
  final
;  (factforce final sf)
);defun

(defun testcomb( / )
  (setq elforce (read2k (getfiled "Open data file" "" "txt" 0)))
  (setq maxmin (combforce elforce '(1001) 2001 1.0 nil))    
;  (setq result (combforce maxmin  '(2001)  2002 2.0 maxmin))
;  (setq result (appendcase result (compforce elforce '(1501 1504) 1003 nil)))
;  (setq result (combforce elforce '(1501 1504) 1003 1.0 result))
;  (setq maxmin (compforce elforce '(1501 1504) maxmin))  
  (princ)
);defun


;------------------------------------------------------------------------
; function : appendcase
;            By Yi Seok Jong
;            2000/8/23
;------------------------------------------------------------------------
(defun appendcase(result1 result2 / result1 result2 elnum count addedresult oldele newele)
  (setq elnum (length result1) ;number of element
	count 0
	addedresult result1)
  (repeat elnum
    (setq oldele (nth count addedresult)
          newele (append oldele (cdr (nth count result2))))
    (setq addedresult (subst newele oldele addedresult))
    (setq count (1+ count))
  );repeat
  addedresult
);defun

;------------------------------------------------------------------------
; function : printresult
;            By Yi Seok Jong
;            2000/8/24
;------------------------------------------------------------------------
(defun printresult(result fn / fn result opf elnum elforce elid casenum casecount caseforce
                            caseid forcenum line0 linei linej forcecount linei linej
                            elcount )
  (print "Print Output")
  (setq opf (open fn "w"))
  (setq elnum (length result)
	elcount 0)
  (repeat elnum
    (setq elforce (nth elcount result))
    (setq elid (nth 0 elforce)
          casenum (1- (length elforce))
	  casecount 1)
    (repeat casenum
      (setq caseforce (nth casecount elforce))
      (setq caseid (nth 0 caseforce)
	    forcenum (length caseforce))
      (if (= forcenum 3)
	(progn
	  (setq line0 (strcat (rtosfw elid 9 0) (rtosfw caseid 6 0)))
	  (setq linei (strcat (strcopy " " 16) "i-node" (force2str (nth 1 caseforce))))
	  (setq linej (strcat (strcopy " " 16) "j-node" (force2str (nth 2 caseforce))))
	  (write-line line0 opf)
	  (write-line linei opf)
	  (write-line linej opf)
	);progn
	(progn
	  (setq line0 (strcat (rtosfw elid 9 0) (rtosfw caseid 6 0)))
	  (setq forcecount 1)
	  (repeat 12
	    (setq linei (strcat (strcopy " " 16) "i-node" (force2str (nth forcecount caseforce))))
	    (setq forcecount (1+ forcecount))
	    (setq linej (strcat (strcopy " " 16) "j-node" (force2str (nth forcecount caseforce))))
	    (cond
	      ((= forcecount  2)  (setq line1 (strcat line0 "   F1MAX")))
	      ((= forcecount  4)  (setq line1 (strcat line0 "   F2MAX")))
	      ((= forcecount  6)  (setq line1 (strcat line0 "   F3MAX")))
	      ((= forcecount  8)  (setq line1 (strcat line0 "   F4MAX")))
	      ((= forcecount 10)  (setq line1 (strcat line0 "   F5MAX")))
	      ((= forcecount 12)  (setq line1 (strcat line0 "   F6MAX")))
	      ((= forcecount 14)  (setq line1 (strcat line0 "   F1MIN")))
	      ((= forcecount 16)  (setq line1 (strcat line0 "   F2MIN")))
	      ((= forcecount 18)  (setq line1 (strcat line0 "   F3MIN")))
	      ((= forcecount 20)  (setq line1 (strcat line0 "   F4MIN")))
	      ((= forcecount 22)  (setq line1 (strcat line0 "   F5MIN")))
	      ((= forcecount 24)  (setq line1 (strcat line0 "   F6MIN")))
	    );cond  
	    (write-line line1 opf)
	    (write-line linei opf)
	    (write-line linej opf)
	    (setq forcecount (1+ forcecount))
	  );repeat  
        );progn	
      );if 	 
      (setq casecount (1+ casecount))
    );repeat  
    (setq elcount (1+ elcount))
  );repeat
  (close opf)	
);defun


;------------------------------------------------------------------------
; function : printresult1
;            By Yi Seok Jong
;            2000/8/24
;------------------------------------------------------------------------
(defun printresult1(result caselist fn / fn result caselist opf elnum elforce elid casenum casecount caseforce
                            caseid forcenum line0 linei linej forcecount linei linej
                            elcount )
  (checkcase result caselist)      ;check case
  (print "Print Output")
  (setq opf (open fn "w"))
  (setq elnum (length result))

  
  (setq npcase (length caselist)    ;printcase
	casecount 0)
  (repeat npcase
    (setq case (nth casecount caselist))
    (setq elcount 0)
    (setq sampleforce (cdr (assoc case (cdr (nth 0 result))))
	  ncsampleforce  (length sampleforce))
    (print case)
    (cond
      ((= ncsampleforce 2)
        (repeat elnum
          (setq elid (car (nth elcount result))
		eforce (cdr (assoc case (cdr (nth elcount result)))))
	  
	  (setq line0 (strcat (rtosfw elid 9 0) (rtosfw case 6 0)))
	  (setq linei (strcat (strcopy " " 16) "i-node" (force2str (nth 0 eforce))))
	  (setq linej (strcat (strcopy " " 16) "j-node" (force2str (nth 1 eforce))))
;	  (print line0)(print linei)(print linej)
	  (write-line line0 opf)
	  (write-line linei opf)
	  (write-line linej opf)
	  
          (print (strcat (itoa case) ":" (itoa elid))) 
          (setq elcount (1+ elcount))
        );repeat
      );subcond
      ((= ncsampleforce 24)
        (setq fcount 0)
        (repeat 12
	  (cond
	    ((= fcount  0)  (setq line1 (setq fid "   F1MAX")))
	    ((= fcount  1)  (setq line1 (setq fid "   F2MAX")))
	    ((= fcount  2)  (setq line1 (setq fid "   F3MAX")))
	    ((= fcount  3)  (setq line1 (setq fid "   F4MAX")))
	    ((= fcount  4)  (setq line1 (setq fid "   F5MAX")))
	    ((= fcount  5)  (setq line1 (setq fid "   F6MAX")))
	    ((= fcount  6)  (setq line1 (setq fid "   F1MIN")))
	    ((= fcount  7)  (setq line1 (setq fid "   F2MIN")))
	    ((= fcount  8)  (setq line1 (setq fid "   F3MIN")))
	    ((= fcount  9)  (setq line1 (setq fid "   F4MIN")))
	    ((= fcount 10)  (setq line1 (setq fid "   F5MIN")))
	    ((= fcount 11)  (setq line1 (setq fid "   F6MIN")))
	  );cond  
	  (setq elcount 0)
          (repeat elnum
            (setq elid (car (nth elcount result))
		  eforce (cdr (assoc case (cdr (nth elcount result)))))
	  
	    (setq line0 (strcat (rtosfw elid 9 0) (rtosfw case 6 0) "  " fid))
	    (setq linei (strcat (strcopy " " 16) "i-node" (force2str (nth (* fcount 2) eforce))))
	    (setq linej (strcat (strcopy " " 16) "j-node" (force2str (nth (1+ (* fcount 2)) eforce))))
;	    (print line0)(print linei)(print linej)
	  (write-line line0 opf)
	  (write-line linei opf)
	  (write-line linej opf)
	  
            (print (strcat (itoa case) ":" (itoa elid))) 
            (setq elcount (1+ elcount))
	  );repeat
	  (setq fcount (1+ fcount))
        );repeat
      );subcond
      
    );cond
    (setq casecount (1+ casecount))
  );repeat  
  (print "Print Completed")
  (close opf)	
);defun


;----------------------------------------------------------------------
; functin : force2str
;           By Yi Seok Jong
;           2000/8/24
;----------------------------------------------------------------------
(defun force2str(forcelist)
  (strcat (rtosfw (nth 0 forcelist) 12 2)
	  (rtosfw (nth 1 forcelist) 12 2)
	  (rtosfw (nth 2 forcelist) 12 2)
	  (rtosfw (nth 3 forcelist) 12 2)
	  (rtosfw (nth 4 forcelist) 12 2)
	  (rtosfw (nth 5 forcelist) 12 2))
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

;---------------------------------------------------------------------------
; function : factforce
;            by Yi Seok Jong
;            2000/8/26
;---------------------------------------------------------------------------
; ex) (factforce '((1001 (1001 (1.0 2.0 3.0 4.0 5.0 6.0)))) 2.0)
;         --> ((1001 (1001 (2.0 4.0 6.0 8.0 10.0 12.0))))
(defun factforce(elforce fact / nele elecount ftotforce cases ncase elenum casecount 
                                elforce fact felforce casenum forces forcenum forcecount
                                fforce )
  (setq nele (length elforce)
	elecount 0
	ftotforce nil)
  (repeat nele
    (setq cases (cdr (nth elecount elforce))
	  ncase (length cases)
	  elenum (car (nth elecount elforce))
	  casecount 0
	  felforce (list elenum))
    
    (repeat ncase
      (setq casenum (car (nth casecount cases))
	    forces (cdr (nth casecount cases))
	    forcenum (length forces)
	    forcecount 0
	    fforce (list  casenum))
      (repeat forcenum
	(setq fforce (append fforce (list (mapcar '(lambda (x) (* x fact)) (nth forcecount forces)))))

	(setq forcecount (1+ forcecount))
      );repeat	
      (setq felforce (append felforce (list fforce)))      
      (setq casecount (1+ casecount))
    );repeat
    (setq ftotforce (append ftotforce (list felforce)))
    (setq elecount (1+ elecount))
  );repeat
  ftotforce 
);

;----------------------------------------------------------------------
; function : factatom
;            By Yi Seok Jong
;            2000/8/30
;----------------------------------------------------------------------
(defun factatom(inlist sf / inlist sf)
  (mapcar '(lambda(x) (* sf x)) inlist)
);defun  

;----------------------------------------------------------------
; functin : commandl
;           by Yi Seok Jong
;           2000/8/26
;----------------------------------------------------------------
; ex1) (commandl '("1001" "1002" "m=1.0" fi="md"))
;                --> (("FROM" . 1001") ("TO" . 1002) ("STEP" . 1) ("M" . "1.0") ("FI" . "md"))
; ex2) (commandl '("1001"  "m=1.0" fi="md"))
;                --> (("FROM" . 1001") ("TO" . 1001) ("STEP" . 1) ("M" . "1.0") ("FI" . "md"))
(defun commandl( oneline / oneline count natom firststr  from to step assline item)
  (setq natom (length oneline)
	firststr nil
	count 0)
  (repeat natom
    (if (= (atoi (nth count oneline)) 0)
      (if (= firststr nil) (setq firststr count)))
    (setq count (1+ count))  
  );repeat  

  (princ firststr)
  (cond
    ((or (= 1 firststr) (and (= nil firststr) (= count 1)))
	            (setq from (atoi (nth 0 oneline))
		          to from
		          step 1))
    ((or (= 2 firststr) (and (= nil firststr) (= count 2)))
                    (setq from (atoi (nth 0 oneline))
		          to   (atoi (nth 1 oneline))
		          step 1))
    ((= 3 firststr) (setq from (atoi (nth 0 oneline))
		          to   (atoi (nth 1 oneline))
		          step (atoi (nth 2 oneline))))
  );cond
  (setq assline (list (cons "FROM" from)
		      (cons "TO" to)
		      (cons "STEP" step)))
  (if (/= firststr nil)
    (progn
      (setq count firststr)
      (repeat (- natom count)
        (setq item (nth count oneline))
        (setq assline (append assline (list (cons (strcase (str_head item "="))
						  (str_tail item "=")))))
        (setq count (1+ count))
      );repeat
    );progn  
  );if  
  assline
);defun  

;------------------------------------------------------------------------
;function : checkcase
;           By Yi Seok Jong
;           2000/8/29
;------------------------------------------------------------------------
; ex) (checkcase '((1001 (1001 (i-froces) (j-forces)) (1002 (i-froces) (j-forces))))
(defun checkcase(elforce caselist / elforce caselist ncase0 firstelforce casecount thiscase iscase)
  (setq ncase0 (length caselist)
        firstelforce (cdr (nth 0 elforce))
        casecount 0)
  (repeat ncase0
    (setq thiscase (nth casecount caselist)
          iscase (assoc thiscase firstelforce))
    (if (= iscase  nil) (progn (alert (strcat "CASE " (itoa thiscase) " Not Found")) (exit)))
    (setq casecount (1+ casecount))
  );repeat
);defun  
  
