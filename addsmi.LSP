(defun c:smiedit(/ iopf oopf addtime linelen strpos time line)
  (setq iopf (open (getfiled "select input SMI" "" "smi" 0) "r"))
  (setq oopf (open (getfiled "select output SMI" "" "smi" 1) "w"))
  (setq addtime (getreal "\nEnter add time: "))
  (while (/= (setq line (read-line iopf)) nil)
    (if (= (substr line 1 12) "<SYNC Start=")
	 (if (/= line nil)
           (setq linelen (strlen line)
	      strpos (str_position line ">")
	      time (atof (substr line 13 (- strpos 13)))  
	      line (strcat "<SYNC Start=" (rtos (+ time addtime) 2 0) (substr line strpos (- linelen strpos -1)))
	   );setq
	 );if  
    );if	      
    (write-line line oopf)
  );while
  (close oopf)
  (close iopf)
);defun

;----------------------------------
; function : str_position
;            Yi Suk Jong
;            00/7/15
;----------------------------------
; str1 : long string
; str2 : short string
;----------------------------------
(defun str_position(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
	len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1)) nil count)
);defun str_position
      