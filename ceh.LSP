;------------------------------------------
; Program : ceh
;           Copy Entities Hatch layer
;           By Yi Suk Jong (http://user.chollian.net/~dolljong)
;           2000/9/24
;------------------------------------------
(defun c:ceh()

  (setq #oldlayer (getvar "CLAYER"))
  
  (setq ssent (ssget))
  (setq nssent (sslength ssent)
	count 0)
  (repeat nssent
    (setq entinfo (entget (ssname ssent count)))
    (setq oldlay (assoc 8 entinfo))
    (setq newlay (cons 8 "hatch"))
    (setq newent (subst newlay oldlay entinfo))
    (entmake newent)
    (setq count (1+ count))
  );  
	

  
;  (command "copy" ssent "" "0,0"  "_'clayer" "hatch" "0,0")
  (command "layer" "on" "hatch"  "set" "hatch")
  (setq lays (gettablelist "layer"))
  (setq nlays (length lays)
	count 0)

  (repeat nlays
    (setq lay (strcase (nth count lays)))
    (if (/= lay "HATCH")
      (command "OFF" lay)
    );if  
    (setq count (1+ count))
  );repeat
  (command)
);defun

;---------------------------------------------------------------
; Function : gettablelist
;            By Yi Suk Jong
;            dolljong@dreamwiz.com
;            2000/9/26
;---------------------------------------------------------------

(defun gettablelist(item / item )
  (setq tablelist (list (cdr (assoc 2 (tblnext item 1))) ))
  (while (setq tbl (tblnext item))
    (setq tablelist (append tablelist (list (cdr (assoc 2 tbl)))))
  );while  
);defun

(defun c:ceh1( )

  (setq lays (gettablelist "layer"))
  (setq nlays (length lays)
	count 0)
  
  (command "layer" ) 
  (repeat nlays
    (setq lay (strcase (nth count lays)))
    (command "ON" lay)
    (setq count (1+ count))
  );repeat
  (command)
  
  (setvar "CLAYER" #oldlayer)
  
);defun
