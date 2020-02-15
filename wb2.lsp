;-------------------
; Program : WBNAME (WBlock File NAME)
;           YI Suk Jong
;           99/9/9
;-------------------
(defun c:wb2()
  (setq ss (entsel "select text: "))             ;select text
  (setq ben (car (entsel "Select Boundary: ")))  ;select boundary 
  (setq sn (car ss))  ;sname
  (setq ipnt (cadr ss))

  (setq fname (getvar "dwgprefix"))    ;filename·i ¤w·¡Ÿq·a¡
  (setq dwgname (getvar "dwgname"))
  (setq fname (strcat fname (substr dwgname 1 (vl-string-search "_" dwgname))
		      "_apt_block_"
		      ))

  (setq txt (cdr (assoc 1 (entget sn)))) 
  (setq fname (strcat fname txt))
  

;  (setq ipnt (getpoint "\nInsertion base point:")) ;¬s·³¸ñ ¬åÈ‚
  (setq sse (ssget))                        ;wblockÐi entity¬åÈ‚
  

  (command "PEDIT" ben "W" "3" "X")
  
  (setvar "FILEDIA" 0)                      ;¡ww»¥Ð—¯¡ dialog box›a»¡ ´g•¡¢

  (command "WBLOCK"          ;wblock¡ww ¯©Ð—
           fname             ;filename
           ""                ;block name
           ipnt              ;insert point
           sse               ;¬åÈ‚–E entity
           "")               ;end selection
  
;  (command "oops")
  
  (setvar "FILEDIA" 1)                      ;

);defun



    ; -------------------------------------
; function : getLwVert
; LwPolylineÀÇ Vertex¸¦ Ã´¾Æ
; ÀÎ¼ö: vlist  : vertext list
;       tmpctr : Á¢±ÙÇÒ vertext ¹øÈ£ 0,1,2
; -------------------------------------

  (defun getLwVert (vlist tmpctr / count tmp)
;    (setq vlist (entget (car (entsel))))       		;½Ç

    (setq count 0)					;Ã¹ vertex Ã£¾Æ°¨
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
