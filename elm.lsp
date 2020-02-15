; Porgram : ELM (elevation marking)
; program : PEL (Pick Elevation)
; program : melm (multi elm)
; function : djdgf_elm

;**********************************
; Porgram : ELM
;           ELevation Marking
;           Jong-Suk Yi
;           96/6/4
;**********************************
; 05/08/09 : functionÀ» ÀÌ¿ëÇÏµµ·Ï ¼öÁ¤
(defun c:elm( / ip pv txt )
  (setq ip (getpoint "\nPick insert point: "))                ;¬s·³¸ñ ·³b
  (setq pv (getpoint ip "\nPick Position point: "))            ;®»¢ ¤wÐ· ·³b
  (setq txt (strcat "EL.=" (getstring "\nElevation: ")))      ;ÉB¯aËa ·³b
  (djdgf_elm ip pv txt)
);defun

;**********************************
; function : ELMF (ELevation Marking)
;           Jong-Suk Yi
;           96/6/4
;**********************************
; 05/08/09 : functiondÀ¸·Î ¼öÁ¤
(defun djdgf_ELM(  ip pv txt / 
		ds ip pv ph dv dh vs hs txt txtl p1 p2 tp
		)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)


  (setq ds (getvar "DIMSCALE")
      th (getvar "DIMTXT"))

  
;(setq ph (getpoint pv "\nPick Holizontal side: "))          ;®Íw¤wÐ· ·³b

  (setq dv (- (cadr pv) (cadr ip)))                           ;®»¢Àa
  (setq dh (- (car pv) (car ip)))                             ;®ÍwÀa

  (if (= dv 0) (setq vs 1) (setq vs (/ (abs dv) dv)))                                   ;®»¢¦Ñ¡
  (if (= dh 0) (setq hs 1) (setq hs (/ (abs dh) dh)))                                   ;®Íw¦Ñ¡


  (setq txtl (* th ds (+ (strlen txt) 2)))  ;–‹i¸aŸi ”áÐe ‹i¸aˆa Àa»¡Ða“e ‹©·¡

  (setq p1 (list (car ip) (+ (cadr ip) (* ds 13.0 vs)) 0.0))  ;13mm ¶á/´aœ point
  (setq p2 (list (+ (car p1) (* txtl hs)) (cadr p1) 0.0))     ;‹i¸a‹©·¡ eÇq ¹Á/¶ pnt

  (setq tp (list (+ (car p1) (/ txtl 2.0 hs)) (+ (cadr p1) (* ds th)) 0.0))  ;‹i¸a¶áÃ¡

  (setq blkdir (strcat (prefix) "BLOCKS/ELM"))                ;ÑÁ¬iÎa¬s·³

  (push-env)                                          ;Ñe¸ ÑÅ‰w¥e® ”Ï¡

  (command "INSERT" blkdir ip ds (* ds vs) "")                ;ÑÁ¬iÎa¬s·³
  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "GREEN")
  (command "PLINE" ip p1 p2 "")                               ;Ÿ¡”á¬å ‹aŸ±
  (setvar "CECOLOR" oldc)
  (command "TEXT" "J" "M" tp (* th ds) "0" (strcase txt))    ;ÉB¯aËa ³q

  (pop-env)                                                   ;ÑÅ‰w¥e® ¥¢Šá
  (setq *error* oer seterr nil)
  (princ)

) ;of defun


;---------------------------
; program : PEL (Pick Elevation)
;           Yi Suk Jong
;           04/10/30
;---------------------------
; ±âÁ¸ ElavationÁ¤º¸¸¦ ÀÌ¿ëÇØ¼­ ¿øÇÏ´Â Elevation¿¡ xlineÀ» ±×·ÁÁØ´Ù.
(defun c:pel()
  (setq scl 1000)  ;mm·Î ±×¸±¶§, 1000, m·Î ±×¸± ¶§ 1.0
  (setq ip (getpoint "\nPick Inital point: ")) ;initial point
  (setq elt (entsel "\nSelect Elevation text: ")) ;elevation text
  (setq nel (getreal "\nEnter New Elevation: ")) ;new elevation
  (setq ent (entget (car elt)))  ;entity info
  (setq txt (cdr (assoc 1 ent)))
  (setq eqpos (vl-string-search "=" txt))
  (if (/=  eqpos nil)
    (setq eltxt (substr txt (+ eqpos 2) (- (strlen txt) eqpos 1)))
  );if
  (setq el (atof eltxt))
  (setq del (* (- nel el) scl))
  (setq newip (list (car ip) (+ (cadr ip) del)))      	; xline½ÃÁ¡
  (setq newipx (list (+ (car ip) 10) (cadr newip))) 	; xlineÁ¾Á¡
  (push-os)
  (command "xline" "h" newip "")
  (pop-os)  
);defun

;-------------------------------------
; program : melm (multi elevation mark)
;           ¿©·¯°³ÀÇ elevation ¸¶Å·À» ±âÁ¸ ¸¶Å·À» ÂüÁ¶·Î ¸¸µé¾îÁØ´Ù.
;           Yi Suk Jong
;           05/08/10
;-------------------------------------
(defun c:melm( / scl ip elt nip pv ent txt eqpos eltxt el deltay nel neltxt )
  (setq scl 1000)  ;mm·Î ±×¸±¶§, 1000, m·Î ±×¸± ¶§ 1.0
  (setq ip (getpoint "\nPick Inital point: ")) 		;initial point
  (setq elt (entsel "\nSelect Elevation text: ")) 	;elevation text
  
  (while (/= nil (setq nip (getpoint "\nPick insert point: "))); »õ·Î¿î »ðÀÔÁ¡ ÀÔ·Â¹Þ±â
    (setq pv (getpoint nip "\nPick Position point: "))            ; »õ·Î¿î elmÀÇ position point

;  (setq nel (getreal "\nEnter New Elevation: ")) ;new elevation
    (setq ent (entget (car elt)))  ;entity info
    (setq txt (cdr (assoc 1 ent)))
    (setq eqpos (vl-string-search "=" txt))
    (if (/=  eqpos nil)
      (setq eltxt (substr txt (+ eqpos 2) (- (strlen txt) eqpos 1)))
    );if
    (setq el (atof eltxt))
;    (setq del (* (- nel el) scl))   

    (setq deltay (- (cadr nip) (cadr ip))) 	; ±âÁ¸ »ðÀÔÁ¡°ú »õ »ðÀÔÁ¡ÀÇ delta y
    (setq nel (+ el (/ deltay scl))) 		;»õ·Î¿î el
    (setq neltxt (strcat "EL.=" (rtos nel 2 3)));»õ·Î¿î el text (¼Ò¼öÁ¡ 3ÀÚ¸®)   	
;  (setq txt (strcat "EL.=" (getstring "\nElevation: ")))      ;ÉB¯aËa ·³b
    (djdgf_elm nip pv neltxt)			; elevation marking »ðÀÔÇÏ±â.
  );while
);defun

