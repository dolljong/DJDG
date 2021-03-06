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
; 05/08/09 : function
(defun c:elm( / ip pv txt )
  (setq ip (getpoint "\nPick insert point: "))                ;삽입점 입력
  (setq pv (getpoint ip "\nPick Position point: "))            ;수직 방향 입력
  (setq txt (strcat "EL.=" (getstring "\nElevation: ")))      ;텍스트 입력
  (djdgf_elm ip pv txt)
);defun

;**********************************
; function : ELMF (ELevation Marking)
;           Jong-Suk Yi
;           96/6/4
;**********************************
; 05/08/09 : functiond
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

  
;(setq ph (getpoint pv "\nPick Holizontal side: "))          ;수평방향 입력

  (setq dv (- (cadr pv) (cadr ip)))                           ;수직차
  (setq dh (- (car pv) (car ip)))                             ;수평차

  (if (= dv 0) (setq vs 1) (setq vs (/ (abs dv) dv)))                                   ;수직부호
  (if (= dh 0) (setq hs 1) (setq hs (/ (abs dh) dh)))                                   ;수평부호


  (setq txtl (* th ds (+ (strlen txt) 2)))  ;두글자를 더한 글자가 차지하는 길이

  (setq p1 (list (car ip) (+ (cadr ip) (* ds 13.0 vs)) 0.0))  ;13mm 위/아래 point
  (setq p2 (list (+ (car p1) (* txtl hs)) (cadr p1) 0.0))     ;글자길이만큼 좌/우 pnt

  (setq tp (list (+ (car p1) (/ txtl 2.0 hs)) (+ (cadr p1) (* ds th)) 0.0))  ;글자위치

  (setq blkdir (strcat (prefix) "BLOCKS/ELM"))                ;화살표삽입

  (push-env)                                          ;현재 환경변수 대피

  (command "INSERT" blkdir ip ds (* ds vs) "")                ;화살표삽입
  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "3")
  (command "PLINE" ip p1 p2 "")                               ;리더선 그림
  (setvar "CECOLOR" oldc)
  (command "TEXT" "J" "M" tp (* th ds) "0" (strcase txt))    ;텍스트 씀

  (pop-env)                                                   ;환경변수 복귀
  (setq *error* oer seterr nil)
  (princ)

) ;of defun


;---------------------------
; program : PEL (Pick Elevation)
;           Yi Suk Jong
;           04/10/30
;---------------------------
;  Elavation
(defun c:pel()
  (setq scl 1000)  ;mm, 1000, m 1.0
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
  (setq newip (list (car ip) (+ (cadr ip) del)))      	; xline
  (setq newipx (list (+ (car ip) 10) (cadr newip))) 	; xline
  (push-os)
  (command "xline" "h" newip "")
  (pop-os)  
);defun

;-------------------------------------
; program : melm (multi elevation mark)
;            elevation
;           Yi Suk Jong
;           05/08/10
;-------------------------------------
(defun c:melm( / scl ip elt nip pv ent txt eqpos eltxt el deltay nel neltxt )
  (setq scl 1000)  ;mm· 1.0
  (setq ip (getpoint "\nPick Inital point: ")) 		;initial point
  (setq elt (entsel "\nSelect Elevation text: ")) 	;elevation text
  
  (while (/= nil (setq nip (getpoint "\nPick insert point: "))); 
    (setq pv (getpoint nip "\nPick Position point: "))            ;  position point

;  (setq nel (getreal "\nEnter New Elevation: ")) ;new elevation
    (setq ent (entget (car elt)))  ;entity info
    (setq txt (cdr (assoc 1 ent)))
    (setq eqpos (vl-string-search "=" txt))
    (if (/=  eqpos nil)
      (setq eltxt (substr txt (+ eqpos 2) (- (strlen txt) eqpos 1)))
    );if
    (setq el (atof eltxt))
;    (setq del (* (- nel el) scl))   

    (setq deltay (- (cadr nip) (cadr ip))) 	;  delta y
    (setq nel (+ el (/ deltay scl))) 		; el
    (setq neltxt (strcat "EL.=" (rtos nel 2 3))); el text ()   	
;  (setq txt (strcat "EL.=" (getstring "\nElevation: ")))      ;텍스트 입력
    (djdgf_elm nip pv neltxt)			; elevation marking .
  );while
);defun

