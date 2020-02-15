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
; 05/08/09 : function을 이용하도록 수정
(defun c:elm( / ip pv txt )
  (setq ip (getpoint "\nPick insert point: "))                ;촶럼목 럼쓇
  (setq pv (getpoint ip "\nPick Position point: "))            ;츃빪 쨢鈞 럼쓇
  (setq txt (strcat "EL.=" (getstring "\nElevation: ")))      ;�B칊�a 럼쓇
  (djdgf_elm ip pv txt)
);defun

;**********************************
; function : ELMF (ELevation Marking)
;           Jong-Suk Yi
;           96/6/4
;**********************************
; 05/08/09 : functiond으로 수정
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

  
;(setq ph (getpoint pv "\nPick Holizontal side: "))          ;츃�w쨢鈞 럼쓇

  (setq dv (- (cadr pv) (cadr ip)))                           ;츃빪픞
  (setq dh (- (car pv) (car ip)))                             ;츃�w픞

  (if (= dv 0) (setq vs 1) (setq vs (/ (abs dv) dv)))                                   ;츃빪쫨朞
  (if (= dh 0) (setq hs 1) (setq hs (/ (abs dh) dh)))                                   ;츃�w쫨朞


  (setq txtl (* th ds (+ (strlen txt) 2)))  ;뻶땓퇫웙 붳�e 땓퇫늏 픞빨�a밻 떓래

  (setq p1 (list (car ip) (+ (cadr ip) (* ds 13.0 vs)) 0.0))  ;13mm 뜬/큑쐛 point
  (setq p2 (list (+ (car p1) (* txtl hs)) (cadr p1) 0.0))     ;땓퇫떓래쟢�q 뮐/턿 pnt

  (setq tp (list (+ (car p1) (/ txtl 2.0 hs)) (+ (cadr p1) (* ds th)) 0.0))  ;땓퇫뜬찼

  (setq blkdir (strcat (prefix) "BLOCKS/ELM"))                ;譏촫�a촶럼

  (push-env)                                          ;�e툈 錤뎩쩯츃 봺區

  (command "INSERT" blkdir ip ds (* ds vs) "")                ;譏촫�a촶럼
  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "GREEN")
  (command "PLINE" ip p1 p2 "")                               ;윞붳у 땇윶
  (setvar "CECOLOR" oldc)
  (command "TEXT" "J" "M" tp (* th ds) "0" (strcase txt))    ;�B칊�a 쿿

  (pop-env)                                                   ;錤뎩쩯츃 ⅱ듻
  (setq *error* oer seterr nil)
  (princ)

) ;of defun


;---------------------------
; program : PEL (Pick Elevation)
;           Yi Suk Jong
;           04/10/30
;---------------------------
; 기존 Elavation정보를 이용해서 원하는 Elevation에 xline을 그려준다.
(defun c:pel()
  (setq scl 1000)  ;mm로 그릴때, 1000, m로 그릴 때 1.0
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
  (setq newip (list (car ip) (+ (cadr ip) del)))      	; xline시점
  (setq newipx (list (+ (car ip) 10) (cadr newip))) 	; xline종점
  (push-os)
  (command "xline" "h" newip "")
  (pop-os)  
);defun

;-------------------------------------
; program : melm (multi elevation mark)
;           여러개의 elevation 마킹을 기존 마킹을 참조로 만들어준다.
;           Yi Suk Jong
;           05/08/10
;-------------------------------------
(defun c:melm( / scl ip elt nip pv ent txt eqpos eltxt el deltay nel neltxt )
  (setq scl 1000)  ;mm로 그릴때, 1000, m로 그릴 때 1.0
  (setq ip (getpoint "\nPick Inital point: ")) 		;initial point
  (setq elt (entsel "\nSelect Elevation text: ")) 	;elevation text
  
  (while (/= nil (setq nip (getpoint "\nPick insert point: "))); 새로운 삽입점 입력받기
    (setq pv (getpoint nip "\nPick Position point: "))            ; 새로운 elm의 position point

;  (setq nel (getreal "\nEnter New Elevation: ")) ;new elevation
    (setq ent (entget (car elt)))  ;entity info
    (setq txt (cdr (assoc 1 ent)))
    (setq eqpos (vl-string-search "=" txt))
    (if (/=  eqpos nil)
      (setq eltxt (substr txt (+ eqpos 2) (- (strlen txt) eqpos 1)))
    );if
    (setq el (atof eltxt))
;    (setq del (* (- nel el) scl))   

    (setq deltay (- (cadr nip) (cadr ip))) 	; 기존 삽입점과 새 삽입점의 delta y
    (setq nel (+ el (/ deltay scl))) 		;새로운 el
    (setq neltxt (strcat "EL.=" (rtos nel 2 3)));새로운 el text (소수점 3자리)   	
;  (setq txt (strcat "EL.=" (getstring "\nElevation: ")))      ;�B칊�a 럼쓇
    (djdgf_elm nip pv neltxt)			; elevation marking 삽입하기.
  );while
);defun

