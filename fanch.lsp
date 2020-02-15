;----------------------------
; program : fanch
;           Frayssinet Anchorage
;           Yi Suk Jong
;           04/06/10
;----------------------------
(defun c:fanch(
	       /
	       )
  ;--------------------------------
  ; sub function : FANCH_DIA
  ;--------------------------------
  (defun FANCH_DIA (
		    / dcl_id 
                   )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "FANCH" dcl_id)) (exit))

;    (start_image "anchimage")                                  ;image 보이기
;    (slide_image  0 0
;                  (dimx_tile "anchimage") (dimy_tile "anchimage")   ;182,37
;                  "djdg(fanchfv)")
;    (end_image)


;-------------------
; 초기값설정
;-------------------
  (cond
    ((= #vfront nil) (set_tile "front" "1"))
    ((= #vfront "1") (set_tile "front" "1"))
    ((= #vfront "0") (set_tile "profile" "1"))
    );cond
  (cond
    ((= #vlongside nil) (set_tile "longside" "1"))
    ((= #vlongside "1") (set_tile "longside" "1"))
    ((= #vlongside "0") (set_tile "shortside" "1"))
  );cond  
  (if (= #vrebar nil)
    (progn 
      (set_tile "rebar" "1")
      (set_tile "grid" "1")
    );
    (progn
      (set_tile "rebar" #vrebar)
      (if (= #vrebar "1")
	(if (= #vgrid "1")
	  (set_tile "grid" "1")
	  (set_tile "helical" "1")
	);if
	(set_tile "grid" "1")
      );if	  
    );progn  
  );if
  (if (= #vjack nil)
    (set_tile "jack" "1")
    (set_tile "jack" #vjack)
  );if
  (if (/= #atype nil)
    (set_tile "alist" #atype)
  );if  
    
  (set_anchimage)  
    
;---------------------------
; dialog box 초기화
;---------------------------
   (action_tile "front" "(set_anchimage)")
   (action_tile "profile" "(set_anchimage)") 
   (action_tile "longside" "(set_anchimage)")    
   (action_tile "shortside" "(set_anchimage)")
   (action_tile "rebar" "(set_anchimage)")
   (action_tile "grid" "(set_anchimage)")
   (action_tile "helical" "(set_anchimage)")
   (action_tile "jack" "(set_anchimage)")
   (action_tile "alist" "(setq atype $value)")
;   (action_tile "livelive" "(setq atype 2)")
;   (action_tile "dltext"   "(setq dltext $VALUE)")
    
  (action_tile "accept" "(do-accept)")
;   (action_tile "accept"  "(done_dialog)")
   (action_tile "cancel"  "(exit)")
    ;   (mode_tile "fl_weld" 2)

    (start_dialog)

    (unload_dialog dcl_id)
  ) ;of sub defun FANCH_DIA


  ;------------------------------
  ; sub function : set_anchimage
  ;------------------------------
  (defun set_anchimage( / )
    (setq vfront (get_tile "front"))
    (setq vlongside (get_tile "longside"))
    (setq vjack (get_tile "jack"))
    (setq vrebar (get_tile "rebar"))
    (setq vgrid (get_tile "grid"))

    ;--------- Front View -----------------------
    ;--------------------------------------------
    (if (= vfront "1")					; Front View
      (progn
	(mode_tile "longside" 1)
	(mode_tile "shortside" 1)
        (djdg_sldimage "anchimage" "djdg(fanchfv)" T)  ;erase and draw front vertical
	(if (= vrebar "1")				;draw front rebar
	  (progn
	    (djdg_sldimage "anchimage" "djdg(fanchfr)" nil)
	    (mode_tile "grid" 0) (mode_tile "helical" 0)
	  );progn
	  (progn
	    (mode_tile "grid" 1) (mode_tile "helical" 1)
	  );progn  
	)  ;if
	(if (= vjack "1") (djdg_sldimage "anchimage" "djdg(fanchfj)" nil))  ;draw front jack
      );progn
    ;--------- Side View ------------------------
    ;--------------------------------------------
      (progn						; Side View
	(mode_tile "longside" 0) (mode_tile "shortside" 0)
	(if (= vrebar "1" )
	  (progn (mode_tile "grid" 0) (mode_tile "helical" 0))
	  (progn (mode_tile "grid" 1) (mode_tile "helical" 1))
	);if  
        (cond						
	  ((and (= vrebar "1") (= vjack "0"))
	    (djdg_sldimage "anchimage" "djdg(fanchpr)" T)
	  );sub cond
	  ((and (= vrebar "0") (= vjack "1"))
	    (djdg_sldimage "anchimage" "djdg(fanchpj)" T)
	  );sub cond 
	  ((and (= vrebar "1") (= vjack "1"))
	    (djdg_sldimage "anchimage" "djdg(fanchprj)" T)
	  );sub cond
	  ((and (= vrebar "0") (= vjack "0"))
	    (djdg_sldimage "anchimage" "djdg(fanchp)" T)
	  );sub cond 
        );cond
      );progn 	
    );if
  );sub defun set_anchimage

  (defun do-accept( )
    (setq atype (get_tile "alist"))
    (setq vatype (nth (atoi atype)
		      '("3C15" "4C15" "7C15" "9C15" "12C15" "13C15" "19C15" "22C15" "25C15" "25C15P" "27C15" "31C15" "37C15" "55C15")))
    (done_dialog)
  );defun  do-accept

  (defun djdg_sldimage( tilename sldname erase / )
    ;arguments
    ; tilename : name of image tile
    ; sldname : name of slide
    ; erase : T  --> erase
    ;        nil --> don't erase
    (start_image tilename)                                  ;image 보이기
    (if erase
      (fill_image 0 0 (dimx_tile tilename) (dimy_tile tilename) 5) ;erase image tile
    );if 
    (slide_image  0 0
                  (dimx_tile tilename) (dimy_tile tilename)   
                  sldname)
    (end_image)
    
  );defun djdg_sldimage

  ;------------------------
  ; Main program
  ;------------------------
  (FANCH_DIA)
  (setq #vfront vfront
	#vlongside vlongside
	#vrebar vrebar
	#vgrid vgrid
	#vjack vjack
	#atype atype
	)

  (setq p1 (getpoint "\nPick insert point: "))
  (setq ang (getangle p1 "\nPick insert point: "))
  (setq path "freyssinet/crange/")
  (if (= vgrid "1") (setq vrtype "g") (setq vrtype "h"))
  (cond
    ((= vfront "1")   ;front
      (djdg_insertblk (strcat path "anchf/" vatype "af") p1 (polar p1 ang 10) nil)
      (setq vtype "f")    ;view type = front ("f")
    );sub cond
    ((= vfront "0")   ;profile
      (if (= vlongside "1") (setq side "l") (setq side "s"))     
      (djdg_insertblk (strcat path "anchp" side "/" vatype "ap" side) p1 (polar p1 ang 10) nil)
      (setq vtype "p");view type = front ("f")
    );sub cond 
  );cond
  (if (= vrebar "1")
    (djdg_insertblk (strcat path "rebar" vrtype vtype "/" vatype "r" vrtype vtype) p1 (polar p1 ang 10) nil)
  );if
  
);defun fanch  