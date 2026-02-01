;-------------------------------------
; Program : DLive
;           draw Dead & live tendon
;           Yi Suk Jong
;           04/06/03(THU)
;-------------------------------------

(defun c:dlive(
	       / ents ent pnt spnt epnt atype ds dt th
	       )


  ;;
  ;; Function: DLIVE_DIA (Dialog box로 입력받기)
  ;;
  (defun DLIVE_DIA (
		    / dcl_id 
                   )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "DLIVE" dcl_id)) (exit))

    (start_image "live")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "live") (dimy_tile "live")   ;182,37
                  "djdg(live)")
    (end_image)

    (start_image "livedead")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "livedead") (dimy_tile "livedead")
                  "djdg(livedead)")
    (end_image)

    (start_image "livelive")                                  ;image 보이기
    (slide_image  0 0
                   (dimx_tile "livelive") (dimy_tile "livelive")
                  "djdg(livelive)")
    (end_image)


;-------------------
; 초기값설정
;-------------------


;  (if (= sclength nil) (setq sclength "200"))
;  (if (= scthick nil) (setq scthick "12"))
;  (if (= sctype nil) (setq sctype 1))
    
;---------------------------
; dialog box 초기화
;---------------------------
   (action_tile "live" "(setq atype 0)")
   (action_tile "livedead" "(setq atype 1)")
   (action_tile "livelive" "(setq atype 2)")
   (action_tile "dltext"   "(setq dltext $VALUE)")
    

   (action_tile "accept"  "(done_dialog)")
   (action_tile "cancel"  "(exit)")
    
;   (mode_tile "fl_weld" 2)

    (start_dialog)

    (unload_dialog dcl_id)
  ) ;of sub defun DLIVE_DIA




  
  (setq ents (entsel "\nSelect a Line: "))
  (setq ent (entget (car ents)))   ;entity information
  (setq pnt (cadr ents))

  (setq spnt (cdr (assoc 10 ent))
	epnt (cdr (assoc 11 ent)))  ;start point & end point

  (if (< (distance pnt epnt) (distance pnt spnt))
    (setq spnt (cdr (assoc 11 ent))
	  epnt (cdr (assoc 10 ent)))
  );if


  (setq atype 2)
  
  (DLIVE_DIA)                  ;call dialog input function

  (push-os)
  (djdg_insertarw1 spnt epnt)  ;insert arrow at pick side
  (pop-os)
  
  (push-os)
  (cond
    ((= atype 1) ;anchorage type  -- live & dead
      (djdg_insertblk "danch" epnt spnt T)     ;insert dead at opposit side
    ); subcond
    ((= atype 2) ;anchorage type  -- live & live
      (djdg_insertarw1 epnt spnt)     ;insert live at opposite side
    ); subcond 
  ) ;cond
  (pop-os)

  (setq ds (getvar "dimscale")
	dt (getvar "dimtxt")
	th (* ds dt))
  
  (push-os)
  (djdg_wtxtonline spnt epnt dltext th th)
  (pop-os)
  
);defun

