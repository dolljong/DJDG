;--------------------------------
; Program : ddrect
;           dialog rectangle
;           Yi Suk Jong
;           04/07/23
;--------------------------------
(defun c:ddrect( / th ln p1 p2 ang p3 vang ip )
  
  ;--------------------------------
  ; sub function : MIDELM_DIA
  ;--------------------------------
  (defun ddrect_DIA (
		    / dcl_id 
                   )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "DDRECT" dcl_id)) (exit))

;    (start_image "anchimage")                                  ;image 보이기
;    (slide_image  0 0
;                  (dimx_tile "anchimage") (dimy_tile "anchimage")   ;182,37
;                  "djdg(fanchfv)")
;    (end_image)
   (djdg_sldimage "ddrectc" "djdg(ddrectc)" nil)
   (djdg_sldimage "ddrectm" "djdg(ddrectm)" nil)
   (djdg_sldimage "ddrects" "djdg(ddrects)" nil)

 
;-------------------
; 초기값설정
;-------------------
  (if (/= ddrect_t nil) (set_tile "ddrect_t" ddrect_t))
  (if (/= ddrect_l nil) (set_tile "ddrect_l" ddrect_l))
  (if (= mtype nil) (setq mtype 0))
  (cond
    ((= mtype 0)(mode_tile "ddrectc" 2))
    ((= mtype 1)(mode_tile "ddrectm" 2))
    ((= mtype 2)(mode_tile "ddrects" 2))
  );cond  
;---------------------------
; dialog box 초기화
;---------------------------
   (action_tile "ddrectc" "(setq mtype 0)")
   (action_tile "ddrectm" "(setq mtype 1)") 
   (action_tile "ddrects" "(setq mtype 2)")    
    
   (action_tile "accept" "(done_dialog)")
   (action_tile "cancel"  "(exit)")
    
   (action_tile "ddrect_t"   "(setq ddrect_t $VALUE)")
   (action_tile "ddrect_l"   "(setq ddrect_l $VALUE)")

    
    (start_dialog)

    (unload_dialog dcl_id)
  ) ;of sub defun FANCH_DIA

  
 (ddrect_dia)

 (setq th (atof ddrect_t))
 (setq ln (atof ddrect_l))
  
 (setq p1 (getpoint "\nPick insert point: "))
 (setq p2 (getpoint p1 "\nPick second point for Angle: "))
 (setq ang (angle p1 p2))
  
 (cond
   ((= mtype 0) (push-os)(djdg_rect p1 ang ln th 0)(pop-os))
   ((= mtype 1) (push-os)(djdg_rect p1 ang ln th 1)(pop-os))
   ((= mtype 2)
    (setq p3 (getpoint "\nPick side point:: "))
    (setq vang (v_angle p1 p2 p3))
    (setq ip (polar p1 vang (* 0.5 th)))
    (push-os)(djdg_rect ip ang ln th 0)(pop-os)
   );subcond
 );cond
  
);defun  