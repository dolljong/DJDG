; program : modelmark
; program : extip
; program : makeattblock
; program : insertattblock


;--------------------------------
; Program : modelmark
;           draw Model markings
;           Yi Suk Jong
;           04/06/21
;--------------------------------
(defun c:modelm( / p1 p2 en scl ang )
  
  ;--------------------------------
  ; sub function : MIDELM_DIA
  ;--------------------------------
  (defun MODELM_DIA (
		    / dcl_id 
                   )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "MODELM" dcl_id)) (exit))

;    (start_image "anchimage")                                  ;image 보이기
;    (slide_image  0 0
;                  (dimx_tile "anchimage") (dimy_tile "anchimage")   ;182,37
;                  "djdg(fanchfv)")
;    (end_image)
   (djdg_sldimage "roller" "djdg(modelr)" nil)
   (djdg_sldimage "hinge" "djdg(modelh)" nil)
   (djdg_sldimage "fix" "djdg(modelf)" nil)
   (djdg_sldimage "lspring" "djdg(modells)" nil)
   (djdg_sldimage "rspring" "djdg(modelrs)" nil)
 
;-------------------
; 초기값설정
;-------------------

    
;---------------------------
; dialog box 초기화
;---------------------------
   (action_tile "roller" "(setq mtype 0)")
   (action_tile "hinge" "(setq mtype 1)") 
   (action_tile "fix" "(setq mtype 2)")    
   (action_tile "lspring" "(setq mtype 3)")
   (action_tile "rspring" "(setq mtype 4)")
    
   (action_tile "accept" "(done_dialog)")
   (action_tile "cancel"  "(exit)")

    (start_dialog)

    (unload_dialog dcl_id)
  ) ;of sub defun FANCH_DIA

  
 (modelm_dia)

 
 (setq p1 (getpoint "\nPick insert point: "))
 (initget "Reference")
 (setq p2 (getpoint p1 "\nPick second point [Reference] : "))
 (if (= p2 "Reference")
   (progn
     (setq en (entget (car (entsel "\Select other block: "))))
     (setq scl (cdr (assoc 41 en)))   ;scale
     (setq ang (cdr (assoc 50 en)))   ;angle
     (setq p2 (polar p1 ang scl))
   );progn
 );if

 (cond
   ((= mtype 0) (push-os)(djdg_insertblkas "modelr" p1 p2)(pop-os))
   ((= mtype 1) (push-os)(djdg_insertblkas "modelh" p1 p2)(pop-os))
   ((= mtype 2) (push-os)(djdg_insertblkas "modelf" p1 p2)(pop-os))
   ((= mtype 3) (push-os)(djdg_insertblkas "modells" p1 p2)(pop-os))
   ((= mtype 4) (push-os)(djdg_insertblkas "modelrs" p1 p2)(pop-os))
 );cond
  
);defun  

(defun c:extip( / )
  (command "vbarun" (strcat (prefix) "djdg/djdg.dvb!extip.extip_start"))
);defun 

(defun c:insertattblock( / )
  (command "vbarun" (strcat (prefix) "djdg/djdg.dvb!extip.insertattblock"))
);defun 

(defun c:makeattblock( / )
  (command "vbarun" (strcat (prefix) "djdg/djdg.dvb!extip.makeattblockss"))
);defun 