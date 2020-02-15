;--------------------------
; program : fcorner
;           fillet corner
;           Yi Suk Jong
;           04/08/12
;--------------------------
; fillet corner rebar

(defun c:fcorner(
		 / r se1 se2 )

  (defun fcorner_dia(   / dcl_id 
                   )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "FCORNER" dcl_id)) (exit))


;-------------------
; 초기값설정
;-------------------
  
  (cond
    ((= #fcdia nil) (set_tile "radio13" "1")(setq fcdia 13))
    ((= #fcdia 13) (set_tile "radio13" "1"))
    ((= #fcdia 16) (set_tile "radio16" "1"))
    ((= #fcdia 19) (set_tile "radio19" "1"))
    ((= #fcdia 22) (set_tile "radio22" "1"))
    ((= #fcdia 25) (set_tile "radio25" "1"))
    ((= #fcdia 29) (set_tile "radio29" "1"))
    ((= #fcdia 32) (set_tile "radio32" "1"))    
  );cond
    
;---------------------------
; dialog box 초기화
;---------------------------
   (action_tile "radio13" "(setq fcdia 13)")
   (action_tile "radio16" "(setq fcdia 16)")
   (action_tile "radio19" "(setq fcdia 19)")
   (action_tile "radio22" "(setq fcdia 22)")
   (action_tile "radio25" "(setq fcdia 25)")
   (action_tile "radio29" "(setq fcdia 29)")
   (action_tile "radio32" "(setq fcdia 32)")
    
   (action_tile "accept" "(done_dialog)")

   (action_tile "cancel"  "(exit)")

   (start_dialog)

    (unload_dialog dcl_id)
  ) ;of sub defun FANCH_DIA

  
  (setq se1 (entsel "\nselect first line: "))
  (setq se2 (entsel "\nselect secont line: "))
  
  (fcorner_dia)
  (setq #fcdia fcdia)


  (cond
    ((= fcdia 13) (setq r 140))
    ((= fcdia 16) (setq r 170))
    ((= fcdia 19) (setq r 200))
    ((= fcdia 22) (setq r 240))
    ((= fcdia 25) (setq r 270))
    ((= fcdia 29) (setq r 310))
    ((= fcdia 32) (setq r 340))
  );cond
  
  (push-os)
  (command "fillet" "R" r)
  (command "fillet" se1 se2)
  (pop-os)

;  (princ fcdia)
  
);defun  