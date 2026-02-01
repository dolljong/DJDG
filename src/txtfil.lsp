;;;     TXTEXP.LSP
;;;     Copyright (C) 1997 by Autodesk, Inc.
;;;
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the above copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  
;;;     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF 
;;;     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC. 
;;;     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;;     UNINTERRUPTED OR ERROR FREE.
;;;
;;;     Use, duplication, or disclosure by the U.S. Government is subject to 
;;;     restrictions set forth in FAR 52.227-19 (Commercial Computer 
;;;     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;     (Rights in Technical Data and Computer Software), as applicable.
;;; 
;;;  ----------------------------------------------------------------
;;; 
;;;     Credits: Randy Kintzley
;;;              Dominic Panholzer
;;;              Bill Kramer
;;;              Greg Robinson                 
;;;
;;; 
;;;  External Functions:
;;;
;;;     INIT_BONUS_ERROR  --> AC_BONUS.LSP   Intializes bonus error routine
;;;     RESTORE_OLD_ERROR --> AC_BONUS.LSP   Restores old error routine
;;;     ZOOM_4_SELECT     --> AC_BONUS.LSP   Zoom boundry to include points given
;;;     B_LAYER_LOCKED    --> AC_BONUS.LSP   Checks to see if layer is locked
;;;     PIXEL_UNIT        --> AC_BONUS.LSP   Size of pixel in drawing units


(defun c:txtexp (/ grplst getgname mtextbox ucs_2_mtext FLTR GLST GDICT SS VIEW
                   UPLFT TMPFIL TMPFIL CNT PT1 PT2 ENT TXT TXTTYP PTLST ZM LOCKED)
  (init_bonus_error 
        (list
         (list   "cmdecho" 0
                 "highlight" 1
         )
         T 
        )
  )

; --------------------- GROUP LIST FUNCTION ----------------------
;   This function will return a list of all the group names in the
;   drawing and their entity names in the form:
;   ((<ename1> . <name1>) ... (<enamex> . <namex>))
; ----------------------------------------------------------------

  (defun grplst (/ GRP MSTR ITM NAM ENT GLST)

    (setq GRP  (dictsearch (namedobjdict) "ACAD_GROUP"))
    (while (setq ITM (car GRP))       ; While edata item is available
      (if (= (car ITM) 3)             ; if the item is a group name
        (setq NAM (cdr ITM)           ; get the name
              GRP (cdr GRP)           ; shorten the edata
              ITM (car GRP)           ; get the next item
              ENT (cdr ITM)           ; which is the ename
              GRP (cdr GRP)           ; shorten the edata
              GLST                    ; store the ename and name
                  (if GLST
                    (append GLST (list (cons ENT NAM)))
                    (list (cons ENT NAM))
                  )
        )
        (setq GRP (cdr GRP))          ; else shorten the edata
      )
    )
    GLST                              ; return the list
  )

; ------------------- GET GROUP NAME FUNCTION --------------------
;   This function returns a list of all the group names in GLST
;   where ENT is a member. The list has the same form as GLST
; ----------------------------------------------------------------

  (defun getgname (ENT GLST / MSTR GRP GDATA ITM NAM NLST)
    (if (and GLST (listp GLST))
      (progn
        (foreach GRP GLST
          (setq GDATA (entget (car GRP)))
          (foreach ITM GDATA                   ; step through the edata
            (if (and
                  (= (car ITM) 340)            ; if the item is a entity name
                  (eq (setq NAM (cdr ITM)) ENT) ; and the ename being looked for
                )
              (setq NLST                       ; store the ename and name
                      (if NLST
                        (append NLST (list (cons (car GRP) (cdr GRP))))
                        (list (cons (car GRP) (cdr GRP)))
                      )
              )
            )
          )
        )
      )
    )
    NLST
  )

; --------------------- MTEXTBOX FUNCTION ------------------------
;   This function returns a list of four points describing the 
;   bounding box of the mtext (MTXT).
; ----------------------------------------------------------------

  (defun mtextbox (MTXT / WDTH HGHT INS JUST ANG P1 P2 P3 P4)
    (if (and (listp MTXT) (= "MTEXT" (cdr (assoc 0 MTXT))))
      (progn
        (setq WDTH (cdr (assoc 42 MTXT))
              HGHT (cdr (assoc 43 MTXT))
              INS  (trans (cdr (assoc 10 MTXT)) 0 1)
              JUST (cdr (assoc 71 MTXT))
              ANG  (cdr (assoc 50 MTXT))
        )
        (cond
          ((= JUST 1)
            (setq P1 (polar INS (- ANG (* Pi 0.5)) HGHT) ; lower-left
                  P2 (polar P1 ANG WDTH)                 ; lower-right
                  P3 (polar INS ANG WDTH)                ; upper-right
                  p4 INS                                 ; upper-left
            )
          )
          ((= JUST 2)
            (setq P3 (polar INS ANG (/ WDTH 2))
                  P4 (polar INS (+ ANG Pi) (/ WDTH 2))
                  P1 (polar P4 (- ANG (* Pi 0.5)) HGHT)
                  P2 (polar P1 ANG WDTH)
            )
          )
          ((= JUST 3)
            (setq P3 INS
                  P4 (polar INS (+ ANG Pi) WDTH)
                  P1 (polar P4 (- ANG (* Pi 0.5)) HGHT)
                  P2 (polar P1 ANG WDTH)
            )
          )
          ((= JUST 4)
            (setq P4 (polar INS (+ ANG (* Pi 0.5)) (/ HGHT 2))
                  P3 (polar P4 ANG WDTH)
                  P1 (polar P4 (- ANG (* Pi 0.5)) HGHT)
                  P2 (polar P1 ANG WDTH)
            )
          )
          ((= JUST 5)
            (setq P4 (polar INS (- ANG Pi) (/ WDTH 2))
                  P4 (polar P4 (+ ANG (* Pi 0.5)) (/ HGHT 2))
                  P3 (polar P4 ANG WDTH)
                  P1 (polar P4 (- ANG (* Pi 0.5)) HGHT)
                  P2 (polar P1 ANG WDTH)
            )
          )
          ((= JUST 6)
            (setq P3 (polar INS (+ ANG (* Pi 0.5)) (/ HGHT 2))
                  P4 (polar P3 (+ ANG Pi) WDTH)
                  P1 (polar P4 (- ANG (* Pi 0.5)) HGHT)
                  P2 (polar P1 ANG WDTH)
            )
          )
          ((= JUST 7)
            (setq P1 INS
                  P2 (polar P1 ANG WDTH)
                  P3 (polar P2 (+ ANG (* Pi 0.5)) HGHT)
                  P4 (polar P1 (+ ANG (* Pi 0.5)) HGHT)
            )
          )
          ((= JUST 8)
            (setq P1 (polar INS (+ ANG Pi) (/ WDTH 2))
                  P2 (polar P1 ANG WDTH)
                  P3 (polar P2 (+ ANG (* Pi 0.5)) HGHT)
                  P4 (polar P1 (+ ANG (* Pi 0.5)) HGHT)
            )
          )
          ((= JUST 9)
            (setq P2 INS
                  P1 (polar INS (+ ANG Pi) WDTH)
                  P3 (polar P2 (+ ANG (* Pi 0.5)) HGHT)
                  P4 (polar P1 (+ ANG (* Pi 0.5)) HGHT)
            )
          )
        )
      )
      (prompt "\nEntity Not Mtext!")
    )
    (list P1 P2 P3 P4)
  )

; ------------------- SET MTEXT UCS FUNCTION ---------------------
;   AutoCAD does not accept mtext as a valid object for setting
;   the ucs. This function will set the current ucs to the 
;   mtext entity name ENT.
; ----------------------------------------------------------------

  (defun ucs_2_mtext (ENT / PTZ PTX PTY PTO)

    (setq PTZ (trans (cdr (assoc 210 (entget ENT))) ENT 1 T)
          PTX (trans (cdr (assoc 11 (entget ENT))) ENT 1 T)
          PTO (trans (cdr (assoc 10 (entget ENT))) ENT 1)
          PTY (list
                (-
                  (* (cadr PTZ) (caddr PTX))
                  (* (cadr PTX) (caddr PTZ))
                );minus
                (* -1
                  (-
                    (* (car PTZ) (caddr PTX))
                    (* (car PTX) (caddr PTZ))
                  );minus
                );multiply by -1
                (-
                  (* (car PTZ) (cadr PTX))
                  (* (car PTX) (cadr PTZ))
                );minus
              );list
          PTX (list (+ (car PTO) (car PTX))
                    (+ (cadr PTO) (cadr PTX))
                    (+ (caddr PTO) (caddr PTX))
              )
          PTY (list (+ (car PTO) (car PTY))
                    (+ (cadr PTO) (cadr PTY))
                    (+ (caddr PTO) (caddr PTY))
              )

    );setq
    (command "_.ucs" "_3" PTO PTX PTY)
  )

; ----------------------------------------------------------------
;                          MAIN PROGRAM
; ----------------------------------------------------------------

  (if (and                                                ; Are we in plan view?
        (equal (car (getvar "viewdir")) 0 0.00001)
        (equal (cadr (getvar "viewdir")) 0 0.00001)
        (> (caddr (getvar "viewdir")) 0)
      )
        
    (progn

      (prompt "\nSelect text to be EXPLODED: ")

      (Setq FLTR    '((-4 . "<AND") 
                        (-4 . "<OR")                      ; filter for mtext and text
                          (0 . "MTEXT")
                          (0 . "TEXT")
                        (-4 . "OR>")
                        (-4 . "<NOT")
                          (102 . "{ACAD_REACTORS")        ; and not leader text
                        (-4 . "NOT>")
                      (-4 . "AND>")
                     )
            GLST     (grplst)                             ; Get all the groups in drawing
            GDICT    (if GLST
                       (dictsearch (namedobjdict) "ACAD_GROUP")
                     )
            SS       (ssget  FLTR) 
            CNT      0
      )

      (if SS
        (progn
          (setq CNT (sslength SS))
          (princ (strcat "\n" (itoa CNT) " found."))       ; Report number of items found

          (command "_.move" SS "")                         ; filter out objects on locked layers

          (if (> (getvar "cmdactive") 0)                   ; if there are still objects left
            (progn
              (command "0,0" "0,0")
              (setq SS  (ssget "p" FLTR)
                    CNT (- CNT (sslength SS))              ; count them
              )
            )
            (setq SS nil)                                  ; else abort operation
          ) 

          (if (> CNT 0)                                    ; if items where filtered out
            (if (= CNT 1)
              (princ (strcat "\n" (itoa CNT) " was on a locked layer."))   ; report it.
              (princ (strcat "\n" (itoa CNT) " were on a locked layer."))
            )
          )
        )
      )

      (if SS
        (progn

          (setq CNT 0)                                 ; Reset counter
          (While (setq ENT (ssname SS CNT))            ; step through each object in set

            (and
              GLST                                     ; if groups are present in the drawing
              (setq GNAM (getgname ENT GLST))          ; and the text item is in one or more
              (foreach GRP GNAM                        ; step through those groups
                (command "_.-group" "_r"               ; and remove the text item
                  (cdr GRP) ENT ""
                )
              )
            )

            (setq TXT   (entget ENT)
                  TXTYP (cdr (assoc 0 TXT))            ; Text or Mtext
            )

            (if (= TXTYP "TEXT")
              (command "_.ucs" "_object" ENT)          ; set UCS to object
               (ucs_2_mtext ENT)
            )

            (if (= TXTYP "TEXT")                       ; get the points for the bounding box
              (progn
                (setq TBX (textbox TXT)                ; normal text
                      TBX (list (car TBX) (list (caadr TBX)(cadar TBX))
                                (cadr TBX) (list (caar TBX)(cadadr TBX))
                          )
                )
              )
              (setq TBX (mtextbox TXT))                ; Mtext
            )

            (setq TBX (mapcar '(lambda (x)
                                 (trans x 1 0)         ; convert the points to WCS
                               )
                        TBX
                      )
            )

            (setq PTLST (append PTLST TBX))            ; Build list of bounding box
                                                       ; points for text items selected


            (command "_.ucs" "_previous")              ; reset the ucs

            (setq CNT (1+ CNT))                        ; get the next text item
          ); while

          (setq PTLST (mapcar '(lambda (x)
                                 (trans x 0 1)         ; convert all the points
                               )                       ; to the current ucs
                      PTLST
                    )
          )

          (if (setq ZM (zoom_4_select PTLST))          ; If current view does not contain
            (progn                                     ; all bounding box points
              (setq ZM
                (list
                  (list (- (caar ZM) (pixel_unit))     ; increase zoom area by
                        (- (cadar ZM) (pixel_unit))    ; one pixel width to
                        (caddar ZM)                    ; sure nothing will be lost
                  )
                  (list (+ (caadr ZM) (pixel_unit))
                        (+ (cadadr ZM) (pixel_unit))
                        (caddr (cadr zm))
                  )
                )
              )
              (command "_.zoom" "_w" (car ZM) (cadr ZM))  ; zoom to include text objects
            )
          )

          (setq VIEW     (viewpnts)
                UPLFT    (list (caar VIEW) (cadadr VIEW))
                TMPFIL   (strcat (getvar "tempprefix") "txtexp.wmf")
                PT1      (getvar "viewctr")
                PT2      (list (car PT1) (cadadr VIEW))
          )

          (if (b_layer_locked (getvar "clayer"))       ; if current layer is locked
            (progn
              (command "_.layer" "_unl" (getvar "clayer") "")  ; unlock it
              (setq LOCKED T)
            )
          )

          (command "_.mirror" SS "" PT1 PT2 "_y"
                   "_.WMFOUT" TMPFIL SS ""
                   "_.ERASE" SS ""
                   "_.WMFIN" TMPFIL UPLFT  "2" "" ""
                   "_.mirror" (entlast) "" PT1 PT2 "_y"
                   "_.EXPLODE" (entlast) 
          );end command
  

          (command "_.erase" (ssget "p") "_R" "_W"
                   (polar (car VIEW) (* 0.25 Pi) (pixel_unit))
                   (cadr VIEW)
                   ""
          )


          (if ZM (command "_.zoom" "_p"))              ; Restore original view if needed
          (if LOCKED (command "_.layer" "_lock" (getvar "clayer") "")) : relock if needed

          (prompt (strcat "\n" (itoa (sslength ss))
                          " text object(s) have been exploded to lines."
                  )
          )
          (prompt "\nThe line objects have been placed on layer 0.")
        )
      )
    )
    (prompt "\nView needs to be in plan (0 0 1).")
  );if equal
  (restore_old_error)                                  ; Retsore values
  (princ)
)

(defun c:txtfil( )

  (prompt "\nSelect text to be Filled: ")

  (Setq FLTR    '((0 . "TEXT"))
        SS       (ssget  FLTR))

  (setq VIEW     (viewpnts)
        UPLFT    (list (caar VIEW) (cadadr VIEW))
        TMPFIL   (strcat (getvar "tempprefix") "txtexp.wmf")
        PT1      (getvar "viewctr")
        PT2      (list (car PT1) (cadadr VIEW))
  )

  (if ss
    (progn
      (command "_.WMFOUT" TMPFIL SS ""
  ;             "_.ERASE" SS ""
               "_.WMFIN" TMPFIL UPLFT  "2" "" ""
  ;             "_.mirror" (entlast) "" PT1 PT2 "_y"
  ;             "_.EXPLODE" (entlast)
      );end command

      (setq wmfinent (entlast))

      (command "_.POINT" UPLFT)

      (setq pnt (entlast))

      (command "_.EXPLODE" wmfinent)

      (command "_.ERASE" (entnext pnt) "")


      (setq ent (entnext pnt))              ;첫 엔티티명
      (setq textlines (ssadd))              ;새로운 ss생성

      (setq textlines (ssadd ent textlines))  ;첫엔티티를 ss에 넣음

      (while (setq ent (entnext ent))           ;다음엔티티명
        (setq textlines (ssadd ent textlines))  ;엔티티명을 ss에 넣음
      );while

      (command "hatch" "s" textlines "")         ;solid로 햇치
      (command "erase" pnt textlines "")        ;기준점, 폴리라인 지움

    );progn
  );if


) ;defun

;----------------------------
; program : textx (Text Exlopde)
;           Yi Suk Jong
;           2000/3/30
;----------------------------
(defun c:textx( )

  (prompt "\nSelect text to be EXPLODED: ")

  (Setq FLTR    '((0 . "TEXT"))
        SS       (ssget  FLTR))

  (setq VIEW     (viewpnts)
        UPLFT    (list (caar VIEW) (cadadr VIEW))
        TMPFIL   (strcat (getvar "tempprefix") "txtexp.wmf")
        PT1      (getvar "viewctr")
        PT2      (list (car PT1) (cadadr VIEW))
  )

  (if ss
    (progn
      (command "_.WMFOUT" TMPFIL SS ""
               "_.ERASE" SS ""         ;erase selected text endtity
               "_.WMFIN" TMPFIL UPLFT  "2" "" ""
  ;             "_.mirror" (entlast) "" PT1 PT2 "_y"
  ;             "_.EXPLODE" (entlast)
      );end command

      (setq wmfinent (entlast))

      (command "_.POINT" UPLFT)

      (setq pnt (entlast))

      (command "_.EXPLODE" wmfinent)

      (command "_.ERASE" (entnext pnt) "")


;      (setq ent (entnext pnt))              ;첫 엔티티명
;      (setq textlines (ssadd))              ;새로운 ss생성

;      (setq textlines (ssadd ent textlines))  ;첫엔티티를 ss에 넣음

;      (while (setq ent (entnext ent))           ;다음엔티티명
;        (setq textlines (ssadd ent textlines))  ;엔티티명을 계속 ss에 추가함
;      );while

;      (command "hatch" "s" textlines "")         ;solid로 햇치
      (command "erase" pnt textlines "")        ;기준점, 폴리라인 지움

    );progn
  );if


) ;defun



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;viewpnts
;returns lower left and upper right coords of current view
(defun viewpnts ( / a b c d x)

(setq b (getvar "viewsize")
      c (car (getvar "screensize"))
      d (cadr (getvar "screensize"))
      a (* b (/ c d))
      x (setq x (getvar "viewctr"))
      x (trans x 1 2)
      c (list (- (car x)  (/ a 2.0))
              (- (cadr x) (/ b 2.0))
              0.0
        );list
      d (list (+ (car x)  (/ a 2.0))
              (+ (cadr x) (/ b 2.0))
              0.0
        );list
      c (trans c 2 1)
      d (trans d 2 1)
);setq

(list c d)
);defun viewpnts


