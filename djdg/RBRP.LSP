;**************************************
; Program : RBRP
;           ReBar Round Plan
;           By Suk-Jong Yi
;           96/5/23
;**************************************
; ¶¥Ñw·a¡ Àé‹e·i ¤‹eÐe”a. (Íw¡e)
; Å¥ÇaŸ¡Ëa ¬å, ›¡Àé‹eŒa»¡ ‹aaº…”a.
;**************************************

(defun C:RBRP(/
    r       ctc     bn      loop    cenccen     oldc    dent
    cen     pdia    nlay    count   cover
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;ÑÅ‰w¥e® ”Ï¡

  (setq cen (getpoint "\nPick Center point: "))                 ;Àé‹e ¤‹e· º—¯±
  (setq pdia (* 1000 (getreal "\nPost DIA(m): ")))              ;post»¢‰w
  (command "CIRCLE" cen "D" pdia)                               ;post‹aŸ¡‹¡

  (setq nlay (getint "\nNumber of layer: "))                    ;¡y”e Àé‹e·¥ˆa?

  (setq count 1)                                                ;Àõ¤å¼ ”e¦Èá
  (repeat nlay
    (princ "\n*** ") (princ count) (princ " LAYER")
    (setq cover (getreal " Cover(mm): "))                        ;”üˆ–A
    (setq r (- (/ pdia 2) cover))                               ;ºÀé‹e ¤‹e ¤e»¡Ÿq

    ;(setq r (getreal "\nRadius: "))                            ;Àé‹e ¤‹e· ¤e»¡Ÿq

    (initget "Number")
    (setq ctc (getreal "\nNumber/<C.T.C(mm)>: "))               ; ctc ·³b¤h·q
    (if (= ctc "Number")
      (setq bn (getint "\nNumber of REBAR: "))                  ;Àé‹e ˆ•® »¢¸ó·³b
      (setq bn (fix (/ (* 2.0 pi r) ctc)))                      ; Àé‹e ˆ•® ‰¬e
    ) ;of if
    (setq ctc (/ (* 2.0 pi r) bn))                              ; ˆ•®µA ·Ðe ctc‰¬e

    (princ "\nNumber of rebar: ")                               ; Àé‹e ˆ•® ¥¡µaº‘
    (princ bn)
    (princ "  New C.T.C.(mm): ")                                ; ctc¥¡µaº‘
    (princ ctc)

    (initget "Num Ctc Ok")                                      ; ®¸÷ µa¦ ¢‰´á¥±
    (setq loop (getkword "\nchange Number/Ctc/<Ok>: "))
    (if (= loop nil) (setq loop "Ok"))

    (while (/= loop "Ok")                   ; Àé‹eˆe‰b £¹ ˆ•® ¤e¥¢  ®¸÷·³b
      (cond
        ((= loop "Num")                                         ; ˆ•® ®¸÷
          (setq bn (getreal "\nNumber of Rebar: "))             ;ˆ•® ·³b
          (setq ctc (/ (* 2.0 pi r) bn))                        ;ctc‰¬e
        ) ;of loop=Num
        ((= loop "Ctc")                                         ; ctc®¸÷
          (setq ctc (getreal "\nC.T.C.(mm): "))                 ;ctc·³b
          (setq bn (fix (/ (* 2.0 pi r) ctc)))                  ;ˆ•® ‰¬e
          (setq ctc (/ (* 2.0 pi r) bn))                        ;ˆ•®µA ·Ðe ctc‰¬e
        ) ;of loop=Ctc
      ) ;of cond
      (princ "\nNumber of rebar: ")                             ; ˆ•® Â‰b
      (princ bn)
      (princ "  New C.T.C.(mm): ")                              ; ctcÂ‰b
      (princ ctc)
      (initget "Num Ctc Ok")                                    ; ”a¯¡ ®¸÷Ði ˆõ·¥ˆa?
      (setq loop (getkword "\nchange Number/Ctc/Ok: "))
      (if (= loop nil) (setq loop "Ok"))                        ; return·³b¯¡ OK¡ ¥±
    ) ;of while


    (setq ccen (polar cen (* 1.5 pi) r))                        ;Àõ Àé‹e ¶áÃ¡(Ða”e)

    (setq oldc (getvar "CECOLOR"))                              ;Ñe¸¬‚ ”Ï¡
    (setvar "CECOLOR" "YELLOW")                                 ;¬‚·i ‘¡œe¬‚·a¡
    (command "CIRCLE" cen "D" (- pdia (* 2 (- cover 15))))      ;›¡Àé‹e
    (command "DONUT" "0" "30" ccen "")    ;•¡a¯a ‹aŸ¡‹¡
    (setvar "CECOLOR" oldc)                                     ;Ñe¸¬‚ –A•©Ÿ¡‹¡

    (setq dent (ssget "L"))                                     ;•¡a¯a µEË¡Ë¡ ¬åÈ‚
    (command "ARRAY" (list (ssname dent 0) ccen) "" "P" cen (fix bn) "360" "Y")  ;array

    (setq count (1+ count))                                     ;”a·q ”e·a¡
  ) ;of repeat

  (pop-env)                                                     ;ÑÅ‰w¥e®ˆt –A•©Ÿ¡‹¡
  (setq *error* oer seterr nil)
  (prin1)

) ;of defun
