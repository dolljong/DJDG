;************************************
; Program : ROUND
;           ROUND lines
;           Suk-Jong Yi
;           1995. 3. 16, 5/25
;************************************

(defun C:ROUND(/
ds ent1 ent2 pnt1s pnt1e pnt2s pnt2e ans mpnts mpnte
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(push-env)  ;ÑÅ‰w¥e® ”Ï¡

(setq ds (getvar "DIMSCALE"))

(setq ent1 (entget (car (entsel "\nSelect first line: ")))) ;œa¶…—·Ði – ¬å ¬åÈ‚
(setq ent2 (entget (car (entsel "\nSelect second line: "))))

(setq pnt1s (cdr (assoc 10 ent1)))              ;– œa·¥· ¯¡¸b¸ñ‰Á {¸ñ
(setq pnt1e (cdr (assoc 11 ent1)))
(setq pnt2s (cdr (assoc 10 ent2)))
(setq pnt2e (cdr (assoc 11 ent2)))
;
(setq ssd (distance pnt1s pnt2s))     ;Àõœa·¥· ¯¡¸b¸ñµA¬á –¼œa·¥· ¯¡¸b¸ñˆáŸ¡
(setq sed (distance pnt1s pnt2e))     ;Àõœa·¥· ¯¡¸b¸ñµA¬á –¼œa·¥· {¸ñˆáŸ¡
(if (< sed ssd)                 ;¶á· – ˆáŸ¡º— ˆaŒa¶…½¢·¡ ¯¡¸b¸ñ·¡ –E”a
  (progn
    (setq tmp pnt2s)
    (setq pnt2s pnt2e)
    (setq pnt2e tmp)
  ) ;of progn
) ; of if

(initget "All Half")                            ;¸åÁAœa¶…—· ™¡“e ¤e œa¶…—· ¢‰·q
(setq ans (getkword "\nAll round/<Half round>: "))
(if (= ans nil) (setq ans "Half"))

(if (= ans "All")                       ;¸åÁAœa¶…—··© ‰w¶ º—ˆeœa·¥·i ŠÐa‰¡
  (progn                                ; ‹a ¹Á¶¡ œa¶…—· ¬å ‹aŸ±
    (setq mpnts (list (/ (+ (car pnt1s) (car pnt2s)) 2.0)
                      (/ (+ (cadr pnt1s) (cadr pnt2s)) 2.0)))
    (setq mpnte (list (/ (+ (car pnt1e) (car pnt2e)) 2.0)
                      (/ (+ (cadr pnt1e) (cadr pnt2e)) 2.0)))
    (setq oldclr (getvar "CECOLOR"))
    (setvar "CECOLOR" "RED")
    (setq oldlt (getvar "CELTYPE"))
    (setvar "CELTYPE" "CENTER")
    (COMMAND "LINE"  mpnts mpnte "")
    (setvar "CELTYPE" oldlt)
    (setvar "CECOLOR" oldclr)
    (hround mpnts mpnte pnt1s pnt1e)
    (hround mpnts mpnte pnt2s pnt2e)
  ) ; of progn
  (hround pnt1s pnt1e pnt2s pnt2e)     ;¤e œa¶…—··© ‰w¶ ¯¡¸b¬åµA¬á {¬å·a¡
) ; of if                              ;œa¶…—· ¬å ‹aŸ±

(pop-env)                              ;”Ï¡¯¡Åa–… ÑÅ‰w¥e®ˆt ¥¢Š
  (setq *error* oer seterr nil)

) ;of defun ROUND

;**************************************
; Function : HROUND
;            Half ROUNDing
;            Suk-Jong Yi
;            1995. 3. 16
;**************************************

(defun HROUND(p1s p1e p2s p2e /             ;¤e œa¶…—··i Ðº“e Function
sdst edst lpnts lpnte ds p1s p1e oldclr     ;Argument“e –¬å· ¯¡¸b¸ñ‰Á {¸ñ
)

(setq ds (getvar "DIMSCALE"))         ;DIMSCALE·i ´i´a…”a.

(setq sdst (distance p1s p2s))    ;Àõ¼œa·¥ ¯¡¸b¸ñµA¬á –‰¼œa·¥ ¯¡¸b¸ñŒa»¡ ˆáŸ¡
(setq edst (distance p1e p2e))    ;Àõ¼œa·¥ {¸ñµA¬á –‰¼œa·¥ {¸ñŒa»¡ ˆáŸ¡

             ;– ¬åˆe· ˆáŸ¡ˆa 2£¡Ÿ¡·¡¬w ·©˜ e – ¬å· º—ˆeµA”a ¬å·i ‹aŸ¥”a
(setq oldclr (getvar "CECOLOR"))
(setvar "CECOLOR" "GREEN")
(while (or (> sdst (* ds 1.0)) (> edst (* ds 1.0)))
  (setq lpnts (list (/ (+ (car p1s) (car p2s)) 2.0)      ;– ¯¡¸b¸ñ· º—ˆe¸ñ X
                    (/ (+ (cadr p1s) (cadr p2s)) 2.0)))  ;                   Y
  (setq lpnte (list (/ (+ (car p1e) (car p2e)) 2.0)      ;– {¸ñ· º—ˆe¸ñ   X
                    (/ (+ (cadr p1e) (cadr p2e)) 2.0)))  ;                   Y
  (command "LINE" lpnts lpnte "")     ; º—ˆe¬å·i ‹aŸ¥”a
  (setq p1s lpnts)             ;º—ˆe¬å· ¯¡¸b¸ñ·i Àõ¼¬å· ¯¡¸b¸ñ·a¡ ¤aŽ…”a
  (setq p1e lpnte)             ;º—ˆe¬å· {¸ñ·i Àõ¼¬å· {¸ñ·a¡ ¤aŽ…”a
  (setq sdst (distance lpnts p2s))  ;– ¯¡¸b¸ñˆe· ˆáŸ¡Ÿi ŠÐe”a
  (setq edst (distance lpnte p2e))  ;– {¸ñˆe· ˆáŸ¡Ÿi ŠÐe”a
) ;of while
(setvar "CECOLOR" oldclr)
) ;of defun HROUND


