;**************************************
; Function : HROUND
;            Half ROUNDing
;            Suk-Jong Yi
;            1995. 3. 16
;**************************************

(defun HROUND(p1s p1e p2s p2e /             ; 渁秴椃穒 衼簛揺 Function
sdst edst lpnts lpnte ds p1s p1e oldclr     ;Argument揺 杹穪 竍格壛 弡格
)

(setq ds (getvar "DIMSCALE"))         ;DIMSCALE穒 磇碼悈攁.

(setq ssdst (distance p1s p2s))     ;栗渁伐穪 竍格礎 杹紒渁伐穪 竍格堘煛
(setq sedst (distance p1s p2e))     ;栗渁伐穪 竍格礎 杹紒渁伐穪 弡格堘煛
(if (< sedst ssdst)                 ;夺穪 杹 堘煛簵 坅宎秴舰贰 竍格贰 朎攁
  (progn
    (setq tmp p2s)
    (setq p2s p2e)
    (setq p2e tmp)
  ) ;of progn
) ; of if

(setq sdst (distance p1s p2s))    ;栗紒渁伐 竍格礎 枆紒渁伐 竍格宎弧 堘煛
(setq edst (distance p1e p2e))    ;栗紒渁伐 弡格礎 枆紒渁伐 弡格宎弧 堘煛

             ;杹 坋穪 堘煛坅 2！煛贰瑆 珐榿爀 杹 穪 簵坋礎攁 穒 媋煡攁
(setq oldclr (getvar "CECOLOR"))
(setvar "CECOLOR" "GREEN")
(while (or (> sdst (* ds 1.0)) (> edst (* ds 1.0)))
  (setq lpnts (list (/ (+ (car p1s) (car p2s)) 2.0)      ;杹 竍格穪 簵坋格 X
                    (/ (+ (cadr p1s) (cadr p2s)) 2.0)))  ;                   Y
  (setq lpnte (list (/ (+ (car p1e) (car p2e)) 2.0)      ;杹 弡格穪 簵坋格   X
                    (/ (+ (cadr p1e) (cadr p2e)) 2.0)))  ;                   Y
  (command "LINE" lpnts lpnte "")     ; 簵坋穒 媋煡攁
  (setq p1s lpnts)             ;簵坋穪 竍格穒 栗紒穪 竍格穉潯 巺攁
  (setq p1e lpnte)             ;簵坋穪 弡格穒 栗紒穪 弡格穉潯 巺攁
  (setq sdst (distance lpnts p2s))  ;杹 竍格坋穪 堘煛焛 妬衑攁
  (setq edst (distance lpnte p2e))  ;杹 弡格坋穪 堘煛焛 妬衑攁
) ;of while
(setvar "CECOLOR" oldclr)
) ;of defun HROUND
