;***************************************
; Program ; HOOK
;           HOOK rebar
;           Suk-Jong Yi
;           96/5/23
;***************************************
; 坕墶煛 篱媏穒 媋漚簠攁.(90暋, 180暋)
;***************************************

(defun C:HOOK(/)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;雅墂畞 攣稀

  (setq r (getreal "\nRadius: "))                               ;弧焣
  (setq l (getreal "\nLength: "))                               ;坕墶煛嫨贰

  (initget "90 180")
  (setq ans (getkword "\n90 180 <90>: "))                       ;90暋 180暋 坕墶煛葌
  (if (= ans nil) (setq ans 90))                                ;return烦漛 90暋潯

  (setq lent (entget (car (entsel "\nSelect LINE: "))))         ;line葌
  (setq spnt (cdr (assoc 10 lent))
        epnt (cdr (assoc 11 lent))                              ;竍格壛 弡格
        newc (cdr (assoc 62 lent)))                             ;竍格壛 弡格

;  (setq angse (angle spnt epnt))                                ;竍格壛 弡格贰 贰瀬揺 坆

  (setq pnt (getpoint "\Pick end point: "))                     ;坕墶煛 弡格 烦漛

  (setq dstsp (distance spnt pnt)                               ;竍格礎 堘煛
        dstep (distance epnt pnt)                               ;弡格礎 堘煛
        dstse (distance spnt epnt))                             ;竍格礎 弡格宎弧 堘煛

  (if (<= dstsp dstep)
    (setq p1 spnt                                               ;坅宎秴舰 格穒 p1
          p2 epnt)                                              ;犲舰 格穒 p2
    (setq p1 epnt
          p2 spnt))

  (setq ang (angle p2 p1))                                      ;竍格~弡格 坆
  (setq tmp (polar pnt (+ (/ pi 2.0) ang) 1))                ;渁伐壛 弡格穪 奱繿格 纗嫛 夺衑 繁 格
  (setq pnt (inters p2 p1 tmp pnt nil))                     ;坕墶煛 弡格壛 渁伐壛穪 奱繿格

  (setq sdpnt (getpoint "\nPick side point: "))                 ;坕墶煛 蟹
  (setq tmp (polar sdpnt (+ (/ pi 2.0) ang) 1))                 ;渁伐壛 side格穪 奱繿格 纗嫛 夺衑 格
  (setq insp (inters p2 p1 tmp sdpnt nil))                      ;side point盗 渁伐壛穪 奱繿格
  (setq ang1 (angle insp sdpnt))                                ;奱繿格壛 side point坅 贰瀬揺 坆
  (setq epnt1 (polar p2 ang (- (distance p2 pnt) r)))           ;选穪 竍格
  (setq cen (polar epnt1 ang1 r))                               ;坕墶煛 顶穪 簵
;  (if (= ans 90)
;    (command "ARC" "C" cen epnt1 "A" (rtod (dang ang ang1)))        ;选 媋煛嫛
;    (command "ARC" "C" cen epnt1 "A" (rtod (* 2 (dang ang ang1))))  ;选 媋煛嫛
;  ) ;of if

  (setq lent (subst (cons 10 p2) (assoc 10 lent) lent))         ;竍格 line各ァ
  (setq lent (subst (cons 11 epnt1) (assoc 11 lent) lent))      ;弡格 各ァ 巵嫛
  (entmod lent)                                                 ;line巵嫛

  (if (= ans 90)
    (setq lpnt1 (polar pnt ang1 r)
          lpnt2 (polar lpnt1 ang1 l))                           ;90暋 坕墶煛 虎
    (setq lpnt1 (polar epnt1 ang1 (* 2 r))                      ;180暋 坕墶煛 虎格
          lpnt2 (polar lpnt1 (- ang pi) l))                     ;180暋 坕墶煛 虎狗格
  ) ;of if

  (setq oldc (getvar "CECOLOR"))
  (command "COLOR" newc)

  (if (= ans 90)
    (command "ARC" "C" cen epnt1 "A" (rtod (dang ang ang1)))        ;选 媋煛嫛
    (command "ARC" "C" cen epnt1 "A" (rtod (* 2 (dang ang ang1))))  ;选 媋煛嫛
  ) ;of if
  (command "LINE" lpnt1 lpnt2 "")                               ;line媋煛嫛

  (setvar "CECOLOR" oldc)

  (pop-env)                                                     ;雅墂畞 ア娽
  (setq *error* oer seterr nil)
  (princ)

);of defun
