;-------------------
; Program : BRACKET
;           draw BRACKET
;           by Yi Suk-Jong
;           98/9/24
;---------------------------
;Bracket媋煛嫛
;---------------------------

(defun C:BRACKET(
/ ent entp sidep dis hbk p1 p2 ang ps pt1 pt2 pt3 pt4 ang1
)

  (push-env)                                        ;雅墂畞攣稀

  (setq ents (entsel "\nSelect Line: "))            ;渁伐各ァ

  (setq ent (entget (car ents)))                    ;渁伐各ァ
  (setq entp (cadr ents))                           ;葌格

  (setq sidep (getpoint "\nPick side: "))           ;渁臮蟹 各

  (setq dis (getdist "\nHeight <880mm>: "))         ;渁臮瑆~憽
  (if (= dis nil) (setq dis 880))

  (setq hbk (getdist "\nB: "))                      ;渁臮 廷

  (setq p1 (cdr (assoc 10 ent)))                    ;竍格沽蝍
  (setq p2 (cdr (assoc 11 ent)))                    ;弡格沽蝍

  (if (< (distance p1 entp) (distance p2 entp))     ;line穪 坆暋 竤碼悂嫛
    (setq ang (angle p1 p2)                         ;奱攣 瑆攅礎 衋攅穉潯穪 坆暋
          ps  p1)                                   ;ps : 奱攣 瑆格
    (setq ang (angle p2 p1)
          ps p2)
  );if

  (setq pt1 (polar ps ang dis)                      ;渁臮 竍格
        pt2 (polar pt1 ang (* hbk 2)))              ;渁臮 弡格

  (command "BREAK" ents "F" pt1 pt2)                ;奱攣 竌焌嫛

  (setq sideang (angle ps sidep))                   ;side point盗 竍格穪 angle

  (if (>= (dang sideang ang) 0)                     ;奱攣壛穪 繿礎 榓焑 +90暋 or -90暋
    (setq ang1 (- ang (* 0.5 pi)))
    (setq ang1 (+ ang (* 0.5 pi)))
  );if

  (setq pt3 (polar pt1 ang1 hbk)
        pt4 (polar pt3 ang  hbk))

  (command "LINE" pt1 pt3 pt4 pt2 "")               ;渁臮 媋煛嫛

  (pop-env)                                         ;雅墂畞 ア娽

  (princ)

) ;of defun
