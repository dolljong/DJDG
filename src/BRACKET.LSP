;-------------------
; Program : BRACKET
;           draw BRACKET
;           by Yi Suk-Jong
;           98/9/24
;---------------------------
;Bracket그리기
;---------------------------

(defun C:BRACKET(
/ ent entp sidep dis hbk p1 p2 ang ps pt1 pt2 pt3 pt4 ang1
)

  (push-env)                                        ;환경변수대피

  (setq ents (entsel "\nSelect Line: "))            ;라인정보

  (setq ent (entget (car ents)))                    ;라인정보
  (setq entp (cadr ents))                           ;선택점

  (setq sidep (getpoint "\nPick side: "))           ;브라켓방향 설정

  (setq dis (getdist "\nHeight <880mm>: "))         ;브라켓상면~노면
  (if (= dis nil) (setq dis 880))

  (setq hbk (getdist "\nB: "))                      ;브라켓 폭

  (setq p1 (cdr (assoc 10 ent)))                    ;시작점좌표
  (setq p2 (cdr (assoc 11 ent)))                    ;끝점좌표

  (if (< (distance p1 entp) (distance p2 entp))     ;line의 각도 잡아내기
    (setq ang (angle p1 p2)                         ;교대 상단에서 하단으로의 각도
          ps  p1)                                   ;ps : 교대 상면점
    (setq ang (angle p2 p1)
          ps p2)
  );if

  (setq pt1 (polar ps ang dis)                      ;브라켓 시작점
        pt2 (polar pt1 ang (* hbk 2)))              ;브라켓 끝점

  (command "BREAK" ents "F" pt1 pt2)                ;교대선 자르기

  (setq sideang (angle ps sidep))                   ;side point와 시작점의 angle

  (if (>= (dang sideang ang) 0)                     ;교대선과의 차에 따른 +90도 or -90도
    (setq ang1 (- ang (* 0.5 pi)))
    (setq ang1 (+ ang (* 0.5 pi)))
  );if

  (setq pt3 (polar pt1 ang1 hbk)
        pt4 (polar pt3 ang  hbk))

  (command "LINE" pt1 pt3 pt4 pt2 "")               ;브라켓 그리기

  (pop-env)                                         ;환경변수 복귀

  (princ)

) ;of defun
