;**************************************
; Program : SHOE
;           SHOE marking
;           Suk-Jong Yi
;           96/5/28
;**************************************
; 슈좌표도의 슈마킹
;**************************************

(defun C:SHOE(/)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;환경변수대피

  (setq ds (getvar "DIMSCALE"))                                 ;스케일값

;  (setvar "cmdecho" 1)

  (setq ent (entget (car (entsel "\nSelect Chain: "))))         ;체인선택
  (setq pnt (getpoint "\nPick shoe insert point: "))            ;삽입점
  (initget "Fix Longi- Trans- 2-dir")
  (setq tp (getkword "\nMOVE Fix/Longi-/Trans-/2-dir: "))

  (setq enty (cdr (assoc 0 ent)))                               ;엔티티 종류

  (cond
    ((= enty "LINE")
      (setq ang (angle (cdr (assoc 10 ent)) (cdr (assoc 11 ent)))) ;chain의 각
    ) ;of LINE
    ((or (= enty "CIRCLE") (= enty "ARC"))
      (setq ang (+ (/ pi 2) (angle (cdr (assoc 10 ent)) pnt)))           ;chain의 각
    ) ;of CIRCLE
  ) ;of cond

  (cond
    ((= tp "Fix")
      (command "INSERT" (strcat (prefix) "blocks/shoemf")
                        pnt ds "" "0")                              ;고정슈
    ) ;of 0
    ((= tp "Longi-")
      (command "INSERT" (strcat (prefix) "blocks/shoem1")          ;교축방향 가동
                        pnt ds "" (rtod ang))
    ) ;of Longi-
    ((= tp "Trans-")
      (command "INSERT" (strcat (prefix) "blocks/shoem1")          ;교축직각방향 가동
                        pnt ds "" (+ (rtod ang) 90))
    ) ;of Trans-
    ((= tp "2-dir")
      (command "INSERT" (strcat (prefix) "blocks/shoem2")          ;양방향 가동
                        pnt ds "" (rtod ang))
    ) ;of 2-dir
  ) ;of cond

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
