;**********************************
;  Program : ARP
;            ARray Polar
;            By Jong-Suk Yi
;            1995. 2. 27,5/24, 7/20
;*********************************
(defun c:ARP(/
        ent     elist   cen     ra      sp      ep
        sang    eang    seang   angsgn  cl      delta   num
        rmd     rmang   rmdst   nang    nnum    arent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)

  (setq ent (entsel "\nSelect Arc or Circle: "))    ;顶贰恆 选葌

  (setq elist (entget (car ent)))
  (setq cen (cdr (assoc 10 elist)))                 ;簵沽蝍
  (setq ra (cdr (assoc 40 elist)))                  ;墂

  (setq sp (getpoint "\nPick start point: "))       ;array 竍格
  (setq ep (getpoint "\nPick End point: "))         ;array 弡格

  (setq sang (angle cen sp))                        ;竍格 搁攣坆
  (princ "\nstart angle") (princ (rtod sang))

  (setq eang (angle cen ep))                        ;弡格 搁攣坆
  (princ "\nend angle") (princ (rtod eang))

  (setq seang (- eang sang))                        ;竍格壛 弡格穪 繿
  (princ "\nseangle: ") (princ (rtod seang))

  (setq angsgn (/ seang (abs seang)))               ;竍格壛 弡格穪 繿穪 选
  (princ "\nSign of angle: ") (princ angsgn)

  (setq cl (/ (* 2 pi ra seang) (* 2 pi)))          ;选穪 嫨贰
  (princ "\nCurve length: ")
  (princ (rtos cl 2 (getvar "LUPREC")))             ;选穪 嫨贰 蝍

  (setq delta (getreal "\nEnter Delta: "))          ;delta L 烦漛
  (setq num (abs (fix (/ cl delta))))               ;array 垥畞 妬衠
  (princ "\nNumber: ") (princ num)

  (setq rmd (rem cl delta))                         ;恆犪弧
  (princ "\nRemand: ") (princ rmd)

  ;(setq rmdst (* rmd delta))
  (setq rmdst rmd)
  (princ "\nRemaind dist: ") (princ rmdst)

  (setq rmang (* (/ (* 360.0 rmdst) (* 2.0 pi ra)) angsgn))  ;恆犪弧 坆

  (princ "\nradius: ") (princ ra)
  (princ "\nremaind ang: ") (princ rmang)
  (setq nang (- (rtod seang) (* rmang angsgn)))
  (princ "\nStart angle: ") (princ (rtod sang))
  (princ "\nEnd angle: ") (princ (rtod eang))
  (princ "\nNew angle: ") (princ nang)

  (setq arent (ssget))           ;array衖 entity葌

  (setq nnum (abs (+ num 1)))                                ;array垥畞

  (command "array" arent "" "p" cen nnum nang "y")           ;array漺 畞袟

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;;; End of program

