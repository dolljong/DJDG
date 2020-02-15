;*****************************************
;       RDLP
;           Read data file and Draw Poly-Line
;           Jong-Suk Yi
;           1995. 2. 11
;*****************************************

;*******************************************
;   Error routine
;*******************************************

(defun C:RDLP(
/ xy oldxy node x y
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
    (setvar "cmdecho" 0)
    (setq th (getvar "textsize"))
    (setq opf (open (getstring "\nEnter data file name: ") "r"))
    (if opf
        (progn
           (setq count 1)
           (setq npline 0)
           (while (setq ch (read-line opf))
;              (princ ch)
              (setq node (car (strloc ch)))
;              (princ "\nnode :")
;              (princ node)
;              (princ "\ncount: ")
;              (princ count)
              (if (and (<= (ascii node) 122) (>= (ascii node) 65))
                  (progn
;                     (princ "\n It's string")
;                     (princ node)
                     (setq cnode node)
                     (setq count 0)
                  ) ;of progn
                  (progn
                     (setq x    (atof (cadr  (strloc ch))))
                     (setq y    (atof (caddr (strloc ch))))
                     (setq xy (list x y 0.0))
;                     (princ "\nnode:")
;                     (princ node)
;                     (princ " x:")
;                     (princ x)
;                     (princ " y:")
;                     (princ y)
;                     (princ "\n")
;                     (princ "\nxy :")
;                     (princ xy)
                     (if (= count 1)
                        (progn
                           (setq npline (+ npline 1))
                           (command "text" xy th "" cnode)
                           (if (and (>= npline 1) (/= npline 0))
                              (entmake '((0 . "SEQEND")))
                           ) ;of if npline/=1
                           (setq entlst (list (cons 0 "POLYLINE")))
                           (entmake entlst)
                        ) ;of progn
                     ) ;of if count=1
                     (setq entlst (list (cons 0 "VERTEX") (cons 10 xy)))
                     (entmake entlst)
                ) ;of progn
              ) ;of if node=string
              (setq count (1+ count))
           ) ;of while read-line is not error
        (entmake '((0 . "SEQEND")))
        ) ;of progn
        (princ "\nFile not found")
    ) ;of if
    (princ)
(princ)
(close opf)
(princ npline)
(princ "Pline Drawed")
(setq *error* oer seterr nil)
) ;;of defun RDL


;****************************************
;     Function : STRLOC
;                Get STRing LOCation
;                Jong-Suk Yi
;                1995. 2. 8
;****************************************

(defun strloc(arg1 / rslt count)
;Store AutoLISP error routine
;(setq oer *error* *error* seterr)
   (setq str arg1)
;   (princ "\nargument =")
;   (princ str)
   (setq strl (strlen arg1))
;   (princ "\nstring length =")
;   (princ strl)
   (setq count 1)
   (setq num 1)
   (setq strt 1)
   (setq nchr 1)
;   (setq rslt (list rslt))
   (repeat (+ strl 1)
      (setq subs (substr str count 1))
;      (princ "\nsubstring=")
;      (princ subs)
      (if (or (= subs ",") (= subs ""))
         (progn
            (setq lst (substr str strt (- nchr 1)))
;            (princ "\n")
;            (princ lst)
            (if (= rslt nil)
               (setq rslt (list lst))
               (setq rslt (append rslt (list lst)))
            ) ;of if
;            (princ "\nNew list=")
;            (princ rslt)
            (setq nchr 0)
            (setq strt (1+ count))
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))
      (setq num (1+ num))
      (setq nchr (1+ nchr))
   ) ;of repeat
   (setq arg1 rslt)
; Restore previous error handler
;(setq *error* oer seterr nil)
;(princ)
) ;of defun STRLOC

