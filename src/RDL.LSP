;*****************************************
;       RDL
;           Read data file and Draw Line
;           Jong-Suk Yi
;           1995. 2. 8
;*****************************************


(defun C:RDL(
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
;    (setq opf (open (getstring "\nEnter data file name: ") "r"))
    (setq opf (open (getfiled "INPUT DATA" "" "DAT" 0) "r"))
    (if opf
        (progn
           (setq count 1)
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
                           (command "text" xy th "" cnode)
                        ) ;of prog
                        (progn
;                           (princ "\noldxy:")
;                           (princ oldxy)
;                           (princ " xy")
;                           (princ xy)
;                           (princ "\n")
                           (command "line" oldxy xy "")
                        ) ;of progn
                     ) ;of if count=1
                     (setq oldxy xy)
                ) ;of progn
              ) ;of if node=string
              (setq count (1+ count))
           ) ;of while read-line is not error
        ) ;of progn
        (princ "\nFile not found")
    ) ;of if
    (princ)
; Restore previous error handler
;(setq *error* oer seterr nil)
(princ)
(close opf)
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

