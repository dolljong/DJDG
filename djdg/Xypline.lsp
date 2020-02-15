;*****************************************
; program : XYPLINE
;           read X-Y data and draw PLINE
;           Jong-Suk Yi
;           1995. 2. 11
;*****************************************

(defun C:XYPLINE(
/ xy oldxy node x y
)
  (defun SETERR(s)                              ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

    (setvar "cmdecho" 0)
    (setq th (getvar "textsize"))
    (if (= xscl nil) (setq dfltxscl 1)(setq dfltxscl xscl ))
    (princ "\nEnter X-scale<")(princ dfltxscl)  (princ ">: ")
    (setq xscl (getreal))
    (if (= xscl nil) (setq xscl dfltxscl))
  
    (if (= yscl nil) (setq dfltyscl 1)(setq dfltyscl yscl ))
    (princ "\nEnter Y-scale<")(princ dfltyscl)  (princ ">: ")
    (setq yscl (getreal))
    (if (= yscl nil) (setq yscl dfltyscl))
  

    (setq opf (open (getfiled "INPUT DATA" "" "DAT" 0) "r"))
    (if opf
        (progn
           (setq count 1)
           (setq npline 0)
           (while (and (setq ch (read-line opf)) (/= ch ""))
              (setq node (car (strloc ch)))
              (if (and (<= (ascii node) 122) (>= (ascii node) 65))
                  (progn
                     (setq cnode node)
                     (setq count 0)
		     (command "")
                  ) ;of progn
                  (progn
                     (setq x    (* xscl (atof (cadr  (strloc ch)))))
                     (setq y    (* yscl (atof (caddr (strloc ch)))))
                     (setq xy (list x y 0.0))
                     (if (= count 1)
                        (progn
                           (setq npline (+ npline 1))
                           (command "text" xy th "" cnode)
                           (if (and (>= npline 1) (/= npline 0))
                               (command)
                           ) ;of if npline/=1
                           (command "PLINE")
                        ) ;of progn
                     ) ;of if count=1
                     (command xy)
                ) ;of progn
              ) ;of if node=string
              (setq count (1+ count))
           ) ;of while read-line is not error
          (command "")
        ) ;of progn
        (princ "\nFile not found")
    ) ;of if
    (princ)
(princ)

(close opf)
(princ npline)
(princ "Pline Drawed")

  (setq *error* oer seterr nil)
 (princ)
) ;;of defun RDL


;****************************************
;     Function : STRLOC
;                Get STRing LOCation
;                Jong-Suk Yi
;                1995. 2. 8
;****************************************

(defun strloc(arg1 / rslt count)
   (setq str arg1)
   (setq strl (strlen arg1))
   (setq count 1)
   (setq num 1)
   (setq strt 1)
   (setq nchr 1)
   (repeat (+ strl 1)
      (setq subs (substr str count 1))
      (if (or (= subs ",") (= subs ""))
         (progn
            (setq lst (substr str strt (- nchr 1)))
            (if (= rslt nil)
               (setq rslt (list lst))
               (setq rslt (append rslt (list lst)))
            ) ;of if
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
) ;of defun STRLOC

