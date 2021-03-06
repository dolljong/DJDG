;*****************************************
;*    BCN
;*        Box/Circle Number
;*        By Jong-Suk Yi
;*        1995. 2. 5.
;*****************************************

(DEFUN C:BCN(/
            sn      cbn     th      txth
            num     pnt     txt     bh    addnum
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;내장에러루틴 가동

  (setvar "cmdecho" 0)
  (setq sn (getint "\nStart Number : "))            ;시작숫자
  ;(setq sn 1)
  (setq addnum (getint "\nAdd Number: "))           ;증분숫자

  (initget "Box Circle None")
  (setq cbn (getkword "\nBox/Circle/<None>: "))
  (setq th (getvar "textsize"))
  (princ "\nPick text/Text height<")
  (princ th)
  (initget "Pick")
  (setq txth (getdist ">: "))
  (cond
      ((= txth nil) (setq txth th))
      ((= txth "Pick")
          (progn
              (setq txth (cdr (assoc 40 (entget (car (entsel "Pick text"))))))
              (princ "Text height is ")
              (princ txth)
          ) ;of progn
      ) ;of cond txth="Pick"
  ) ;of cond
  (setq num sn)
  (princ "\nNumber ")
  (princ num)

  (initget "Change")
  (while (setq pnt (getpoint ", ChangeNum/<Pick point>: "))
      (if (= pnt "Change")
          (setq num (getint "Enter New Number"))
          (progn
              (setq txt (itoa num))
              (command "text" "j" "m" pnt txth "0" TXT)
              (setq bh (* 1.2 txth))
              (cond
                  ((= cbn "Circle") (command "CIRCLE" pnt bh))
                  ((= cbn "Box")
                      (progn
                          (setq lu (list (- (car pnt) bh) (+ (cadr pnt) bh)))
                          (setq ru (list (+ (car pnt) bh) (+ (cadr pnt) bh)))
                          (setq rl (list (+ (car pnt) bh) (- (cadr pnt) bh)))
                          (setq ll (list (- (car pnt) bh) (- (cadr pnt) bh)))
                          (command "pline" lu ru rl ll "c")
                      ) ;of progn
                  ) ;of cond cbn="Box"
              ) ;of cond
;              (setq num (+ num 1))                 ;1만큼 더함
              (setq num (+ num addnum))             ;증분만큼 더함
          ) ;of progn
      ) ;of if pnt="Change"
      (princ "\nNumber")
      (princ num)
      (initget "Change")
  ) ; of while

  (setq *error* oer seterr nil)                     ;에러루틴 복귀

  (princ)
);;; End of program
