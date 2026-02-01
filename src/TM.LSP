;****************************************
; Program : TM
;           Text Marking
;           By Suk-Jong Yi
;           1995/5/29
;****************************************

; 기존 text에다 각종 마킹을 해준다. 해준다. gows

(defun C:TM(/

)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                          ;환경변수 대피
  ;(setvar "CMDECHO" 1)

  (setq ents (ssget))                                 ;text선택
  (setq nents (sslength ents))
  (initget 1 "Circle Box Ellipse Obang")                ;키워드 초기화
  (setq ans (getkword "Circle/Box/Ellipse/Obang: "))  ;마킹종류선택


  (cond
    ((= ans "Circle")
                     (setq cc (getpoint "\nPick center point: "))
                     (setq cd (getdist cc "\nRadius: "))
                     (setq count 0)
                     (repeat nents
                       (setq ent (entget (ssname ents count)))
                       (if (= (cdr (assoc 0 ent)) "TEXT")
                         (progn
                           (setq cpnt (txtmid ent))
                           (command "CIRCLE" cpnt cd)
                         ) ;of progn
                       ) ;of if
                       (setq count (+ count 1))
                     ) ;of repeat
                   ) ;of cond ans=circle
    ((or (= ans "Box") (= ans "Obang") (= ans "Ellipse"))
                  (initget "Fit")
                  (setq w (getdist "\n<Box width>/Fit: "))
                  (if (/= w "Fit") (setq h (getdist "\nBox Height: ")))
                  (setq count 0)
                  (repeat nents
                    (princ "\ncount = ")
                    (princ count)
                    (setq ent (entget (ssname ents count)))
                    (if (= (cdr (assoc 0 ent)) "TEXT")
                      (progn
                        (setq ta (cdr (assoc 50 ent)))    ;text의 회전각
                        (setq ipnt (cdr (assoc 10 ent)))  ;insert point
                        (setq th (cdr (assoc 40 ent)))    ;text 높이
                        (setq tbp1 (mapcar '+ (car (textbox ent)) ipnt)
                              tbp2 (mapcar '+ (cadr (textbox ent)) ipnt))
                        (setq tbw (- (car  tbp2) (car  tbp1))
                              tbh (- (cadr tbp2) (cadr tbp1)))
                        (if (= w "Fit")
                          (setq gaph (/ th 2.0)
                                gapv (/ th 2.0)
                                h    (+ tbh th))
                          (setq gaph (/ (- w tbw) 2.0)
                                gapv (/ (- h tbh) 2.0)))
                        (setq gang (atan (/ gapv gaph)))
                        (setq gdst (sqrt (+ (* gaph gaph) (* gapv gapv))))
                        (setq dst1 (distance ipnt tbp1)
                              dst2 (distance ipnt tbp2))
                        (setq ang1 (angle ipnt tbp1)
                              ang2 (angle ipnt tbp2))
                        (setq sp (polar ipnt (+ ta ang1) dst1)
                              ep (polar ipnt (+ ta ang2) dst2))
                        (setq ll (polar sp (+ ta pi gang) gdst))
                        (setq lu (polar ll (+ ta (/ Pi 2.0)) h))
                        (setq ru (polar ep (+ ta gang)    gdst))
                        (setq rl (polar ru (- ta (/ pi 2.0)) h))
                        (cond
                          ((= ans "Box") (command "PLINE" ll rl ru lu "C"))
                          ((= ans "Obang")
                            (command "PLINE" ll rl "A" ru "L" lu "A" "CL"))
                          ((= ans "Ellipse")
                            (setq lm (mid-point ll lu)
                                  rm (mid-point rl ru))
                            (command "ELLIPSE" lm rm (/ h 2.0)))
                        ) ;of cond
                      ) ;of progn
                    ) ;of if ent-text
                    (setq count (+ count 1))
                   ) ;of repeat
                  ) ;of cond ans=box

  ;  ((= ans "Ellipse"))
  ;  ((= ans "Obang"))
  ) ;of cond
  (setq *error* oer seterr nil)
  (princ)
) ;of defun


;***********************************
; Function : TXTMID
;            TeXT MIDdle point
;            By Suk-Jong Yi
;            1995/5/29
;***********************************

(defun TXTMID (ent / ent ta ipnt tbp1 tbp2 dst1 dst2 ang1 ang2 sp ep)
  (setq ta (cdr (assoc 50 ent)))    ;text의 회전각
  (setq ipnt (cdr (assoc 10 ent)))  ;insert point
  (setq tbp1 (mapcar '+ (car (textbox ent)) ipnt)
        tbp2 (mapcar '+ (cadr (textbox ent)) ipnt))
  (setq dst1 (distance ipnt tbp1)
        dst2 (distance ipnt tbp2))
  (setq ang1 (angle ipnt tbp1)
        ang2 (angle ipnt tbp2))
  (setq sp (polar ipnt (+ ta ang1) dst1)
        ep (polar ipnt (+ ta ang2) dst2))
  (mid-point sp ep)
)

