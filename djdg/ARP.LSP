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

  (setq ent (entsel "\nSelect Arc or Circle: "))    ;원이나 호선택

  (setq elist (entget (car ent)))
  (setq cen (cdr (assoc 10 elist)))                 ;중심좌표
  (setq ra (cdr (assoc 40 elist)))                  ;반경

  (setq sp (getpoint "\nPick start point: "))       ;array 시작점
  (setq ep (getpoint "\nPick End point: "))         ;array 끝점
  (setq dpnt (getpoint sp "\nPick Direction Pint: "))
  
  (setq sang (angle cen sp))                        ;시작점 절대각
  (princ "\nstart angle") (princ (rtod sang))

  (setq eang (angle cen ep))                        ;끝점 절대각
  (princ "\nend angle") (princ (rtod eang))

  (setq dpang (angle cen dpnt))  ;angle of direction point 
  
  (setq seang (- eang sang))                        ;시작점과 끝점의 차
  (princ "\nseangle: ") (princ (rtod seang))

  (setq pang (+ (- (* 2 pi) sang) eang)) ;시작점~끝점의 양의 방향각
  (if (> pang (* 2.0 pi)) (setq pang (- pang (* 2.0 pi))))
  (setq nang (- pang (* 2 pi)))             ;시작점~끝점의 음의 방향각
  (setq sdsgn (/ (abs (dang sang dpang)) (dang sang dpang))) ;방향을 구함

  (cond
    ((> sdsgn 0.0) (setq deltaa pang))
    ((< sdsgn 0.0) (setq deltaa nang))
  );cond

  (setq angsgn sdsgn) 
  ;(setq angsgn (/ seang (abs seang)))               ;시작점과 끝점의 차의 부호
  ;(princ "\nSign of angle: ") (princ angsgn)

  (princ "\nSign of angle: ") (princ sdsgn)

  (setq seang deltaa)
  (setq cl (/ (* 2 pi ra seang) (* 2 pi)))          ;호의 길이
  (princ "\nCurve length: ")
  (princ (rtos cl 2 (getvar "LUPREC")))             ;호의 길이 표시

  (initget "Distance Number")
  (setq kword (getkword "Distance / Number <Number>: "))
  (if (= kword nil) (setq kword "Number"))
  (cond
    ((= kword "Number")
      (setq nnum (getint "Enter divide Number: "))
      (setq nnum (+ nnum 1))  ;array num = divide num + 1
      (setq nang (rtod deltaa))  
    );subcond
    ((= kword "Distance")
      (setq delta (getreal "\nEnter Delta: "))          ;delta L 입력

      (setq num (abs (fix (/ cl delta))))               ;array 갯수 구함
      (princ "\nNumber: ") (princ num)

      (setq rmd (rem cl delta))                         ;나머지
      (princ "\nRemand: ") (princ rmd)

      ;(setq rmdst (* rmd delta))
      (setq rmdst rmd)
      (princ "\nRemaind dist: ") (princ rmdst)

      (setq rmang (* (/ (* 360.0 rmdst) (* 2.0 pi ra)) angsgn))  ;나머지 각

      (princ "\nradius: ") (princ ra)
      (princ "\nremaind ang: ") (princ rmang)
      (setq nang (- (rtod seang) (* rmang angsgn)))
      (princ "\nStart angle: ") (princ (rtod sang))
      (princ "\nEnd angle: ") (princ (rtod eang))
      (princ "\nNew angle: ") (princ nang)
      (setq nnum (abs (+ num 1)))                                ;array갯수
    );subcond
  );cond  

  (setq arent (ssget))           ;array할 entity선택

  (command "array" arent "" "p" cen nnum nang "y")           ;array명령 수행

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;;; End of program

