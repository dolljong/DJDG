; Function : RTOD           Radian TO Degree
; Function : DTOR           Degree TO Radian
; function : DANG           Delta ANGle
; Program  : GANG           Get Angle
; Program  : MIDP           MIDdle Point
; Program  : LINEXT         LINE EXTend
; Program  : CRSP           CRoSs Point of two line
; Program  : ORIP           Get start point from original point
; program  : XY             Get .x and .y
; Porgram  : LARC           Get length of arc or circle
; Program  : PARC           Point on the ARC
; Program  : DIVIDEA        DIVIDE Arc
; Program  : MEASA          MEASure Arc
; Program  : NUMCOOR        Number & Coordinate
; Function : WHICH4         WHICH   (95.3.16)
; Function : PREFIX         get PREFIX of 'acad=' variable (95.3.28)
; Function : MID-POINT      get MIDdle POINT of two point
; Program  : CHB            CHange a line type to reBar
; Program : ALLPLOT         ALL PLOT (A3용)
; Program : ALLPLOT1        ALL PLOT (A1용)  한개의 파일에서 사용
; Program : PLOTA1          PLOT A1 (A1용) 여러개의 파일에서 사용
; Program : WDWGN           Write DWG Name
; Function : F_DH           Dim Horizontal
; Function : F_DV           Dim Vertical



;* 시스템변수의 현재값을 대피시킬 전역변수(list 형태)
(setq #ENV nil)


;* 사용할 시스템 변수의 현재값을 대피시킨다.
;* 해당 시스템 변수의 값을 설정한다
(defun push-env()
  (setq #ENV (list (getvar "BLIPMODE")
                   (getvar "CMDECHO")
                   (getvar "EXPERT")
                   (getvar "OSMODE")
             )
  )
  (setvar "BLIPMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "EXPERT" 1)
  (setvar "OSMODE" 0)
) ;of defun

;* 대피시켜둔 시스템 변수의 값을 복구시킨다.
(defun pop-env()
  (setvar "BLIPMODE" (car #ENV))
  (setvar "CMDECHO" (cadr #ENV))
  (setvar "EXPERT" (caddr #ENV))
  (setvar "OSMODE" (cadddr #ENV))
) ;of defun

;
;************************************
; function : DANG
;            Delta ANGle
;            Suk-Jong Yi
;            1995. 3. 9
;************************************
; 두각의 차를 구해준다.
;************************************
(defun DANG(ang1 ang2 /     ; ang1, ang2 : radian
wang1 wang2 ang
)
  (setq wang1 (+ (fix (/ ang1 (dtor 90.0))) 1))
  (setq wang2 (+ (fix (/ ang2 (dtor 90.0))) 1))
  (if (and (= wang1 1) (= wang2 4))
    (setq ang (* (+ ang1 (- (dtor 360.0) ang2)) -1))
    (if (and (= wang1 4) (= wang2 1))
      (setq ang (+ (- (dtor 360.0) ang1) ang2))
      (setq ang (- ang2 ang1))
    ) ;of if
  ) ;of if
) ;of defun


;*****************************
; Function : RTOD
;           Radian TO Degree
;          By Jong-Suk Yi
;          1995. 2. 26
;*****************************
(defun rtod(r)
  (/ (* r 180) pi)
)

;*****************************
; Function : DTOR
;            Degree TO Radian
;            By Jong-Suk Yi
;            1995. 2. 26
;*****************************
(defun dtor(d)
  (* (/ d 180) pi)
)

;*****************************
;  Program : GANG
;            Get Angle
;*****************************
(DEFUN C:GANG()
  (/ (* (GETANGLE "PICK TWO POINTS") 180) PI)
)

;****************************
; Program : MIDP
;           MIDdle Point
;****************************
(defun C:MIDP(/ p1 p2 rad dist)
  (setq p1 (getpoint "Pick first point: "))(terpri)
  (setq p2 (getpoint p1 "Pick second point: "))(terpri)
  (setq rad (angle p1 p2))
  (setq dist (distance p1 p2))
  (setq dist2 (/ dist 2))
  (setq mp (polar p1 rad dist2))
)

;****************************
; Program : LINEXT
;           LINE EXTend
;****************************
(defun c:linext(/ ent a dis pt1 pt2 pt3 pt10 pt11)
   (setvar "osmode" 512)
   (setq e (entsel "\nPick endpart of line : (nearest of) "))
   (setq ent (entget (car e)))
   (setvar "osmode" 0)
   (setq pt10 (cdr (assoc 10 ent)))
   (setq pt11 (cdr (assoc 11 ent)))
   (setq a (angle pt10 pt11))
   (setq pt1 (cadr e))
   (if (< (distance pt1 pt10) (distance pt1 pt11))
      (setq frpt pt10)
      (setq frpt pt11)
   ) ;; if

   (setq dis (getdist frpt "\nDistance to extend :"))
   (if (< (distance pt1 pt10) (distance pt1 pt11))
       (progn
          (setq pt2 (polar pt10 (+ a pi) dis))
          (setq ent1 (subst (cons 10 pt2) (assoc 10 ent) ent))
       ) ;;prog
       (progn
          (setq pt2 (polar pt11 a dis))
          (setq ent1 (subst (cons 11 pt2) (assoc 11 ent) ent))
       ) ;;prog
   ) ;;if
   (entmod ent1)
   (princ)
   )

;***********************************
; Program : CRSP
;           CRoSs Point of two line
;             Yi Suk Jong
;                     1994, 5
;***********************************

(defun c:crsp(/ ent1 ent2 crsp)
  (setq ent1 (entget (car (entsel "Select first object: "))))
  (setq ent2 (entget (car (entsel "Select second object: "))))
  (setq pt110 (cdr (assoc 10 ent1)))
  (setq pt111 (cdr (assoc 11 ent1)))
  (setq pt210 (cdr (assoc 10 ent2)))
  (setq pt211 (cdr (assoc 11 ent2)))
  (setq crsp (inters pt110 pt111 pt210 pt211 nil))
)


;************************************************
;  Program : ORIP
;            Get start point from original point
;                       By Yi suk jong
;************************************************

(defun c:orip(/ opt pt)
(setq opt (getpoint "Pick Origin point"))
(setq pt (getpoint "Pick Last point" opt))
)

;**************************************************
; change a line type to hidden line
;               By  Yi Suk Jong
;                        1994,7
;**************************************************

(defun c:chhid(/ ss count e oldlt newlt)
  (setq ss (ssget))
  (setq count 0)
  (while
    (setq e (ssname ss count))
    (setq ent (entget e))
    (setq oldlt (assoc 6 ent))
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "hidden"))))
       (progn
          (setq newlt (cons 6 "hidden"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (princ)
);;; End of program CHHID

;**************************************************
; PROGRAM: CHCEN
;          CHange a line type to CENter line
;          Yi Suk-Jong
;          94/5, 95/4/15
;**************************************************

(defun C:CHCEN (/ ss count e oldlt newlt)
  (setq ss (ssget))
  (setq count 0)
  (while
    (setq e (ssname ss count))
    (setq ent (entget e))
    (setq oldlt (assoc 6 ent))
    (setq oldc (assoc 62 ent))
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "center"))))
       (progn
          (setq newlt (cons 6 "center"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
    (if (= oldc nil)
       (setq ent (append ent (list (cons 62 1))))
       (progn
          (setq newc (cons 62 1))
          (setq ent (subst newc oldc ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (princ)
)

;********************************************
; Program : CHDASH
;           Change a line to Dashed Line
;           Yi Suk-Jong
;           94/5
;********************************************

(defun C:CHDASH(/ ss count e oldlt newlt)
  (setq ss (ssget))
  (setq count 0)
  (while
    (setq e (ssname ss count))
    (setq ent (entget e))
    (setq oldlt (assoc 6 ent))
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "dashed"))))
       (progn
          (setq newlt (cons 6 "DASHED"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (princ)
 ) ;;; End of program CHDASH

;******************************************
; Program : CHLAY
;           Change a line to BaLayer type
;           Yi Suk-Jong
;           94/5
;******************************************
; 선택한 entity들의 line type을 bylayer로
;******************************************

(defun c:chlay(/ ss count e oldlt newlt)
  (setq ss (ssget))
  (setq count 0)
  (while
    (setq e (ssname ss count))
    (setq ent (entget e))
    (setq oldlt (assoc 6 ent))
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "bylayer"))))
       (progn
          (setq newlt (cons 6 "bylayer"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (princ)
);;; End of program CHLAYER

;*********************************************
; Program : COORD
;           COORD Marking
;           By Yi Suk Jong
;           94/
;*********************************************
(defun c:coord()
  (setq pt1 (getpoint "\nPick First Point:"))
  (setq pt2 (getpoint pt1 "Pick Second Point:"))
  (setq px (car pt1))
  (setq py (cadr pt1))
  ;****** real to string
  (setq pxt (rtos px 2 (getvar "luprec")))
  (setq pyt (rtos py 2 (getvar "luprec")))
  (setq pxy (strcat "(" pxt "," pyt ")"))
  ;(setq pt12d (list (car pt1) (cadr pt1)))
  (command "dim1" "leader" pt1 pt2 "" pxy)
);;; End of program COORD


;**************************************************
; program : XY
;           Get .x and .y
;           Yi suk jong
;           95/2/6
;**************************************************

(defun c:xy(/ xyp)
  (setq xyp (list (car (getpoint "X of ")) (cadr (getpoint "  Y of"))))
);;;


;******************************************
; Porgram : LARC
;           Get length of arc or circle
;           By Yi suk jong
;           1995. 1. 20, 3. 22.
;******************************************
; ARC(호)의 길이를 구함
;******************************************
(defun c:LARC(/
ent elist cen ra sp                     ;지역변수 정의
ep sang eang seang cl
)

  (setq ent (entsel "Select Arc or Circle: "))
  (setq elist (entget (car ent)))
  (setq cen (cdr (assoc 10 elist)))
  (setq ra (cdr (assoc 40 elist)))
  (setq sp (getpoint "Pick start point: "))
  (setq ep (getpoint "Pick End point: "))
  (setq sang (angle cen sp))
  (setq eang (angle cen ep))
  (setq seang (abs (dang sang eang)))

  (setq cl (/ (* 2 Pi ra seang) (* 2 pi)))
  (princ "\nLength of ARC : ")
  (princ cl)
  (princ)

);;; End of program LARC


;************************************************
; Program : PARC
;           Point on the ARC
;           By Yi suk jong
;           1995. 1. 24, 3. 22, 5/27
;************************************************
; 호위의 한점에서 일정길이(호의 길이)떨어진 점의 좌표를 구함
;

(defun C:PARC(/
ent elist cen ra sp sang                ;지역변수 정의
len rang ps spang rrang rxy
)

  (setq ent (entsel "Select Arc or Circle: "))
  (setq elist (entget (car ent)))
  (setq cen (cdr (assoc 10 elist)))
  (setq ra (cdr (assoc 40 elist)))
  (setq sp (getpoint "Pick start point: "))
  (setq sang (angle cen sp))
  (setq len (getdist "Enter Arc Lenth: "))
  (setq rang (/ len ra))
  (setq ps (getpoint sp "Pick Which side: "))
  (setq spang (angle cen ps))
  (setq deltaa (dang sang spang))
  (setq absang (/ deltaa (abs deltaa)))
  (setq rrang (+ sang (* absang rang)))

  (setq rxy (polar cen rrang ra))

) ;;; End of program PARC


;**********************************
;*  DIVIDEA
;*       Divide Arc
;*            Suk-Jong Yi
;*            1995. 2. 2.
;**********************************
; 호위의 두점 사이를 divide
;**********************************

(defun C:DIVIDEC(/
ent cen r spnt epnt sang eang    ;지역변수 정의
dvnum dtang count ang dpnt
)

  ;*** Select entity
  (setq ent (entget (car (entsel "Select Arc or Circle"))))

  ;*** Get center & radius
  (setq cen (cdr (assoc 10 ent)))
  (setq r (cdr (assoc 40 ent)))

  ;*** Get start point & end point
  (setq spnt (getpoint "\nPick start point: "))
  (setq epnt (getpoint "\nPick end point: "))

  ;*** Get start angle & end angle
  (setq sang (angle cen spnt))
  (setq eang (angle cen epnt))

  ;*** Get divide number
  (setq dvnum (getint "\nNumber of divide"))

  ;*** Calc delta ang
  (setq dtang (/ (dang eang sang) dvnum))

  ;*** Repeat divide while count<=dvnum-1
  (setq count 1)
  (while (< count dvnum)
     (progn
        (setq ang (+ sang (* count dtang)))
        (setq dpnt (polar cen ang r))
        (command "point" dpnt)
        (setq count (+ count 1))
     ) ;of progn
     nil
  ) ;of while
) ;of defun

;**********************************
;*  MEASA
;*       Measure Arc
;*            Suk-Jong Yi
;*            1995. 2. 3.
;**********************************
; 호위의 두점 사이를 measure
;**********************************

(defun C:MEASA(/
ent cen ra spnt sang eang anges             ;지역변수 정의
larc mleng dvnum dtang count ang dpnt
)

  ;*** Select entity
  (setq ent (entget (car (entsel "Select Arc or Circle: "))))

  ;*** Get center & radius
  (setq cen (cdr (assoc 10 ent)))
  (setq ra (cdr (assoc 40 ent)))

  ;*** Get start point & end point
  (setq spnt (getpoint "\nPick start point: "))
  (setq epnt (getpoint "\nPick end point: "))

  ;*** Get start angle & end angle
  (setq sang (angle cen spnt))
  (setq eang (angle cen epnt))
  (setq anges (abs (- eang sang)))
  (setq sign (/ (abs (dang eang sang)) (dang eang sang)))
  (princ sign)
  ;*** Get length of arc
  (setq larc (* ra anges))

  ;*** Get measure length
  (setq mleng (getreal "\nMeasure length: "))

  ;*** Get number of divide
  (setq dvnum (fix (/ larc mleng)))

  ;*** Calc delta ang
  (setq dtang (/ mleng ra))
  (princ "dtang: ")
  (princ dtang)

  ;*** Repeat divide while count<=dvnum-1
  (setq count 1)
  (while (<= count dvnum)
     (progn
        (setq ang (+ sang (* count dtang sign)))
        (princ "\n  Count: ")
        (princ count)
        (princ "  Angle : ")
        (princ ang)
        (setq dpnt (polar cen ang ra))
        (princ "  Coord. : ")
        (princ dpnt)
        (command "point" dpnt)
        (setq count (+ count 1))
     ) ;of progn
     nil
  ) ;of while
) ;of defun

;*******************************************
; Program : NUMCOOR
;           Number & Coordinate
;           Yi Suk-Jong
;           95/2/3,96/4/8
;*******************************************
; 좌표들에 순차적으로 번호를 매기고
; 그 좌표값을 도면에 표시하거나 파일로 만들어준다.
;*******************************************
;   Error routine
;*******************************************
(defun SETERR(s)
;If an error (CTRL-C) occurs when this command is active.
  (if (/= s "Function cancelled")
      (princ (strcat "\nError: " s))
  ); of If
;Restore previous error handler
  (setq *error* oer seterr nil)
  (princ)
); of SETERR

;*************
; Main program
;*************

(DEFUN C:NUMCOOR()
  ;Store AutoLISP error routine
  ;(setq oer *error* *error* seterr)

  (setq lp (getvar "LUPREC"))

  (setvar "cmdecho" 0)
  (setq sn (getint "\nStart Number <1>: "))
  (if (= sn nil) (setq sn 1))
  (setq txts (getvar "textsize"))
  (princ "Text hight <")
  (princ txts)
  (setq txth (getdist ">: "))
  (terpri)
  (if (= nil txth) (setq txth txts))
  (setq sf (getreal "Scale factor <1.0>: "))                  ;scale factor입력
  (if (= sf nil) (setq sf 1.0))
  (setq lc (getpoint "Pick location of Coordinate list: "))
  (terpri)
  (setq num sn)

  ;*** Question file writing
  (initget "Yes No")
  (setq ans (getkword "Make file?(Yes/No): "))
  (if (= ans "Yes")
      (progn
  ; get file name
  ;        (setq fn (getstring "Enter file name :"))
          (setq fn (getfiled "Open data file" "" "dat" 1))

  ;*** check exist file
          (if (findfile fn)
              (progn
                (princ "\nFile allready exist, ")
                (initget 1 "OverWrite Append")
                (setq ansOA (getkword "OverWrite / Append: "))
                (cond
                   ((= ansOA "OverWrite") (setq opf (open fn "w")))
                   ((= ansOA "Append") (setq opf (open fn "a")))
                ); of cond
               ); of progn
  ;*** open write file (if file isn't exist)
          (setq opf (open fn "a"))
          ); of if
      ) ; End prog
      nil
  ) ; End if

  (setq count 1)
  (princ "\nPoint# ")
  (princ num)

  (while

  ;*** get node point
     (setq pnt (getpoint " Pick point: "))
     (setq pntx (car pnt))
     (setq pnty (cadr pnt))

  ;*** integer to number
     (setq txt (itoa num))

  ;*** get number text position
     (setq pnt1x (+ pntx txth))
     (setq pnt1y (+ pnty txth))
     (setq pnt1 (list pnt1x pnt1y))

  ;*** draw number text
     (command "text" "j" "m" pnt1 txth "" TXT)
     (terpri)

  ;*** get coord text
     (setq pxy (strcat txt "(" (rtos (* pntx sf) 2 lp)
                           "," (rtos (* pnty sf) 2 lp) ")"))
     ;(princ pxyf opf)

     (if (= ans "Yes")
        (progn
     ; Write File (ans = "Yes")
  ;*** get coord text to write file
     (setq pxyf (strcat txt "," (rtos (* pntx sf) 2 lp)
                            "," (rtos (* pnty sf) 2 lp)))
     (write-line pxyf opf)
     ;(princ "coord= ")
     ;(princ pxyf)
        ) ; End then
     ) ; End if

  ;*** get coord text position
     (setq cpy (- (cadr lc) (* count (* 1.5 txth))))
     (setq cpx (car lc))
     (setq cpxy (list cpx cpy))

  ;*** draw coord text
     (command "text" cpxy txth "" pxy)
     (terpri)
     (setq num (+ num 1))
     (setq count (+ count 1))
     (princ "Point# ")
     (princ num)
  ) ; of while

  (if (= ans "Yes")
     (close opf)
  ) ;End if

  ; Restore previous error handler
  ;(setq *error* oer seterr nil)
  (princ)
);;; End of Program


;******************************************
; Function : WHICH4
;            WHICH
;            Suk-Jong Yi
;            1995. 3. 15
;******************************************
; 어떤 각이 몇사분면에 있는가를 되돌려준다.
;******************************************
(defun WHICH4(ang / ang)

(cond
  ((<= ang (dtor 90.0)) 1)
  ((and (> ang (dtor 90.0)) (<= ang (dtor 180.0))) 2)
  ((and (> ang (dtor 180.0)) (<= ang (dtor 270.0))) 3)
  ((and (> ang (dtor 270.0)) (<= ang (dtor 360.0))) 4)
) ; of cond

) ; of defun

;*****************************************
; Function : PREFIX
;            Get prefix of acad= variable
;            acad풀그림이 위치한 디렉토리를 찾아낸다
;            Suk-Jong Yi
;            1995. 3/28, 4/28
;*****************************************

(defun prefix( / cfg count)
  (setq cfg (strcat (getenv "acadcfg") "\\"))
;  (setq ver (substr (getvar "ACADVER") 1 2))
;  (cond
;    ((= ver "12") (strcat cfg "\\"))
;    ((= ver "13")
;      (progn
;        (setq count 1)
;        (while (/= (substr cfg count 1) "\\")
;         (setq count (1+ count))
;        ) ;end while
;        (setq count (1+ count))
;        (while (/= (substr cfg count 1) "\\")
;          (setq count (1+ count))
;        ) ;end while
;        (substr cfg 1 count)
;      ) ;of progn
;    ) ;of sub-cond
;    ((= ver "14") (strcat cfg "\\"))
;  ) ;of cond
  (setq sl (strlen cfg))
  (setq count 1)
  (repeat sl
    (if (= (substr cfg count 1) "\\")
      (setq cfg (strcat (substr cfg 1 (1- count)) "/"
                      (substr cfg (+ count 1) (- sl count))))
    ) ;of if
    (setq count (1+ count))
  ); repeat
  cfg
) ;of defun


;*****************************************
; Function : MID-POINT
;            get MID POINT of two point
;*****************************************

(defun mid-point (sp ep / dp)
    (setq dp (mapcar '+ sp ep))
    (mapcar '/ dp '(2.0 2.0 2.0))
) ;of defun

;**************************************************
; CHange a line type to reBar
;               By  Yi Suk Jong
;                        95/4/14
;**************************************************

(defun C:CHB(/ ss count e oldlt newlt oldc newc)
  (setq ss (ssget))
  (setq count 0)
  (while
    (setq e (ssname ss count))
    (setq ent (entget e))
    (setq oldlt (assoc 6 ent))
    (setq oldc (assoc 62 ent))
;라인타입바꾸기
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "CONTINUOUS"))))
       (progn
          (setq newlt (cons 6 "CONTINUOUS"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
;색깔 바꾸기
    (if (= oldc nil)
       (setq ent (append ent (list (cons 62 2))))
       (progn
          (setq newc (cons 62 2))
          (setq ent (subst newc oldc ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (princ)
)
;*******************************


(defun C:CH1()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "1" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:CH2()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "2" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:CH3()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "3" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:CH4()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "4" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:CH5()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "5" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:CH6()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "6" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:CH7()
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (command "CHPROP" ss "" "C" "7" "")
  (setvar "CMDecho" 1)
  (princ)
) ;of defun

(defun C:BB()
  (push-env)
  (setq ent (entsel "\nSelect Entity: "))
  (setq pt (getpoint "\nPick break point: "))
  (command "BREAK" ent "F" pt pt)
  (pop-env)
  (princ)
) ;of defun

;****************************************************************************
; Program : ALLPLOT
;           ALL PLOT
;           By Suk-Jong Yi
;           1997/12
;****************************************************************************
; 도면내에 있는 모든 Border를 출력
; Device  : 명령내리기 전에 미리 선택
; plt이름 : Border가 한개인 경우 - DWG name과 같게
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;****************************************************************************

(defun C:ALLPLOT( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

;  (push-env)                                        ;환경변수 대피

  (setq bdr_B 815                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER*"                             ;블럭 이름
        xg    -15                                   ;x gap
        yg    -5                                    ;y gap
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row허용높이

  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (initget "Fit Scale")
  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale: "))


  (setq dwgn (dwg_name))                                ;파일 이름
  (setq dwgnl (strlen dwgn))                            ;파일 이름의 길이
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))    ;filter list
  (setq ss_lst (ssget "X" f_list))                      ;entity 선택
  (setq ss_num (sslength ss_lst))                       ;선택된 entity갯수

  ;------ 선택된 border를 x,y방향으로 sort
  (setq ssn_lst (sort_xy ss_lst))


;------ border삽입점 x,y값 출력 (test용 source)
;  (setq count 0)
;  (repeat ss_num                                                ;border수만큼 반복
;    (setq ip (cdr (assoc 10 (entget (nth count ssn_lst)))))     ;y좌표잡아내기
;    (princ ip) ;(princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat

  ;-------- plt파일 이름 정하기(R12용)
;  (if (> ss_num 9)                                  ;border가 10개가 넘을 경우
;    (if (> dwgnl 6)                                   ;파일이름이 6자가 넘을 경우
;      (setq pltn1 (substr dwgn 1 6))
;      (setq pltn1 dwgn)
;    ) ;of if
;    (if (> dwgnl 7)                                   ;파일이름이 7자가 넘을 경우
;      (setq pltn1 (substr dwgn 1 7))
;      (setq pltn1 dwgn)
;    ) ;of if
;  ) ;of IF

;  (setq pltn1 dwgn)                                   ;출력파일은 cad방에 생김
  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))      ;   "       dwg방에 생김

  ;--------- 첫번째 border부터 출력하기
  (setq index 0)                        ;첫번째 border부터
  (repeat ss_num                        ;선택된 border 갯수만큼 반복
    (if (= index 0)
;      (setq pltn dwgn)
      (setq pltn pltn1)
      (if (and (<= index 9) (> ss_num 9))
        (setq pltn (strcat pltn1 "0" (itoa index)))
        (setq pltn (strcat pltn1 (itoa index)))
      ) ;of IF
    ) ;of IF
    (setq bdr_ent (entget (nth index ssn_lst)))     ;border entity정보
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
    (if (= fitscl "Scale") (setq fitscl (strcat "1=" (rtos i_scale))))
    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                         (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))
;    (if (findfile pltn0)                                    ; 같은 이름의 plt파일이 존재하면
;      (command "DEL" pltn0)                                 ;기존 plt파일 지우기
;    ) ;of if
;    (command "PLOT" "W" low_left up_right "N" pltn)  ;plot명령  r12인 경우

;    (if (= (getvar "PLOTID") "Default System Printer")       ;plot명령 r14인 경우
;      (command "PLOT" "W" low_left up_right "0"  pltn)      ;default system printer인 경우 spool여부 물어보지 않음
;      (command "PLOT" "W" low_left up_right "0" "N" pltn)   ;일반 프린터인 경우
;    ) ;of if
    (command "PLOT" "W" low_left up_right
             "5"  ;Enter choice, 0-5 <0>:
             "N"  ;Do you want to change plotters?
             "N"  ;Do you want to change any of the above parameters?
            fplot ;Write the plot to a file?
             "M"  ;Size units (Inches or Millimeters)
             ""   ;plot origin in Millimeters
             ""   ;Enter the Size or Width,Height (in millimeters)
             ""   ;Rotate plot clockwise 0/90/180/270 degrees
             ""   ;Remove hidden lines?
           fitscl ;Plotter Millimeters=Drawing units or Fit or ?
            "0"   );Enter choice, 0-5
    (if (/= (getvar "PLOTID") "Default System Printer")
        (command "N"))
    (if (= fplot "File")
        (command pltn))
    (command)

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;다음 border로
  ) ;of repeat

;  (pop-env)                                                 ;환경변수 복귀

  (princ)
) ;of defun


;----------------------------------------------------------------
; function SORT_XY
;          Yi Suk Jong
;          97/7/24
;----------------------------------------------------------------
; 주어진 entity list를 x,y좌표를 이용하여 sort한다.
; sort방법은
;     1. y값이 비슷한 것끼리 행을 만든다
;     2. 각행들은 x값으로 sort한다.
; 넘어오는 값
;      entity list
; 넘어가는 값
;      sort된 entity list
;----------------------------------------------------------------
(defun SORT_XY(ss_lst
/ ss_lst  ss_num  ssn_lst  row_col  row  cy  cn  y  ygap  ytol
  count1  rown    coln
)
  (setq ss_num (sslength ss_lst))              ;list갯수

  ;------- border엔티티명 list만들기
  (setq ssn_lst nil)
  (setq count 0)
  (repeat ss_num
    (setq ssn_lst (append ssn_lst (list (ssname ss_lst count))))
    (setq count (1+ count))
  ) ;of repeat

   ;------- insert y값으로 정렬
  (setq ssn_lst (reverse (sort_ent ssn_lst 10 2)))  ;오름차순-->내림차순으로 변경

  ;------- 행과 열로 나누기
  (setq row_col nil)                                            ;행렬list 비우기
  (setq row nil)                                                ;행 list비우기
  (setq count 0)
  (setq cy (nth 2 (assoc 10 (entget (nth count ssn_lst)))))     ;현재 y값
  (setq cn 0)                                                   ;현재 번호
  (setq count 1)                                                ;첫번째 요소부터
  (repeat (1- ss_num)
    (setq y (nth 2 (assoc 10 (entget (nth count ssn_lst)))))    ;현재 y값
    (setq ygap (abs (- cy y)))                                  ;y값차
    (if (> ygap ytol)                        ;y값차가 border높이를 넘을때
      (progn
        (setq count1 cn)
        (repeat (- count cn)                 ;row형성
          (setq row (append row (list (nth count1 ssn_lst))))
          (setq count1 (1+ count1))
        ) ;of repeat
        (setq row_col (append row_col (list row)))          ;row를 행렬에 추가
        (setq cn count)
        (setq cy y)
        (setq row nil)
      ) ;of progn
    ) ;of if
    (setq count (1+ count))                                     ;다음 요소로
  ) ;of repeat
  (setq count1 cn)                                              ;마지막 row처리
  (repeat (- ss_num cn)
    (setq row (append row (list (nth count1 ssn_lst))))
    (setq count1 (1+ count1))
  ) ;of repeat
  (setq row_col (append row_col (list row)))

  ; ------------- row별로 나누어져 있는 list를 한개의 list로 통합
  (setq ssn_lst nil)
  (setq rown (length row_col))                          ;row수
  (setq count 0)                                        ;첫번 row부터
  (repeat rown
    (setq row (sort_ent (nth count row_col) 10 1))      ;x좌표로 sort
    (setq coln (length row))                            ;현재 row의 column수
    (setq count1 0)                                     ;첫번째 column부터
    (repeat coln
      (setq ssn_lst (append ssn_lst (list (nth count1 row))))  ;entity이름 list에 추가
      (setq count1 (1+ count1))
    ) ;of repeat
    (setq count (1+ count))
  ) ;of repeat
  (setq ssn_lst ssn_lst)
) ;of defun

;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open 명령으로 파일을 불러올 경우 DWGNAME이 full path명이 되므로
; full path명중 파일명 부분을 추출해냄
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;파일이름 인식
  (setq ls (strlen dn))                                 ;string 길이
  (setq count ls)                                       ;마지막 string부터
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg에서 '.dwg'제거

) ;of defun


;******************************************************
; Function : SORT_ENT
;           SORT ENT
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; SSGET list를 sort해준다.
; 넘어오는 값
;     ALIST : SORT되어야할 SSGET LIST
;       ASS : 기준이 되는 sub list (예:insert point --> 10)
         TH : sub list의 몇번째 atom을 기준으로 정렬할 것인가를 알려준다.
; 넘겨지는 값
;             SORT된 LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq min (nth th (assoc ass (entget (nth 0 rlist)))))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;현재 list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;현재 값
      (if (< c_val min)                            ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq min c_val)                          ;최소값을 현재 값으로
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;다음 list로
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;최소값을 sort된 list에 추가
    (setq rlist (del_atom rlist min_th))            ;남은list에서 최소 list 제거
    (setq count (1- count))                         ;한개 줄여서
  ) ;of repeat
;-------------- test용 source ---------------------------------------------
;  (setq count 0)
;  (repeat nl
;    (princ (nth th (assoc ass (entget (nth count slist))))) (princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat
;--------------------------------------------------------------------------
  (setq slist slist)
) ;of defun


;************************************************
; Function : DEL_ATOM
;           DELete ATOM
;           Yi Suk-Jong
;           1996/2/23
;************************************************
; list에서 특정 atom을 지운다
; 넘어오는 값
;             b_list : 축출전 list
;               anth : 축출되야할 atom의 위치
; 넘겨가는 값
;                    : 축출후 list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;지역변수
)

  (setq nlist (length b_list))                      ;list의 갯수

  (setq a_list nil)                                 ;빈 list생성
  (setq count 0)                                    ;첫번째 list부터

  (repeat nlist                                     ;list갯수만큼 반복
    (if (/= count anth)                             ;지정된 atom이 아닌경우만
      (setq a_list (append a_list (list (nth count b_list))))   ;list에다 추가
    ) ;of if
    (setq count (1+ count))
  ) ;of repeat

  (setq a_list a_list)

) ;of defun



;****************************************************************************
; Program : ALLPLOT1
;           ALL PLOT
;           By Suk-Jong Yi
;           96/6/27
;****************************************************************************
; 도면내에 있는 모든 Border를 출력 (A1용)
; Device  : 명령내리기 전에 미리 선택
; Scale   : Fit / Factor 두가지 옵션
;           (Factor를 선택했을 때 DIMSCALE 변수자동인식)
; plt이름 : Border가 한개인 경우 - DWG name과 같게
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;****************************************************************************

(defun C:ALLPLOT1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

  (setq bdr_B 800                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER*"                             ;블럭 이름
        xg    -5                                    ;x gap
        yg    -5)                                   ;y gap

  (setq ds (getvar "DIMSCALE"))                     ;scale 값구함

  (setq dwgn (dwg_name))                            ;file이름 인식
  (setq dwgnl (strlen dwgn))
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))  ;filter list
  (setq ss_lst (ssget "X" f_list))                          ;entity 선택
  (setq ss_num (sslength ss_lst))                   ;선택된 entity갯수

  (if (> ss_num 9)                                  ;border가 10개가 넘을 경우
    (if (> dwgnl 6)                                 ;파일이름이 6자가 넘을 경우
      (setq pltn1 (substr dwgn 1 6))
      (setq pltn1 dwgn)
    ) ;of if
    (if (> dwgnl 7)                                 ;파일이름이 7자가 넘을 경우
      (setq pltn1 (substr dwgn 1 7))
      (setq pltn1 dwgn)
    ) ;of if
  ) ;of IF

  (setq index 0)
  (repeat ss_num                                    ;선택된 border 갯수만큼 반복
    (princ "\nDWG NUMBER: ") (princ index)
    (if (= index 0)
      (setq pltn dwgn)                              ;첫번째 border인 경우
      (if (and (<= index 9) (> ss_num 9))
        (setq pltn (strcat pltn1 "0" (itoa index))) ;열개이상의 보더중 1~9일때 0을 삽입
        (setq pltn (strcat pltn1 (itoa index)))     ;
      ) ;of IF
    ) ;of IF
    (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity정보
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                         (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))
    (if (findfile pltn0)                                    ; 같은 이름의 plt파일이 존재하면
      (command "DEL" pltn0)                                 ;기존 plt파일 지우기
    ) ;of if
    (if (= index 0)
      (progn
        (princ "\nPLT NUM: ") (princ index) (princ "\n")
        (setq pnts (strcat (rtos (car low_left) 2 3)
                       "," (rtos (cadr low_left) 2 3)
                       " " (rtos (car up_right) 2 3)
                       "," (rtos (cadr up_right) 2 3)))
        (setq scl (strcat "1=" (rtos ds 2 3)))
        (setq scr (open "a1.scr" "w"))
        (write-line "PLOT W" scr)
        (write-line pnts scr)
        (write-line "Y" scr)
        (write-line "N" scr)
        (write-line " " scr)
        (write-line " " scr)
        (write-line " " scr)
        (write-line " " scr)
        (write-line scl scr)
        (write-line pltn scr)
         (princ "\nINDEX = 0")
      ) ;of THEN
      (progn
        (setq pnts (strcat (rtos (car low_left) 2 3)
                       "," (rtos (cadr low_left) 2 3)
                       " " (rtos (car up_right) 2 3)
                       "," (rtos (cadr up_right) 2 3)))
        (setq pltxt (strcat "PLOT W " pnts " N " pltn))       ;change anything
        (write-line pltxt scr)
      ) ;of ELSE
    ) ;of IF
    (setvar "CMDDIA" 1)                                     ;command echo ON
;    (princ pltn) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;다음 border로
  ) ;of repeat

  (close scr)

  (command "SCRIPT" "A1")
;  (POP-ENV)                                                 ;환경변수 복귀
  (princ)
) ;of defun


;****************************************************************************
; Program : PLOTA1
;           PLOTA1
;           By Suk-Jong Yi
;           1995/11/3
;****************************************************************************
; 도면내에 있는 모든 Border를 출력 (HP GL/2용)
; Device  : 명령내리기 전에 미리 선택되어있어야 함
; Scale   : DIMSCALE변수값을 자동으로 인식한다.
; plt이름 : Border가 한개인 경우 - DWG name과 같게 출력
;           Border가 두개 이상인 경우 - DWG파일 이름의 마지막 두자를 번호로
;                                       바꿔서 출력
;****************************************************************************

(defun C:PLOTA1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                              low_right
)

  (setq bdr_B 800                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER*"                             ;블럭 이름
        xg    -5                                    ;x gap
        yg    -5)                                   ;y gap

;*** 파일 리스트 읽기
  (setq opf (open "PLOTA1.LST" "r"))                          ;file open
  (setq dwglst nil)
  (if opf
    (progn
      (setq count 1)
      (while (setq dwgn (read-line opf))
        (setq dwglst (append dwglst (list dwgn)))
        (setq count (1+ count))
      ) ;of while
      (close opf)
    ) ;of progn
  ) ;of IF

  (princ "PLT LIST: ") (princ dwglst)

  (setq dwgnum (length dwglst))
  (setq opf (open "PLOTA1.LST" "w"))
  (setq count 1)
  (repeat (1- dwgnum)
    (write-line (nth count dwglst) opf)
    (setq count (1+ count))
  ) ;of repeat
  (close opf)

  (princ dwglst)

  (setq ds (getvar "DIMSCALE"))                     ;scale 값구함

  (setq dwgn (getvar "DWGNAME"))
  (setq dwgnl (strlen dwgn))                        ;파일 이름의 길이
  (setq dwgnext (car dwglst))                       ;다음파일
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))  ;filter list
  (setq ss_lst (ssget "X" f_list))                          ;entity 선택


  (if (/= ss_lst nil)                                       ;border가 있을 때만
    (progn
      (setq ss_num (sslength ss_lst))                   ;선택된 entity갯수
      (if (> ss_num 9)                                  ;border가 10개가 넘을 경우
        (if (> dwgnl 6)                                 ;파일이름이 6자가 넘을 경우
          (setq pltn1 (substr dwgn 1 6))
          (setq pltn1 dwgn)
        ) ;of if
        (if (> dwgnl 7)                                   ;파일이름이 7자가 넘을 경우
          (setq pltn1 (substr dwgn 1 7))
          (setq pltn1 dwgn)
        ) ;of if
      ) ;of IF

      (setq index 0)
      (repeat ss_num                        ;선택된 border 갯수만큼 반복
;        (princ "\nDWG NUMBER: ") (princ index)
        (if (= index 0)
          (setq pltn dwgn)
          (if (and (<= index 9) (> ss_num 9))
            (setq pltn (strcat pltn1 "0" (itoa index)))
            (setq pltn (strcat pltn1 (itoa index)))
          ) ;of IF
        ) ;of IF
        (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity정보
        (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
        (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
        (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                             (+ (cadr ipnt) (* yg i_scale))))
        (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                             (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))
    (if (findfile pltn0)                                    ; 같은 이름의 plt파일이 존재하면
      (command "DEL" pltn0)                                 ;기존 plt파일 지우기
    ) ;of if
        (if (= index 0)
          (progn
            (princ "\nPLT NUM: ") (princ index) (princ "\n")
            (setq pnts (strcat (rtos (car low_left) 2 3)
                           "," (rtos (cadr low_left) 2 3)
                           " " (rtos (car up_right) 2 3)
                           "," (rtos (cadr up_right) 2 3)))
            (setq scl (strcat "1=" (rtos ds 2 3)))
            (setq scrf (strcat dwgn ".scr"))
            (setq scr (open scrf "w"))
            (write-line "PLOT W" scr)
            (write-line pnts scr)
            (write-line "Y" scr)
            (write-line "N" scr)
            (write-line " " scr)
            (write-line " " scr)
            (write-line " " scr)
            (write-line " " scr)
            (write-line scl scr)
            (write-line pltn scr)
             (princ "\nINDEX = 0")
          ) ;of THEN
          (progn
            (setq pnts (strcat (rtos (car up_left) 2 3)
                           "," (rtos (cadr up_left) 2 3)
                           " " (rtos (car low_right) 2 3)
                           "," (rtos (cadr low_right) 2 3)))
            (setq pltxt (strcat "PLOT W " pnts " N " pltn))       ;change anything
            (write-line pltxt scr)
          ) ;of ELSE
        ) ;of IF
        (setvar "CMDDIA" 1)                                     ;command echo ON
    ;    (princ pltn) (princ " is Plotted") (terpri)
        (setq index (1+ index))                                 ;다음 border로
      ) ;of repeat

      (if (> dwgnum 0)
        (write-line (strcat "OPEN " dwgnext " PLOTA1") scr)
      ) ;of IF
      (close scr)
      (command "SCRIPT" scrf)                                   ;script file선택
    ) ;of THEN
    (progn                                                      ;border가 없을 때
      (princ "\nBORDER NOT FOUND")
      (setq scrf (strcat dwgn ".scr"))
      (setq scr (open scrf "w"))
      (write-line (strcat "OPEN " dwgnext " PLOTA1") scr)
      (close scr)
      (command "SCRIPT" scrf)
    ) ;of ELSE
  ) ;of if
;  (POP-ENV)                                                 ;환경변수 복귀
  (princ)
) ;of defun




;******************************************
; Program : F_DH
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29
;******************************************
; 수평치수선을 함수로 처리해준다.
; 넘어오는 변수
;        SP : 시작점
;       DST : 거리
;         N : 반복갯수
;        UD : Up/DOWN (절대값은 LEVEL)
; 돌려주는 값 - 끝점 좌표
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (if (> ud 0)                                              ;위 아래
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;치수선 위치 계산 (절대값)

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep 위치계산

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치

  (setq dx (distance sp ep))                          ;거리 계산

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;골뱅이 옵션일 경우
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수 계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000이상일 때
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds 2.5)) 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)
          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                 ;치수보조선 내에 text 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*********************************
; Function : F_DV
;           Fuction Dimension Vertical
;           Jong-Suk Yi
;           96/7/1
;*********************************

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th 2.5                                        ;text크기 = 2.5
        dim_gap 10.0)                                 ;치수선 간격
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (> lr 0)                                        ;왼쪽/오른쪽
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))

  (setq next (* dst n))                                 ;끝점까지 거리

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;수정된 끝점

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치

  (setq dy (distance sp ep))                          ;두 점의 거리

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;나누는 길이가 1000미만시
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;나누는 길이가 1000이상시
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dy)
        (progn                                  ;text가 보조선 내에 안들어가면
          (setq dtxt1 (strcat divn "@" divl))   ;두줄로 나눔
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list (- (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy " ")               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                  ;text가 보조선 내에 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 nil) (setq txt1 txt))                    ;리턴입력시 옛 text를 씀
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if
  ep
) ;defun

;****************************************************************
; Program : WDWGN
;           Write DWG Name
;           Yi Suk Jong
;           97/1/13
;****************************************************************
(defun C:WDWGN(/ th ds fn p)

  (setq th 2.0)

  (setq ds (getvar "DIMSCALE"))
  (setq th (* th ds))
  (setq fn (strcat (dwg_name) ".DWG"))
  (setq p (getpoint "Pick Insert point: "))
  (command "TEXT" p th "0" fn)

) ;of defun

(load (strcat (prefix) "djdg/loadjdg"))       ;autoload 읽어옴