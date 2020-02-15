;*****************************************
;       3DP
;           3D Point
;           Jong-Suk Yi
;           2000. 12. 15
;*****************************************
; 3d point�� ����ְ� elevation�� �����ش�.
; �����ϱ� ���� layer1,layer2,layer3�� ������� �־�� �ϰ���, 
; ������ layer�� point�������� layer, node��ȣ�� ������ layer, 
; z��ǥ���� ������ layer�Դϴ�. ���� ���Ϸ� 3dp.dat�� �÷��帳�ϴ�.
; pdmode : 34
; pdsize : 2.5




(defun C:3DP(
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

    (setq layer1 "LAYER1"                ;point�� layer��
	  layer2 "LAYER2"                ;node��ȣ�� layer��
	  layer3 "LAYER3"                ;z��ǥ�� layer��
	  th (getreal "\nText height: ")
  	  nxgap (* th  1.0)              ;node��ȣ text x gap
  	  nygap (* th  1.0)              ;node��ȣ text y gap
  	  zxgap (* th  1.0)              ;z��ǥ text x gap
  	  zygap (* th -1.0))	         ;z��ǥ text y gap

    ; layer �����
    (if (not (tblsearch "layer" layer1)) (command "layer" "n" layer1 ""))
    (if (not (tblsearch "layer" layer2)) (command "layer" "n" layer2 ""))	  
    (if (not (tblsearch "layer" layer3)) (command "layer" "n" layer3 ""))
    
    (setvar "cmdecho" 0)
    (setvar  "pdmode" 34)
    (setvar "pdsize" 2.5)
    
    (setq opf (open (getfiled "INPUT DATA" "" "DAT" 0) "r"))
    (if opf
        (progn
           (setq count 1)
           (while (setq ch (read-line opf))
              (setq node (car (strloc ch)))
              (if (and (<= (ascii node) 122) (>= (ascii node) 65))
                  (progn
                     (setq cnode node)          
                     (setq count 0)
                  ) ;of progn
                  (progn
		     (setq nxyz (strloc ch))
                     (setq n    (car (sp-trunc (nth 0 nxyz)))
		           x    (atof (nth 1 nxyz))
                           y    (atof (nth 2 nxyz))
                           z    (atof (nth 3 nxyz))
			   zt   (rtos z 2 (getvar "luprec")))
                     (setq xyz  (list x y z)
			   nxyz (list (+ x nxgap) (+ y nygap) z)
		           zxyz (list (+ x zxgap) (+ y zygap) z))
		     (setvar "clayer" layer1)
		     (command "point" xyz)
		     (setvar "clayer" layer2)		    
		     (command "text" nxyz th "0" n)
		     (setvar "clayer" layer3)		    
		     (command "text" zxyz th "0" zt)		    
                ) ;of progn
              ) ;of if node=string
              (setq count (1+ count))
           ) ;of while read-line is not error
        ) ;of progn
        (princ "\nFile not found")
    ) ;of if
(close opf)
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

;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; �Է¹��ڿ��� ��,�ڿ� �ִ� ��ĭ�� ©�󳽴�.
; ���ϰ���
; (©�� ���ڿ�,
;  ù ���� ������ ��ġ,
;  ������ ���� ������ ��ġ,
;  �����ΰ�?)
;***************************************************************************

(defun SP-TRUNC(txt
/               txtl        frntn       backn       txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if

);of defun

;************************************
; Function : IS-NUM
;            IS NUMber ?
;            By Suk-Jong Yi
;            1996/2/23
;************************************
; ���ڿ��� �����ΰ�?�� �Ǵ����ش�.
;************************************

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (= ch 44)
             (= ch 46)
             (and (= count 1) (= ch 43))
             (and (= count 1) (= ch 45))
         )
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun