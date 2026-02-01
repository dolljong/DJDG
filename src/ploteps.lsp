;****************************************************************************
; Program : PLOTEPS
;           PLOT EPS
;           By Suk-Jong Yi
;           2002/10
;****************************************************************************
; ªµµøÎ eps∆ƒ¿œ¿ª ø¯«œ¥¬ ≈©±‚∑Œ ∏∏µÈæÓ¡›¥œ¥Ÿ.
;****************************************************************************

(defun C:ploteps( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                           low_right
)

  ;(push-env)                                        ;»Ø∞Ê∫Øºˆ ¥Î««

  (setq offset "-5.0,-2.0"
        bdr_B 815                                   ;border¿« ∆¯
        bdr_H 570                                   ;border¿« ≥Ù¿Ã(µø¥Î±∏-∞Ê¡÷)
;        bdr_H 600                                   ;border¿« ≥Ù¿Ã(ºˆ¿Œ∏Æ)
        bn    "BORDER*"                             ;∫Ì∑∞ ¿Ã∏ß
        xg    -15                                   ;x gap ø©πÈ¿∏∑Œº≠ $pp∞° æ¯¿ª∂ß∏∏ ¿€µø(µø¥Î±∏-∞Ê¡÷)
        yg     -5                                    ;y gap (µø¥Î±∏-∞Ê¡÷)
;        xg    -25                                   ;x gap(ºˆ¿Œ∏Æ)
;        yg    0                                     ;y gap(ºˆ¿Œ∏Æ)
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border row«„øÎ≥Ù¿Ã


  (setq epsplotter_name "PostScript Level 2.pc3"       ;setting printer name
	printer_name    "HP LaserJet 5000LE PCL 6")
  (setq epsplotter_papersize "ISO A4 (297.00 x 210.00 MM)"
	printer_papersize "A4 (210 x 297 mm)")
  (setq ctbfile "dolsanht.ctb")
  
   (initget "File Plotter")
;  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
;  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

;  (initget "Fit Scale A3")
;  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale/<A3>: "))
  (setq p1 (getpoint "\nPick lower left corner: ")
	p2 (getcorner p1 "\nPick upper right corner: "))

  (setq txte (entget (car (entsel "Select name text: "))))
;  (setq sizee (entget (car (entsel "select size text: "))))

  (initget "Size")
  (setq sizesel (entsel "Select size text: "))
  
  (if (or (= sizesel "Size") (= sizesel nil))
    (setq sizetxt (getstring "Enter size: "))
    (progn
      (setq sizent (entget (car sizesel)))
      (setq sizetxt (cdr (assoc 1 sizent)))
    );progn
  );if
    
  (setq indexstar (vl-string-search "*" sizetxt))
  (setq wsize (substr sizetxt 1 indexstar))
  (setq hsize (substr sizetxt (+ 2 indexstar) (- (strlen sizetxt) 1 indexstar)))
  
  (setq ipnt (cdr (assoc 10 txte)))  ;insert point
  (setq txt  (cdr (assoc 1 txte )))
  (setq bounding (textbox txte))
;  (setq bpnt1 (car bounding)  ;point-1 of bounding point
;	bpnt2 (cdr bounding))  ;point-2 of bounding point
  (setq tbp1 (mapcar '+ (car (textbox txte)) ipnt)
        tbp2 (mapcar '+ (cadr (textbox txte)) ipnt))
  (setq x1 (car tbp1)    ;x
	y1 (cadr tbp1)   ;y
	x2 (car tbp2)    ;x
	y2 (cadr tbp2))  ;y
  
  (setq minx (min x1 x2 (car p1) (car p2))
	maxx (max x1 x2 (car p1) (car p2))
	miny (min y1 y2 (cadr p1) (cadr p2))
	maxy (max y1 y2 (cadr p2) (cadr p2)))
	
  (setq print_pnt1 (list minx miny)
	print_pnt2 (list maxx maxy))
  
  (setq hlen (abs (- (car p2) (car p1)))
	vlen (abs (- (cadr p2) (cadr p1))))

;  (setq bbox (getreal "\nHorizontal size of Box(mm): ")
;        hbox (getreal "\nVertial size of Box(mm): "))

  (setq bbox (atof wsize)
	hbox (atof hsize))
  
  (initget 1 "Eps Printer")
  (setq ans (getkword "Select printer Device Eps Printer: "))
  (cond
    ((= ans "Eps") (setq desti epsplotter_name
			 paper_size epsplotter_papersize
                         fplot "Y"))
    ((= ans "Printer") (setq desti printer_name
                             paper_size printer_papersize
			     fplot "N"))
  ); cond  



  
  (setq hratio (/ bbox hlen)
	vratio (/ hbox vlen))

  (if (<= hratio vratio)
    (setq scalestring (strcat (rtos bbox 2 3) "=" (rtos hlen 2 3)))
    (setq scalestring (strcat (rtos hbox 2 3) "=" (rtos vlen 2 3)))
  );of if

  (princ scalestring)

;  (exit)
    
;  (setq pltn (dwg_name))                                ;∆ƒ¿œ ¿Ã∏ß
  (setq pltn txt)
  (setq pltn (strcat (getvar "DWGPREFIX") pltn))  

    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))

; r14øÎ plot routine
;    (command "PLOT" "W" low_left up_right
;             "5"  ;Enter choice, 0-5 <0>:
;             "N"  ;Do you want to change plotters?
;             "N"  ;Do you want to change any of the above parameters?
;            fplot ;Write the plot to a file?
;             "M"  ;Size units (Inches or Millimeters)
;             ""   ;plot origin in Millimeters
;             ""   ;Enter the Size or Width,Height (in millimeters)
;             ""   ;Rotate plot clockwise 0/90/180/270 degrees
;             ""   ;Remove hidden lines?
;           fitscl ;Plotter Millimeters=Drawing units or Fit or ?
;            "0"   );Enter choice, 0-5

; 2000øÎ plot routine
;    (setq ctr "center")
    ;command
(command "PLOT"
"y"                 ;Detailed plot configuration? [Yes/No] <No>: y
""                  ;Enter a layout name or [?] <Model>:
desti                ;Enter an output device name or [?] <HP LaserJet 4V>:
paper_size	 
;"PostScript Level 2.pc3"  ;Enter an output device name or [?] <HP LaserJet 4V>:
;"HP LaserJet 4V"   ;Enter an output device name or [?] <HP LaserJet 4V>:
;"HP LaserJet 5000LE PCL 6"
;"A3 297 x 420 mm"   ;Enter paper size or [?] <A3 297 x 420 mm>:
;"A3 (297 x 420 mm)"
;"A4 210 x 297 mm"
;"ISO A4 (210.00 x 297.00 MM)"  
;"ISO A4 (297.00 x 210.00 MM)"  ;postscript printer
"m"                 ;Enter paper units [Inches/Millimeters] <Inches>: m
;"Portrait"
"Landscape"
;	     "Landscape"         ;Enter drawing orientation [Portrait/Landscape] <Landscape>:
"n"                 ;Plot upside down? [Yes/No] <No>:
"w"                 ;Enter plot area [Display/Extents/Limits/View/Window] <Display>: w
;"E"
print_pnt1             ;Enter lower left corner of window <0.000000,0.000000>:
print_pnt2             ;Enter upper right corner of window <0.000000,0.000000>: 100,100
;fitscl              ;Enter plot scale (Plotted Millimeters=Drawing Units) or [Fit] <Fit>:
scalestring
;offset	     
"CENTER"
;"-5.0,-2.0"            ;Enter plot offset (x,y) or [Center] <0.00,0.00>: center
"Y"                ;Plot with plot styles? [Yes/No] <Yes>:
ctbfile          ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:
"Y"               ;Plot with lineweights? [Yes/No] <Yes>:
"N"                ;Remove hidden lines? [Yes/No] <No>:
fplot               ;Write the plot to a file [Yes/No] <N>:
);command	      
(if (= fplot "Y") (command pltn))  ;(Enter file name <C:\Program Files\ACAD2000\djdg\sample>:

(command  "N")                ;Save changes to model tab [Yes/No]? <N>
(command "y")                 ;Proceed with plot [Yes/No] <Y>: y

;(command "line"
;	 low_left            ;Enter lower left corner of window <0.000000,0.000000>:
;up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
;"")
;    (if (/= (getvar "PLOTID") "Default System Printer")     ;µ∆˙∆Æ ææΩ∫≈€ «¡∏∞≈Õ¿œ∂ß ≈Î∞˙
;        (command "N"))                                      ;¥Ÿ∏• «¡∏∞≈Õ¿œ ∂ß autospool "N"
;    (if (= fplot "Y")                                       ;file∑Œ √‚∑¬«“ ∂ß
;        (command "N"))                                      ;Autospool "N"
;
;    (if (= fplot "Y")                                       ;file∑Œ √‚∑¬«“∂ß
;        (command pltn))
;
;    (command)                                               ;plot∏Ì∑… ≥°≥ø

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)
;    (setq index (1+ index))                                 ;¥Ÿ¿Ω border∑Œ


;  (pop-env)                                                 ;»Ø∞Ê∫Øºˆ ∫π±Õ

  (princ)
) ;of defun



;----------------------------------------------------------------
; function SORT_XY
;          Yi Suk Jong
;          97/7/24
;----------------------------------------------------------------
; ¡÷æÓ¡¯ entity list∏¶ x,y¡¬«•∏¶ ¿ÃøÎ«œø© sort«—¥Ÿ.
; sortπÊπ˝¿∫
;     1. y∞™¿Ã ∫ÒΩ¡«— ∞Õ≥¢∏Æ «‡¿ª ∏∏µÁ¥Ÿ
;     2. ∞¢«‡µÈ¿∫ x∞™¿∏∑Œ sort«—¥Ÿ.
; ≥—æÓø¿¥¬ ∞™
;      entity list
; ≥—æÓ∞°¥¬ ∞™
;      sortµ» entity list
;----------------------------------------------------------------
(defun SORT_XY(ss_lst
  / ss_lst ss_num ssn_lst row_col row  cy cn y ygap count1 rown coln
)
  (setq ss_num (sslength ss_lst))              ;list∞πºˆ

  ;------- borderø£∆º∆º∏Ì list∏∏µÈ±‚
  (setq ssn_lst nil)
  (setq count 0)
  (repeat ss_num
    (setq ssn_lst (append ssn_lst (list (ssname ss_lst count))))
    (setq count (1+ count))
  ) ;of repeat

   ;------- insert y∞™¿∏∑Œ ¡§∑ƒ
  (setq ssn_lst (reverse (sort_ent ssn_lst 10 2)))  ;ø¿∏ß¬˜º¯-->≥ª∏≤¬˜º¯¿∏∑Œ ∫Ø∞Ê

  ;------- «‡∞˙ ø≠∑Œ ≥™¥©±‚
  (setq row_col nil)                                            ;«‡∑ƒlist ∫ÒøÏ±‚
  (setq row nil)                                                ;«‡ list∫ÒøÏ±‚
  (setq count 0)
  (setq cy (nth 2 (assoc 10 (entget (nth count ssn_lst)))))     ;«ˆ¿Á y∞™
  (setq cn 0)                                                   ;«ˆ¿Á π¯»£
  (setq count 1)                                                ;√ππ¯¬∞ ø‰º“∫Œ≈Õ
  (repeat (1- ss_num)
    (setq y (nth 2 (assoc 10 (entget (nth count ssn_lst)))))    ;«ˆ¿Á y∞™
    (setq ygap (abs (- cy y)))                                  ;y∞™¬˜
    (if (> ygap ytol)                        ;y∞™¬˜∞° border≥Ù¿Ã∏¶ ≥—¿ª∂ß
      (progn
        (setq count1 cn)
        (repeat (- count cn)                 ;row«¸º∫
          (setq row (append row (list (nth count1 ssn_lst))))
          (setq count1 (1+ count1))
        ) ;of repeat
        (setq row_col (append row_col (list row)))          ;row∏¶ «‡∑ƒø° √ﬂ∞°
        (setq cn count)
        (setq cy y)
        (setq row nil)
      ) ;of progn
    ) ;of if
    (setq count (1+ count))                                     ;¥Ÿ¿Ω ø‰º“∑Œ
  ) ;of repeat
  (setq count1 cn)                                              ;∏∂¡ˆ∏∑ row√≥∏Æ
  (repeat (- ss_num cn)
    (setq row (append row (list (nth count1 ssn_lst))))
    (setq count1 (1+ count1))
  ) ;of repeat
  (setq row_col (append row_col (list row)))

  ; ------------- row∫∞∑Œ ≥™¥©æÓ¡Æ ¿÷¥¬ list∏¶ «—∞≥¿« list∑Œ ≈Î«’
  (setq ssn_lst nil)
  (setq rown (length row_col))                          ;rowºˆ
  (setq count 0)                                        ;√ππ¯ row∫Œ≈Õ
  (repeat rown
    (setq row (sort_ent (nth count row_col) 10 1))      ;x¡¬«•∑Œ sort
    (setq coln (length row))                            ;«ˆ¿Á row¿« columnºˆ
    (setq count1 0)                                     ;√ππ¯¬∞ column∫Œ≈Õ
    (repeat coln
      (setq ssn_lst (append ssn_lst (list (nth count1 row))))  ;entity¿Ã∏ß listø° √ﬂ∞°
      (setq count1 (1+ count1))
    ) ;of repeat
    (setq count (1+ count))
  ) ;of repeat
;  (princ "\nin subroutine : ")
;  (princ ytol)
;   (princ ssn_lst)
  (setq ssn_lst ssn_lst)
) ;of defun




;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open ∏Ì∑…¿∏∑Œ ∆ƒ¿œ¿ª ∫“∑Øø√ ∞ÊøÏ DWGNAME¿Ã full path∏Ì¿Ã µ«π«∑Œ
; full path∏Ì¡ﬂ ∆ƒ¿œ∏Ì ∫Œ∫–¿ª √ﬂ√‚«ÿ≥ø
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;∆ƒ¿œ¿Ã∏ß ¿ŒΩƒ
  (setq ls (strlen dn))                                 ;string ±Ê¿Ã
  (setq count ls)                                       ;∏∂¡ˆ∏∑ string∫Œ≈Õ
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwgø°º≠ '.dwg'¡¶∞≈

) ;of defun


;******************************************************
; Function : SORT_ENT
;           SORT ENT
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; SSGET list∏¶ sort«ÿ¡ÿ¥Ÿ.
; ≥—æÓø¿¥¬ ∞™
;     ALIST : SORTµ«æÓæﬂ«“ SSGET LIST
;       ASS : ±‚¡ÿ¿Ã µ«¥¬ sub list (øπ:insert point --> 10)
;         TH : sub list¿« ∏Óπ¯¬∞ atom¿ª ±‚¡ÿ¿∏∑Œ ¡§∑ƒ«“ ∞Õ¿Œ∞°∏¶ æÀ∑¡¡ÿ¥Ÿ.
; ≥—∞‹¡ˆ¥¬ ∞™
;             SORTµ» LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      minv
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list¿« ∞πºˆ

  (setq slist nil)                          ;∫Û sortµ» list∏∏µÎ
  (setq rlist alist)                        ;√÷¥Î∞™¿ª √‡√‚«— ≥™∏”¡ˆ list

  (setq count nl)                           ;list ∞πºˆ∫Œ≈Õ «—∞≥æø ª©∏Èº≠ π›∫π

  (repeat nl                                        ;list∞πºˆ∏∏≈≠
    (setq minv (nth th (assoc ass (entget (nth 0 rlist)))))             ;√ππ¯¬∞ list∏¶ ¿€¿∫ ∞™¿∏∑Œ
    (setq min_th 0)                                 ;√÷º“∞™¿« ¿ßƒ°∏¶ √≥¿Ω¿∏∑Œ
    (setq count1 1)                                 ;µŒπ¯¬∞ list∫Œ≈Õ
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;«ˆ¿Á list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;«ˆ¿Á ∞™
      (if (< c_val minv)                            ;«ˆ¿Á ∞™¿Ã min∫∏¥Ÿ ¿€¿ª∂ß
        (progn
          (setq min_th count1)                      ;√÷º“∞™¿ßƒ°∏¶ «ˆ¿Á ¿ßƒ°∑Œ
          (setq minv c_val)                          ;√÷º“∞™¿ª «ˆ¿Á ∞™¿∏∑Œ
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;¥Ÿ¿Ω list∑Œ
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;√÷º“∞™¿ª sortµ» listø° √ﬂ∞°
    (setq rlist (del_atom rlist min_th))            ;≥≤¿∫listø°º≠ √÷º“ list ¡¶∞≈
    (setq count (1- count))                         ;«—∞≥ ¡Ÿø©º≠
  ) ;of repeat
;-------------- testøÎ source ---------------------------------------------
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
; listø°º≠ ∆Ø¡§ atom¿ª ¡ˆøÓ¥Ÿ
; ≥—æÓø¿¥¬ ∞™
;             b_list : √‡√‚¿¸ list
;               anth : √‡√‚µ«æﬂ«“ atom¿« ¿ßƒ°
; ≥—∞‹∞°¥¬ ∞™
;                    : √‡√‚»ƒ list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;¡ˆø™∫Øºˆ
)

  (setq nlist (length b_list))                      ;list¿« ∞πºˆ

  (setq a_list nil)                                 ;∫Û listª˝º∫
  (setq count 0)                                    ;√ππ¯¬∞ list∫Œ≈Õ

  (repeat nlist                                     ;list∞πºˆ∏∏≈≠ π›∫π
    (if (/= count anth)                             ;¡ˆ¡§µ» atom¿Ã æ∆¥—∞ÊøÏ∏∏
      (setq a_list (append a_list (list (nth count b_list))))   ;listø°¥Ÿ √ﬂ∞°
    ) ;of if
    (setq count (1+ count))
  ) ;of repeat

  (setq a_list a_list)

) ;of defun

;********************************************
; function : ipnt_nblk
;            insert point of nested block
;            Yi Suk-Jong
;            1999/7/15
;********************************************
; ±‚¥… : ∫Œ∏block¿« table data∏¶ µ⁄¡Æ æ∆µÈ∫Ì∑œµÈ¿«
;        ª¿‘¡°µÈ¿ª µπ∑¡¡ÿ¥Ÿ. µµ∞¢¿« «¡∏∞∆Æøµø™, µµ∏È∏Ìøµø™,
;        µµ∏È∫Ø»£øµø™ µÓ¿ª √£±‚¿ß«— «‘ºˆ¿Ã¥Ÿ.
; ≥—æÓø¿¥¬ ∞™
;   pblkname : parent block name ;∫Œ∏∫Ì∑œ¿Ã∏ß
;   sblkname : son block name    ;æ∆µÈ∫Ì∑œ¿Ã∏ß
; ≥—æÓ∞°¥¬ ∞™
;   ipnt_list : insert point list;æ∆µÈ∫Ì∑œ¿« ª¿‘¡°(insert point±‚¡ÿ ¡¬«•)
; øπ)
; (ipnt_nblk "BORDERKK" "$PUL")
;     -> borderkk∂Û¥¬ ¿Ã∏ß¿« blockæ»¿« $PUL¿Ã∂ı ∫Ì∑∞¿Ã
;        ª¿‘µ» ¡°µÈ¿ª ∏Æ≈œ«ÿ¡ÿ¥Ÿ.
;     -> ªÁøÎ¿⁄¥¬ ¿ßø°º≠ æÚæÓ¡¯ insert point∞™ø° ∫Ì∑œ¿« scale
;        ∞™¿ª ∞ˆ«— »ƒ insert¡°¡¬«•∞™ø°¥Ÿ ¥ı«œ∏È $PULµÈ¿« ¿˝¥Î¡¬«•∏¶
;        ±∏«“ ºˆ ¿÷¥Ÿ.
;******************************

(defun ipnt_nblk(pblkname sblkname
/ tlbh bname t-1 t-list ipnt_list
)


  
  (setq tblh (tblsearch "BLOCK" pblkname))   ;table data head
  (setq base_point (cdr (assoc 10 tblh)))  ;block¿« base point

  (setq t-1 (cdr (assoc -2 tblh)))           ;block≥ª √π entity∏Ì

  (if (= (cdr (assoc 70 tblh)) 0)
    (setq bname sblkname)
    (setq bname (strcat pblkname "|" sblkname))
  );if  
  
  (setq ipnt_list nil)                      ;insert point list∏¶ ∏∏µÍ

  (setq t-list (entget t-1))                ;√ππ¯¬∞ entity

  (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;           (= (strcase (strcat pblkname "|" sblkname))
           (= (strcase bname)	   
              (cdr (assoc 2 t-list))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
  );if

  (while (setq t-1 (entnext t-1))           ;¥Ÿ¿Ω entity
    (setq t-list (entget t-1))
    (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;             (= (strcase (strcat pblkname "|" sblkname))
             (= (strcase bname)
	     (strcase (cdr (assoc 2 t-list)))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
    );if

  );while

  (setq ipnt_list ipnt_list)                    ;Return insert point list

);defun


;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; ¿Ã «‘ºˆ¥¬ ,∑Œ ∫“∏Æµ» data∏¶ ≥™¥©æÓ «—∞≥¿« listø° π≠æÓ¡ÿ¥Ÿ.
; ¿Ã∂ß «¸∫Ø»Ø æ¯¿Ã ∏µÁ data¥¬ πÆ¿⁄ø≠∑Œ returnµ»¥Ÿ.
;******************************************************************

(defun DATA-IN(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;≥—æÓø¬ πÆ¿⁄ø≠
   (setq strl (strlen arg1))                    ;≥—æÓø¬ πÆ¿⁄ø≠¿« ±Ê¿Ã
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;√ﬂ√‚Ω√¿€ ¿ßƒ°
   (setq nchr 1)                                ;√ﬂ√‚πÆ¿⁄ ∞πºˆ
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;πÆ¿⁄ «—∞≥
      (if (or (= subs ",") (= subs ""))         ;«ˆ¿Á πÆ¿⁄∞° ,¿Ã∞≈≥™ ≥°¿œ∂ß
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;Ω√¿€¿ßƒ°∫Œ≈Õ
            (if (= rslt nil)
               (setq rslt (list lst))                  ;µπ∏≤∞™¿Ã ∫Òæ˙¿ª∂ß
               (setq rslt (append rslt (list lst)))    ;µπ∏≤∞™ø°¥Ÿ √ﬂ∞°
            ) ;of if
            (setq nchr 0)                       ;√ﬂ√‚∞πºˆ ¥ŸΩ√ 0¿∏∑Œ
            (setq strt (1+ count))              ;¥Ÿ¿Ω √ﬂ√‚Ω√¿€¿ª ¥Ÿ¿ΩπÆ¿⁄∑Œ
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;¥Ÿ¿Ω πÆ¿⁄∑Œ
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;πÆ¿⁄ ∞πºˆ «—∞≥ ¡ı∞°
   ) ;of repeat
   (setq arg1 rslt)                             ;µπ∏≤∞™ µπ∏≤
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


(defun text2string()
  (setq ss (ssget '((0 . "TEXT"))))                 ;text entity∑≥ùb
  (setq ns (sslength ss))                           ;selection set∑Å entityàïÆÅ
;  (setq fname (strcat (getvar "dwgprefix") "/"))    ;filename∑i §w∑°üq∑aù°
  (setq fname (getvar "dwgprefix"))    ;filename∑i §w∑°üq∑aù°
  (setq index 0)
  (repeat ns
    (setq txt (cdr (assoc 1 (entget (ssname ss index))))) ;textàt
    (if (> index 0)
      (setq fname (strcat fname "-" txt))   ;ñÅ§ÂºÅ text¶Å»·ìe text¥|µA "-"¬Åàa
      (setq fname (strcat fname txt))       ;¿ı§ÂºÅ text∑• âw∂ÅµE §w∑°üqµA ¶õ∑±
    );if
    (setq index (1+ index))
    fname
  );repeat

  ;(setq ipnt (getpoint "\nInsertion base point:")) ;¨s∑≥∏Ò ¨Â»Ç
  ;(setq sse (ssget))                        ;wblock–i entity¨Â»Ç

  ;(setvar "FILEDIA" 0)                      ;°wùwª•–óØ° dialog boxõaª° ¥gï°ù¢
); defun