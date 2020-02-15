;****************************************************************************
; Program : ALLPLOT
;           ALL PLOT
;           By Suk-Jong Yi
;           1997/12
;****************************************************************************
; 옷절앁찌 첐얙 젏왢 Border쟍 Ãâ·Â
; Device  : 젌·톱짖쉄â 첲찌 좗쟕 쨤콲
; plt첇쟎 : Border쌰 Ç홇³첉 썭¿ì - DWG name쎀 쌿썞
;           Border쌰 왍썁 첇»ó첉 썭¿ì - DWG퀛첊 첇쟎첂 쟜쵔쟝 왍첔쟍 좾탺·Î
;****************************************************************************

(defun C:ALLPLOT( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                           low_right
)

  ;(push-env)                                        ;턆썭줦쩠 얾ÇÇ

 (if (not printer_name)
   (progn
     (setq fn (strcat (prefix) "djdg/allplot.set"))
     (setq opf (open fn "r"))                           ;open file
     (setq printer_name (read-line opf))
     (setq printer_papersize (read-line opf))
     (setq style_name (read-line opf))
     (setq offset (read-line opf))
     (close opf) 
   );progn
 );if  


  
  (setq ;offset "-5.0,-2.0"
        bdr_B 815                                   ;border첂 큊
        bdr_H 570                                   ;border첂 ³ô첇(µ찞ë쏮-썭촸)
        bn    "BORDER*"                             ;쥜잃 첇쟎
        xg    -15                                   ;x gap 찔좰챶·풮­ $pp쌰 쮲챹웒쟞 첕µ¿(µ찞ë쏮-썭촸)
        yg     -5                                    ;y gap (µ찞ë쏮-썭촸)
        ytol (* (getvar "DIMSCALE") bdr_H))         ;border rowÇã¿ë³ô첇

  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (initget "Fit Scale A3")
  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale/<A3>: "))


  (setq dwgn (dwg_name))                                ;퀛첊 첇쟎
  (setq dwgnl (strlen dwgn))                            ;퀛첊 첇쟎첂 쐕첇
  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))    ;filter list
  (setq ss_lst (ssget "X" f_list))                      ;entity 쨤콲
  (setq ss_num (sslength ss_lst))                       ;쨤콲왇 entity썇쩠

  ;------ 쨤콲왇 border쟍 x,y좭Çâ챶·Î sort
  (setq ssn_lst (sort_xy ss_lst))

;------ border»ð첎초 x,y쌹 Ãâ·Â (test¿ë source)
;  (setq count 0)
;  (repeat ss_num                                                ;border쩠쟞콠 좧줯
;    (setq ip (cdr (assoc 10 (entget (nth count ssn_lst)))))     ;y촥킨척쮅앁쐑
;    (princ ip) ;(princ "\n")
;    (setq count (1+ count))
;  ) ;of repeat

  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))      ; dwg좭찌 »ý쐗

  ;--------- 칯좾춿 border쥁콼 Ãâ·췒핌â
  (setq index 0)                        ;칯좾춿 border쥁콼
  (repeat ss_num                        ;쨤콲왇 border 썇쩠쟞콠 좧줯
    (if (= index 0)
      (setq pltn pltn1)
      (if (and (<= index 9) (> ss_num 9))
        (setq pltn (strcat pltn1 "0" (itoa index)))
        (setq pltn (strcat pltn1 (itoa index)))
      ) ;of IF
    ) ;of IF
    (setq bdr_ent (entget (nth index ssn_lst)))     ;border entity촋줮
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border첂 insert point
    (setq bdrname (cdr (assoc 2 bdr_ent)))         ;border첇쟎
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border첂 scale factor
    (if (= fitscl "Scale") (setq fitscl (strcat "1=" (rtos i_scale))))
    (if (= fitscl "A3") (setq fitscl (strcat "1=" (rtos (* 2 i_scale)))))
    
    (setq wpoint (ipnt_nblk bdrname "$PP"))         ;print window찟찕 point

    (if (= wpoint nil)                              ;$pp쥜·Ï첇 쮲챹 웒
      (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border첂 촥Ãø 쮅이
                           (+ (cadr ipnt) (* yg i_scale)))
            up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border첂 ¿ìÃø 챦
                           (+ (cadr ipnt) (* bdr_H i_scale))))
      (setq low_left (nth 0 wpoint)                   ;¿Þ췕쮅이
            up_right (nth 1 wpoint)                   ;¿챶봑Ê챦
            low_left (list (+ (car ipnt) (* i_scale (car low_left)))
                           (+ (cadr ipnt) (* i_scale (cadr low_left))))
            up_right (list (+ (car ipnt) (* i_scale (car up_right)))
                           (+ (cadr ipnt) (* i_scale (cadr up_right)))))
    );if
    (if (> (car low_left) (car up_right))
      (setq temp low_left
            low_left up_right
            up_right temp)
    ); if

;    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))

; r14¿ë plot routine
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

; 2000¿ë plot routine
;    (setq ctr "center")
    ;command
(command "PLOT"
"y"                 ;Detailed plot configuration? [Yes/No] <No>: y
""                  ;Enter a layout name or [?] <Model>:
printer_name        ;Enter an output device name or [?] <HP LaserJet 4V>:	 
;"HP LaserJet 4V"   ;Enter an output device name or [?] <HP LaserJet 4V>:
;"HP LaserJet 5000LE PCL 6"
;"HP LaserJet 5100 PCL 6"
printer_papersize   ;Enter paper size or [?] <A3 297 x 420 mm>:	 
;"A3 297 x 420 mm"  ;Enter paper size or [?] <A3 297 x 420 mm>:
;"A3 (297 x 420 mm)"
;"A3"
"m"                 ;Enter paper units [Inches/Millimeters] <Inches>: m
"Landscape"
;	     "Landscape"         ;Enter drawing orientation [Portrait/Landscape] <Landscape>:
"n"                 ;Plot upside down? [Yes/No] <No>:
"w"                 ;Enter plot area [Display/Extents/Limits/View/Window] <Display>: w
low_left            ;Enter lower left corner of window <0.000000,0.000000>:
up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
fitscl              ;Enter plot scale (Plotted Millimeters=Drawing Units) or [Fit] <Fit>:
offset	     
;"-5.0,-2.0"            ;Enter plot offset (x,y) or [Center] <0.00,0.00>: center
"Y"                ;Plot with plot styles? [Yes/No] <Yes>:
style_name         ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:	 
;"acad.ctb"          ;Enter plot style table name or [?] (enter . for none) <Default R14 penassignments.ctb>:
"Y"               ;Plot with lineweights? [Yes/No] <Yes>:
"N"                ;Remove hidden lines? [Yes/No] <No>:
fplot               ;Write the plot to a file [Yes/No] <N>:
);command	      
(if (= fplot "Y") (command pltn))  ;(Enter file name <C:\Program Files\ACAD2000\djdg\sample>:

(command  "N")                 ;Save changes to model tab [Yes/No]? <N>
(command "y")                 ;Proceed with plot [Yes/No] <Y>: y

;(command "line"
;	 low_left            ;Enter lower left corner of window <0.000000,0.000000>:
;up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
;"")
;    (if (/= (getvar "PLOTID") "Default System Printer")     ;왫큌퀉 ¾쭿줸Û Ç촞썐Í첊웒 쾖쎀
;        (command "N"))                                      ;얯쟌 Ç촞썐Í첊 웒 autospool "N"
;    (if (= fplot "Y")                                       ;file·Î Ãâ·췒Ò 웒
;        (command "N"))                                      ;Autospool "N"
;
;    (if (= fplot "Y")                                       ;file·Î Ãâ·췒훴§
;        (command pltn))
;
;    (command)                                               ;plot젌·É 씨³¿

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)
;    (command "text" up_right "1000" "0" (rtos index 3 0) "")
    (setq index (1+ index))                                 ;얯챻 border·Î
  ) ;of repeat

;  (pop-env)                                                 ;턆썭줦쩠 줯쐀

  (princ)
) ;of defun
;----------------------------------------------------------------
; function SORT_XY
;          Yi Suk Jong
;          97/7/24
;----------------------------------------------------------------
; 촸쮩쵖 entity list쟍 x,y촥킨쟍 첇¿ëÇÏ찔 sortÇ홊Ù.
; sort좭죃챸
;     1. y쌹첇 쥠쫘ÇÑ 썘씩쟕 Çà챹 쟞왢얯
;     2. 쌱Çà왤챸 x쌹챶·Î sortÇ홊Ù.
; ³Ñ쮩¿챲Â 쌹
;      entity list
; ³Ñ쮩쌰얙 쌹
;      sort왇 entity list
;----------------------------------------------------------------
(defun SORT_XY(ss_lst
  / ss_lst ss_num ssn_lst row_col row  cy cn y ygap count1 rown coln
)
  (setq ss_num (sslength ss_lst))              ;list썇쩠

  ;------- border찎퀖퀖젌 list쟞왤쐑
  (setq ssn_lst nil)
  (setq count 0)
  (repeat ss_num
    (setq ssn_lst (append ssn_lst (list (ssname ss_lst count))))
    (setq count (1+ count))
  ) ;of repeat

   ;------- insert y쌹챶·Î 촋·Ä
  (setq ssn_lst (reverse (sort_ent ssn_lst 10 2)))  ;¿챶§췽쩢-->앁¸쒝÷쩢챶·Î 줦썭

  ;------- Çà쎀 찘·Î 씱얄쐑
  (setq row_col nil)                                            ;Çà·칿ist 쥠¿ì쐑
  (setq row nil)                                                ;Çà list쥠¿ì쐑
  (setq count 0)
  (setq cy (nth 2 (assoc 10 (entget (nth count ssn_lst)))))     ;Çö첞 y쌹
  (setq cn 0)                                                   ;Çö첞 좾탺
  (setq count 1)                                                ;칯좾춿 ¿ä¼훸풴Í
  (repeat (1- ss_num)
    (setq y (nth 2 (assoc 10 (entget (nth count ssn_lst)))))    ;Çö첞 y쌹
    (setq ygap (abs (- cy y)))                                  ;y쌹췽
    (if (> ygap ytol)                        ;y쌹췽쌰 border³ô첇쟍 ³Ñ챹웒
      (progn
        (setq count1 cn)
        (repeat (- count cn)                 ;rowÇü쨬
          (setq row (append row (list (nth count1 ssn_lst))))
          (setq count1 (1+ count1))
        ) ;of repeat
        (setq row_col (append row_col (list row)))          ;row쟍 Çà·Ä찌 Ãß쌰
        (setq cn count)
        (setq cy y)
        (setq row nil)
      ) ;of progn
    ) ;of if
    (setq count (1+ count))                                     ;얯챻 ¿ä¼훵Î
  ) ;of repeat
  (setq count1 cn)                                              ;쟜쵔쟝 row칩쟕
  (repeat (- ss_num cn)
    (setq row (append row (list (nth count1 ssn_lst))))
    (setq count1 (1+ count1))
  ) ;of repeat
  (setq row_col (append row_col (list row)))

  ; ------------- row줧·Î 씱얄쮩촕 첐얙 list쟍 Ç홇³첂 list·Î 쾖ÇÕ
  (setq ssn_lst nil)
  (setq rown (length row_col))                          ;row쩠
  (setq count 0)                                        ;칯좾 row쥁콼
  (repeat rown
    (setq row (sort_ent (nth count row_col) 10 1))      ;x촥킨·Î sort
    (setq coln (length row))                            ;Çö첞 row첂 column쩠
    (setq count1 0)                                     ;칯좾춿 column쥁콼
    (repeat coln
      (setq ssn_lst (append ssn_lst (list (nth count1 row))))  ;entity첇쟎 list찌 Ãß쌰
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
; open 젌·É챶·Î 퀛첊챹 º훵¯¿Ã 썭¿ì DWGNAME첇 full path젌첇 왆좒·Î
; full path젌Áß 퀛첊젌 쥁쥃챹 ÃßÃâÇØ³¿
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;퀛첊첇쟎 첉쫛
  (setq ls (strlen dn))                                 ;string 쐕첇
  (setq count ls)                                       ;쟜쵔쟝 string쥁콼
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg찌쨠 '.dwg'촍썐

) ;of defun


;******************************************************
; Function : SORT_ENT
;           SORT ENT
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; SSGET list쟍 sortÇØ촺얯.
; ³Ñ쮩¿챲Â 쌹
;     ALIST : SORT왆쮩¾ßÇÒ SSGET LIST
;       ASS : 쐑촺첇 왆얙 sub list (찣:insert point --> 10)
;         TH : sub list첂 젍좾춿 atom챹 쐑촺챶·Î 촋·컎Ò 썘첉쌰쟍 쮊·촤Ø얯.
; ³홇Ü쵔얙 쌹
;             SORT왇 LIST
;******************************************************

(defun SORT_ENT(alist ass th
/       alist       nl       rlist       slist        count      minv
        min_th      count1   c_list      c_val        ass        th
)

  (setq nl (length alist))                  ;list첂 썇쩠

  (setq slist nil)                          ;쥡 sort왇 list쟞왦
  (setq rlist alist)                        ;ÃÖ얾쌹챹 ÃàÃâÇÑ 씱쟵쵔 list

  (setq count nl)                           ;list 썇쩠쥁콼 Ç홇³¾¿ 질절쨠 좧줯

  (repeat nl                                        ;list썇쩠쟞콠
    (setq minv (nth th (assoc ass (entget (nth 0 rlist)))))             ;칯좾춿 list쟍 첕챸 쌹챶·Î
    (setq min_th 0)                                 ;ÃÖ¼훯ª첂 챦캬쟍 칩챻챶·Î
    (setq count1 1)                                 ;왍좾춿 list쥁콼
    (repeat (1- count)
      (setq c_list (nth count1 rlist))                          ;Çö첞 list
      (setq c_val (nth th (assoc ass (entget (nth count1 rlist)))))    ;Çö첞 쌹
      (if (< c_val minv)                            ;Çö첞 쌹첇 min줮얯 첕챹웒
        (progn
          (setq min_th count1)                      ;ÃÖ¼훯ª챦캬쟍 Çö첞 챦캬·Î
          (setq minv c_val)                          ;ÃÖ¼훯ª챹 Çö첞 쌹챶·Î
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;얯챻 list·Î
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;ÃÖ¼훯ª챹 sort왇 list찌 Ãß쌰
    (setq rlist (del_atom rlist min_th))            ;³²챸list찌쨠 ÃÖ¼Ò list 촍썐
    (setq count (1- count))                         ;Ç홇³ 촻찔쨠
  ) ;of repeat
;-------------- test¿ë source ---------------------------------------------
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
; list찌쨠 퀊촋 atom챹 쵔¿î얯
; ³Ñ쮩¿챲Â 쌹
;             b_list : ÃàÃâ첲 list
;               anth : ÃàÃâ왆¾ßÇÒ atom첂 챦캬
; ³홇Ü쌰얙 쌹
;                    : ÃàÃâ턗 list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;쵔찕줦쩠
)

  (setq nlist (length b_list))                      ;list첂 썇쩠

  (setq a_list nil)                                 ;쥡 list»ý쨬
  (setq count 0)                                    ;칯좾춿 list쥁콼

  (repeat nlist                                     ;list썇쩠쟞콠 좧줯
    (if (/= count anth)                             ;쵔촋왇 atom첇 쮅얨썭¿ì쟞
      (setq a_list (append a_list (list (nth count b_list))))   ;list찌얯 Ãß쌰
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
; 쐑얠 : 쥁젏block첂 table data쟍 왘촕 쮅왤쥜·핏é첂
;        »ð첎초왤챹 옻·촤Ø얯. 옷쌱첂 Ç촞썑®찟찕, 옷절젌찟찕,
;        옷절줦탺찟찕 왩챹 칚쐑챦ÇÑ ÇÔ쩠첇얯.
; ³Ñ쮩¿챲Â 쌹
;   pblkname : parent block name ;쥁젏쥜·Ï첇쟎
;   sblkname : son block name    ;쮅왤쥜·Ï첇쟎
; ³Ñ쮩쌰얙 쌹
;   ipnt_list : insert point list;쮅왤쥜·Ï첂 »ð첎초(insert point쐑촺 촥킨)
; 찣)
; (ipnt_nblk "BORDERKK" "$PUL")
;     -> borderkk윕얙 첇쟎첂 block쮇첂 $PUL첇윗 쥜잃첇
;        »ð첎왇 초왤챹 쟕콾ÇØ촺얯.
;     -> »ç¿ë첔얙 챦찌쨠 ¾ò쮩쵖 insert point쌹찌 쥜·Ï첂 scale
;        쌹챹 썼ÇÑ 턗 insert초촥킨쌹찌얯 엇Ç핒é $PUL왤첂 첳얾촥킨쟍
;        쏮ÇÒ 쩠 첐얯.
;******************************

(defun ipnt_nblk(pblkname sblkname
/ tlbh bname t-1 t-list ipnt_list
)


  
  (setq tblh (tblsearch "BLOCK" pblkname))   ;table data head
  (setq base_point (cdr (assoc 10 tblh)))  ;block첂 base point

  (setq t-1 (cdr (assoc -2 tblh)))           ;block앁 칯 entity젌

  (if (= (cdr (assoc 70 tblh)) 0)
    (setq bname sblkname)
    (setq bname (strcat pblkname "|" sblkname))
  );if  
  
  (setq ipnt_list nil)                      ;insert point list쟍 쟞왥

  (setq t-list (entget t-1))                ;칯좾춿 entity

  (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;           (= (strcase (strcat pblkname "|" sblkname))
           (= (strcase bname)	   
              (cdr (assoc 2 t-list))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
  );if

  (while (setq t-1 (entnext t-1))           ;얯챻 entity
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
