;****************************************************************************
; Program : ALLPLOTX
;           ALL PLOT EXTEND
;           By Suk-Jong Yi
;           2004/06/09
;****************************************************************************
; ���鳻�� �ִ� ��� Border�� ���
; Device  : ��ɳ����� ���� �̸� ����
; plt�̸� : Border�� �Ѱ��� ��� - DWG name�� ����
;           Border�� �ΰ� �̻��� ��� - DWG���� �̸��� ������ ���ڸ� ��ȣ��
;****************************************************************************

(defun C:ALLPLOTX( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left
                                                           low_right
)

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


  

  (initget "File Plotter")
  (setq fplot (getkword "\nWrite the plot to a file? <F>ile/<P>lotter: "))
  (if (= fplot "Plotter") (setq fplot "N") (setq fplot "Y"))

  (initget "Fit Scale A3")
  (setq fitscl (getkword "\nPlot Millimeters=Drawing Units or Fit or ? <F>it/<S>cale/<A3>: "))


  (setq dwgn (dwg_name))                                ;���� �̸�

  (setq pltn (strcat (getvar "DWGPREFIX") dwgn))      ; dwg�濡 ����

    (setq i_scale (getvar "DIMSCALE"))         ;border�� scale factor  
    (if (= fitscl "Scale") (setq fitscl (strcat "1=" (rtos i_scale))))
    (if (= fitscl "A3") (setq fitscl (strcat "1=" (rtos (* 2 i_scale)))))
    

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
"E"                 ;Enter plot area [Display/Extents/Limits/View/Window] <Display>: w
;low_left            ;Enter lower left corner of window <0.000000,0.000000>:
;up_right            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
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

    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri)

  (princ)
) ;of defun


;****************************************************************
; function DWG_NAME
;          DraWinG NAME
;          Yi Suk-Jong
;          96/6/27
;****************************************************************
; open ������� ������ �ҷ��� ��� DWGNAME�� full path���� �ǹǷ�
; full path���� ���ϸ� �κ��� �����س�
;****************************************************************
(defun DWG_NAME(/ dn ls count ch )

  (setq dn (getvar "DWGNAME"))                          ;�����̸� �ν�
  (setq ls (strlen dn))                                 ;string ����
  (setq count ls)                                       ;������ string����
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (setq dn (substr dn (1+ count) (- ls count)))
    (setq dn (substr dn count (- ls (1- count))))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg���� '.dwg'����

) ;of defun



