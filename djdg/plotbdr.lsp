;****************************************************************************
; Program : PLOTBDR
;           PLOT BorDeR
;           By Suk-Jong Yi
;           1997/1/9,98/7/1
;****************************************************************************
; 2004/11/18
;  2002���� ��밡���ϵ��� ����
;-----------------------------------
; ���鳻�� �ִ� ��� (BLOCK�̸��� BDR$�� �����ϴ�)Border�� ������ش�.
; Border  : block���� �Ǿ��־�� �ϸ�
;           plot�� window�� ����� �� ���� point�� �Ǿ��־�� �ϸ�
;           block�� �̸��� BDR$�� �����Ͽ��� �Ѵ�.
;           (MKBDR.LSP�̿� ����)
; Device  : ��ɳ����� ���� �̸� ����
; Scale   : ��ɳ����� ���� �̸� ����1
; plt�̸� : DWG���� �̸� + 0, 1, 2, 3....
;****************************************************************************

(defun C:PLOTBDR( /
                    dwgn    dwgnl   f_list   ss_lst   ss_num   index
                    pltn    pltn1   bdr_ent  bdr_nm   ipnt     i_scale
                    dxy     dx      dy       pnt2     plotn0

)                                          ;������������

  
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


  (setq dwgn (dwg_name))                                    ;���� �̸�
  (setq dwgnl (strlen dwgn))                                ;���� �̸��� ����

  (initget "All Select")
  (setq ans (getkword "\nAll/<Select>: "))
  (cond
    ((or (= ans nil) (= ans "Select"))
      (setq ss_lst (ssget))
    ) ;of ans-nil
    ((= ans "All")
      (setq f_list (list (cons 0 "INSERT") (cons 2 "BDR*")))    ;filter list
      (princ f_list)
      (setq ss_lst (ssget "X" f_list))                          ;entity ����
    ) ;of ans=All
  ) ;of cond

  (setq ss_num (sslength ss_lst))               ;���õ� entity����

  (setq pltn1 (strcat (getvar "DWGPREFIX") dwgn))

  (setq index 0)                        ;ù��° border����
  (repeat ss_num                        ;���õ� border ������ŭ �ݺ�
    (setq pltn (strcat pltn1 (itoa index)))         ;dwg file�� ���ں��̱�
    (setq bdr_ent (entget (ssname ss_lst index)))   ;border entity����
    (setq bdr_nm (cdr (assoc 2 bdr_ent)))           ;border�̸�
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
    (setq dxy (bdr_size bdr_nm)                     ;border size �˾Ƴ�
          dx (car dxy)                              ;xũ��
          dy (cadr dxy))                            ;yũ��
    (setq pnt2 (list (+ (car ipnt) dx)              ;border�� ���� �Ʒ���
                     (+ (cadr ipnt) dy)))
    (setvar "CMDDIA" 0)                             ;command echo OFF
    (setq pltn0 (strcat pltn ".plt"))

;    (command "PLOT" "W" ipnt pnt2 "N") ;Enter choice, 0-5 <0>:


(setq fitscl "Fit"
      fplot  "No")
    
; 2000�� plot routine
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
ipnt            ;Enter lower left corner of window <0.000000,0.000000>:
pnt2            ;Enter upper right corner of window <0.000000,0.000000>: 100,100
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




    

;    (if (/= (getvar "PLOTID") "Default System Printer")     ;����Ʈ ������ �������϶� ���
;        (command "N"))                                      ;�ٸ� �������� �� autospool "N"

;    (command pltn0)                                          ;plt name�Է�

    (setvar "CMDDIA" 1)                         ;command echo ON
    (princ pltn) (princ " is Plotted") (terpri) ;��� �޽���
    (setq index (1+ index))                     ;���� border��

  ) ;of repeat

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
  (setq count ls)                                  ;������ string����
  (while (and (/= (setq ch (substr dn count 1)) "\\")
              (> count 1))
    (setq count (1- count))
  ) ;of while
  (if (= ch "\\")
    (substr dn (1+ count) (- ls count))
    (substr dn count (- ls (1- count)))
  ) ;of if

  (substr dn 1 (- (strlen dn) 4))                       ;*.dwg���� '.dwg'����

) ;of defun

;****************************************************************
; function BDR_SIZE
;          BorDeR SIZE
;          Yi Suk-Jong
;          97/1/9
;****************************************************************
; BLOCK table�� �˻��Ͽ� border BLOCK�� ũ�⸦ ã���ش�.
; �Ѿ���� ��
;   BDR_NM : BorDeR NaMe (ũ�⸦ �˰� ���� border�̸�)
; �Ѿ�� ��
;            X, Y ũ�� (���������κ��� ������ �Ÿ�)
;****************************************************************

(defun BDR_SIZE( BDR_NM / head ent1 pnt1 ent2 pnt2)
  (setq head (tblsearch "BLOCK" bdr_nm))        ;block table ã��
  (setq ent1 (cdr (assoc -2 head))
        pnt1 (cdr (assoc 10 (entget ent1))))    ;ù��° point ��ǥ
  (setq ent2 (entnext ent1)
        pnt2 (cdr (assoc 10 (entget ent2))))    ;�ι�° point ��ǥ
  (list (- (car pnt2) (car pnt1))               ;x����
        (- (cadr pnt2) (cadr pnt1)))            ;y����
) ;of defun

