;****************************************************************************
; Program : IBLK
;           Insert BLocK using block that was already inserted
;           By Suk-Jong Yi
;           2004/07/26
;****************************************************************************
; �־��� ���� ���ϴ� block�� insert(1���ϴ� 1����)
; �ַ� ������ �����κп� block�� insert��Ű�µ� ����
;   ex) ������ sign�� insert ��ų��
; bname : "borderch"    -- border��
; offset : "688,4"     -- insert��ġ
; ibname : "c:/program files/autocad 2002/blocks/sign"  -- insert�� block�̸�
;****************************************************************************

(defun C:aa1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left  low_right
)

  ;(push-env)                                        ;ȯ�溯�� ����


;;;(setq bname "borderbpo"
;;;      offset "688,4"
;;;      ibname "c:/program files/autocad 2002/blocks/sign1")

  (setq bdname "����(�𵨿�)")
;  (setq bdname "����(�����)")
;  (setq bdname "����(������)")
;  (setq bdname "����(���μ�)")
;  (setq bdname "����(����ȣ)")
;  (setq bdname "����(������)")
;  (setq bdname "����(�ӹٷ�)")
;  (setq bdname "����(������)")
;  (setq bdname "����(�̱���)")
;  (setq bdname "����(������)")
;  (setq bdname "����(��â��)")                  

  (setq mbname (strcat (prefix) "blocks/" bdname ".dwg"))
  
  (setq ibpnt (getpoint "\nPick insert point: "))

  (setq ds (getvar "dimscale"))
  
  (push-os)(command "INSERT" mbname ibpnt ds ds 0)(pop-os)

  (setq bdr (entlast))

  
;;;  (setq ibname (getstring "\nBlock name to be inserted: "))
;;;
;;;  (setq ibname (strcat (getvar "dwgprefix") ibname))
;;;  
;;;  (setq bn bname)
;;;  (setq bnstr (divide_str bname ","))			;block namestr
;;;  (setq nbn 	(length bnstr))				;number of block name
;;;  (if (> nbn 1)
;;;    (progn
;;;  	(setq index 0
;;;	      bnlst (list '(-4 . "<OR")))
;;;  	(repeat nbn
;;;    	  (setq bnlst (append bnlst (list (cons 2 (nth index bnstr)))))
;;;    	  (setq index (1+ index))
;;;  	);repeat
;;;  	(setq bnlst (append bnlst (list '(-4 . "OR>"))))
;;;        (setq f_list (append (list (cons 0 "INSERT")) bnlst))    ;filter list
;;;      );True
;;;    (progn
;;;      (setq f_list (list (cons 0 "INSERT")  (cons 2 bn)))    ;filter list  
;;;    );FLASE
;;;  );if  
;;;  (setq ss_lst (ssget "X" f_list))                      ;entity ����
;;;  (setq ss_num (sslength ss_lst))                       ;���õ� entity����
;;;
;;;
;;;  ;--------- ù��° border���� insert�ϱ�
;;;  (setq index 0)                        ;ù��° border����
;;;  (repeat ss_num                        ;���õ� border ������ŭ �ݺ�
;;;    (setq bdr_ent (entget (ssname ss_lst index)))     ;border entity����
;;;    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
;;;;    (setq bdrname (cdr (assoc 2 bdr_ent)))         ;border�̸�
;;;    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border�� scale factor
;;;
;;;    (if (> ss_num 0)  ;block�� ���� ��
;;;      (setq
;;;             offsetv (divide_str offset ",")
;;;	     xg1 (atof (car offsetv))
;;;	     yg1 (atof (cadr offsetv))
;;;
;;;	    ibpnt (list (+ (car ipnt) (* xg1 i_scale))        ;insertion point
;;;                           (+ (cadr ipnt) (* yg1 i_scale)))
;;;      );setq 	     
;;;      (progn
;;;	(princ "Block Not Found")
;;;	(exit)
;;;      );of false	
;;;    );if
;;;
;;;
;;;  (push-os)(command "INSERT" ibname ibpnt i_scale i_scale 0)(pop-os)
;;;
;    (setvar "CMDDIA" 1)                                     ;command echo ON
;    (princ pltn) (princ " is Plotted") (terpri)
;;;    (setq index (1+ index))                                 ;���� border��
;;;  ) ;of repeat

;  (pop-env)                                                 ;ȯ�溯�� ����

  (princ)
) ;of defun

  

(defun c:aa2()

  (if (= llist nil) (get_filist))  ;list�ҷ�����

  (setq fn (strcase (getvar "dwgname")))
  (setq fn (substr fn 1 (- (strlen fn) 4)))  
  (setq name (nth 4 (assoc fn llist)))

  (setq bdname (strcat "����(" name ")"))  
;  (setq bdname "����(�����)")
;  (setq bdname "����(������)")
;  (setq bdname "����(���μ�)")
;  (setq bdname "����(����ȣ)")
;  (setq bdname "����(������)")
;  (setq bdname "����(�ӹٷ�)")
;  (setq bdname "����(������)")
;  (setq bdname "����(�̱���)")
;  (setq bdname "����(������)")
;  (setq bdname "����(��â��)")                  
  (if (= bdr nil) (setq bdr (ssname (ssget "X" '((0 . "INSERT") (2 . "����(�𵨿�)"))) 0)))
  (setq pbname (strcat (prefix) "blocks/" bdname ".dwg"))
  (setq ds (getvar "dimscale"))
  (setq dstxt (strcat "1/" (rtos ds 2 0) "XP"))
  (command "zoom" "e")  
  (command "layout" "s" "")
  (command "erase" "all" "")
  (command "insert" pbname "0,0" "1" "1" "0")
  (setq b (entlast))
  (command "explode" b) ; "")
  (command "mview" "0,0" "841,594")
  (command "mspace")
  (command "zoom" dstxt)
  (command "pspace")
  (command "ltscale" "10")
  (command "mspace")
  (command "erase"  bdr "")
  (command "layout" "s" "")
  (command "pspace")
);defun  

(defun c:aa3()

  (command "layout" "s" "")
  
  (if (= llist nil) (get_filist))  ;list�ҷ�����

  (setq fn (strcase (getvar "dwgname")))
  (setq fn (substr fn 1 (- (strlen fn) 4)))  
  (setq name (nth 4 (assoc fn llist)))
;  

;  (setq bdname "����(�����)")
;  (setq bdname "����(������)")
;  (setq bdname "����(���μ�)")
;  (setq bdname "����(����ȣ)")
;  (setq bdname "����(������)")
;  (setq bdname "����(�ӹٷ�)")
;  (setq bdname "����(������)")
;  (setq bdname "����(�̱���)")
;  (setq bdname "����(������)")
;  (setq bdname "����(��â��)")                  

  (setq pbname (strcat (prefix) "blocks/����(" name ").dwg"))
  (setq ip (cdr (assoc 10 (entget (ssname (ssget "X" '((0 . "INSERT") (2 . "W")))0)))))
  
  (setq	ss (ssget "X"))  ;all
  (setq	ssv (ssget "X" '((0 . "VIEWPORT"))))
  (setq vs (ssname ss 0 ))
  (setq ss (ssdel vs ss))
;  (setq ip (cdr (assoc 10 (entget vs))))
  
  (command "erase" ss "")

;  (setq ds (getvar "dimscale"))
;  (setq dstxt (strcat "1/" (rtos ds 2 0) "XP"))
;  (command "zoom" "e")  
;  (command "layout" "s" "")
;  (command "erase" "all" "")
  
;  (command "insert" pbname "0,0" "1" "1" "0")
  (command "insert" pbname ip "1" "1" "0")  
  (setq b (entlast))
  (command "explode" b ) ;"")
;  (command "mview" "0,0" "841,594")
;  (command "mspace")
;  (command "zoom" dstxt)
;  (command "pspace")
  (command "ltscale" "10")
;  (command "mspace")
;  (command "erase"  bdr "")
;  (command "layout" "s" "")
;  (command "pspace")
);defun  


;********************************************
; function : ipnt_nblk
;            insert point of nested block
;            Yi Suk-Jong
;            1999/7/15
;********************************************
; ��� : �θ�block�� table data�� ���� �Ƶ��ϵ���
;        ���������� �����ش�. ������ ����Ʈ����, �������,
;        ���麯ȣ���� ���� ã������ �Լ��̴�.
; �Ѿ���� ��
;   pblkname : parent block name ;�θ����̸�
;   sblkname : son block name    ;�Ƶ����̸�
; �Ѿ�� ��
;   ipnt_list : insert point list;�Ƶ����� ������(insert point���� ��ǥ)
; ��)
; (ipnt_nblk "BORDERKK" "$PUL")
;     -> borderkk��� �̸��� block���� $PUL�̶� ����
;        ���Ե� ������ �������ش�.
;     -> ����ڴ� ������ ����� insert point���� ����� scale
;        ���� ���� �� insert����ǥ������ ���ϸ� $PUL���� ������ǥ��
;        ���� �� �ִ�.
;******************************

(defun ipnt_nblk(pblkname sblkname
/ tlbh bname t-1 t-list ipnt_list
)


  
  (setq tblh (tblsearch "BLOCK" pblkname))   ;table data head
  (setq base_point (cdr (assoc 10 tblh)))  ;block�� base point

  (setq t-1 (cdr (assoc -2 tblh)))           ;block�� ù entity��

  (if (= (cdr (assoc 70 tblh)) 0)
    (setq bname sblkname)
    (setq bname (strcat pblkname "|" sblkname))
  );if  
  
  (setq ipnt_list nil)                      ;insert point list�� ����

  (setq t-list (entget t-1))                ;ù��° entity

  (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;           (= (strcase (strcat pblkname "|" sblkname))
           (= (strcase bname)	   
              (cdr (assoc 2 t-list))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
  );if

  (while (setq t-1 (entnext t-1))           ;���� entity
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


(defun c:setatt()
  
  (command "layout" "s" "")
  
  (if (= llist nil) (get_filist))
  
  ;(0 filename , 1 scale , 2 page , 3 subject)
  (setq fname_full (strcase (getvar "dwgname")))   ;���ϸ�
  (setq fname (substr fname_full 1 (- (strlen fname_full) 4)))
  (setq finfo (assoc fname llist))

  (setq ndwgnum (strcat (substr fname 1 4) "-"
			 (substr fname 5 6) "-"
			 (substr fname 11 6) "-"
			 (substr fname 17 3) "-"
			 (substr fname 20 3)))

  (setq nscale (nth 1 finfo))
  
  (setq strdimscl (strcat "1 : " (rtos (getvar "dimscale") 2 0)))
  
  (setq npage (nth 2 finfo))
  (setq nsubject (nth 3 finfo))
  
  (setq	main_nb
	 (ssname (ssget "X" '((0 . "INSERT") (2 . "001"))) 0)
  )
  (setq ne (entnext main_nb))  ;next entity
  (repeat 23 (setq ne (entnext ne)))                 ;�����ȣ�»��
  (djdg_entmod 1 ndwgnum ne)
  (repeat 10 (setq ne (entnext ne)))   ;scale
;  (djdg_entmod 1 nscale ne)
  (djdg_entmod 1 strdimscl ne)  
  (repeat 1 (setq ne (entnext ne)))   ;������
  (djdg_entmod 1 npage ne)
  (repeat 2 (setq ne (entnext ne)))   ;�����ȣ(���ϴ�)
  (djdg_entmod 1 ndwgnum ne)
  (repeat 1 (setq ne (entnext ne)))   ;CAD���ϸ�
  (djdg_entmod 1 fname ne)
  (repeat 1 (setq ne (entnext ne)))   ;�����Ī
  (djdg_entmod 1 nsubject ne)

;  (repeat 23 (setq ne (entnext ne)))                 ;�����ȣ�»��  
);defun



(defun get_filist()
  (setq fn (strcat (prefix) "djdg/list.csv"))      ;file name�Է�  
;  (setq fn (strcat (prefix) "djdg/att.dat"))      ;file name�Է�
  (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file�� ���� ���
        (progn
           (setq count 1)
	   (setq llist nil)
           (while (setq ch (read-line opf))             ;������ �д´�
;              (princ (chr 13))                          ;�Է��� �޼��� ���
;              (princ count)
;              (princ " Line Processing...")
              (setq inline (data-in ch))
;              (setq lst (cons                           ;���� data�� ���� data��
;			  (strcase (nth 0 inline))   ;subject
;			  inline)) ;filename (number)
;              (setq llist (append llist (list lst)))                   ;llist�� �߰�
              (setq llist (append llist (list inline)))                   ;llist�� �߰�
	      (setq count (1+ count))                   ;line��ȣ ����
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file�� ���� ���
      ) ;of if
      (close opf)                                           ;file close
);defun


(defun djdg_entmod(ass nvalue entname)
  (setq ent (entget entname))
  (setq ovalue (cdr (assoc ass ent)))
  (entmod (subst (cons ass nvalue) (cons ass ovalue)  ent))
  (entupd entname)
);defun  