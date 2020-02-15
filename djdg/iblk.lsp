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

(defun C:IBLK( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left  low_right
)

  ;(push-env)                                        ;ȯ�溯�� ����


;;;(setq bname "borderbpo"
;;;      offset "688,4"
;;;      ibname "c:/program files/autocad 2002/blocks/sign1")

  (setq bname (getstring "\nInserted Block name: "))
  (setq offset (getstring "\nOffset(x,y): "))
  (setq ibname (getstring "\nBlock name to be inserted: "))

  (setq ibname (strcat (getvar "dwgprefix") ibname))
  
  (setq bn bname)
  (setq bnstr (divide_str bname ","))			;block namestr
  (setq nbn 	(length bnstr))				;number of block name
  (if (> nbn 1)
    (progn
  	(setq index 0
	      bnlst (list '(-4 . "<OR")))
  	(repeat nbn
    	  (setq bnlst (append bnlst (list (cons 2 (nth index bnstr)))))
    	  (setq index (1+ index))
  	);repeat
  	(setq bnlst (append bnlst (list '(-4 . "OR>"))))
        (setq f_list (append (list (cons 0 "INSERT")) bnlst))    ;filter list
      );True
    (progn
      (setq f_list (list (cons 0 "INSERT")  (cons 2 bn)))    ;filter list  
    );FLASE
  );if  
  (setq ss_lst (ssget "X" f_list))                      ;entity ����
  (setq ss_num (sslength ss_lst))                       ;���õ� entity����


  ;--------- ù��° border���� insert�ϱ�
  (setq index 0)                        ;ù��° border����
  (repeat ss_num                        ;���õ� border ������ŭ �ݺ�
    (setq bdr_ent (entget (ssname ss_lst index)))     ;border entity����
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
;    (setq bdrname (cdr (assoc 2 bdr_ent)))         ;border�̸�
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border�� scale factor

    (if (> ss_num 0)  ;block�� ���� ��
      (setq
             offsetv (divide_str offset ",")
	     xg1 (atof (car offsetv))
	     yg1 (atof (cadr offsetv))

	    ibpnt (list (+ (car ipnt) (* xg1 i_scale))        ;insertion point
                           (+ (cadr ipnt) (* yg1 i_scale)))
      );setq 	     
      (progn
	(princ "Block Not Found")
	(exit)
      );of false	
    );if


  (push-os)(command "INSERT" ibname ibpnt i_scale i_scale 0)(pop-os)

;    (setvar "CMDDIA" 1)                                     ;command echo ON
;    (princ pltn) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;���� border��
  ) ;of repeat

;  (pop-env)                                                 ;ȯ�溯�� ����

  (princ)
) ;of defun

  



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
