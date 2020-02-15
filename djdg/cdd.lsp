;****************************************
;*    CDD
;*              Change Dia Donut
;*              By Suk-Jong Yi
;*              1998. 2. 22
;****************************************
; Donut�� ������ �ٲ�


(defun C:CDD()

  (defun SETERR(s)                              ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)             ;���� ������ƾ ����

  (push-env)

   (initget "Dia All Select")
   (setq ans (getkword "\nDia/All/<Select>: "))

   (cond
       ((or (= ans nil) (= ans "Select"))
           (princ "\nSelect Rebar(Donut): ")
           (setq ss1 (ssget))
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((0 . "LWPOLYLINE") (-4 . ">") (40 . 0))))
       ) ;of ans=All
       ((= ans "Dia")
         (initget "Pick")  (princ "\n")
         (setq kword (getreal "Dia <Pick>: "))                   ;���ϴ� ���� dia
         (cond
           ((numberp  kword)
             (setq rod (/ kword 2.0))                             ;���ӹ�����
           ) ;of sub cond

           ((or (= kword "Pick") (= kword nil))
             (setq entl (entget (car (entsel "\nPick a Donut: "))))
             (setq rod (distance                                  ;������������
                         (getLwVert entl 0)
                         (getLwVert entl 1)
                       );distance
             );setq 
           ) ;of sub cond
         ) ;of cond
         (setq sslst (list (cons 40 rod) '(0 . "LWPOLYLINE")))    ;�ش� ���� ����
         (setq ss1 (ssget "X" sslst))
	     
       ) ;of ans="Radius"
   ); of cond


   (setq num (sslength ss1))                                ;���� ����

   (print num) (princ " found\n")
;   (princ "Old dia: ") (princ (* 2 rod))

   (initget "Dia Pick")
   (setq ndia (getreal "\nEnter New Dia/Pick: "))

   (cond
     ((numberp ndia)
       (setq ndia ndia)                                     ;������������
     ) ;of sub cond

     ((or (= ndia "Pick") (= ndia nil))
       (setq entl (entget (car (entsel "\nPick a Donut: "))))
       (setq ndia (* 2 (distance                            ;������������
                    (getLwVert entl 0)
                    (getLwVert entl 1)
                  )))
     ) ;of sub cond
   ) ;of cond

   (setq plst nil)                                          ;������ ����Ʈ����
   (setq index 0)
   (repeat num                                              ;������ŭ �ݺ�
     (setq entl (entget (ssname ss1 index)))
     (setq pnt1 (getLwVert entl 0))                             ;�������� ù��
     (setq pnt2 (getLwVert entl 1))                             ;�������� ��°��
     (setq plst (append plst (list (mid-point pnt1 pnt2))))     ;�������� �߽���
     (setq index (1+ index))
   ) ;of repeat

   (command "ERASE" ss1 "")                                 ;�������� �����

   (command "DONUT" "0" ndia)                               ;������ ��� ����

   (setq index 0)
   (repeat num                                              ;������ŭ �ݺ�
     (command (nth index plst))
     (setq index (1+ index))
   ) : of repeat

   (command "")

   (princ num)
   (princ " Modified")

  (setq *error* oer seterr nil)                             ;������ƾ ����

  (pop-env)                                                 ;ȯ�溯�� ����

) ;of defun



; -------------------------------------
; function : getLwVert
; LwPolyline�� Vertex�� ô�Ƴ�
; �μ�: vlist  : vertext list
;       tmpctr : ������ vertext ��ȣ 0,1,2
; -------------------------------------

  (defun getLwVert (vlist tmpctr /
  count tmp pt1
  )

    (setq count 0)                                      ;ù vertex ã�ư�
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )
    ;; If the counter reaches the number of vertices,
    ;; reset ctr and tmpctr to zero again.
    (if (= tmpctr (cdr (assoc 90 vlist)))
        (progn
        (setq ctr 0)
        (setq tmpctr 0)
        )
    )
    (setq tmp (nth (+ count (* tmpctr 4)) vlist))
    (setq tmp (append tmp (list(cdr (assoc 38 vlist)))))
    (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))
    (setq pt1 pt1)
  ) ;of defun

