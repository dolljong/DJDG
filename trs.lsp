;****************************************
;*    trs
;*              Copye TeXT
;*              By Suk-Jong Yi
;*              99/2/9
;****************************************

(defun C:trs(
              / llist txtlst txtlst1)     ;������������

;      (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name�Է�
      (setq fn (getstring "\nEnter file name: "))      ;file name�Է�
      (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file�� ���� ���
        (progn
           (setq count 1)
           (while (setq ch (read-line opf))             ;������ �д´�
              (princ (chr 13))                          ;�Է��� �޼��� ���
              (princ count)
              (princ " Line Processing...")
;              (setq inline (data-in ch))
              (setq llist (append llist (list ch)))                   ;llist�� �߰�
              (setq count (1+ count))                                  ;line��ȣ ����
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file�� ���� ���
      ) ;of if
      (close opf)                                           ;file close
    
  (setq nline (length llist))   ;�Էµ����� ���� ��
  (setq nrepeat (fix (/ nline 2)))    ;�ݺ�ȸ�� = ��ü��/2
  (setq i 0)
  (repeat nrepeat
    (setq txtlst (append txtlst (list (nth (* i 2) llist))))          	;0, 2, 4
    (setq txtlst1 (append txtlst1 (list (nth (1+ (* i 2)) llist))))     ;1, 3, 5
    (setq i (1+ i))
  );repeat  

  (setq sstxt (ssget "X" '((0 . "TEXT"))))   ;��� text ��
  
  (setq nss (sslength sstxt))			;ss����
  (setq index 0)

  (repeat nss					;text������ŭ �ݺ�
    (setq ent (entget (ssname sstxt index)))	;entity���� ����
    (setq oldass1 (assoc 1 ent))   		;text��������
    (setq i 0)

    (repeat (length txtlst)			;text list��ŭ �ݺ�
      (if (= (delspc (cdr oldass1))   (nth i txtlst))  	;���ں�,���� �� space�� ����� ��
        (progn
          (setq newass1 (cons 1 (nth i txtlst1)))	;
          (setq entl1 (subst newass1 oldass1 ent))         ;���ο� entity�����ۼ�
          (entmod entl1)                            ;���ο� text�� ������Ʈ
        );prog
      );if
      (setq i (1+ i))
    ); repeat
    (setq index (1+ index))
  );repeat
);defun  

;****************************************
;*    function : DELSPC
;*              DELete SPaCe
;*              By Suk-Jong Yi
;*              03/10/30
;****************************************

(defun delspc( txt / txt1 result lstr i ch)
  (setq txt1 txt)
  (setq result "")
  (setq lstr (strlen txt1))
  (setq i 1)
  (repeat lstr
    (setq ch (substr txt1 i 1))
    (if (/= ch " ")
     (setq result (strcat result ch))
    );if		  
    (setq i (1+ i))
  );repeat
  result
);defun