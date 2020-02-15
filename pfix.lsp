;----------------------
; program : pfix
;           PreFix
;           Yi Suk Jong
;----------------------

(defun c:pfix()
;  (setq srctxt (entget (car (entsel "\nPick Source Text: ")))) ;����text����
;  (if (= (cdr (assoc 0 srctxt)) "TEXT")
   (setq ptxt (getstring "\nEnter Prefix text: "))
;      (setq srctxt (cdr (assoc 1 srctxt)))                     ;����text����
      (princ "\nSelct destination text: ")
      (setq ss1 (ssget))                                    ;���text����
      (setq num (sslength ss1))                             ;text����

      (princ num)
      (princ " Text Found")                             ;�߰ߵ� text���� ǥ��

      (setq index 0)                                    ;�ʱ�ȭ
      (setq cnum 0)

      (repeat num                                       ;text ������ŭ �ݺ�
        (setq entl (entget (ssname ss1 index)))         ;���ƼƼ����Ȯ��
        (if (= (cdr (assoc 0 entl)) "TEXT")             ;text�ΰ�쿡��
          (progn
            (setq ass1 (assoc 1 entl))                ;���text assoc����Ÿ Ȯ��
            (setq srctxt (strcat ptxt (cdr ass1)))     ;prefix���ϱ�
	    (setq co (cons (car ass1) srctxt))        ;����text assoc����Ÿ �ۼ�
            (setq entl1 (subst co ass1 entl))         ;���ο� entity�����ۼ�
            (entmod entl1)                            ;���ο� text�� ������Ʈ
            (setq cnum (1+ cnum))                     ;������ text����
          );progn

          (princ "\nNon-Text entity was filtered")    ;text�ƴ� ���
        );if
        (setq index (1+ index))                   ;���� text��
      ) ;of repeat
;    );progn

    (princ "\Text not found")

;  );if

   (terpri)
   (princ cnum)
   (princ " Text Modified")

;  (setq *error* oer seterr nil)

   (princ)

 
);defun  
	      
